"""
NPE Backend v5.2 - Enhanced PyTorch implementation with production-ready controls
"""

import torch
import torch.nn as nn
import torch.nn.functional as F
import numpy as np
import json
import os
import logging
from typing import Dict, Tuple, Optional, List
from dataclasses import dataclass

# Configure logging
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)


# =============================================================================
# CONFIGURATION DATA CLASSES
# =============================================================================

@dataclass
class NPESpec:
    """Enhanced NPE specification with v5.2 features"""
    # Architecture
    tier: str
    embedding_dim: int
    tcn_blocks: int
    tcn_channels: int
    tcn_kernel_size: int
    attention_heads: int

    # Flow
    hidden_features: int
    num_transforms: int
    num_bins: int

    # Training
    batch_size: int
    max_epochs: int
    learning_rate: float
    gradient_clip_value: float
    scheduler_patience: int

    # Device
    device: str
    use_amp: bool

    # Metadata
    rationale: Dict

    @classmethod
    def from_dict(cls, spec_dict: Dict) -> 'NPESpec':
        """Create spec from dictionary"""
        return cls(
            tier=spec_dict['tier'],
            embedding_dim=spec_dict['embedding']['embedding_dim'],
            tcn_blocks=spec_dict['embedding']['tcn_blocks'],
            tcn_channels=spec_dict['embedding']['tcn_channels'],
            tcn_kernel_size=spec_dict['embedding'].get('tcn_kernel_size', 3),
            attention_heads=spec_dict['pooling']['attention_heads'],
            hidden_features=spec_dict['flow']['hidden_features'],
            num_transforms=spec_dict['flow']['num_transforms'],
            num_bins=spec_dict['flow']['num_bins'],
            batch_size=spec_dict['training']['batch_size'],
            max_epochs=spec_dict['training']['max_epochs'],
            learning_rate=spec_dict['optimization']['learning_rate'],
            gradient_clip_value=spec_dict['optimization']['gradient_clip_value'],
            scheduler_patience=spec_dict['optimization']['scheduler_patience'],
            device=spec_dict['device']['device_type'],
            use_amp=spec_dict['device'].get('use_amp', False),
            rationale=spec_dict.get('rationale', {})
        )


# =============================================================================
# ENHANCED ENCODER WITH GUARDS
# =============================================================================

class EnhancedSpatialEncoder(nn.Module):
    """
    Spatial encoder with long-T and large-J guards from v5.2
    """

    def __init__(self, spec: NPESpec, n_timesteps: int, n_locations: int):
        super().__init__()

        self.spec = spec
        self.n_timesteps = n_timesteps
        self.n_locations = n_locations

        # Log architecture decisions
        logger.info(f"Building encoder: T={n_timesteps}, J={n_locations}")
        logger.info(f"  TCN: {spec.tcn_blocks} blocks, {spec.tcn_channels} channels")
        logger.info(f"  Attention: {spec.attention_heads} heads")

        # Ensure GroupNorm compatibility
        tcn_channels = self._ensure_groupnorm_divisible(spec.tcn_channels)
        if tcn_channels != spec.tcn_channels:
            logger.info(f"  Adjusted TCN channels for GroupNorm: {spec.tcn_channels} -> {tcn_channels}")

        # TCN blocks with exponential dilation
        self.tcn_blocks = nn.ModuleList()
        for i in range(spec.tcn_blocks):
            dilation = 2 ** i
            block = self._make_tcn_block(
                in_channels=tcn_channels if i > 0 else n_locations,
                out_channels=tcn_channels,
                kernel_size=spec.tcn_kernel_size,
                dilation=dilation
            )
            self.tcn_blocks.append(block)

        # Attention mechanism if heads > 1
        if spec.attention_heads > 1:
            # Ensure channels divisible by heads
            if tcn_channels % spec.attention_heads != 0:
                logger.warning(f"Channels {tcn_channels} not divisible by heads {spec.attention_heads}")
                # Adjust heads down
                while spec.attention_heads > 1 and tcn_channels % spec.attention_heads != 0:
                    spec.attention_heads -= 1
                logger.info(f"  Adjusted attention heads: {spec.attention_heads}")

            self.attention = nn.MultiheadAttention(
                embed_dim=tcn_channels,
                num_heads=spec.attention_heads,
                dropout=0.1,
                batch_first=True
            )
        else:
            self.attention = None

        # Output projection
        self.output_proj = nn.Linear(tcn_channels, spec.embedding_dim)

    def _ensure_groupnorm_divisible(self, channels: int) -> int:
        """Ensure channels are divisible by a valid GroupNorm group size"""
        valid_groups = [1, 2, 4, 8, 16, 32]
        for g in reversed(valid_groups):
            if channels % g == 0 and g <= channels:
                return channels
        # Bump to next valid multiple
        for g in [8, 4, 2]:
            if g <= channels:
                return g * ((channels + g - 1) // g)
        return channels

    def _make_tcn_block(self, in_channels: int, out_channels: int,
                        kernel_size: int, dilation: int) -> nn.Module:
        """Create a TCN block with GroupNorm"""
        # Determine valid group size
        groups = 1
        for g in [8, 4, 2, 1]:
            if out_channels % g == 0:
                groups = g
                break

        return nn.Sequential(
            nn.Conv1d(in_channels, out_channels, kernel_size,
                     padding=(kernel_size - 1) * dilation // 2,
                     dilation=dilation),
            nn.GroupNorm(groups, out_channels),
            nn.ReLU(),
            nn.Dropout1d(0.1)
        )

    def forward(self, x: torch.Tensor) -> torch.Tensor:
        """
        Forward pass
        Args:
            x: Input tensor [batch, locations, timesteps]
        Returns:
            Embedded representation [batch, embedding_dim]
        """
        # Apply TCN blocks
        for block in self.tcn_blocks:
            x = block(x) + x if x.shape[1] == block[0].in_channels else block(x)

        # Apply attention if available
        if self.attention is not None:
            # Reshape for attention [batch, timesteps, channels]
            x = x.transpose(1, 2)
            x_att, _ = self.attention(x, x, x)
            x = x + x_att
            x = x.transpose(1, 2)

        # Global pooling
        x = F.adaptive_avg_pool1d(x, 1).squeeze(-1)

        # Output projection
        x = self.output_proj(x)

        return x


# =============================================================================
# ENHANCED NSF BUILDER WITH CAPS
# =============================================================================

def build_enhanced_nsf(spec: NPESpec, theta_dim: int, x_dim: int):
    """
    Build Neural Spline Flow with v5.2 enhancements and caps
    """
    from zuko.flows import NSF
    from zuko.distributions import BoxUniform

    # Log flow configuration
    logger.info(f"Building NSF: {spec.num_transforms} transforms, {spec.num_bins} bins")

    # Apply caps based on backend limitations
    num_transforms = min(spec.num_transforms, 20)
    num_bins = min(spec.num_bins, 32)

    if num_transforms != spec.num_transforms or num_bins != spec.num_bins:
        logger.warning(f"Applied caps: transforms {spec.num_transforms}->{num_transforms}, "
                      f"bins {spec.num_bins}->{num_bins}")

    # Check if ramp or override was applied
    if 'transform_ramp_details' in spec.rationale:
        logger.info(f"Transform ramp applied: {spec.rationale['transform_ramp_details']}")
    if 'flow_complexity_adjustment' in spec.rationale:
        logger.info(f"Flow complexity: {spec.rationale['flow_complexity_adjustment']}")

    # Build flow
    flow = NSF(
        features=theta_dim,
        context=x_dim,
        transforms=num_transforms,
        hidden_features=[spec.hidden_features] * 3,
        bins=num_bins,
        tail=10.0,
        base=BoxUniform(torch.zeros(theta_dim), torch.ones(theta_dim))
    )

    return flow


# =============================================================================
# ENHANCED TRAINER WITH V5.2 CONTROLS
# =============================================================================

class EnhancedNPETrainer:
    """
    NPE trainer with gradient clipping, scheduler controls, and stability safeguards
    """

    def __init__(self, spec: NPESpec, npe_model, encoder, device: str):
        self.spec = spec
        self.npe = npe_model.to(device)
        self.encoder = encoder.to(device)
        self.device = device

        # Optimizer with weight decay
        self.optimizer = torch.optim.AdamW(
            list(self.npe.parameters()) + list(self.encoder.parameters()),
            lr=spec.learning_rate,
            weight_decay=spec.spec.optimization.get('weight_decay', 1e-5)
        )

        # Scheduler with configurable patience
        self.scheduler = torch.optim.lr_scheduler.ReduceLROnPlateau(
            self.optimizer,
            mode='min',
            factor=spec.spec.optimization.get('scheduler_factor', 0.5),
            patience=spec.scheduler_patience,
            verbose=True
        )

        # Mixed precision scaler if using CUDA
        self.scaler = torch.cuda.amp.GradScaler() if spec.use_amp and device == 'cuda' else None

        # Training metrics
        self.train_losses = []
        self.val_losses = []
        self.gradient_norms = []

        logger.info(f"Trainer initialized: LR={spec.learning_rate}, "
                   f"clip={spec.gradient_clip_value}, patience={spec.scheduler_patience}")

    def train_epoch(self, train_loader, val_loader, epoch: int):
        """Train for one epoch with v5.2 enhancements"""

        # Training
        self.npe.train()
        self.encoder.train()
        epoch_loss = 0.0
        nan_count = 0
        clip_count = 0

        for batch_idx, (theta, x) in enumerate(train_loader):
            theta = theta.to(self.device)
            x = x.to(self.device)

            self.optimizer.zero_grad()

            # Mixed precision training
            if self.scaler is not None:
                with torch.autocast(device_type='cuda', dtype=torch.float16):
                    x_embedded = self.encoder(x)
                    loss = -self.npe.flow(x_embedded).log_prob(theta).mean()

                # Check for NaN/Inf BEFORE backward (v5.2 fix)
                if torch.isnan(loss) or torch.isinf(loss):
                    nan_count += 1
                    if nan_count % 10 == 0:
                        logger.warning(f"Skipped {nan_count} batches with NaN/Inf loss")
                    continue

                self.scaler.scale(loss).backward()

                # Gradient clipping
                self.scaler.unscale_(self.optimizer)
                grad_norm = torch.nn.utils.clip_grad_norm_(
                    list(self.npe.parameters()) + list(self.encoder.parameters()),
                    self.spec.gradient_clip_value
                )

                self.scaler.step(self.optimizer)
                self.scaler.update()
            else:
                # Standard precision
                x_embedded = self.encoder(x)
                loss = -self.npe.flow(x_embedded).log_prob(theta).mean()

                # Check for NaN/Inf BEFORE backward
                if torch.isnan(loss) or torch.isinf(loss):
                    nan_count += 1
                    continue

                loss.backward()

                # Gradient clipping
                grad_norm = torch.nn.utils.clip_grad_norm_(
                    list(self.npe.parameters()) + list(self.encoder.parameters()),
                    self.spec.gradient_clip_value
                )

                self.optimizer.step()

            # Track metrics
            epoch_loss += loss.item()
            self.gradient_norms.append(grad_norm.item() if isinstance(grad_norm, torch.Tensor) else grad_norm)

            if grad_norm >= self.spec.gradient_clip_value:
                clip_count += 1

        avg_train_loss = epoch_loss / len(train_loader)
        self.train_losses.append(avg_train_loss)

        # Validation
        self.npe.eval()
        self.encoder.eval()
        val_loss = 0.0

        with torch.no_grad():
            for theta, x in val_loader:
                theta = theta.to(self.device)
                x = x.to(self.device)

                x_embedded = self.encoder(x)
                loss = -self.npe.flow(x_embedded).log_prob(theta).mean()
                val_loss += loss.item()

        avg_val_loss = val_loss / len(val_loader)
        self.val_losses.append(avg_val_loss)

        # Step scheduler
        self.scheduler.step(avg_val_loss)

        # Log epoch summary
        if epoch % 10 == 0:
            logger.info(f"Epoch {epoch}: train_loss={avg_train_loss:.4f}, "
                       f"val_loss={avg_val_loss:.4f}, grad_clips={clip_count}, "
                       f"nan_batches={nan_count}")

        return avg_train_loss, avg_val_loss

    def get_training_summary(self) -> Dict:
        """Get training summary with v5.2 metrics"""
        return {
            'final_train_loss': self.train_losses[-1] if self.train_losses else None,
            'final_val_loss': self.val_losses[-1] if self.val_losses else None,
            'best_val_loss': min(self.val_losses) if self.val_losses else None,
            'gradient_norm_mean': np.mean(self.gradient_norms) if self.gradient_norms else None,
            'gradient_norm_max': np.max(self.gradient_norms) if self.gradient_norms else None,
            'gradient_clips_ratio': np.mean([g >= self.spec.gradient_clip_value
                                            for g in self.gradient_norms]) if self.gradient_norms else None
        }


# =============================================================================
# MAIN TRAINING FUNCTION
# =============================================================================

def train_npe_v5_2(spec_dict: Dict, theta_matrix: np.ndarray, x_matrix: np.ndarray,
                   output_dir: str, seed: int = 42) -> Dict:
    """
    Main training function with v5.2 enhancements

    Args:
        spec_dict: NPE specification dictionary from R
        theta_matrix: Parameter matrix [n_sims, n_params]
        x_matrix: Observation matrix [n_sims, n_features]
        output_dir: Directory to save models
        seed: Random seed

    Returns:
        Training results dictionary
    """

    # Set seeds
    torch.manual_seed(seed)
    np.random.seed(seed)

    # Parse specification
    spec = NPESpec.from_dict(spec_dict)

    # Log v5.2 features
    logger.info("=== NPE Training v5.2 ===")
    logger.info(f"Tier: {spec.tier}")
    logger.info(f"Data shape: θ={theta_matrix.shape}, x={x_matrix.shape}")

    if spec.rationale:
        if spec.rationale.get('longT_guard'):
            logger.info("Long-T guard activated")
        if spec.rationale.get('largeJ_guard'):
            logger.info("Large-J guard activated")
        if spec.rationale.get('transforms_ramp_applied'):
            logger.info("Transform ramp applied")

    # Prepare data
    device = torch.device(spec.device)
    theta_tensor = torch.tensor(theta_matrix, dtype=torch.float32, device=device)
    x_tensor = torch.tensor(x_matrix, dtype=torch.float32, device=device)

    # Infer dimensions
    n_params = theta_matrix.shape[1]
    n_features = x_matrix.shape[1]
    n_locations = spec_dict['data']['n_locations'] if 'data' in spec_dict else 1
    n_timesteps = n_features // n_locations

    # Build models
    encoder = EnhancedSpatialEncoder(spec, n_timesteps, n_locations)
    embedding_dim = spec.embedding_dim

    # Import lampe components
    from lampe.inference import NPE

    # Build flow with enhancements
    flow_builder = lambda theta_dim, x_dim: build_enhanced_nsf(spec, theta_dim, x_dim)

    npe_model = NPE(
        theta_dim=n_params,
        x_dim=embedding_dim,
        build=flow_builder
    )

    # Create trainer
    trainer = EnhancedNPETrainer(spec, npe_model, encoder, device)

    # Create data loaders
    from torch.utils.data import DataLoader, TensorDataset, random_split

    dataset = TensorDataset(theta_tensor, x_tensor)
    val_split = spec.spec.training.get('validation_split', 0.2)
    train_size = int((1 - val_split) * len(dataset))
    val_size = len(dataset) - train_size

    train_dataset, val_dataset = random_split(dataset, [train_size, val_size])

    train_loader = DataLoader(train_dataset, batch_size=spec.batch_size, shuffle=True)
    val_loader = DataLoader(val_dataset, batch_size=spec.batch_size, shuffle=False)

    # Training loop
    best_val_loss = float('inf')
    patience_counter = 0
    early_stop_patience = spec.spec.training.get('early_stopping_patience', 30)

    for epoch in range(spec.max_epochs):
        train_loss, val_loss = trainer.train_epoch(train_loader, val_loader, epoch)

        # Early stopping
        if val_loss < best_val_loss:
            best_val_loss = val_loss
            patience_counter = 0
            # Save best model
            torch.save({
                'npe_state_dict': npe_model.state_dict(),
                'encoder_state_dict': encoder.state_dict(),
                'spec': spec_dict,
                'epoch': epoch,
                'best_val_loss': best_val_loss
            }, os.path.join(output_dir, 'npe_state.pt'))
        else:
            patience_counter += 1

        if patience_counter >= early_stop_patience:
            logger.info(f"Early stopping at epoch {epoch}")
            break

    # Get training summary
    summary = trainer.get_training_summary()
    summary['epochs_trained'] = epoch + 1
    summary['early_stopped'] = patience_counter >= early_stop_patience

    # Save metadata with v5.2 information
    metadata = {
        'version': '5.2',
        'spec': spec_dict,
        'training_summary': summary,
        'data_shape': {
            'n_simulations': theta_matrix.shape[0],
            'n_parameters': n_params,
            'n_features': n_features,
            'n_locations': n_locations,
            'n_timesteps': n_timesteps
        },
        'enhancements_applied': {
            'gradient_clipping': spec.gradient_clip_value,
            'scheduler_patience': spec.scheduler_patience,
            'transform_ramp': spec.rationale.get('transforms_ramp_applied', False),
            'longT_guard': spec.rationale.get('longT_guard', {}).get('triggered', False),
            'largeJ_guard': spec.rationale.get('largeJ_guard', {}).get('triggered', False)
        }
    }

    with open(os.path.join(output_dir, 'npe_metadata.json'), 'w') as f:
        json.dump(metadata, f, indent=2, default=str)

    logger.info(f"Training complete. Best val loss: {best_val_loss:.4f}")

    return summary


if __name__ == "__main__":
    # Test the enhanced backend
    test_spec = {
        'tier': 'medium',
        'embedding': {'embedding_dim': 256, 'tcn_blocks': 6, 'tcn_channels': 96},
        'pooling': {'attention_heads': 4},
        'flow': {'hidden_features': 256, 'num_transforms': 10, 'num_bins': 12},
        'training': {'batch_size': 256, 'max_epochs': 100},
        'optimization': {'learning_rate': 1e-3, 'gradient_clip_value': 1.0, 'scheduler_patience': 15},
        'device': {'device_type': 'cpu', 'use_amp': False},
        'data': {'n_locations': 10, 'n_timesteps': 52},
        'rationale': {'transforms_ramp_applied': True}
    }

    # Test with dummy data
    theta = np.random.randn(1000, 20)
    x = np.random.randn(1000, 520)  # 10 locations x 52 timesteps

    result = train_npe_v5_2(test_spec, theta, x, './test_output', seed=42)
    print("Training result:", result)