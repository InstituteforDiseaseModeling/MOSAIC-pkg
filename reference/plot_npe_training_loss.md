# Plot NPE Training Loss Curves

Creates visualizations of training and validation loss over epochs for
NPE models. This function reads the training history from the NPE model
directory and generates plots to assess model convergence and potential
overfitting.

## Usage

``` r
plot_npe_training_loss(
  npe_dirs,
  output_file = NULL,
  plot_width = 10,
  plot_height = 8,
  show_best_epoch = TRUE,
  smooth_curves = FALSE,
  log_scale = FALSE,
  verbose = TRUE
)
```

## Arguments

- npe_dirs:

  List with model component (containing training_history.rds) or string
  path to NPE directory

- output_file:

  Optional path to save the plot. If NULL, plot is returned but not
  saved.

- plot_width:

  Width of the saved plot in inches (default 10)

- plot_height:

  Height of the saved plot in inches (default 6)

- show_best_epoch:

  Whether to highlight the epoch with best validation loss (default
  TRUE)

- smooth_curves:

  Whether to apply smoothing to the loss curves (default FALSE)

- log_scale:

  Whether to use log scale for the y-axis (default FALSE)

- verbose:

  Whether to print progress messages (default TRUE)

## Value

ggplot2 object containing the training loss visualization

## Details

This function loads the training history from training_history.rds and
creates publication-quality plots showing:

1.  Training and validation loss curves over epochs

2.  Best validation loss point (if show_best_epoch = TRUE)

3.  Convergence diagnostics and potential overfitting indicators

The plot helps identify:

- Training convergence

- Overfitting (validation loss increasing while training loss decreases)

- Optimal stopping points

- Learning rate effectiveness

## Examples

``` r
if (FALSE) { # \dontrun{
# Plot training curves for NPE model
loss_plot <- plot_npe_training_loss(
  npe_dirs = list(model = "./results/npe/model"),
  output_file = "./figures/npe_training_loss.png"
)

# Display the plot
print(loss_plot)
} # }
```
