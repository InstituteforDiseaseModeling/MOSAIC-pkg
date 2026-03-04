FROM rocker/geospatial:4.4

LABEL maintainer="MOSAIC Team"
LABEL description="MOSAIC for Coiled.io - Auto-scaling cloud workflows"
LABEL version="0.13.24"

# Prevent interactive prompts during build
ENV DEBIAN_FRONTEND=noninteractive

# Set environment variables for threading (critical for MOSAIC - see CLAUDE.md)
ENV OMP_NUM_THREADS=1
ENV MKL_NUM_THREADS=1
ENV NUMBA_NUM_THREADS=1
ENV TBB_NUM_THREADS=1
ENV KMP_DUPLICATE_LIB_OK=TRUE
ENV R_DATATABLE_NUM_THREADS=1

# ============================================================
# Install additional system dependencies
# ============================================================
# Note: rocker/geospatial already includes:
#   - R 4.4+, build-essential, gfortran, cmake
#   - GDAL, PROJ, GEOS, UDUNITS
#   - sf, terra, raster packages
#
# Add MOSAIC-specific requirements:
# ============================================================

RUN apt-get update && apt-get install -y --no-install-recommends \
    # HDF5 for LASER simulation outputs
    libhdf5-dev \
    zlib1g-dev \
    # Python 3.9+ for LASER-cholera
    python3 \
    python3-pip \
    python3-venv \
    python3-dev \
    software-properties-common \
    # Graphics/font libraries for R plotting
    libfontconfig1-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    libfreetype6-dev \
    libpng-dev \
    libtiff5-dev \
    libjpeg-dev \
    # Git for GitHub installation
    git \
    ca-certificates \
    wget \
    curl \
    && rm -rf /var/lib/apt/lists/*

# ============================================================
# Ensure Python >= 3.9
# ============================================================
RUN PYTHON_VERSION=$(python3 --version 2>&1 | grep -oP '\d+\.\d+' | head -1) && \
    PYTHON_MAJOR=$(echo $PYTHON_VERSION | cut -d. -f1) && \
    PYTHON_MINOR=$(echo $PYTHON_VERSION | cut -d. -f2) && \
    if [ "$PYTHON_MAJOR" -lt 3 ] || ([ "$PYTHON_MAJOR" -eq 3 ] && [ "$PYTHON_MINOR" -lt 9 ]); then \
        echo "Python $PYTHON_VERSION detected, installing Python 3.9..." && \
        add-apt-repository -y ppa:deadsnakes/ppa && \
        apt-get update && \
        apt-get install -y python3.9 python3.9-venv python3.9-dev && \
        update-alternatives --install /usr/bin/python3 python3 /usr/bin/python3.9 1 && \
        rm -rf /var/lib/apt/lists/*; \
    else \
        echo "Python $PYTHON_VERSION detected (OK)"; \
    fi

# ============================================================
# ============================================================
# Install critical R packages that often fail (from DESCRIPTION Imports)
# ============================================================
RUN R -e "options(repos = c(CRAN = 'https://cloud.r-project.org')); \
    if (!requireNamespace('remotes', quietly = TRUE)) { \
        install.packages('remotes'); \
    }; \
    # Install packages that need compilation or often fail \
    install.packages(c( \
        'arrow', \
        'exactextractr', \
        'hdf5r', \
        'elevatr', \
        'rnaturalearth', \
        'rnaturalearthdata', \
        'countrycode', \
        'dbscan', \
        'FNN', \
        'gridExtra', \
        'viridis', \
        'cowplot', \
        'furrr', \
        'slider', \
        'RhpcBLASctl', \
        'truncnorm' \
    )); \
    # Install older ggrepel compatible with R 4.4 \
    remotes::install_version('ggrepel', version = '0.9.5', repos = 'https://cloud.r-project.org'); \
    cat('✓ Critical R packages installed\n')"

# ============================================================
# Install MOSAIC R package from GitHub
# ============================================================
RUN R -e "options(repos = c(CRAN = 'https://cloud.r-project.org')); \
    remotes::install_github('InstituteforDiseaseModeling/MOSAIC-pkg', \
        ref = 'main', \
        dependencies = TRUE, \
        upgrade = 'never', \
        force = TRUE); \
    # Verify MOSAIC actually installed \
    if (!requireNamespace('MOSAIC', quietly = FALSE)) { \
        stop('MOSAIC installation failed!'); \
    }; \
    cat('✓ MOSAIC R package installed\n')"

# ============================================================
# Install Python dependencies (LASER-cholera, PyTorch, NPE)
# ============================================================
# This creates conda environment: ~/.virtualenvs/r-mosaic
# Packages: laser-cholera==0.9.1, pytorch==2.1.2, sbi==0.22.0, zuko, lampe
RUN R -e "MOSAIC::install_dependencies(force = TRUE)"

# ============================================================
# Install Dask/Distributed/Coiled for orchestration
# ============================================================
# Install to r-mosaic virtualenv (created by install_dependencies)
# coiled is needed when this container is used as the R orchestrator
# (run_MOSAIC_dask connects to Coiled.io via reticulate::import("coiled"))
RUN /root/.virtualenvs/r-mosaic/bin/pip install --no-cache-dir \
    dask \
    distributed \
    coiled

# ============================================================
# Fix OpenSSL version mismatch
# ============================================================
# The r-mosaic virtualenv's Python was compiled against OpenSSL 3.3.0,
# but the Ubuntu base ships libcrypto 3.0.x which lacks the OPENSSL_3.3.0
# versioned symbol. Copy the newer libs from the virtualenv to the system
# path so all processes (R via libcurl and Python via _ssl) use the same
# OpenSSL. OpenSSL 3.3 is ABI-backward-compatible with 3.0.
RUN if [ -f /root/.virtualenvs/r-mosaic/lib/libcrypto.so.3 ]; then \
      cp /root/.virtualenvs/r-mosaic/lib/libcrypto.so.3 \
         /usr/lib/x86_64-linux-gnu/libcrypto.so.3 && \
      cp /root/.virtualenvs/r-mosaic/lib/libssl.so.3 \
         /usr/lib/x86_64-linux-gnu/libssl.so.3 && \
      ldconfig && \
      echo "✓ OpenSSL updated to $(openssl version)"; \
    else \
      echo "⚠ virtualenv libcrypto not found, skipping OpenSSL fix"; \
    fi

# ============================================================
# Verify MOSAIC installation
# ============================================================
RUN R -e "library(MOSAIC); \
    result <- tryCatch({ \
        MOSAIC::check_dependencies(); \
        cat('✓ MOSAIC installation verified successfully\n'); \
        cat('✓ Version:', as.character(packageVersion('MOSAIC')), '\n'); \
        TRUE \
    }, error = function(e) { \
        cat('ERROR:', e\$message, '\n'); \
        FALSE \
    }); \
    if (!result) quit(status = 1)"

# ============================================================
# Clone MOSAIC-data repository
# ============================================================
# Include data in image for direct access (no mounting needed)
RUN git clone --depth 1 \
    https://github.com/InstituteforDiseaseModeling/MOSAIC-data \
    /workspace/MOSAIC/MOSAIC-data && \
    echo "✓ MOSAIC-data cloned ($(du -sh /workspace/MOSAIC/MOSAIC-data | cut -f1))"

# Create MOSAIC-pkg directory structure
# (Workers only need model input/output directories)
RUN mkdir -p /workspace/MOSAIC/MOSAIC-pkg/model/input && \
    mkdir -p /workspace/MOSAIC/MOSAIC-pkg/model/output && \
    echo "✓ MOSAIC-pkg directory structure created"

# ============================================================
# Setup for Coiled workers
# ============================================================
# Add r-mosaic conda environment to PATH so Coiled can find Python/dask
ENV PATH=/root/.virtualenvs/r-mosaic/bin:$PATH

WORKDIR /workspace

# Create additional directories for MOSAIC workflows
RUN mkdir -p /workspace/model/input \
             /workspace/model/output \
             /workspace/data

# Default command for Coiled workers
CMD ["/bin/bash"]

# ============================================================
# Notes for Coiled.io usage:
# ============================================================
# This image is optimized for Coiled cloud workers:
#   - MOSAIC pre-installed (no runtime setup needed)
#   - Python environment ready (laser-cholera, pytorch, sbi)
#   - Dask/distributed for worker communication
#   - Threading limits set for parallel execution
#
# Create Coiled environment:
#   coiled.create_software_environment(
#       name='mosaic-docker-workers',
#       container='YOUR_REGISTRY/mosaic-worker:latest',
#       region_name='westus2'
#   )
#
# Workers start in <1 minute with MOSAIC fully functional!
# ============================================================
