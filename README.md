\

# **MOSAIC**: The Metapopulation Outbreak Simulation with Agent-based Implementation for Cholera <a href="https://institutefordiseasemodeling.github.io/MOSAIC-docs/"><img src="man/figures/logo.png" align="right" height="130px" alt="MOSAIC website" /></a>

\ 

Welcome to the R package for the MOSAIC framework. The MOSAIC R package provides tools for simulating cholera transmission dynamics using metapopulation models, estimating mobility patterns and other drivers of transmission such as environmental forcing, and visualizing disease dynamics and outbreak projections.

Note that this repo holds code for data and models only, for full documentation see: [**https://institutefordiseasemodeling.github.io/MOSAIC-docs/**](https://institutefordiseasemodeling.github.io/MOSAIC-docs/).


## MOSAIC Project Directory Structure

The MOSAIC R package is designed to be used within a triad of Github repos where all functions are documented here at [MOSAIC-pkg](https://github.com/InstituteforDiseaseModeling/MOSAIC-pkg). The package downloads and processes all required data to be saved in [MOSAIC-data](https://github.com/InstituteforDiseaseModeling/MOSAIC-data) and prepares model quantities to run MOSAIC which are saved in [MOSAIC-pkg/model/input](https://github.com/InstituteforDiseaseModeling/MOSAIC-pkg/model/input). The package also produces all figures and tables used to make the documentation located at [MOSAIC-docs](https://github.com/InstituteforDiseaseModeling/MOSAIC-docs).

Below is the directory structure for the **MOSAIC** project:
```bash
MOSAIC/               # Local parent directory to hold 3 repositories, root directory in get_paths()
├── MOSAIC-data/      
│   ├── raw/          # Raw data files supplied to MOSAIC-pkg
│   └── processed/    # Processed data files supplied to MOSAIC-pkg
├── MOSAIC-pkg/       
│   ├── [R package]   # Contents of MOSAIC R package: https://gilesjohnr.github.io/MOSAIC-pkg/
│   └── model/
│       ├── input/    # Files used as input to MOSAIC framework
│       ├── output/   # Location of output from MOSAIC framework
│       └── LAUNCH.R  # LAUNCH.R file runs data acquisition functions, a priori models, and runs MOSAIC
└── MOSAIC-docs/      
    ├── [Website]     # Documentation and model description: https://gilesjohnr.github.io/MOSAIC-docs/
    ├── figures/      # Output images and figures from MOSAIC-pkg used in documentation
    └── tables/       # Output data and parameter values from MOSAIC-pkg used in documentation
```

## Contact 
For questions or further information about the MOSAIC project, please contact:

- **John Giles**: [john.giles@gatesfoundation.org](mailto:john.giles@gatesfoundation.org)
- **Jillian Gauld**: [jillian.gauld@gatesfoundation.org](mailto:jillian.gauld@gatesfoundation.org)
- **Rajiv Sodhi**: [rajiv.sodhi@gatesfoundation.org](mailto:rajiv.sodhi@gatesfoundation.org)
