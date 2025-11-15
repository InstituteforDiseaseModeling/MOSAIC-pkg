# Running MOSAIC

``` r

if (!requireNamespace("devtools", quietly = TRUE)) install.packages("devtools")

devtools::install_github("InstituteforDiseaseModeling/MOSAIC-pkg", dependencies = TRUE, force = TRUE)

library(MOSAIC)

lc <- reticulate::import("laser_cholera.metapop.model")
params <- MOSAIC::default_config

model <- lc$run_model(
     quiet = FALSE,
     paramfile = params
)

names(model$params)
names(model$patches)
```
