% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/run_LASER_model.R
\name{run_LASER_model}
\alias{run_LASER_model}
\title{Run LASER Model Simulation}
\usage{
run_LASER_model(
  paramfile,
  seed = 123L,
  visualize = FALSE,
  pdf = FALSE,
  outdir = tempdir(),
  py_module = NULL
)
}
\arguments{
\item{paramfile}{Character. Path to the LASER parameter file (e.g., YAML or JSON) used to configure the simulation.}

\item{seed}{Integer. Random seed for reproducibility. Defaults to 123L.}

\item{visualize}{Logical. If TRUE, generate and display visualizations during the run. Defaults to FALSE.}

\item{pdf}{Logical. If TRUE, save visualizations as PDF files. Defaults to FALSE.}

\item{outdir}{Character. Directory where LASER outputs (e.g., logs, results) will be written. Defaults to a temporary directory.}

\item{py_module}{Python module. An optional pre-loaded reticulate LASER module. If NULL, the module is imported via reticulate::import("laser").}
}
\value{
A Python object (reticulate) representing the LASER model simulation results.
}
\description{
A wrapper function to execute a LASER model simulation via the Python interface (reticulate).
}
\examples{
\dontrun{
# Run with default settings:
result <- run_LASER_model(
  paramfile = "path/to/laser_params.yml",
  seed      = 20250418L,
  visualize = FALSE,
  pdf       = FALSE,
  outdir    = "./laser_output"
)

# Inspect results:
print(result)
}

}
