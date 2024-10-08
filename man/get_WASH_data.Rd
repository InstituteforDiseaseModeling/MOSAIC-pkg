% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_WASH_data.R
\name{get_WASH_data}
\alias{get_WASH_data}
\title{Download, Process, and Save WASH Coverage Data}
\usage{
get_WASH_data(PATHS)
}
\arguments{
\item{PATHS}{A list containing the paths to where WASH data should be saved. Typically generated by the \code{get_paths()} function and should include:
\itemize{
\item \strong{DATA_WASH}: Path to the directory where the processed WASH data will be saved.
}}
}
\value{
The function does not return a value but processes the WASH coverage data and saves it to the specified directory as a CSV file.
}
\description{
This function processes WASH (Water, Sanitation, and Hygiene) coverage data for African countries from Sikder et al. (2023). The data includes indicators of water and sanitation access, including piped water, improved sanitation, unimproved water sources, and open defecation. The processed data is saved as a CSV file in the specified path.
}
\details{
The WASH coverage data is based on percentages of the population with access to piped water, other improved water sources, septic or sewer sanitation, and other improved sanitation. The data also includes the proportions of the population using unimproved water sources, surface water, unimproved sanitation, and practicing open defecation.

The risk-related variables (unimproved water, surface water, unimproved sanitation, and open defecation) are converted to a complement (1 - value) to reflect protective factors. All data are scaled between 0 and 1.

Data Source: Sikder et al. (2023). Water, Sanitation, and Hygiene Coverage and Cholera Incidence in Sub-Saharan Africa. \doi{10.1021/acs.est.3c01317}.
}
\examples{
\dontrun{
# Define paths for saving WASH data
PATHS <- get_paths()

# Download, process, and save WASH coverage data
get_WASH_data(PATHS)
}

}
