#' Create Lagged Versions of Data
#'
#' This function generates lagged versions of all columns in a given data frame.
#' It creates new columns for each lag, allowing time series data to be structured
#' in a way that includes previous time steps as predictors.
#'
#' @param x A data frame or tibble containing the variables for which lagged versions are to be created.
#' @param lags A vector of lag values indicating how many time steps to lag the data. For example, \code{lags = 1:3}
#' will create 1-lagged, 2-lagged, and 3-lagged versions of each variable.
#'
#' @return A data frame containing the original data and the lagged versions of the variables. The lagged columns
#' are named in the format \code{<column_name>_lag_<lag>}.
#'
#' @details This function takes a data frame and creates new columns that are lagged versions of the original columns.
#' The number of lags is specified by the \code{lags} argument. The function creates new columns for each lag value,
#' appending the lag number to the original column names.
#'
#' @importFrom dplyr mutate across
#' @importFrom purrr map
#'
#' @examples
#' \dontrun{
#' # Example usage of create_lagged_data:
#' data <- data.frame(var1 = 1:10, var2 = 11:20)
#' make_lagged_data(data, lags = 1:2)
#' }
#'
#' @export

make_lagged_data <- function(x, lags) {

     # Initialize an empty list to store the lagged variables
     lag_list <- list()

     # Loop through each lag and generate lagged columns
     for (lag in lags) {

          # Lag each column and create new column names with the format "<column_name>_lag_<lag>"
          tmp <- x %>%
               mutate(across(everything(), ~lag(.x, n = lag, default = NA), .names = "{col}_lag_{lag}"))

          # Select only the lagged columns
          tmp <- tmp[, grep('lag', colnames(tmp))]

          # Append the lagged columns to the list
          lag_list[[lag]] <- tmp
     }

     # Bind the original data with the lagged columns
     out <- bind_cols(x, do.call(cbind, lag_list))

     # Sort the columns by name to ensure the lagged columns appear in a consistent order
     out <- out[, sort(colnames(out))]

     return(out)
}
