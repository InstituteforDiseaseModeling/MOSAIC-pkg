#' Get Template for Parameter Values
#'
#' This function generates a template long-form data frame for parameter values, including origin (`i`), destination (`j`), and time (`t`), used to simulate variables such as sigma.
#'
#' @param variable_name A character string representing the name of the variable (e.g., 'sigma').
#' @param variable_description A character string describing the variable (e.g., 'proportion symptomatic').
#' @param parameter_distribution A character string representing the distribution of the parameter (e.g., 'beta').
#' @param i A vector representing the origin locations.
#' @param j A vector representing the destination locations.
#' @param t A vector representing the time points.
#' @param parameter_name A character vector of parameter names (e.g., shape1, shape2 for a beta distribution).
#' @param parameter_value A numeric vector of parameter values (corresponding to the parameter names).
#'
#' @return A data frame containing the long-form template for the parameter values. If all arguments are `NULL`, an empty data frame is returned.
#'
#' @examples
#' # Example usage for sigma
#' prm <- list(shape1 = 2, shape2 = 5)
#' param_df <- make_param_df("sigma", "proportion symptomatic", "beta",
#'                           i = "A", j = "B", t = 1, names(prm), unlist(prm))
#' print(param_df)
#'
#' @export

make_param_df <- function(variable_name = NULL,
                          variable_description = NULL,
                          parameter_distribution = NULL,
                          i = NULL,
                          j = NULL,
                          t = NULL,
                          parameter_name = NULL,
                          parameter_value = NULL) {

     # If all arguments are NULL, return an empty data frame
     if (is.null(variable_name) && is.null(variable_description) &&
         is.null(parameter_distribution) && is.null(i) && is.null(j) && is.null(t) &&
         is.null(parameter_name) && is.null(parameter_value)) {

          return(data.frame(
               variable_name = character(0),
               variable_description = character(0),
               parameter_distribution = character(0),
               i = character(0),
               j = character(0),
               t = numeric(0),
               parameter_name = character(0),
               parameter_value = numeric(0)
          ))
     }

     # Assign NA to any missing specific arguments
     if (is.null(variable_name)) variable_name <- NA
     if (is.null(variable_description)) variable_description <- NA
     if (is.null(parameter_distribution)) parameter_distribution <- NA
     if (is.null(i)) i <- NA
     if (is.null(j)) j <- NA
     if (is.null(t)) t <- NA
     if (is.null(parameter_name)) parameter_name <- NA
     if (is.null(parameter_value)) parameter_value <- NA

     # Create the data frame with the provided inputs
     param_df <- data.frame(
          variable_name = variable_name,
          variable_description = variable_description,
          parameter_distribution = parameter_distribution,
          i = i,
          j = j,
          t = t,
          parameter_name = parameter_name,
          parameter_value = parameter_value,
          row.names = NULL
     )

     return(param_df)
}
