#' Set OpenMeteo API Key
#'
#' This function sets the OpenMeteo API key and stores it in a global option called `openmeteo_api_key`.
#' If an API key is provided as an argument, it will be used. If not, the function will prompt for the key interactively.
#'
#' @param api_key A character string representing the OpenMeteo API key. If `NULL`, the function will prompt for the key interactively.
#'
#' @return A message confirming that the API key has been set, and instructions for retrieving the key.
#'
#' @details The API key is stored in a global option called `openmeteo_api_key` using `options()`.
#' The key can be retrieved at any time using `getOption("openmeteo_api_key")`.
#'
#' @examples
#' # Set the OpenMeteo API key interactively
#' set_openmeteo_api_key()
#'
#' # Set the OpenMeteo API key programmatically
#' set_openmeteo_api_key("your-api-key-here")
#'
#' # Access the API key
#' getOption("openmeteo_api_key")
#'
#' @export

set_openmeteo_api_key <- function(api_key = NULL) {

     # If no API key is provided, prompt the user to enter it interactively
     if (is.null(api_key)) {
          api_key <- readline(prompt = "Please enter your OpenMeteo API key: ")
     }

     # Check if the API key is valid (non-empty)
     if (nchar(api_key) == 0) {
          stop("API key cannot be empty. Please provide a valid API key.")
     }

     # Store the API key in global options
     options(openmeteo_api_key = api_key)

     # Confirm that the API key has been set
     message("OpenMeteo API key set successfully.")
     message("Retrieve with getOption('openmeteo_api_key')")
}
