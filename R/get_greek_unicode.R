#' Get Unicode Hex Key for a Greek Letter
#'
#' This function returns the Unicode hex key for a specified Greek letter name.
#' The user can choose whether the returned letter should be in uppercase or lowercase.
#'
#' @param letter_name A character string representing the name of the Greek letter.
#'                    Accepted values are the English names of the Greek letters (e.g., "alpha", "beta", "gamma").
#' @param uppercase A logical argument indicating whether to return the uppercase version of the Greek letter (default is `TRUE`).
#'
#' @return A character string representing the Unicode hex key for the specified Greek letter.
#'         This can be used in plots, titles, or text where Greek symbols are needed.
#'
#' @details This function allows users to obtain the Unicode hex representation of any common Greek letter
#'          by its name. The function supports both uppercase and lowercase Greek letters. The returned hex key
#'          can be inserted directly into strings or titles, for example, in `ggplot2` plots.
#'
#' @examples
#' # Get the Unicode for uppercase Alpha
#' get_greek_unicode("alpha", TRUE)
#'
#' # Get the Unicode for lowercase Omega
#' get_greek_unicode("omega", FALSE)
#'
#' # Get the Unicode for uppercase Sigma
#' get_greek_unicode("sigma", TRUE)
#'
#' # Use in ggplot2 title
#' ggplot() + labs(title = paste("Example with Greek letter", get_greek_unicode("Psi", TRUE)))
#'
#' @seealso
#' \code{\link{ggplot2}} for using the Unicode symbols in plot titles.
#'
#' @export

get_greek_unicode <- function(letter_name, uppercase = TRUE) {

     # Define a named list for uppercase Greek letters
     greek_uppercase <- list(
          Alpha = "\u0391", Beta = "\u0392", Gamma = "\u0393", Delta = "\u0394", Epsilon = "\u0395",
          Zeta = "\u0396", Eta = "\u0397", Theta = "\u0398", Iota = "\u0399", Kappa = "\u039A",
          Lambda = "\u039B", Mu = "\u039C", Nu = "\u039D", Xi = "\u039E", Omicron = "\u039F",
          Pi = "\u03A0", Rho = "\u03A1", Sigma = "\u03A3", Tau = "\u03A4", Upsilon = "\u03A5",
          Phi = "\u03A6", Chi = "\u03A7", Psi = "\u03A8", Omega = "\u03A9"
     )

     # Define a named list for lowercase Greek letters
     greek_lowercase <- list(
          Alpha = "\u03B1", Beta = "\u03B2", Gamma = "\u03B3", Delta = "\u03B4", Epsilon = "\u03B5",
          Zeta = "\u03B6", Eta = "\u03B7", Theta = "\u03B8", Iota = "\u03B9", Kappa = "\u03BA",
          Lambda = "\u03BB", Mu = "\u03BC", Nu = "\u03BD", Xi = "\u03BE", Omicron = "\u03BF",
          Pi = "\u03C0", Rho = "\u03C1", Sigma = "\u03C3", Tau = "\u03C4", Upsilon = "\u03C5",
          Phi = "\u03C6", Chi = "\u03C7", Psi = "\u03C8", Omega = "\u03C9"
     )

     # Normalize the letter name to capitalize the first letter and lowercase the rest
     letter_name <- gsub("(^[[:alpha:]])", "\\U\\1", tolower(letter_name), perl = TRUE)

     # Return the corresponding Unicode for the letter depending on 'uppercase' argument
     if (uppercase) {
          return(greek_uppercase[[letter_name]])
     } else {
          return(greek_lowercase[[letter_name]])
     }
}
