#' Inverse Logit (Logistic) Transformation
#'
#' Applies the inverse logit (logistic) transformation to a real-valued input, mapping it from the real line (-Inf, Inf) to the interval (0, 1).
#'
#' @param x A numeric vector of real values.
#'
#' @return A numeric vector where each value is the inverse logit of the corresponding input, representing a probability in the range (0, 1).
#'
#' @details The inverse logit function is defined as:
#'   \deqn{inv\_logit(x) = \frac{e^x}{1 + e^x}}
#' This is often used to map real-valued predictions (e.g., from logistic regression) back to probabilities.
#'
#' @examples
#' inv_logit(0)     # Returns 0.5
#' inv_logit(1.39)  # Returns approximately 0.8
#' inv_logit(-1.39) # Returns approximately 0.2
#'
#' @seealso \code{\link{logit}} for the logit transformation function.
#'
#' @export
inv_logit <- function(x) {
     exp(x) / (1 + exp(x))
}
