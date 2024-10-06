#' Logit Transformation
#'
#' Applies the logit transformation to a probability value, mapping it from the interval (0, 1) to the real line (-Inf, Inf).
#'
#' @param p A numeric vector of probabilities, where each value must be in the interval (0, 1).
#'
#' @return A numeric vector where each value is the logit of the corresponding input probability. The output will be in the range (-Inf, Inf).
#'
#' @details The logit function is defined as:
#'   \deqn{logit(p) = \log\left(\frac{p}{1 - p}\right)}
#' This is commonly used in logistic regression and other models where probabilities need to be mapped to a real-valued scale.
#'
#' @examples
#' logit(0.5)   # Returns 0
#' logit(0.8)   # Positive value
#' logit(0.2)   # Negative value
#'
#' @seealso \code{\link{inv_logit}} for the inverse logit (logistic) function.
#'
#' @export
logit <- function(p) {
     log(p / (1 - p))
}
