#' Fourier Series Model with Two Harmonics (Sine-Cosine Form)
#'
#' This function implements a generalized Fourier series model with two harmonics in the sine-cosine form. The model is often used to capture seasonal or periodic dynamics in time series data, such as temperature, precipitation, or disease cases.
#'
#' @param t A numeric vector representing time points (e.g., day of the year or week of the year).
#' @param beta0 A numeric value representing the intercept term (fixed at 0 by default).
#' @param a1 A numeric value representing the amplitude of the first cosine term (first harmonic).
#' @param b1 A numeric value representing the amplitude of the first sine term (first harmonic).
#' @param a2 A numeric value representing the amplitude of the second cosine term (second harmonic).
#' @param b2 A numeric value representing the amplitude of the second sine term (second harmonic).
#' @param p A numeric value representing the period of the seasonal cycle (e.g., 52 for weekly data over a year).
#'
#' @return A numeric vector of predicted values based on the Fourier series model.
#'
#' @details The model is defined as follows:
#' \deqn{\beta_t = \beta_0 + a_1 \cos\left(\frac{2 \pi t}{p}\right) + b_1 \sin\left(\frac{2 \pi t}{p}\right) + a_2 \cos\left(\frac{4 \pi t}{p}\right) + b_2 \sin\left(\frac{4 \pi t}{p}\right)}
#' The model includes an intercept term \code{beta0} (set to 0 by default) and two harmonics with coefficients \code{a1}, \code{b1}, \code{a2}, and \code{b2}. The period \code{p} controls the periodicity of the series.
#'
#' This function is based on the sine-cosine form of the Fourier series. For more details, see the Wikipedia page on [Fourier series](https://en.wikipedia.org/wiki/Fourier_series).
#'
#' This function is commonly used for modeling seasonal dynamics in environmental or epidemiological data, where periodic patterns are observed.
#'
#' @examples
#' \dontrun{
#' # Example usage with weekly data (p = 52 weeks in a year)
#' time_points <- 1:52
#' beta0 <- 0
#' a1 <- 1.5
#' b1 <- -0.5
#' a2 <- 0.8
#' b2 <- 0.3
#' p <- 52
#'
#' predictions <- fourier_series_double(time_points, beta0, a1, b1, a2, b2, p)
#' print(predictions)
#' }
#'
#' @export

fourier_series_double <- function(t, beta0, a1, b1, a2, b2, p) {
     beta0 +
          a1 * cos(2 * pi * t / p) +
          b1 * sin(2 * pi * t / p) +
          a2 * cos(4 * pi * t / p) +
          b2 * sin(4 * pi * t / p)
}
