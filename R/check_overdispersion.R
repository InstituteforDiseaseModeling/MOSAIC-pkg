#' Check for overdispersion in a count time series
#'
#' This function checks for overdispersion in a univariate count time series using:
#' (1) a Poisson GLM with intercept only, and
#' (2) an optional Poisson GLM with 2-harmonic seasonal Fourier terms based on day-of-year.
#'
#' Overdispersion is assessed using the Pearson dispersion statistic and the
#' variance-to-mean ratio. If the Pearson statistic exceeds 1.5, the series is flagged
#' as overdispersed.
#'
#' @param y A numeric vector of non-negative integer counts (e.g., cases or deaths).
#' @param date A vector of class \code{Date} corresponding to \code{y}.
#'             Required if \code{seasonality = TRUE}.
#' @param seasonality Logical; if \code{TRUE}, also fits a GLM with 2-harmonic seasonal terms. Default = \code{FALSE}.
#' @param period Integer; seasonal period (default = 365 for daily data).
#' @param print Logical; if \code{TRUE}, prints a summary of results. Default = \code{TRUE}.
#'
#' @return A list with dispersion metrics:
#' \item{dispersion_ratio}{Variance-to-mean ratio}
#' \item{pearson_dispersion_intercept}{Pearson statistic for intercept-only model}
#' \item{pearson_dispersion_seasonal}{Pearson statistic for seasonal model (if applicable)}
#' \item{overdispersed_intercept}{Logical; TRUE if intercept model dispersion > 1.5}
#' \item{overdispersed_seasonal}{Logical; TRUE if seasonal model dispersion > 1.5 (if applicable)}
#' \item{n_obs}{Number of non-missing observations}
#'
#' @examples
#' \dontrun{
#'   check_overdispersion(y = c(0, 2, 1, 0, 1, 3, 2, 0))
#'   dates <- as.Date("2023-03-01") + 0:13
#'   cases <- c(0, 1, 2, 0, 2, 4, 3, 1, 0, 5, 2, 3, 1, 0)
#'   check_overdispersion(y = cases, date = dates, seasonality = TRUE)
#' }
#'
#' @export
#'

check_overdispersion <- function(y,
                                 date = NULL,
                                 seasonality = FALSE,
                                 period = 365,
                                 print = TRUE) {

     if (!is.numeric(y)) stop("'y' must be numeric.")
     if (any(y < 0, na.rm = TRUE) || any(y %% 1 != 0, na.rm = TRUE)) {
          stop("'y' must contain non-negative integers.")
     }

     # Filter NA values
     if (seasonality) {
          if (is.null(date)) stop("If seasonality = TRUE, 'date' must be provided.")
          if (!inherits(date, "Date")) stop("'date' must be of class 'Date'.")
          if (length(y) != length(date)) stop("'y' and 'date' must be the same length.")
          valid_idx <- which(!is.na(y))
          y_clean <- y[valid_idx]
          date_clean <- date[valid_idx]
          doy <- as.numeric(format(date_clean, "%j"))
     } else {
          y_clean <- y[!is.na(y)]
     }

     if (length(y_clean) < 2) {
          stop("Not enough non-missing data to assess overdispersion.")
     }

     # Intercept-only model
     df_intercept <- data.frame(y = y_clean)
     model_intercept <- glm(y ~ 1, family = poisson, data = df_intercept)
     pearson_disp_int <- sum(residuals(model_intercept, type = "pearson")^2) / df.residual(model_intercept)
     overdispersed_int <- pearson_disp_int > 1.5

     # Simple variance-to-mean ratio
     mu <- mean(y_clean)
     s2 <- var(y_clean)
     disp_ratio <- s2 / mu

     # Optional seasonal model
     pearson_disp_season <- NA
     overdispersed_season <- NA
     if (seasonality && length(y_clean) >= 5) {
          df_seasonal <- data.frame(
               y = y_clean,
               cos1 = cos(2 * pi * doy / period),
               sin1 = sin(2 * pi * doy / period),
               cos2 = cos(4 * pi * doy / period),
               sin2 = sin(4 * pi * doy / period)
          )

          model_seasonal <- glm(y ~ cos1 + sin1 + cos2 + sin2, family = poisson, data = df_seasonal)
          pearson_disp_season <- sum(residuals(model_seasonal, type = "pearson")^2) / df.residual(model_seasonal)
          overdispersed_season <- pearson_disp_season > 1.5
     }

     # Print summary
     if (print) {
          cat("Mean:", round(mu, 2), " | Variance:", round(s2, 2), "\n")
          cat("Dispersion Ratio (Var / Mean):", round(disp_ratio, 2), "\n")
          cat("Pearson Dispersion (intercept-only):", round(pearson_disp_int, 2), "\n")
          if (overdispersed_int) {
               cat("→ Overdispersion detected (intercept-only model)\n")
          } else {
               cat("→ No strong overdispersion (intercept-only model)\n")
          }
          if (seasonality && !is.na(pearson_disp_season)) {
               cat("Pearson Dispersion (seasonal model):", round(pearson_disp_season, 2), "\n")
               if (overdispersed_season) {
                    cat("→ Overdispersion remains after adjusting for seasonality\n")
               } else {
                    cat("→ No strong overdispersion after seasonal adjustment\n")
               }
          }
     }

     return(invisible(list(
          dispersion_ratio = disp_ratio,
          pearson_dispersion_intercept = pearson_disp_int,
          pearson_dispersion_seasonal = pearson_disp_season,
          overdispersed_intercept = overdispersed_int,
          overdispersed_seasonal = overdispersed_season,
          n_obs = length(y_clean)
     )))
}
