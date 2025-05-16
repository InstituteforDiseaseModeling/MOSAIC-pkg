#' Estimate Spatial Pattern of Human Transmission Rate (β_hum)
#'
#' Fits a negative‐binomial spatial random‐field model for the location‐specific transmission
#' parameter \eqn{\beta^{hum}} using an INLA SPDE mesh. Requires only total incidence,
#' population size, and observation window per site plus their coordinates.
#'
#' @param location_name Character vector of site identifiers.
#' @param longitude Numeric vector of site x‐coordinates (projected).
#' @param latitude Numeric vector of site y‐coordinates (projected).
#' @param incidence_total Numeric vector of total incidence per site.
#' @param population_size Numeric vector of population size per site.
#' @param time_span Numeric vector (or scalar) of observation window length (days) per site.
#' @return A list with:
#'   \itemize{
#'     \item{relative_multiplier:}{ Numeric vector of site‐level multipliers \eqn{\exp(u_j)}.}
#'     \item{model:}{ Fitted INLA model object.}
#'   }
#' @importFrom INLA inla inla.mesh.2d inla.spde2.pcmatern inla.spde.make.A inla.stack inla.stack.data inla.stack.A
#' @export
#' @examples
#' \dontrun{
#' res <- est_transmission_spatial_structure(
#'      location_name   = c("A","B"),
#'      longitude       = c(0,10),
#'      latitude        = c(0,5),
#'      incidence_total = c(5,3),
#'      population_size = c(1000,800),
#'      time_span       = c(14,14)
#' )
#'
#' plot(factor(c("A","B")), res$relative_multiplier)
#' abline(h=1, col='red', lty=2)
#'
#' tmp <- MOSAIC::config_simulation_epidemic
#'
#' res <- est_transmission_spatial_structure(
#'      location_name   = tmp$location_name,
#'      longitude       = tmp$longitude,
#'      latitude        = tmp$latitude,
#'      incidence_total = rowSums(tmp$reported_cases),
#'      population_size = tmp$N_j_initial,
#'      time_span       = ncol(tmp$reported_cases)
#' )
#'
#' plot(factor(tmp$location_name), res$relative_multiplier)
#' abline(h=1, col='red', lty=2)
#' }
#'

est_transmission_spatial_structure <- function(
          location_name,
          longitude,
          latitude,
          incidence_total,
          population_size,
          time_span
) {
     if (!requireNamespace("INLA", quietly = TRUE)) {
          stop("Package 'INLA' is required but not installed.")
     }
     message("[1/7] Assembling data frame...")
     df <- data.frame(
          location_name    = location_name,
          longitude        = longitude,
          latitude         = latitude,
          incidence_total  = incidence_total,
          population_size  = population_size,
          time_span        = time_span,
          stringsAsFactors = FALSE
     )
     message("[2/7] Building INLA mesh...")
     mesh <- INLA::inla.mesh.2d(
          loc      = as.matrix(df[, c("longitude", "latitude")]),
          max.edge = c(40, 150),
          cutoff   = 5,
          offset   = c(50, 100)
     )
     message("[3/7] Defining SPDE model...")
     spde <- INLA::inla.spde2.pcmatern(
          mesh        = mesh,
          prior.range = c(50, 0.5),
          prior.sigma = c(1,   0.01)
     )
     message("[4/7] Projecting observations and building stack...")
     A_obs <- INLA::inla.spde.make.A(
          mesh,
          loc = as.matrix(df[, c("longitude", "latitude")])
     )
     stk   <- INLA::inla.stack(
          data    = list(inc = df$incidence_total),
          A       = list(A_obs, 1),
          effects = list(
               spatial   = 1:spde$n.spde,
               intercept = rep(1, nrow(df))
          )
     )
     message("[5/7] Fitting negative‐binomial spatial model...")
     res <- INLA::inla(
          formula           = incidence_total ~ 0 + intercept + f(spatial, model = spde),
          data              = INLA::inla.stack.data(stk),
          family            = "nbinomial",
          offset            = log(df$population_size * df$time_span),
          control.predictor = list(A = INLA::inla.stack.A(stk), compute = TRUE),
          control.family    = list(
               hyper = list(theta = list(prior = "loggamma", param = c(1, 0.01)))
          )
     )
     message("[6/7] Extracting spatial random effects...")
     w_mesh <- res$summary.random$spatial$mean
     A_all  <- INLA::inla.spde.make.A(
          mesh,
          loc = as.matrix(df[, c("longitude", "latitude")])
     )
     u_hat  <- as.vector(A_all %*% w_mesh)
     r_hat  <- exp(u_hat)
     message("[7/7] Done. Returning results.")
     list(
          relative_multiplier = r_hat,
          model               = res
     )
}
