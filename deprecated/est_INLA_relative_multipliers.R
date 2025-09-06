#' Estimate INLA Spatial Relative Multipliers
#'
#' Fits a negative-binomial spatial random-field model to estimate location-specific relative multipliers
#' using an INLA SPDE mesh. Requires total incidence, population size, observation window per site,
#' coordinates, and optional additional covariates.
#'
#' @param location_name Character vector of site identifiers.
#' @param longitude Numeric vector of site x-coordinates (projected).
#' @param latitude Numeric vector of site y-coordinates (projected).
#' @param incidence_total Numeric vector of total incidence per site.
#' @param population_size Numeric vector of population size per site.
#' @param time_span Numeric vector (or scalar) of observation window length (days) per site.
#' @param X Optional data frame or matrix of additional covariates. Column names are used as covariate names.
#' @return A list with:
#'   \describe{
#'     \item{relative_multiplier}{Numeric vector of site-level multipliers \eqn{\exp(u_j)}.}
#'     \item{model}{Fitted INLA model object.}
#'     \item{inputs}{List containing input lists used for the INLA model (mesh, data_list, A_list, effects_list).}
#'   }
#' @importFrom INLA inla inla.mesh.2d inla.spde2.pcmatern inla.spde.make.A inla.stack inla.stack.data inla.stack.A
#' @export
#' @examples
#' \dontrun{
#' res <- est_INLA_relative_multipliers(
#'   location_name   = c("A","B","C"),
#'   longitude       = c(0, 10, 15),
#'   latitude        = c(0, 5, 8),
#'   incidence_total = c(5, 3, 4),
#'   population_size = c(1000, 800, 1200),
#'   time_span       = c(14, 14, 14),
#'   X               = data.frame(temperature = c(25, 28, 30), sanitation_index = c(0.7, 0.4, 0.6))
#' )
#' plot(factor(c("A","B","C")), res$relative_multiplier)
#' abline(h=1, col='red', lty=2)
#' }
#'

est_INLA_relative_multipliers <- function(
          location_name,
          longitude,
          latitude,
          incidence_total,
          population_size,
          time_span,
          similarity_matrix = NULL,
          X = NULL
) {
     if (!requireNamespace("INLA", quietly = TRUE)) {
          stop("Package 'INLA' is required but not installed.")
     }


     message("[1/7] Assembling data frame...")
     df <- data.frame(
          location_name   = location_name,
          longitude       = longitude,
          latitude        = latitude,
          incidence_total = incidence_total,
          population_size = population_size,
          time_span       = time_span,
          stringsAsFactors = FALSE
     )

     if (!is.null(X)) {
          df <- cbind(df, X)
     }

     coords <- as.matrix(df[, c("longitude", "latitude")])

     message("[2/7] Building INLA mesh...")
     mesh <- INLA::inla.mesh.2d(
          loc      = coords,
          max.edge = c(40, 150),
          cutoff   = 5,
          offset   = c(50, 100)
     )

     dmat <- as.matrix(dist(coords))
     r    <- max(dmat)/2

     mesh <- inla.mesh.2d(coords,
                          cutoff=r/10,
                          max.edge=c(r/4, r/2),
                          offset=c(r/2, r))

     message("[3/7] Defining SPDE model...")
     spde <- INLA::inla.spde2.pcmatern(
          mesh        = mesh,
          prior.range = c(25, 0.5),
          prior.sigma = c(2, 0.01)
     )

     message("[4/7] Projecting observations and building stack...")
     A_obs <- INLA::inla.spde.make.A(
          mesh,
          loc = as.matrix(df[, c("longitude", "latitude")])
     )
     data_list <- list(incidence_total = df$incidence_total)

     effects_list <- list(
          spatial   = 1:spde$n.spde,
          intercept = rep(1, nrow(df))
     )

     if (!is.null(X)) {
          effects_list <- c(effects_list, list(covariates = X))
          A_list <- list(A_obs, 1, 1)
     } else {
          A_list <- list(A_obs, 1)
     }

     stk <- INLA::inla.stack(
          data    = data_list,
          A       = A_list,
          effects = effects_list
     )

     message("[5/7] Building formula...")
     covariate_terms <- if (!is.null(X)) paste(names(X), collapse = " + ") else ""
     form_text <- paste0("incidence_total ~ 0 + intercept + f(spatial, model = spde)",
                         if (covariate_terms != "") paste("+", covariate_terms))
     message(sprintf("Formula: %s", form_text))

     message("[6/7] Fitting negative-binomial spatial model...")
     res <- INLA::inla(
          formula           = as.formula(form_text),
          data              = INLA::inla.stack.data(stk),
          family            = "nbinomial",
          offset            = log(df$population_size * df$time_span),
          control.predictor = list(A = INLA::inla.stack.A(stk), compute = TRUE),
          control.family    = list(
               hyper = list(theta = list(prior = "loggamma", param = c(1, 0.01)))
          )
     )

     message("[7/7] Extracting spatial random effects and returning results...")
     w_mesh <- res$summary.random$spatial$mean
     A_all  <- INLA::inla.spde.make.A(
          mesh,
          loc = as.matrix(df[, c("longitude", "latitude")])
     )
     u_hat <- as.vector(A_all %*% w_mesh)
     list(
          relative_multiplier = exp(u_hat),
          model               = res,
          inputs              = list(mesh = mesh, data_list = data_list, A_list = A_list, effects_list = effects_list)
     )
}
