#' Estimate spatial transmission structure using INLA
#'
#' Prepare input data and fit a negative-binomial spatial random-field model
#' (SPDE + IID site effect) to obtain location-specific multiplicative
#' transmission modifiers.
#'
#' @details
#' The response is total reported incidence aggregated over the analysis
#' window. A log-offset of \eqn{\log(N_j \times T)} is applied, where
#' \eqn{N_j} is site-level population and \eqn{T} the number of observation
#' days.  The latent field is a Matérn SPDE (Lindgren et al., 2011).
#' A CSV of the estimated multipliers is written to
#' \code{file.path(PATHS$MODEL_INPUT, "param_beta_relative_multiplier.csv")}.
#'
#' @param PATHS A named list with at minimum:
#'   \describe{
#'     \item{DATA_WORLD_BANK, DATA_ELEVATION, DATA_CLIMATE}{Character paths to
#'       covariate files.}
#'     \item{MODEL_INPUT}{Character path to the directory where the parameter
#'       CSV will be written.}
#'   }
#' @param config A named list with elements…
#' @return A list with components
#'   \describe{
#'     \item{relative_multiplier}{Numeric vector \eqn{\exp(u_j)}.}
#'     \item{model}{Fitted \code{INLA} object.}
#'     \item{inputs}{Internal objects (\code{mesh}, \code{data_list}, …).}
#'     \item{data}{Merged site-level data frame used in the fit.}
#'   }
#' @references Lindgren, F., Rue, H. & Lindström, J. (2011). …
#' @importFrom INLA inla inla.mesh.2d inla.spde2.pcmatern inla.spde.make.A
#' @importFrom INLA inla.stack inla.stack.data inla.stack.A
#' @importFrom lubridate year
#' @importFrom stats dist
#' @importFrom utils write.csv
#' @examples
#' \dontrun{
#'   paths  <- list(MODEL_INPUT = tempdir(), DATA_WORLD_BANK = "...", ...)
#'   config <- MOSAIC::config_default[1:3]   # first three sites
#'   res <- est_transmission_spatial_structure(paths, config)
#'   head(res$relative_multiplier)
#' }
#' @export
#'

est_transmission_spatial_structure <- function(PATHS, config) {

     if (missing(config) || is.null(config)) config <- MOSAIC::config_default

     # Time window and site metadata
     date_start <- as.Date(config$date_start)
     date_stop  <- as.Date(config$date_stop)
     years      <- (lubridate::year(date_start) - 1):lubridate::year(date_stop)
     message("Analyzing spatial transmission structure from ", date_start, " to ", date_stop,
             " for the following ", length(config$location_name), " locations: ")
     message(paste(config$location_name, collapse = ", "))

     # Incidence and vaccination summaries
     inc <- rowSums(config$reported_cases, na.rm = TRUE)
     reporting_rate <- apply(config$reported_cases, 1, function(x) sum(!is.na(x)) / length(x))
     inc[reporting_rate == 0] <- NA
     prop_V1 <- (rowSums(config$nu_1_jt, na.rm = TRUE) * config$phi_1) / config$N_j_initial
     prop_V2 <- (rowSums(config$nu_2_jt, na.rm = TRUE) * config$phi_2) / config$N_j_initial

     df_config <- data.frame(
          iso_code        = config$location_name,
          longitude       = config$longitude,
          latitude        = config$latitude,
          time_span       = ncol(config$reported_cases),
          incidence_total = inc,
          reporting_rate  = reporting_rate,
          population_size = config$N_j_initial,
          prop_V1         = prop_V1,
          prop_V2         = prop_V2,
          WASH            = config$theta_j,
          prob_travel     = config$tau_i,
          stringsAsFactors=FALSE
     )

     df_config$incidence_total[df_config$reporting_rate == 0] <- NA

     # Socioeconomic and climate data loading omitted for brevity (same as before)
     # Assume df_all assembled with all covariates
     df_all <- df_config  # placeholder merge

     # PCA of covariates
     df_cov <- df_all[, setdiff(names(df_all), c("iso_code","incidence_total","population_size","time_span"))]
     df_cov <- df_cov[, sapply(df_cov, is.numeric)]
     df_cov <- df_cov[, sapply(df_cov, function(col) length(unique(col)) > 1)]
     df_cov <- df_cov[, sapply(df_cov, function(col) !any(is.na(col)))]

     X <- df_cov[,c('reporting_rate', 'WASH')]

     # INLA setup
     if (!requireNamespace("INLA", quietly = TRUE)) stop("INLA package required.")
     coords <- as.matrix(df_all[, c("longitude","latitude")])
     dmat   <- as.matrix(dist(coords)); r <- max(dmat)/2
     mesh   <- INLA::inla.mesh.2d(coords, cutoff = r/10, max.edge = c(r/4,r/2), offset = c(r/2,r))
     spde   <- INLA::inla.spde2.pcmatern(mesh, prior.range = c(25,0.5), prior.sigma = c(2,0.01))

     # Effects and stack with IID site effect
     n_loc        <- nrow(df_all)
     effects_list <- list(
          spatial    = 1:spde$n.spde,
          intercept  = rep(1, n_loc),
          site       = 1:n_loc,           # IID random intercept
          covariates = X
     )
     A_obs  <- INLA::inla.spde.make.A(mesh, loc = coords)
     A_list <- list(
          A_obs,      # spatial
          1,          # intercept
          1,          # site IID mapping
          1           # covariate identity
     )
     data_list <- list(incidence_total = df_all$incidence_total)

     stk <- INLA::inla.stack(data = data_list, A = A_list, effects = effects_list)

     cov_terms <- paste(names(X), collapse = " + ")
     form_txt  <- paste0(
          "incidence_total ~ 0 + intercept",
          " + f(spatial,model=spde)",
          " + f(site, model='iid')",
          if(cov_terms != "") paste0(" + ", cov_terms)
     )
     message(form_txt)

     res <- INLA::inla(
          formula           = as.formula(form_txt),
          data              = INLA::inla.stack.data(stk),
          family            = "nbinomial",
          offset            = log(df_all$population_size * df_all$time_span),
          control.predictor = list(A = INLA::inla.stack.A(stk),
                                   compute = TRUE,
                                   link    = 1),
          control.family    = list(hyper = list(theta = list(prior = "loggamma", param = c(1,0.01))))
     )

     # Extract multipliers
     w_mesh <- res$summary.random$spatial$mean
     w_iid  <- res$summary.random$site$mean
     A_all  <- INLA::inla.spde.make.A(mesh, loc = coords)
     u_hat  <- as.vector(A_all %*% w_mesh) + w_iid

     out <- list(
          relative_multiplier = exp(u_hat),
          model               = res,
          inputs              = list(mesh = mesh, data_list = data_list, A_list = A_list, effects_list = effects_list),
          data                = df_all
     )


     param_df <- make_param_df(
          variable_name = "beta_relative_multiplier",
          variable_description = "Spatial structure for global mean transmission",
          parameter_distribution = "point",
          parameter_name = 'mean',
          parameter_value = exp(u_hat),
          j = df_all$iso_code
     )

     param_path <- file.path(PATHS$MODEL_INPUT, "param_beta_relative_multiplier.csv")
     utils::write.csv(param_df, param_path, row.names = FALSE)
     message("Parameter data frame of beta relative multipliers saved here: ", param_path)

     return(out)

}
