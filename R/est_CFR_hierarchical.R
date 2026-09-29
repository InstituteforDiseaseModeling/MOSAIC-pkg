#' Estimate the time-varying reported case fatality ratio (mu_jt) from WHO annual data
#'
#' @description
#' Fits a hierarchical binomial GAM to every country-year in the WHO annual
#' cholera record and returns a per-country, per-year estimate of the reported
#' case fatality ratio (CFR, reported deaths per reported suspected case). The
#' estimates are the prior for the engine's time-varying \code{mu_jt}: they are
#' expanded to a daily [location x day] matrix by \code{\link{make_mu_jt}} and
#' carry the widths used by the calibration's integrated deaths likelihood.
#'
#' @param PATHS List of paths from \code{\link{get_paths}}; reads
#'   \code{PATHS$DATA_WHO_ANNUAL/who_afro_annual.csv} and writes to
#'   \code{PATHS$MODEL_INPUT}.
#' @param min_cases Integer; a country-year enters the fit only with at least this many reported cases (default 1).
#' @param k_year Integer; basis dimension of the global temporal smooth (default 12).
#' @param k_trend Integer; basis dimension of each country's trend smooth (default 10).
#' @param include_country_trends Logical; include country-specific trend smooths (default TRUE).
#' @param forecast_years Integer; number of years past the last data year to supply estimates for (default 3).
#' @param forecast_method One of \code{"carry_forward"} (default: every year after the last data year repeats that year's estimate) or \code{"project"} (evaluate the fitted smooths beyond the data).
#' @param validate Logical; run the rolling-origin out-of-sample check (default TRUE).
#' @param population_weighted Deprecated and ignored (the former branch counted cases twice).
#' @param save_diagnostics Logical; write \code{cfr_model_diagnostics.pdf} to \code{PATHS$MODEL_INPUT} (default TRUE).
#' @param verbose Logical; print progress messages (default TRUE).
#'
#' @details
#' \strong{Model.} For country \eqn{j} in year \eqn{y}, with \eqn{D_{jy}}
#' deaths out of \eqn{C_{jy}} cases,
#' \deqn{D_{jy} \sim \mathrm{Binomial}(C_{jy}, p_{jy}),\quad
#'   \mathrm{logit}\, p_{jy} = f(y) + u_j + g_j(y) + e_{jy},}
#' where \eqn{f} is a global smooth trend, \eqn{u_j \sim N(0, \tau^2)} a country
#' random intercept, \eqn{g_j} a country-specific penalised trend (a factor
#' smooth, shrunk toward the global curve), and \eqn{e_{jy} \sim N(0,
#' \sigma^2)} a country-year random effect. Fitted by fREML with
#' \code{mgcv::bam()}.
#'
#' The country-year term \eqn{e_{jy}} is what makes the hierarchy work. Annual
#' CFR varies far more between years than a binomial allows (Pearson dispersion
#' ~29 without it); with no term to absorb that, the country trends soak up the
#' year-to-year noise, the country intercepts carry no information, and
#' countries with little data receive an extrapolated per-country curve. With it,
#' countries with few observations shrink toward the global curve.
#'
#' \strong{Data.} Every country-year in the WHO annual file, 1970 onward, for
#' every country in the file (not only MOSAIC locations): the extra countries
#' inform the global curve and the between-country spread. Held-out testing
#' (fit through 2022, predict 2023-25) favoured using all years with country
#' trends over discarding the pre-2000 record.
#'
#' \strong{Estimates and widths.} The point estimate for a country-year
#' excludes \eqn{e_{jy}}. Its predictive standard deviation on the logit scale
#' is \eqn{\sqrt{se^2 + \sigma^2}}, where \eqn{se} is the estimation error of
#' \eqn{f + u_j + g_j}; a MOSAIC location absent from the fit receives the
#' global curve with \eqn{\tau^2} added. These are \emph{predictive} widths for a
#' single year, not the confidence interval of the mean.
#'
#' \strong{Forecast years.} Years after the last year in the data are filled by
#' \code{forecast_method}. The rolling-origin check compares both rules and
#' records the result in \code{summary$validation}.
#'
#' @return A list of class \code{cfr_hierarchical_model}:
#' \itemize{
#'   \item \code{model}: the fitted \code{bam} object;
#'   \item \code{predictions}: one row per MOSAIC location and year with
#'     \code{cfr_estimate} (median), predictive \code{cfr_lower}/\code{cfr_upper}
#'     (95\%), \code{cfr_se} (logit-scale estimation error), \code{logit_mean},
#'     \code{logit_sd} (predictive), \code{is_forecast}, \code{pooled} (location
#'     absent from the fit), \code{n_country_years};
#'   \item \code{country_effects}: the country random intercepts;
#'   \item \code{temporal_trend}: the population-average curve;
#'   \item \code{validation}: rolling-origin results, or \code{NULL};
#'   \item \code{summary}: fit statistics, \code{tau}, \code{sigma} and settings.
#' }
#' Files written to \code{PATHS$MODEL_INPUT}: \code{param_mu_disease_mortality.csv}
#' (MOSAIC parameter format; per location-year \code{point} median,
#' \code{beta} shapes and \code{logitnormal} mean/sd), \code{cfr_hierarchical_estimates.csv},
#' \code{cfr_temporal_trend.csv}, \code{cfr_country_effects.csv} and
#' \code{cfr_model_summary.rds}.
#'
#' @seealso \code{\link{make_mu_jt}}
#' @importFrom stats binomial predict AIC qlogis plogis dnorm dbinom sd median
#' @importFrom utils read.csv write.csv
#' @importFrom grDevices pdf dev.off
#' @examples
#' \dontrun{
#' PATHS <- get_paths()
#' fit <- est_CFR_hierarchical(PATHS)
#' head(fit$predictions)
#' }
#' @export
est_CFR_hierarchical <- function(
    PATHS,
    min_cases = 1,
    k_year = 12,
    k_trend = 10,
    include_country_trends = TRUE,
    forecast_years = 3L,
    forecast_method = c("carry_forward", "project"),
    validate = TRUE,
    population_weighted = FALSE,
    save_diagnostics = TRUE,
    verbose = TRUE
) {

    if (!requireNamespace("mgcv", quietly = TRUE)) {
        stop("Package 'mgcv' is required. Please install it using: install.packages('mgcv')")
    }
    forecast_method <- match.arg(forecast_method)
    forecast_years <- as.integer(forecast_years)
    if (length(forecast_years) != 1L || is.na(forecast_years) || forecast_years < 0L)
        stop("forecast_years must be a single non-negative integer.")
    if (isTRUE(population_weighted)) {
        warning("est_CFR_hierarchical(): `population_weighted` is deprecated and ignored. ",
                "The former branch multiplied the binomial weights by the case count, ",
                "counting cases twice.", call. = FALSE)
    }

    who_data_path <- file.path(PATHS$DATA_WHO_ANNUAL, "who_afro_annual.csv")
    if (!file.exists(who_data_path)) {
        stop("WHO annual data not found. Please run process_WHO_annual_data(PATHS) first.")
    }
    if (verbose) message("Loading WHO annual cholera data...")
    who_data <- utils::read.csv(who_data_path, stringsAsFactors = FALSE)

    # One human-readable name per ISO. The WHO file spells some countries more
    # than one way (CIV); the model keys on iso_code and this map only labels
    # the outputs. The most frequent spelling wins, ties broken alphabetically.
    who_named <- who_data[who_data$country != "AFRO Region" &
                          !is.na(who_data$iso_code) & nzchar(who_data$iso_code), ]
    iso_name_map <- data.frame(iso_code = sort(unique(who_named$iso_code)),
                               stringsAsFactors = FALSE)
    iso_name_map$country <- vapply(iso_name_map$iso_code, function(z) {
        nm <- who_named$country[who_named$iso_code == z]
        nm <- nm[!is.na(nm) & nzchar(nm)]
        if (!length(nm)) return(z)
        tab <- table(nm)
        sort(names(tab)[tab == max(tab)])[1]
    }, character(1), USE.NAMES = FALSE)

    model_data <- .cfr_model_data(who_data, min_cases)
    if (!nrow(model_data)) stop("No WHO annual country-years pass the filters.")
    last_data_year <- max(model_data$year)

    if (verbose) {
        message(sprintf("  %d country-years from %d countries, %d-%d",
                        nrow(model_data), length(unique(model_data$iso_code)),
                        min(model_data$year), last_data_year))
    }

    if (verbose) message("Fitting hierarchical GAM (bam, fREML)...")
    fit <- .cfr_fit_gam(model_data, k_year, k_trend, include_country_trends)
    if (verbose) {
        message(sprintf("  tau (between-country) %.3f | sigma (year-to-year) %.3f [logit]",
                        fit$tau, fit$sigma))
    }

    mosaic_iso <- MOSAIC::iso_codes_mosaic
    pred_years <- seq.int(min(model_data$year), last_data_year + forecast_years)
    predictions <- .cfr_predict(fit, mosaic_iso, pred_years, last_data_year, forecast_method)
    predictions$country <- iso_name_map$country[match(predictions$iso_code, iso_name_map$iso_code)]
    predictions$country[is.na(predictions$country)] <- predictions$iso_code[is.na(predictions$country)]
    predictions <- predictions[, c("country", "iso_code", "year", "cfr_estimate", "cfr_lower",
                                   "cfr_upper", "cfr_se", "logit_mean", "logit_sd",
                                   "is_forecast", "pooled", "n_country_years")]

    temporal <- .cfr_predict(fit, "__global__", pred_years, last_data_year, forecast_method,
                             global = TRUE)
    temporal_trend <- data.frame(
        year = temporal$year,
        cfr_trend = temporal$cfr_estimate,
        cfr_trend_lower = stats::plogis(temporal$logit_mean - 1.96 * temporal$cfr_se),
        cfr_trend_upper = stats::plogis(temporal$logit_mean + 1.96 * temporal$cfr_se),
        is_forecast = temporal$is_forecast
    )

    country_effects <- .cfr_country_effects(fit, iso_name_map)

    validation <- NULL
    if (isTRUE(validate)) {
        if (verbose) message("Rolling-origin validation...")
        validation <- .cfr_validate(model_data, mosaic_iso, k_year, k_trend,
                                    include_country_trends, forecast_method)
        if (verbose && !is.null(validation)) {
            s <- validation$summary
            for (r in seq_len(nrow(s))) {
                message(sprintf("  %-13s h=%d: log score %.1f | 95%% coverage %.2f | median |logit err| %.3f (n=%d)",
                                s$method[r], s$horizon[r], s$log_score[r], s$coverage95[r],
                                s$median_abs_logit_err[r], s$n[r]))
            }
            cov <- s$coverage95[s$method == forecast_method]
            if (length(cov) && any(cov < 0.80)) {
                warning(sprintf(paste0("est_CFR_hierarchical(): predictive 95%% coverage is %.2f out of sample ",
                                       "for the configured forecast_method; the widths are too narrow."),
                                min(cov)), call. = FALSE)
            }
        }
    }

    summary_list <- list(
        model_type = if (include_country_trends)
            "Binomial GAM: global trend + country intercepts + country trends + country-year effect"
        else "Binomial GAM: global trend + country intercepts + country-year effect",
        n_observations = nrow(model_data),
        n_countries = length(unique(model_data$iso_code)),
        year_range = range(model_data$year),
        last_data_year = last_data_year,
        min_cases_threshold = min_cases,
        k_year = k_year,
        k_trend = k_trend,
        tau = fit$tau,
        sigma = fit$sigma,
        forecast_method = forecast_method,
        forecast_years = forecast_years,
        deviance_explained = summary(fit$model)$dev.expl,
        validation = if (is.null(validation)) NULL else validation$summary,
        timestamp = Sys.time()
    )

    if (verbose) message("Saving outputs...")
    if (!dir.exists(PATHS$MODEL_INPUT)) dir.create(PATHS$MODEL_INPUT, recursive = TRUE)

    utils::write.csv(.cfr_param_table(predictions),
                     file.path(PATHS$MODEL_INPUT, "param_mu_disease_mortality.csv"),
                     row.names = FALSE)
    utils::write.csv(predictions, file.path(PATHS$MODEL_INPUT, "cfr_hierarchical_estimates.csv"),
                     row.names = FALSE)
    utils::write.csv(temporal_trend, file.path(PATHS$MODEL_INPUT, "cfr_temporal_trend.csv"),
                     row.names = FALSE)
    utils::write.csv(country_effects, file.path(PATHS$MODEL_INPUT, "cfr_country_effects.csv"),
                     row.names = FALSE)
    saveRDS(summary_list, file.path(PATHS$MODEL_INPUT, "cfr_model_summary.rds"))

    if (save_diagnostics) {
        grDevices::pdf(file.path(PATHS$MODEL_INPUT, "cfr_model_diagnostics.pdf"), width = 10, height = 10)
        mgcv::gam.check(fit$model)
        plot(fit$model, pages = 1, all.terms = TRUE)
        grDevices::dev.off()
    }

    results <- list(model = fit$model, predictions = predictions,
                    country_effects = country_effects, temporal_trend = temporal_trend,
                    validation = validation, summary = summary_list,
                    settings = list(min_cases = min_cases, k_year = k_year, k_trend = k_trend,
                                    include_country_trends = include_country_trends,
                                    forecast_years = forecast_years,
                                    forecast_method = forecast_method))
    class(results) <- c("cfr_hierarchical_model", "list")
    invisible(results)
}


# Country-years that enter the fit: every country in the file except the AFRO
# aggregate, with finite counts and at least `min_cases` cases. Deaths are capped
# at cases (a handful of source rows report more deaths than cases).
.cfr_model_data <- function(who_data, min_cases) {
    if (!"iso_code" %in% names(who_data)) stop("WHO annual data has no iso_code column.")
    d <- who_data[who_data$country != "AFRO Region" & who_data$iso_code != "AFRO" &
                  !is.na(who_data$iso_code) & nzchar(who_data$iso_code) &
                  is.finite(who_data$cases_total) & is.finite(who_data$deaths_total) &
                  who_data$cases_total >= max(1, min_cases), , drop = FALSE]
    d$deaths_total <- pmin(d$deaths_total, d$cases_total)
    d$survivors <- d$cases_total - d$deaths_total
    d$iso <- factor(d$iso_code)
    d$obs <- factor(seq_len(nrow(d)))
    d
}

.cfr_fit_gam <- function(d, k_year, k_trend, include_country_trends) {
    rhs <- sprintf("s(year, k = %d) + s(iso, bs = 're') + s(obs, bs = 're')", as.integer(k_year))
    if (include_country_trends) {
        rhs <- paste(rhs, sprintf("+ s(year, iso, bs = 'fs', k = %d, m = 2)", as.integer(k_trend)))
    }
    fml <- stats::as.formula(paste("cbind(deaths_total, survivors) ~", rhs))
    # openMP is unavailable on some builds; the warning is informational only.
    m <- suppressWarnings(mgcv::bam(fml, family = stats::binomial(), data = d,
                                    method = "fREML", discrete = TRUE))
    vc <- mgcv::gam.vcomp(m, rescale = FALSE)
    vcm <- if (is.list(vc) && !is.null(vc$vc)) vc$vc else vc
    sd_of <- function(label) {
        r <- which(rownames(vcm) == label)
        if (length(r)) unname(vcm[r[1], "std.dev"]) else 0
    }
    list(model = m, data = d, tau = sd_of("s(iso)"), sigma = sd_of("s(obs)"))
}

# Predict logit CFR for locations x years. Country-year effects are always
# excluded (they are the year-to-year noise that sigma describes). A location
# absent from the fit gets the population-average curve plus tau^2. Years after
# `last_data_year` follow `forecast_method`.
.cfr_predict <- function(fit, isos, years, last_data_year, forecast_method, global = FALSE) {
    m <- fit$model
    known <- levels(fit$data$iso)
    obs1 <- levels(fit$data$obs)[1]
    country_labels <- vapply(m$smooth, function(s) s$label, character(1))[
        vapply(m$smooth, function(s) "iso" %in% s$term, logical(1))]
    eval_years <- if (forecast_method == "carry_forward") pmin(years, last_data_year) else years
    out <- lapply(isos, function(iso) {
        seen <- !global && iso %in% known
        nd <- data.frame(year = eval_years,
                         iso = factor(if (seen) iso else known[1], levels = known),
                         obs = factor(obs1, levels = levels(fit$data$obs)))
        excl <- c("s(obs)", if (!seen) country_labels)
        p <- stats::predict(m, newdata = nd, type = "link", se.fit = TRUE, exclude = excl)
        extra <- if (seen || global) 0 else fit$tau^2
        mu <- as.numeric(p$fit)
        se <- as.numeric(p$se.fit)
        sd_pred <- sqrt(se^2 + extra + (if (global) 0 else fit$sigma^2))
        data.frame(iso_code = iso, year = years, logit_mean = mu, logit_sd = sd_pred,
                   cfr_se = se, cfr_estimate = stats::plogis(mu),
                   cfr_lower = stats::plogis(mu - 1.96 * sd_pred),
                   cfr_upper = stats::plogis(mu + 1.96 * sd_pred),
                   is_forecast = years > last_data_year, pooled = !seen,
                   n_country_years = sum(fit$data$iso_code == iso),
                   stringsAsFactors = FALSE)
    })
    do.call(rbind, out)
}

.cfr_country_effects <- function(fit, iso_name_map) {
    m <- fit$model
    re <- Filter(function(s) inherits(s, "random.effect") && identical(s$term, "iso"), m$smooth)
    if (length(re) != 1L) return(NULL)
    idx <- re[[1]]$first.para:re[[1]]$last.para
    lv <- levels(fit$data$iso)
    if (length(idx) != length(lv)) {
        stop(sprintf("Random-effect coefficient block has %d entries for %d country levels.",
                     length(idx), length(lv)))
    }
    ce <- data.frame(iso_code = lv,
                     country = iso_name_map$country[match(lv, iso_name_map$iso_code)],
                     random_effect = unname(stats::coef(m)[idx]),
                     stringsAsFactors = FALSE)
    ce$country[is.na(ce$country)] <- ce$iso_code[is.na(ce$country)]
    ce <- ce[order(ce$random_effect, decreasing = TRUE), ]
    rownames(ce) <- NULL
    ce
}

# Gauss-Hermite nodes/weights for moments of a logit-normal.
.cfr_gh <- function() {
    x <- c(-3.436159118837738, -2.532731674232790, -1.756683649299882, -1.036610829789514,
           -0.342901327223705, 0.342901327223705, 1.036610829789514, 1.756683649299882,
           2.532731674232790, 3.436159118837738)
    w <- c(7.640432855232621e-06, 1.343645746781233e-03, 3.387439445548106e-02,
           2.401386110823147e-01, 6.108626337353258e-01, 6.108626337353258e-01,
           2.401386110823147e-01, 3.387439445548106e-02, 1.343645746781233e-03,
           7.640432855232621e-06)
    list(x = x * sqrt(2), w = w / sqrt(pi))
}

# Beta shapes with the same mean and variance as a logit-normal(mu, sd).
.cfr_logitnormal_to_beta <- function(mu, sd) {
    gh <- .cfr_gh()
    out <- t(mapply(function(m, s) {
        p <- stats::plogis(m + s * gh$x)
        mn <- sum(gh$w * p)
        v <- sum(gh$w * p^2) - mn^2
        k <- mn * (1 - mn) / v - 1
        c(shape1 = mn * k, shape2 = (1 - mn) * k)
    }, mu, sd))
    out
}

# Log predictive density of D deaths out of C cases under logit CFR ~ N(mu, sd^2).
# Fine grid rather than Gauss-Hermite: with tens of thousands of cases the
# binomial term is far narrower than the prior.
.cfr_lpd <- function(D, C, mu, sd) {
    eta <- seq(mu - 7 * sd, mu + 7 * sd, length.out = 4001L)
    l <- stats::dnorm(eta, mu, sd, log = TRUE) + stats::dbinom(D, C, stats::plogis(eta), log = TRUE)
    mx <- max(l)
    mx + log(sum(exp(l - mx)) * (eta[2] - eta[1]))
}

# Rolling-origin check: refit on data up to year Y and predict each MOSAIC
# location's CFR in Y+1 and Y+2 under both forecast rules.
.cfr_validate <- function(model_data, mosaic_iso, k_year, k_trend, include_country_trends,
                          forecast_method, n_origins = 5L) {
    last <- max(model_data$year)
    origins <- seq.int(last - 1L - n_origins, last - 2L)
    rows <- list()
    for (Y in origins) {
        tr <- model_data[model_data$year <= Y, , drop = FALSE]
        tr$iso <- droplevels(tr$iso); tr$obs <- factor(seq_len(nrow(tr)))
        fit <- tryCatch(.cfr_fit_gam(tr, k_year, k_trend, include_country_trends),
                        error = function(e) NULL)
        if (is.null(fit)) next
        for (h in 1:2) {
            te <- model_data[model_data$year == Y + h & model_data$iso_code %in% mosaic_iso, , drop = FALSE]
            if (!nrow(te)) next
            for (meth in c("carry_forward", "project")) {
                p <- .cfr_predict(fit, te$iso_code, rep(Y + h, 1L), Y, meth)
                p <- p[match(te$iso_code, p$iso_code), ]
                lpd <- mapply(.cfr_lpd, te$deaths_total, te$cases_total, p$logit_mean, p$logit_sd)
                obs_logit <- stats::qlogis(pmax(te$deaths_total, 0.5) / te$cases_total)
                rows[[length(rows) + 1L]] <- data.frame(
                    origin = Y, horizon = h, method = meth, iso_code = te$iso_code,
                    deaths = te$deaths_total, lpd = lpd,
                    covered95 = abs(obs_logit - p$logit_mean) <= 1.96 * p$logit_sd,
                    abs_logit_err = abs(obs_logit - p$logit_mean), stringsAsFactors = FALSE)
            }
        }
    }
    if (!length(rows)) return(NULL)
    units <- do.call(rbind, rows)
    summ <- do.call(rbind, lapply(split(units, list(units$method, units$horizon), drop = TRUE), function(z) {
        dense <- z$deaths >= 50
        data.frame(method = z$method[1], horizon = z$horizon[1], n = nrow(z),
                   log_score = sum(z$lpd), coverage95 = mean(z$covered95),
                   median_abs_logit_err = if (any(dense)) stats::median(z$abs_logit_err[dense]) else NA_real_,
                   stringsAsFactors = FALSE)
    }))
    rownames(summ) <- NULL
    list(units = units, summary = summ[order(summ$horizon, summ$method), ])
}

# MOSAIC parameter-format table: per location-year point median, beta shapes and
# logit-normal mean/sd, all describing the predictive distribution of one year.
.cfr_param_table <- function(predictions) {
    desc <- "reported case fatality ratio (reported deaths per reported suspected case)"
    bb <- .cfr_logitnormal_to_beta(predictions$logit_mean, predictions$logit_sd)
    j <- predictions$iso_code; t <- predictions$year
    rbind(
        make_param_df("mu", desc, "point", NA, j, t, "mean", predictions$cfr_estimate),
        make_param_df("mu", desc, "beta", NA, j, t, "shape1", bb[, "shape1"]),
        make_param_df("mu", desc, "beta", NA, j, t, "shape2", bb[, "shape2"]),
        make_param_df("mu", desc, "logitnormal", NA, j, t, "mean", predictions$logit_mean),
        make_param_df("mu", desc, "logitnormal", NA, j, t, "sd", predictions$logit_sd)
    )
}
