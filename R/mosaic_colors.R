# =============================================================================
# MOSAIC Color Palette Framework
# =============================================================================
#
# A complete color system for the MOSAIC package anchored to the primary
# MOSAIC blue (#0167AF). Provides semantic named colors for recurring model
# quantities, palette generators for discrete/sequential/diverging/uncertainty
# scales, ggplot2 integration, and a clean publication theme.
#
# Quick reference:
#   mosaic_colors()              — named semantic colors (data, cases, deaths, ...)
#   mosaic_palette()             — general palette dispatcher
#   mosaic_pal_discrete()        — qualitative palette for categories
#   mosaic_pal_sequential()      — ordered palette for magnitudes
#   mosaic_pal_diverging()       — centered palette for deviations
#   mosaic_pal_uncertainty()     — graduated ribbons from a base color
#   mosaic_pal_scenarios()       — dedicated scenario comparison palette
#   mosaic_pal_countries()       — 40-country palette grouped by region
#   mosaic_color_variant()       — lighten/darken/mute a color
#   plot_mosaic_palette()        — display swatches for any palette
#   theme_mosaic()               — publication-quality ggplot2 theme
#   scale_color_mosaic_d()       — ggplot2 discrete color scale
#   scale_fill_mosaic_d()        — ggplot2 discrete fill scale
#   scale_color_mosaic_c()       — ggplot2 continuous color scale
#   scale_fill_mosaic_c()        — ggplot2 continuous fill scale
#   scale_color_mosaic_div()     — ggplot2 diverging color scale
#   scale_fill_mosaic_div()      — ggplot2 diverging fill scale
#   scale_color_mosaic_scenario() — ggplot2 scenario color scale
#   scale_fill_mosaic_scenario() — ggplot2 scenario fill scale
#
# Design principles:
#   - Anchor color: #0167AF (MOSAIC blue)
#   - Uncertainty is derived from point estimate color, never standalone
#   - Prior = gray, Posterior = MOSAIC blue (BFRS only)
#   - Scenarios always lead with baseline = MOSAIC blue
#   - CVD-safe qualitative palettes based on Paul Tol's bright scheme
#   - Sequential palettes enforce monotone luminance for grayscale safety
#   - Zero new dependencies (base R grDevices + existing ggplot2)
#
# =============================================================================


# =============================================================================
# Internal Color Database
# =============================================================================

.mosaic_color_db <- list(

  # --- Core semantic colors ---
  core = c(
    mosaic_blue   = "#0167AF",
    data          = "#2D2D2D",
    forecast      = "#4A90C9",
    cases         = "#0167AF",
    deaths        = "#B5123B",
    environmental = "#009988",
    vaccination   = "#228833",
    reference     = "#888888",
    highlight     = "#EE6677",
    mobility      = "#CCBB44"
  ),

  # --- SEIR compartment colors ---
  compartments = c(
    S  = "#4477AA",
    E  = "#EE7733",
    I  = "#CC3311",
    R  = "#CCBB44",
    V1 = "#66AA44",
    V2 = "#228833",
    W  = "#009988"
  ),

  # --- Calibration colors (Prior + Posterior only) ---
  calibration = c(
    Prior     = "#888888",
    Posterior = "#0167AF"
  ),

  # --- Status/diagnostic colors ---
  status = c(
    pass = "#228833",
    warn = "#CCBB44",
    fail = "#CC3311",
    info = "#0167AF"
  ),

  # --- Qualitative palettes (for unrelated categories) ---
  qualitative = list(
    default = c(
      "#0167AF", "#EE6677", "#228833", "#CCBB44",
      "#66CCEE", "#AA3377", "#888888"
    ),
    extended = c(
      "#0167AF", "#EE6677", "#228833", "#CCBB44",
      "#66CCEE", "#AA3377", "#888888", "#EE7733",
      "#009988", "#332288", "#CC79A7", "#DDCC77"
    )
  ),

  # --- Scenario palette (baseline always MOSAIC blue) ---
  scenarios = list(
    default = c(
      "#0167AF", "#EE6677", "#228833", "#CCBB44",
      "#66CCEE", "#AA3377", "#EE7733", "#009988"
    ),
    extended = c(
      "#0167AF", "#EE6677", "#228833", "#CCBB44",
      "#66CCEE", "#AA3377", "#EE7733", "#009988",
      "#332288", "#CC79A7", "#DDCC77", "#88CCEE",
      "#44AA99", "#999933", "#661100", "#6699CC"
    )
  ),

  # --- Sequential palette ramps ---
  sequential = list(
    blues  = c("#E8F1FA", "#B8D4ED", "#7FAED4", "#4A90C9", "#0167AF", "#014F85", "#003D66"),
    reds   = c("#FCEAE8", "#F0B8B0", "#E08888", "#D05060", "#B5123B", "#8A0E2D", "#660020"),
    greens = c("#E8F5E8", "#B0DDB0", "#78C078", "#44AA55", "#228833", "#165522", "#0A4A1A"),
    teals  = c("#E0F5F3", "#A8E0D8", "#66CCBB", "#33BB99", "#009988", "#007766", "#005544"),
    heat   = c("#FFFFDD", "#FFEE99", "#FFCC55", "#FF9944", "#EE6633", "#DD3322", "#AA0011"),
    grays  = c("#F5F5F5", "#DDDDDD", "#BBBBBB", "#999999", "#777777", "#555555", "#333333")
  ),

  # --- Diverging palette ramps ---
  diverging = list(
    blue_red     = c("#0167AF", "#4A90C9", "#A8CCE8", "#F2F2F2", "#E8A09A", "#CC5544", "#B5123B"),
    blue_orange  = c("#0167AF", "#4A90C9", "#A8CCE8", "#F2F2F2", "#F0CC88", "#DD9933", "#AA6600"),
    green_purple = c("#228833", "#66BB66", "#BBDDBB", "#F2F2F2", "#CCAADD", "#9966BB", "#663399")
  ),

  # --- Parameter group colors ---
  param_groups = c(
    Transmission        = "#0167AF",
    "Disease Progression" = "#EE7733",
    Vaccination         = "#228833",
    Environmental       = "#009988",
    Observation         = "#888888",
    Mobility            = "#CCBB44",
    Seasonality         = "#66CCEE",
    Suitability         = "#AA3377"
  ),

  # --- WHO sub-region hue assignments for country palette ---
  region_hues = list(
    West    = c(hue_start = 0,   hue_end = 90),
    East    = c(hue_start = 100, hue_end = 180),
    Central = c(hue_start = 190, hue_end = 260),
    South   = c(hue_start = 270, hue_end = 350)
  )
)


# =============================================================================
# mosaic_colors — Semantic Named Colors
# =============================================================================

#' Get MOSAIC Semantic Colors
#'
#' Returns named hex color codes for recurring MOSAIC quantities. Use these
#' for consistent coloring across all package visualizations.
#'
#' @param ... Color names to retrieve. If empty, returns all core semantic colors.
#'   Special set names: \code{"core"}, \code{"compartments"}, \code{"calibration"},
#'   \code{"status"}, \code{"param_groups"}, \code{"all"}.
#'
#' @return Named character vector of hex color codes.
#'
#' @details
#' The core semantic colors are:
#' \itemize{
#'   \item \code{mosaic_blue} (\code{#0167AF}): Primary identity color, model estimates
#'   \item \code{data} (\code{#2D2D2D}): Observed data (near-black, always background layer)
#'   \item \code{forecast} (\code{#4A90C9}): Forecast extensions (lighter blue)
#'   \item \code{cases} (\code{#0167AF}): Cholera cases (same as mosaic_blue)
#'   \item \code{deaths} (\code{#B5123B}): Deaths (burgundy-crimson)
#'   \item \code{environmental} (\code{#009988}): Environmental reservoir, WASH (teal)
#'   \item \code{vaccination} (\code{#228833}): Vaccination/OCV (green)
#'   \item \code{reference} (\code{#888888}): Reference lines, baselines, priors (gray)
#'   \item \code{highlight} (\code{#EE6677}): Emphasis, alerts (coral-red)
#'   \item \code{mobility} (\code{#CCBB44}): Human mobility (gold)
#' }
#'
#' Compartment colors (S/E/I/R/V1/V2/W) are accessed via
#' \code{mosaic_colors("compartments")} or by name (e.g., \code{mosaic_colors("S")}).
#'
#' @examples
#' mosaic_colors()                            # All core colors
#' mosaic_colors("cases", "deaths")           # Specific colors
#' mosaic_colors("compartments")              # SEIR compartment colors
#' mosaic_colors("S", "E", "I", "R")          # Subset of compartments
#' mosaic_colors("calibration")               # Prior (gray) + Posterior (blue)
#' mosaic_colors("status")                    # pass/warn/fail/info
#' mosaic_colors("param_groups")              # Parameter group colors
#'
#' @export
mosaic_colors <- function(...) {

  args <- c(...)
  db <- .mosaic_color_db

  # No arguments: return core semantic colors

  if (is.null(args) || length(args) == 0) {
    return(db$core)
  }

  # Single set name
  sets <- c("core", "compartments", "calibration", "status", "param_groups", "all")
  if (length(args) == 1 && args %in% sets) {
    if (args == "all") {
      return(c(db$core, db$compartments, db$calibration, db$status, db$param_groups))
    }
    return(db[[args]])
  }

  # Look up individual names across all sets
  all_colors <- c(db$core, db$compartments, db$calibration, db$status, db$param_groups)
  found <- args %in% names(all_colors)
  if (!all(found)) {
    warning("Unknown color name(s): ", paste(args[!found], collapse = ", "))
  }
  all_colors[args[found]]
}


# =============================================================================
# mosaic_palette — General Dispatcher
# =============================================================================

#' Generate a MOSAIC Color Palette
#'
#' Dispatches to the appropriate palette generator based on type.
#'
#' @param n Number of colors needed.
#' @param type Palette type: \code{"discrete"}, \code{"sequential"}, or \code{"diverging"}.
#' @param name Palette name within type. For sequential: \code{"blues"}, \code{"reds"},
#'   \code{"greens"}, \code{"teals"}, \code{"heat"}, \code{"grays"}.
#'   For diverging: \code{"blue_red"}, \code{"blue_orange"}, \code{"green_purple"}.
#' @param direction \code{1} (default) or \code{-1} to reverse.
#' @param alpha Transparency (0-1, default 1).
#'
#' @return Character vector of hex color codes.
#'
#' @export
mosaic_palette <- function(n,
                           type = c("discrete", "sequential", "diverging"),
                           name = NULL,
                           direction = 1,
                           alpha = 1) {

  type <- match.arg(type)
  switch(type,
    discrete   = mosaic_pal_discrete(n, palette = name %||% "default",
                                     direction = direction, alpha = alpha),
    sequential = mosaic_pal_sequential(n, palette = name %||% "blues",
                                       direction = direction),
    diverging  = mosaic_pal_diverging(n, palette = name %||% "blue_red",
                                      direction = direction)
  )
}


# =============================================================================
# mosaic_pal_discrete — Qualitative Palette
# =============================================================================

#' Generate a Qualitative MOSAIC Palette
#'
#' Returns a set of perceptually distinct, CVD-safe colors for categorical data.
#' For n <= 7, uses the curated default palette. For n <= 12, uses the extended
#' palette. For n > 12, generates colors in HCL space.
#'
#' @param n Number of colors.
#' @param palette Palette name: \code{"default"} (7 colors) or \code{"extended"} (12 colors).
#' @param direction \code{1} (default order) or \code{-1} (reversed).
#' @param alpha Transparency (0-1, default 1).
#'
#' @return Character vector of hex color codes.
#'
#' @export
mosaic_pal_discrete <- function(n,
                                palette = "default",
                                direction = 1,
                                alpha = 1) {

  if (!is.numeric(n) || n < 1) stop("n must be a positive integer")

  pal <- .mosaic_color_db$qualitative[[palette]]
  if (is.null(pal)) pal <- .mosaic_color_db$qualitative[["default"]]

  if (n <= length(pal)) {
    cols <- pal[seq_len(n)]
  } else {
    # Generate in HCL space for large n
    hues <- (245 + seq(0, 360 - 360 / n, length.out = n)) %% 360
    cols <- grDevices::hcl(h = hues, c = 50, l = 55, fixup = TRUE)
  }

  if (direction == -1) cols <- rev(cols)
  if (alpha < 1) cols <- .apply_alpha(cols, alpha)
  unname(cols)
}


# =============================================================================
# mosaic_pal_sequential — Sequential Palette
# =============================================================================

#' Generate a Sequential MOSAIC Palette
#'
#' Interpolates from a stored ramp with monotone luminance for grayscale safety.
#'
#' @param n Number of colors.
#' @param palette Palette name: \code{"blues"} (default), \code{"reds"},
#'   \code{"greens"}, \code{"teals"}, \code{"heat"}, \code{"grays"}.
#' @param direction \code{1} (light to dark) or \code{-1} (dark to light).
#' @param begin Start position in \[0, 1\] (default 0).
#' @param end End position in \[0, 1\] (default 1).
#'
#' @return Character vector of hex color codes.
#'
#' @export
mosaic_pal_sequential <- function(n,
                                  palette = "blues",
                                  direction = 1,
                                  begin = 0,
                                  end = 1) {

  if (!is.numeric(n) || n < 1) stop("n must be a positive integer")

  ramp_colors <- .mosaic_color_db$sequential[[palette]]
  if (is.null(ramp_colors)) {
    warning("Unknown sequential palette '", palette, "', using 'blues'")
    ramp_colors <- .mosaic_color_db$sequential[["blues"]]
  }

  ramp <- grDevices::colorRampPalette(ramp_colors)(1000)
  idx <- seq(max(1, round(begin * 1000)), round(end * 1000), length.out = n)
  cols <- ramp[round(idx)]

  if (direction == -1) cols <- rev(cols)
  cols
}


# =============================================================================
# mosaic_pal_diverging — Diverging Palette
# =============================================================================

#' Generate a Diverging MOSAIC Palette
#'
#' Produces a palette with a neutral midpoint and symmetric endpoints.
#'
#' @param n Number of colors.
#' @param palette Palette name: \code{"blue_red"} (default), \code{"blue_orange"},
#'   \code{"green_purple"}.
#' @param direction \code{1} (default) or \code{-1} (reversed).
#'
#' @return Character vector of hex color codes.
#'
#' @export
mosaic_pal_diverging <- function(n,
                                 palette = "blue_red",
                                 direction = 1) {

  if (!is.numeric(n) || n < 1) stop("n must be a positive integer")

  ramp_colors <- .mosaic_color_db$diverging[[palette]]
  if (is.null(ramp_colors)) {
    warning("Unknown diverging palette '", palette, "', using 'blue_red'")
    ramp_colors <- .mosaic_color_db$diverging[["blue_red"]]
  }

  cols <- grDevices::colorRampPalette(ramp_colors)(n)
  if (direction == -1) cols <- rev(cols)
  cols
}


# =============================================================================
# mosaic_pal_uncertainty — Graduated Uncertainty Ribbons
# =============================================================================

#' Generate Graduated Uncertainty Colors from a Base Color
#'
#' Produces progressively lighter/desaturated variants of the base color for
#' layered credible interval ribbons. The standard levels are 50\%, 75\%, 95\%.
#'
#' Uncertainty is never a standalone color. It is always derived from the
#' point estimate color.
#'
#' @param base_color Hex color for the point estimate (default: MOSAIC blue).
#' @param n_levels Number of uncertainty levels (default 3).
#' @param labels Character vector of level names. Default: \code{c("50\%", "75\%", "95\%")}.
#'
#' @return Named character vector of hex color codes for ribbon fills.
#'
#' @examples
#' # Cases (blue)
#' mosaic_pal_uncertainty()
#'
#' # Deaths (burgundy)
#' mosaic_pal_uncertainty("#B5123B")
#'
#' # Environmental (teal)
#' mosaic_pal_uncertainty("#009988")
#'
#' # Custom levels
#' mosaic_pal_uncertainty(n_levels = 5,
#'   labels = c("10%", "25%", "50%", "75%", "95%"))
#'
#' @export
mosaic_pal_uncertainty <- function(base_color = "#0167AF",
                                   n_levels = 3,
                                   labels = NULL) {

  if (is.null(labels)) {
    if (n_levels == 3) {
      labels <- c("50%", "75%", "95%")
    } else {
      labels <- paste0("Level_", seq_len(n_levels))
    }
  }
  if (length(labels) != n_levels) stop("labels must have length n_levels")

  rgb_base <- grDevices::col2rgb(base_color) / 255
  amounts <- seq(0.25, 0.75, length.out = n_levels)

  cols <- vapply(amounts, function(amt) {
    # Lighten: blend toward white
    lightened <- rgb_base + (1 - rgb_base) * amt * 0.85
    # Desaturate: blend toward gray at same brightness
    gray_val <- 0.2126 * lightened[1] + 0.7152 * lightened[2] + 0.0722 * lightened[3]
    desaturated <- lightened + (gray_val - lightened) * amt * 0.25
    desaturated <- pmin(pmax(desaturated, 0), 1)
    grDevices::rgb(desaturated[1], desaturated[2], desaturated[3])
  }, character(1))

  names(cols) <- labels
  cols
}


# =============================================================================
# mosaic_pal_scenarios — Scenario Comparison Palette
# =============================================================================

#' Generate a Scenario Comparison Palette
#'
#' Returns a dedicated palette for comparing intervention scenarios.
#' The baseline scenario always receives MOSAIC blue (position 1).
#' Scales from 2 to 16 curated colors, then generates in HCL for larger n.
#'
#' @param n Number of scenarios (including baseline).
#' @param labels Optional character vector of scenario names.
#'
#' @return Named character vector of hex color codes.
#'
#' @examples
#' mosaic_pal_scenarios(4)
#' mosaic_pal_scenarios(4, labels = c("Baseline", "Vaccination", "WASH", "Combined"))
#' mosaic_pal_scenarios(12)
#'
#' @export
mosaic_pal_scenarios <- function(n, labels = NULL) {

  if (!is.numeric(n) || n < 1) stop("n must be a positive integer")

  if (n <= 8) {
    cols <- .mosaic_color_db$scenarios$default[seq_len(n)]
  } else if (n <= 16) {
    cols <- .mosaic_color_db$scenarios$extended[seq_len(n)]
  } else {
    # Use curated 16 + HCL for remainder
    curated <- .mosaic_color_db$scenarios$extended
    extra_n <- n - 16
    extra_hues <- seq(30, 330, length.out = extra_n + 2)[2:(extra_n + 1)]
    extra <- grDevices::hcl(h = extra_hues, c = 50, l = 55, fixup = TRUE)
    cols <- c(curated, extra)
  }

  if (is.null(labels)) {
    labels <- c("Baseline", paste0("Scenario_", seq_len(n - 1) + 1))
    if (n == 1) labels <- "Baseline"
  }
  if (length(labels) != n) {
    warning("labels length doesn't match n, using default names")
    labels <- c("Baseline", paste0("Scenario_", seq_len(n - 1) + 1))
  }

  names(cols) <- labels
  cols
}


# =============================================================================
# mosaic_pal_countries — 40-Country SSA Palette
# =============================================================================

#' Generate Colors for MOSAIC SSA Countries
#'
#' Assigns colors to the 40 MOSAIC countries grouped by WHO sub-region.
#' Within each region, hues are evenly spaced at constant chroma and luminance.
#' This ensures geographically close countries have similar but distinguishable colors.
#'
#' @param iso_codes Character vector of ISO3 codes to include. Default: all 40 MOSAIC countries.
#' @param group_by_region If \code{TRUE} (default), assign hue ranges by WHO sub-region.
#'   If \code{FALSE}, use simple HCL rotation for all countries.
#'
#' @return Named character vector keyed by ISO3 code.
#'
#' @examples
#' mosaic_pal_countries()
#' mosaic_pal_countries(c("ETH", "KEN", "TZA"))
#'
#' @export
mosaic_pal_countries <- function(iso_codes = NULL, group_by_region = TRUE) {

  all_isos <- MOSAIC::iso_codes_mosaic

  # Regional groupings (intersected with MOSAIC countries)
  region_map <- list(
    West    = intersect(MOSAIC::iso_codes_africa_west, all_isos),
    East    = intersect(MOSAIC::iso_codes_africa_east, all_isos),
    Central = intersect(MOSAIC::iso_codes_africa_central, all_isos),
    South   = intersect(MOSAIC::iso_codes_africa_south, all_isos)
  )

  if (group_by_region) {
    cols <- c()
    hue_cfg <- .mosaic_color_db$region_hues

    # Find countries not in any region and assign to nearest region
    assigned <- unlist(region_map, use.names = FALSE)
    unassigned <- setdiff(all_isos, assigned)
    # Assign unassigned to Central Africa (reasonable geographic default)
    if (length(unassigned) > 0) {
      region_map[["Central"]] <- sort(c(region_map[["Central"]], unassigned))
    }

    for (region in names(region_map)) {
      isos <- sort(region_map[[region]])
      n <- length(isos)
      if (n == 0) next
      hr <- hue_cfg[[region]]
      hues <- seq(hr["hue_start"], hr["hue_end"], length.out = n + 1)[seq_len(n)]
      region_cols <- grDevices::hcl(h = hues, c = 50, l = 55, fixup = TRUE)
      names(region_cols) <- isos
      cols <- c(cols, region_cols)
    }
  } else {
    n <- length(all_isos)
    hues <- (245 + seq(0, 360 - 360 / n, length.out = n)) %% 360
    cols <- grDevices::hcl(h = hues, c = 50, l = 55, fixup = TRUE)
    names(cols) <- sort(all_isos)
  }

  if (!is.null(iso_codes)) {
    found <- iso_codes %in% names(cols)
    if (!all(found)) {
      warning("Unknown ISO code(s): ", paste(iso_codes[!found], collapse = ", "))
    }
    cols <- cols[iso_codes[found]]
  }

  cols
}


# =============================================================================
# mosaic_color_variant — Color Manipulation
# =============================================================================

#' Create Lighter, Darker, or Muted Variants of a Color
#'
#' Manipulates colors in RGB space. Useful for creating secondary line colors,
#' muted backgrounds, or custom uncertainty shades.
#'
#' @param col One or more hex colors.
#' @param mode Adjustment mode: \code{"lighten"}, \code{"darken"}, or \code{"mute"}.
#' @param amount Adjustment amount (0-1, default 0.3).
#'
#' @return Character vector of adjusted hex colors.
#'
#' @examples
#' mosaic_color_variant("#0167AF", "lighten", 0.4)
#' mosaic_color_variant("#B5123B", "darken", 0.3)
#' mosaic_color_variant(mosaic_colors("cases", "deaths"), "mute", 0.5)
#'
#' @export
mosaic_color_variant <- function(col,
                                 mode = c("lighten", "darken", "mute"),
                                 amount = 0.3) {

  mode <- match.arg(mode)
  if (amount < 0 || amount > 1) stop("amount must be between 0 and 1")

  rgb_mat <- grDevices::col2rgb(col) / 255

  result <- switch(mode,
    lighten = {
      rgb_mat + (1 - rgb_mat) * amount
    },
    darken = {
      rgb_mat * (1 - amount)
    },
    mute = {
      gray_vals <- 0.2126 * rgb_mat[1, ] + 0.7152 * rgb_mat[2, ] + 0.0722 * rgb_mat[3, ]
      gray_mat <- matrix(rep(gray_vals, each = 3), nrow = 3)
      rgb_mat + (gray_mat - rgb_mat) * amount
    }
  )

  result <- pmin(pmax(result, 0), 1)
  grDevices::rgb(result[1, ], result[2, ], result[3, ])
}


# =============================================================================
# plot_mosaic_palette — Palette Visualization
# =============================================================================

#' Display a Color Palette as Swatches
#'
#' Renders colored rectangles with hex codes for visual inspection and QA.
#' Works with any character vector of hex colors.
#'
#' @param colors Character vector of hex colors (named or unnamed).
#' @param labels If \code{TRUE} (default), show hex codes and names below each swatch.
#' @param main Optional plot title.
#'
#' @return Invisible \code{NULL}. Called for its side effect (plot).
#'
#' @examples
#' plot_mosaic_palette(mosaic_colors())
#' plot_mosaic_palette(mosaic_pal_sequential(9, "blues"), main = "Blues")
#' plot_mosaic_palette(mosaic_colors("compartments"), main = "SEIR")
#'
#' @export
plot_mosaic_palette <- function(colors, labels = TRUE, main = NULL) {

  n <- length(colors)
  nms <- names(colors)
  if (is.null(nms)) nms <- rep("", n)

  old_par <- graphics::par(mar = c(2, 0.5, if (!is.null(main)) 2.5 else 0.5, 0.5))
  on.exit(graphics::par(old_par))

  graphics::plot.new()
  graphics::plot.window(xlim = c(0, n), ylim = c(0, 1.6))

  for (i in seq_len(n)) {
    graphics::rect(i - 0.95, 0.3, i - 0.05, 1.2, col = colors[i], border = NA)
  }

  if (labels) {
    for (i in seq_len(n)) {
      graphics::text(i - 0.5, 0.15, colors[i], cex = 0.6, col = "#555555")
      if (nchar(nms[i]) > 0) {
        graphics::text(i - 0.5, 1.35, nms[i], cex = 0.65, col = "#333333", font = 2)
      }
    }
  }

  if (!is.null(main)) {
    graphics::title(main = main, cex.main = 1.1, col.main = "#333333", font.main = 2)
  }

  invisible(NULL)
}


# =============================================================================
# theme_mosaic — Publication-Quality ggplot2 Theme
# =============================================================================

#' MOSAIC ggplot2 Theme
#'
#' A clean, minimal theme designed for publication-quality scientific figures.
#' White background, light gridlines, appropriate text sizing.
#'
#' @param base_size Base font size (default 11).
#' @param base_family Base font family (default \code{""}).
#' @param grid Grid lines to show: \code{"major"} (default), \code{"none"}, or \code{"both"}.
#'
#' @return A ggplot2 theme object.
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#' ggplot(data, aes(x, y)) + geom_point() + theme_mosaic()
#' }
#'
#' @export
theme_mosaic <- function(base_size = 11, base_family = "", grid = "major") {

  th <- ggplot2::theme_minimal(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      panel.grid.minor     = ggplot2::element_blank(),
      panel.grid.major     = ggplot2::element_line(linewidth = 0.3, color = "#E8E8E8"),
      strip.text           = ggplot2::element_text(face = "bold", color = "#333333",
                                                    size = base_size),
      strip.background     = ggplot2::element_rect(fill = "#F5F5F5", color = NA),
      axis.text            = ggplot2::element_text(color = "#555555"),
      axis.title           = ggplot2::element_text(color = "#333333"),
      plot.title           = ggplot2::element_text(face = "bold", color = "#333333",
                                                    size = base_size + 3, hjust = 0),
      plot.subtitle        = ggplot2::element_text(color = "#666666",
                                                    size = base_size + 1, hjust = 0),
      plot.caption         = ggplot2::element_text(color = "#888888",
                                                    size = base_size - 2, hjust = 1),
      legend.background    = ggplot2::element_rect(fill = "white", color = NA),
      legend.key           = ggplot2::element_rect(fill = "white", color = NA),
      plot.margin          = ggplot2::margin(12, 12, 12, 12)
    )

  if (grid == "none") {
    th <- th + ggplot2::theme(panel.grid.major = ggplot2::element_blank())
  } else if (grid == "both") {
    th <- th + ggplot2::theme(
      panel.grid.minor = ggplot2::element_line(linewidth = 0.15, color = "#F0F0F0")
    )
  }

  th
}


# =============================================================================
# ggplot2 Scale Functions
# =============================================================================

#' MOSAIC Discrete Color/Fill Scales for ggplot2
#'
#' Apply the MOSAIC qualitative palette to discrete aesthetics.
#'
#' @param palette Palette name: \code{"default"} or \code{"extended"}.
#' @param direction \code{1} or \code{-1}.
#' @param alpha Transparency (0-1).
#' @param ... Passed to \code{\link[ggplot2]{discrete_scale}}.
#'
#' @return A ggplot2 scale object.
#'
#' @examples
#' \dontrun{
#' ggplot(df, aes(x, y, color = group)) + geom_point() + scale_color_mosaic_d()
#' }
#'
#' @export
scale_color_mosaic_d <- function(palette = "default", direction = 1, alpha = 1, ...) {
  pal_func <- function(n) mosaic_pal_discrete(n, palette, direction, alpha)
  ggplot2::discrete_scale("colour", palette = pal_func, ...)
}

#' @rdname scale_color_mosaic_d
#' @export
scale_colour_mosaic_d <- scale_color_mosaic_d

#' @rdname scale_color_mosaic_d
#' @export
scale_fill_mosaic_d <- function(palette = "default", direction = 1, alpha = 1, ...) {
  pal_func <- function(n) mosaic_pal_discrete(n, palette, direction, alpha)
  ggplot2::discrete_scale("fill", palette = pal_func, ...)
}


#' MOSAIC Sequential Color/Fill Scales for ggplot2
#'
#' Apply a MOSAIC sequential palette to continuous aesthetics.
#'
#' @param palette Palette name: \code{"blues"}, \code{"reds"}, \code{"greens"},
#'   \code{"teals"}, \code{"heat"}, \code{"grays"}.
#' @param direction \code{1} (light to dark) or \code{-1}.
#' @param begin Start position in \[0, 1\].
#' @param end End position in \[0, 1\].
#' @param ... Passed to \code{\link[ggplot2]{scale_colour_gradientn}}.
#'
#' @return A ggplot2 scale object.
#'
#' @examples
#' \dontrun{
#' ggplot(df, aes(x, y, fill = value)) + geom_tile() + scale_fill_mosaic_c("heat")
#' }
#'
#' @export
scale_color_mosaic_c <- function(palette = "blues", direction = 1,
                                  begin = 0, end = 1, ...) {
  cols <- mosaic_pal_sequential(256, palette, direction, begin, end)
  ggplot2::scale_colour_gradientn(colours = cols, ...)
}

#' @rdname scale_color_mosaic_c
#' @export
scale_colour_mosaic_c <- scale_color_mosaic_c

#' @rdname scale_color_mosaic_c
#' @export
scale_fill_mosaic_c <- function(palette = "blues", direction = 1,
                                 begin = 0, end = 1, ...) {
  cols <- mosaic_pal_sequential(256, palette, direction, begin, end)
  ggplot2::scale_fill_gradientn(colours = cols, ...)
}


#' MOSAIC Diverging Color/Fill Scales for ggplot2
#'
#' Apply a MOSAIC diverging palette to continuous aesthetics.
#'
#' @param palette Palette name: \code{"blue_red"}, \code{"blue_orange"},
#'   \code{"green_purple"}.
#' @param direction \code{1} or \code{-1}.
#' @param ... Passed to \code{\link[ggplot2]{scale_colour_gradientn}}.
#'
#' @return A ggplot2 scale object.
#'
#' @export
scale_color_mosaic_div <- function(palette = "blue_red", direction = 1, ...) {
  cols <- mosaic_pal_diverging(256, palette, direction)
  ggplot2::scale_colour_gradientn(colours = cols, ...)
}

#' @rdname scale_color_mosaic_div
#' @export
scale_colour_mosaic_div <- scale_color_mosaic_div

#' @rdname scale_color_mosaic_div
#' @export
scale_fill_mosaic_div <- function(palette = "blue_red", direction = 1, ...) {
  cols <- mosaic_pal_diverging(256, palette, direction)
  ggplot2::scale_fill_gradientn(colours = cols, ...)
}


#' MOSAIC Scenario Color/Fill Scales for ggplot2
#'
#' Apply the dedicated MOSAIC scenario palette to discrete aesthetics.
#' Baseline is always MOSAIC blue (position 1).
#'
#' @param ... Passed to \code{\link[ggplot2]{discrete_scale}}.
#'
#' @return A ggplot2 scale object.
#'
#' @examples
#' \dontrun{
#' ggplot(df, aes(x, y, color = scenario)) +
#'   geom_line() + scale_color_mosaic_scenario()
#' }
#'
#' @export
scale_color_mosaic_scenario <- function(...) {
  pal_func <- function(n) unname(mosaic_pal_scenarios(n))
  ggplot2::discrete_scale("colour", palette = pal_func, ...)
}

#' @rdname scale_color_mosaic_scenario
#' @export
scale_colour_mosaic_scenario <- scale_color_mosaic_scenario

#' @rdname scale_color_mosaic_scenario
#' @export
scale_fill_mosaic_scenario <- function(...) {
  pal_func <- function(n) unname(mosaic_pal_scenarios(n))
  ggplot2::discrete_scale("fill", palette = pal_func, ...)
}


# =============================================================================
# Internal Helpers
# =============================================================================

# %||% is defined in R/aaa_utils.R (loaded first alphabetically)

# Apply alpha transparency to hex colors
.apply_alpha <- function(cols, alpha) {
  rgb_mat <- grDevices::col2rgb(cols)
  grDevices::rgb(rgb_mat[1, ], rgb_mat[2, ], rgb_mat[3, ],
                 alpha = alpha * 255, maxColorValue = 255)
}
