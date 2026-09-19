#!/usr/bin/env Rscript
# Combined production-vs-A1b fit comparison across countries.
# Usage: Rscript fig_multi.R <dir_with_csvs> <out_png>
suppressMessages({library(ggplot2); library(dplyr); library(zoo); library(scales)})
args <- commandArgs(trailingOnly = TRUE)
DIR <- args[1]; OUT <- if (length(args) >= 2) args[2] else "fig_multi.png"

files <- list.files(DIR, pattern = "^(prod|A1b)_[A-Z]{3}\\.csv$", full.names = TRUE)
stopifnot(length(files) > 0)
d <- bind_rows(lapply(files, function(f) {
     b   <- sub("\\.csv$", "", basename(f))
     arm <- sub("_[A-Z]{3}$", "", b); iso <- sub("^.*_", "", b)
     x <- read.csv(f, stringsAsFactors = FALSE)
     x$date <- as.Date(x$date)
     x$arm <- ifelse(arm == "prod", "Production", "A1b variant")
     x$iso <- iso
     x
}))
cname <- c(COD = "DR Congo (COD)", MOZ = "Mozambique (MOZ)", ETH = "Ethiopia (ETH)")
d$chan <- factor(ifelse(grepl("case", d$metric, ignore.case = TRUE),
                        "Suspected cases", "Deaths"),
                 levels = c("Suspected cases", "Deaths"))
d$arm  <- factor(d$arm, levels = c("Production", "A1b variant"))
d$country <- factor(cname[d$iso], levels = cname[c("COD","MOZ","ETH")])

sm <- function(x) rollapply(x, 7, function(v) mean(v, na.rm = TRUE), fill = NA, align = "center")
d <- d %>% arrange(arm, country, chan, date) %>%
     group_by(arm, country, chan) %>%
     mutate(across(c(observed, predicted_median, ci_2_lower, ci_2_upper), sm,
                   .names = "s_{.col}")) %>% ungroup() %>%
     filter(date >= as.Date("2021-01-01"), date <= as.Date("2026-06-01"))
obs <- d %>% filter(arm == "Production") %>% select(country, chan, date, s_observed)

pal <- c("Production" = "#C2413C", "A1b variant" = "#2E6F9E")
p <- ggplot() +
     geom_ribbon(data = d %>% filter(is.finite(s_ci_2_lower)),
                 aes(date, ymin = s_ci_2_lower, ymax = s_ci_2_upper, fill = arm), alpha = .16) +
     geom_line(data = obs %>% filter(is.finite(s_observed)),
               aes(date, s_observed), colour = "grey12", linewidth = .62) +
     geom_line(data = d %>% filter(is.finite(s_predicted_median)),
               aes(date, s_predicted_median, colour = arm), linewidth = .62) +
     facet_grid(chan ~ country, scales = "free_y", switch = "y") +
     scale_colour_manual(values = pal, name = NULL) +
     scale_fill_manual(values = pal, name = NULL) +
     scale_y_continuous(labels = comma) +
     scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
     labs(title = "Production vs A1b: observed data and fitted posterior ensemble",
          subtitle = "Full calibration, 30,000 draws per run - black = observed, band = 2-98% CI, 7-day mean",
          x = NULL, y = "Count per day") +
     theme_minimal(base_size = 11) +
     theme(legend.position = "top", panel.grid.minor = element_blank(),
           strip.text = element_text(face = "bold"),
           strip.placement = "outside",
           plot.title = element_text(face = "bold"),
           panel.spacing = unit(0.9, "lines"))
ggsave(OUT, p, width = 13, height = 7.2, dpi = 150)
cat("written:", OUT, "\n")
