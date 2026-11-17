# ---------------------------------------------------------------------------
# Preliminary assessment of UETCL's current day-ahead forecasting method
# (maximum of four similar days), with weekly seasonal naive as comparison.
#
# Proposal: Mpalamapa Mwebaze Reagan, Master of Statistics, Makerere University
# Output:   Appendix B table and figure; key numbers for Section 1.2
#
# Usage:    Rscript baseline_current_method.R  path/to/hourly_demand.csv
# Packages: data.table, ggplot2
# ---------------------------------------------------------------------------
install.packages("ggplot2")
library(data.table)
library(ggplot2)

args    <- commandArgs(trailingOnly = TRUE)
infile  <- if (length(args) >= 1) args[1] else "hourly_demand.csv"
outdir  <- "baseline_results"
dir.create(outdir, showWarnings = FALSE)

# ---- Settings --------------------------------------------------------------
N_SIMILAR   <- 4            # number of similar days used by the rule
MAX_WEEKS   <- 8            # how far back the rule may look if days are missing
PEAK_HOURS  <- 19:22        # hours starting 19:00-22:00, i.e. 19:00-23:00
RECENT_FROM <- as.IDate("2025-01-01")

# ---- 1. Data ---------------------------------------------------------------
raw <- fread(infile, select = c("date", "hour_of_day", "domestic_demand_mw"))
raw[, date := as.IDate(date)]
setnames(raw, c("date", "hour", "load"))

# Complete calendar so that missing days appear as NA (Nov 2017, Aug 2023)
grid <- CJ(date = seq(min(raw$date), max(raw$date), by = 1), hour = 0:23)
d    <- raw[grid, on = .(date, hour)]

# Wide matrix: one row per day, one column per hour (fast lookups by date)
days <- sort(unique(d$date))
L    <- as.matrix(dcast(d, date ~ hour, value.var = "load")[, -1])
row_of <- function(dates) match(dates, days)

# ---- 2. Forecasts ----------------------------------------------------------
# Similar days = same weekday in previous weeks (d-7, d-14, ...).
# For each hour, take the most recent N_SIMILAR weeks that have data,
# stepping back one more week whenever a similar day is missing.
# Holidays receive no special treatment at this preliminary stage.
forecast_day <- function(i) {
  past <- i - 7 * seq_len(MAX_WEEKS)
  past <- past[past >= 1]
  if (length(past) == 0) return(NULL)
  H <- L[past, , drop = FALSE]                     # weeks x 24 hours
  rule  <- apply(H, 2, function(x) {
    x <- x[!is.na(x)]
    if (length(x) < N_SIMILAR) NA_real_ else max(x[1:N_SIMILAR])
  })
  snaive <- apply(H, 2, function(x) {              # last available same weekday
    x <- x[!is.na(x)]
    if (length(x) == 0) NA_real_ else x[1]
  })
  data.table(date = days[i], hour = 0:23, actual = L[i, ],
             current_method = rule, seasonal_naive = snaive)
}

fc <- rbindlist(lapply(seq_along(days), forecast_day))
fc <- fc[!is.na(actual) & !is.na(current_method) & !is.na(seasonal_naive)]

long <- melt(fc, id.vars = c("date", "hour", "actual"),
             variable.name = "method", value.name = "forecast")
long[, method := factor(method, labels = c("Current method (max of 4 similar days)",
                                           "Seasonal naive (same hour last week)"))]
long[, err := forecast - actual]          # positive = over-forecast

# ---- 3. Accuracy measures --------------------------------------------------
# MAPE excludes hours with actual load below 100 MW: these are grid
# disturbances (e.g. 1-10 MW readings) where percentage errors explode.
metrics <- function(x) x[, .(
  hours          = .N,
  MAE_MW         = mean(abs(err)),
  RMSE_MW        = sqrt(mean(err^2)),
  MAPE_pct       = 100 * mean(abs(err[actual >= 100]) / actual[actual >= 100]),
  Bias_MW        = mean(err),
  Bias_pct       = 100 * mean(err) / mean(actual),
  Over_forecast_pct = 100 * mean(err > 0)
), by = method]

periods <- list(
  "All hours, 2016-2026"          = long,
  "Evening peak (19-23h), 2016-2026" = long[hour %in% PEAK_HOURS],
  "All hours, 2025-2026"          = long[date >= RECENT_FROM]
)
tab <- rbindlist(lapply(names(periods), function(p)
  cbind(scope = p, metrics(periods[[p]]))))
num <- names(tab)[sapply(tab, is.double)]
tab[, (num) := lapply(.SD, round, 1), .SDcols = num]

fwrite(tab, file.path(outdir, "table_B1_accuracy.csv"))
fwrite(fc,  file.path(outdir, "hourly_forecasts.csv"))

cat("\nEvaluation period:", format(min(fc$date)), "to", format(max(fc$date)),
    "|", uniqueN(fc$date), "days,", nrow(fc), "hours\n\n")
print(tab, row.names = FALSE)

# ---- 4. Figure: mean error by hour of day ----------------------------------
by_hour <- long[, .(bias = mean(err)), by = .(method, hour)]
p <- ggplot(by_hour, aes(hour, bias, colour = method)) +
  geom_hline(yintercept = 0, colour = "grey50") +
  annotate("rect", xmin = min(PEAK_HOURS) - 0.5, xmax = max(PEAK_HOURS) + 0.5,
           ymin = -Inf, ymax = Inf, alpha = 0.08) +
  geom_line(linewidth = 0.9) + geom_point(size = 1.6) +
  scale_x_continuous(breaks = seq(0, 23, 2)) +
  scale_colour_manual(values = c("#B2442C", "#2C6E9E")) +
  labs(x = "Hour of day", y = "Mean error, forecast minus actual (MW)",
       colour = NULL,
       caption = "Domestic demand, 2016-2026. Shaded: evening peak 19:00-23:00. Positive = over-forecast.") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "top", panel.grid.minor = element_blank())
ggsave(file.path(outdir, "figure_B1_bias_by_hour.png"), p,
       width = 7, height = 4.2, dpi = 300, bg = "white")

cat("\nResults written to", normalizePath(outdir), "\n")
