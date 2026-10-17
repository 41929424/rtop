format_bytes <- function(x, digits = 1) {
  if (length(x) != 1L || is.na(x) || !is.finite(x) || x < 0) return("—")
  units <- c("B", "KiB", "MiB", "GiB", "TiB", "PiB")
  i <- 1L
  while (x >= 1024 && i < length(units)) { x <- x / 1024; i <- i + 1L }
  sprintf(paste0("%.", digits, "f %s"), x, units[[i]])
}

format_rate <- function(x) paste0(format_bytes(x), "/s")

format_pct <- function(x, digits = 1) {
  if (length(x) != 1L || is.na(x) || !is.finite(x)) return("—")
  sprintf(paste0("%.", digits, "f%%"), max(0, min(100, x)))
}

# termr 1.0.1 computes bar_w - round(bar_w * value / total) without
# clamping the value. Keep unavailable metrics indeterminate and constrain
# measured percentages before handing them to ProgressBar.
progress_percent <- function(value) {
  if (length(value) != 1L) return(NA_real_)
  missing <- tryCatch(is.na(value), error = function(e) TRUE)
  if (length(missing) != 1L || isTRUE(missing)) return(NA_real_)
  if (!is.numeric(value) || !is.finite(value)) return(NA_real_)
  max(0, min(100, as.numeric(value)))
}

clamp_value <- function(x, lower = 0, upper = 100) {
  if (length(x) != 1L || is.na(x) || !is.finite(x)) return(NA_real_)
  max(lower, min(upper, x))
}

append_history <- function(history, value, limit = 120L) {
  if (length(value) != 1L || !is.finite(value)) return(tail(history, limit))
  tail(c(history, value), max(1L, as.integer(limit)))
}
