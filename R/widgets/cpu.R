cpu_core_grid <- function(cores, terminal_width = 80L) {
  if (!length(cores)) return("Per-core usage collecting…")
  panel_width <- max(20L, floor((as.integer(terminal_width) - 6L) / 2L))
  cell_width <- 12L
  columns <- max(1L, min(length(cores), floor(panel_width / cell_width)))
  values <- vapply(seq_along(cores), function(i) {
    value <- cores[[i]]
    valid <- length(value) == 1L && is.finite(value)
    pct <- if (valid) sprintf("%3.0f%%", max(0, min(100, value))) else "  — "
    filled <- if (valid) max(0L, min(4L, round(value / 100 * 4L))) else 0L
    bar <- paste0(strrep("█", filled), strrep("░", 4L - filled))
    core <- sub("^cpu", "", names(cores)[[i]])
    sprintf("%2s %s %s", core, pct, bar)
  }, character(1))
  rows <- split(values, ceiling(seq_along(values) / columns))
  lines <- vapply(rows, function(row) paste(sprintf("%-*s", cell_width, row), collapse = ""), character(1))
  paste(sub(" +$", "", lines), collapse = "\n")
}

cpu_panel <- function(state) {
  graph_height <- chart_heights(state$terminal_size()[[2L]])[["cpu"]]
  termr::panel(termr::vertical(
    termr::label(function() {
      x <- state$cpu()
      status <- collector_status_text(state, "cpu")
      if (is.null(x) || is.null(x$usage)) return(paste0("CPU  collecting baseline", status))
      paste0("CPU  ", format_pct(x$usage$total), "   LOAD  ",
        paste(ifelse(is.finite(x$load), sprintf("%.2f", x$load), "—"), collapse = "  "), status)
    }, id = "cpu-summary"),
    history_chart(function() state$cpu_history(), "cpu-history", graph_height, 100),
    termr::label(function() {
      x <- state$cpu()
      cores <- if (is.null(x) || is.null(x$usage)) numeric() else x$usage$cores
      cpu_core_grid(cores, state$terminal_size()[[1L]])
    }, id = "cpu-cores"),
    termr::label(function() {
      x <- state$cpu()
      if (is.null(x)) return(paste0("Frequency —   Temperature —", collector_status_text(state, "cpu")))
      freq <- if (is.finite(x$frequency_mhz)) sprintf("%.0f MHz", x$frequency_mhz) else "—"
      temp <- if (is.finite(x$temperature_c)) sprintf("%.1f°C", x$temperature_c) else "—"
      paste("Frequency", freq, "  Temp", temp, collector_status_text(state, "cpu"))
    }, id = "cpu-detail")
  ), title = "CPU", id = "cpu-panel", style = termr::style(height = "1fr"))
}
