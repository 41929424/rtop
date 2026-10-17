# Multi-row history charts rendered with the public termr custom widget API.
nice_axis_max <- function(values) {
  values <- as.numeric(values)
  values <- values[is.finite(values) & values >= 0]
  peak <- if (length(values)) max(values) else 0
  if (!is.finite(peak) || peak <= 0) return(1)
  magnitude <- 10 ^ floor(log10(peak))
  candidates <- c(1, 2, 5, 10) * magnitude
  candidates[[which(candidates >= peak)[[1L]]]]
}

history_scale_label <- function(values, fixed_max = NULL) {
  if (!is.null(fixed_max) && is.finite(fixed_max) && fixed_max > 0) {
    return(sprintf("0–%.0f%%", fixed_max))
  }
  paste0("0–", format_rate(nice_axis_max(values)))
}

history_plot_lines <- function(values, width, height, fixed_max = NULL) {
  width <- max(1L, as.integer(width))
  height <- max(1L, as.integer(height))
  values <- as.numeric(values)
  values <- values[is.finite(values) & values >= 0]
  if (!length(values)) return(rep(strrep(" ", width), height))
  values <- tail(values, width)
  if (length(values) < width) values <- c(rep(NA_real_, width - length(values)), values)
  ceiling <- if (!is.null(fixed_max) && is.finite(fixed_max) && fixed_max > 0) fixed_max else nice_axis_max(values)
  levels <- c(" ", "▁", "▂", "▃", "▄", "▅", "▆", "▇", "█")
  rows <- matrix(" ", nrow = height, ncol = width)
  for (col in seq_len(width)) {
    value <- values[[col]]
    if (!is.finite(value)) next
    units <- if (value <= 0) 0L else max(1L,
      min(height * 8L, as.integer(round(value / ceiling * height * 8L))))
    if (units == 0L) {
      rows[height, col] <- "·"
      next
    }
    for (row in seq_len(height)) {
      from_bottom <- (height - row) * 8L
      fill <- max(0L, min(8L, units - from_bottom))
      if (fill > 0L) rows[row, col] <- levels[[fill + 1L]]
    }
  }
  apply(rows, 1L, paste0, collapse = "")
}

history_chart_lines <- function(values, width, height, fixed_max = NULL, series = NULL) {
  width <- max(1L, as.integer(width))
  height <- max(1L, as.integer(height))
  scale <- history_scale_label(values, fixed_max)
  if (!is.null(series) && nzchar(series)) scale <- paste(series, scale)
  values <- as.numeric(values)
  has_data <- any(is.finite(values) & values >= 0)
  if (!has_data) {
    line <- paste0(scale, "  no samples")
    return(c(substr(line, 1L, width), rep(strrep(" ", width), max(0L, height - 1L))))
  }
  plot_height <- max(1L, height - 1L)
  plot <- history_plot_lines(values, width, plot_height, fixed_max)
  prefix <- paste0(scale, " ┤")
  prefix_width <- nchar(prefix, type = "width")
  first <- paste0(prefix, substr(plot[[1L]], 1L, max(0L, width - prefix_width)))
  c(first, plot[-1L])
}

history_chart <- function(data, id, height = "1fr", max_value = NULL, series = NULL) {
  chart_type <- termr::widget("HistoryChart", state = list(values = numeric()),
    render = function(self) {
      area <- self$region
      width <- if (!is.null(area)) max(1L, area$width) else 60L
      height <- if (!is.null(area)) max(1L, area$height) else 3L
      history_chart_lines(self$values, width, height, max_value, series)
    }, style = termr::style(height = height))
  chart <- chart_type(id = id)
  chart$bind_reactive("values", data)
  chart
}
