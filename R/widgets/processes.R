process_table <- function() termr::data_table(
  data.frame(PID = numeric(), USER = character(), NAME = character(), `CPU%` = character(),
    `MEM%` = character(), RSS = character(), STATE = character(), check.names = FALSE),
  id = "process-table", zebra = FALSE,
  row_style = function(row, data) termr::style(background = "#1c2a3a"),
  columns = list(PID = termr::column(width = 7, align = "right"), USER = termr::column(width = 8),
    NAME = termr::column(width = 22), `CPU%` = termr::column(width = 7, align = "right"),
    `MEM%` = termr::column(width = 7, align = "right"), RSS = termr::column(width = 9, align = "right"),
    STATE = termr::column(width = 5))
)

process_rows <- function(x, sort_by, search) {
  if (is.null(x) || !nrow(x)) return(data.frame(PID = numeric(), USER = character(), NAME = character(),
    `CPU%` = character(), `MEM%` = character(), RSS = character(), STATE = character(), check.names = FALSE))
  x <- sort_processes(x, sort_by)
  if (nzchar(search)) x <- x[grepl(search, x$name, ignore.case = TRUE), , drop = FALSE]
  data.frame(PID = x$pid, USER = x$user, NAME = x$name,
    `CPU%` = ifelse(is.finite(x$cpu), sprintf("%.1f", x$cpu), "—"),
    `MEM%` = ifelse(is.finite(x$mem), sprintf("%.1f", x$mem), "—"),
    RSS = vapply(x$rss, format_bytes, character(1)), STATE = x$state,
    check.names = FALSE, stringsAsFactors = FALSE)
}

selected_process_text <- function(state) {
  x <- state$processes(); pid <- state$selected_pid()
  if (!nrow(x) || !is.finite(pid)) return("Select a process to inspect it")
  row <- x[match(pid, x$pid), , drop = FALSE]
  if (!nrow(row) || is.na(row$pid[[1L]])) return("Selected process has exited")
  paste0("PID ", format(row$pid[[1L]], scientific = FALSE), "  ", row$user[[1L]], "  ", row$name[[1L]],
    "  CPU ", format_pct(row$cpu[[1L]]), "  MEM ", format_pct(row$mem[[1L]]), "  RSS ", format_bytes(row$rss[[1L]]), "  state ", row$state[[1L]])
}
