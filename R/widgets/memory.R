memory_panel <- function(state) {
  graph_height <- chart_heights(state$terminal_size()[[2L]])[["memory"]]
  termr::panel(termr::vertical(
    termr::label(function() {
      x <- state$memory()
      status <- collector_status_text(state, "memory")
      if (is.null(x) || !isTRUE(x$available)) return(paste0("RAM unavailable", status))
      paste0("RAM  ", format_pct(x$percent), "   ", format_bytes(x$used), " / ", format_bytes(x$total), status)
    }, id = "memory-summary"),
    termr::progress_bar(function() {
      x <- state$memory()
      if (is.null(x) || !isTRUE(x$available)) NA_real_ else progress_percent(x$percent)
    }, total = 100, id = "memory-bar"),
    history_chart(function() state$memory_history(), "memory-history", graph_height, 100),
    termr::label(function() {
      x <- state$memory()
      if (is.null(x) || !isTRUE(x$available)) return("Swap unavailable")
      paste0("SWAP  ", format_pct(x$swap_percent), "   ", format_bytes(x$swap_used), " / ", format_bytes(x$swap_total))
    }, id = "memory-swap"),
    termr::progress_bar(function() {
      x <- state$memory()
      if (is.null(x) || !isTRUE(x$available)) NA_real_ else progress_percent(x$swap_percent)
    }, total = 100, id = "memory-swap-bar")
  ), title = "Memory", id = "memory-panel", style = termr::style(height = "1fr"))
}
