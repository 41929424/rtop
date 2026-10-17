selected_network_history <- function(state, direction) {
  x <- state$network()
  histories <- state$network_history()
  if (is.null(x) || is.null(x$selected) || is.null(histories[[x$selected]])) return(numeric())
  result <- histories[[x$selected]][[direction]]
  if (is.null(result)) numeric() else result
}

network_panel <- function(state) {
  graph_height <- chart_heights(state$terminal_size()[[2L]])[["network"]]
  termr::panel(termr::vertical(
    termr::label(function() {
      x <- state$network()
      status <- collector_status_text(state, "network")
      if (is.null(x) || !length(x$interfaces)) return(paste0("No network interfaces", status))
      iface <- x$selected
      rate <- x$rates[[iface]]
      rx <- if (is.null(rate)) NA_real_ else rate[["rx_rate"]]
      tx <- if (is.null(rate)) NA_real_ else rate[["tx_rate"]]
      paste0(iface, "   ↓ ", format_rate(rx), "   ↑ ", format_rate(tx), status)
    }, id = "network-summary"),
    termr::label(function() {
      x <- state$network(); h <- if (is.null(x) || is.null(x$selected)) NULL else x$counters[[x$selected]]
      if (is.null(h)) "RX —   TX —" else paste("RX", format_bytes(h[["rx"]]), "  TX", format_bytes(h[["tx"]]))
    }, id = "network-totals"),
    history_chart(function() selected_network_history(state, "rx"), "network-rx-history", graph_height, series = "RX"),
    history_chart(function() selected_network_history(state, "tx"), "network-tx-history", graph_height, series = "TX")
  ), title = "Network", id = "network-panel", style = termr::style(height = "1fr"))
}
