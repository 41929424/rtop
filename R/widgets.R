collector_status_text <- function(state, name) {
  status <- state$collector_status()[[name]]
  if (is.null(status) || isTRUE(status$ok)) return("")
  paste0("  ERROR: ", status$message)
}

system_panel <- function(state, table) {
  narrow <- state$terminal_size()[[1L]] < 72L
  cpu <- cpu_panel(state)
  memory <- memory_panel(state)
  network <- network_panel(state)
  disks <- disk_panel(state)
  left <- termr::vertical(cpu, network, id = "left-metrics-column", style = termr::style(height = "1fr"))
  right <- termr::vertical(memory, disks, id = "right-metrics-column", style = termr::style(height = "1fr"))
  dashboard <- termr::grid_layout(
    left, right,
    columns = if (narrow) 1L else 2L, rows = if (narrow) 2L else 1L, gap = 0L, id = "dashboard-grid",
    style = termr::style(height = "3fr")
  )
  processes <- termr::panel(termr::vertical(
    termr::horizontal(
      termr::label(function() paste0(toupper(state$sort_by()), " sort  ·  / filter  ·  c/m/p sort  ·  q quit", collector_status_text(state, "processes")),
        style = termr::style(width = "1fr"), id = "process-hint"),
      termr::input(id = "search", placeholder = "Filter processes", style = termr::style(width = 20L, height = "auto")),
      termr::label(function() paste0(state$status(), "  ", state$clock()),
        style = termr::style(width = 17L, align = "right"), id = "status-text")
    ),
    table
  ), title = "Processes", id = "processes-panel", style = termr::style(height = "2fr"))
  root <- termr::vertical(dashboard, processes, id = "rtop-root", style = termr::style(height = "1fr"))
  attr(root, "rtop_dashboard_grid") <- dashboard
  root
}

chart_heights <- function(screen_height, disk_count = 2L) {
  top_height <- max(8L, floor((as.integer(screen_height) - 4L) * 0.60))
  column_panel <- max(4L, floor(top_height / 2L))
  disk_panel <- max(5L, min(8L, as.integer(disk_count) + 4L))
  memory_panel <- max(4L, top_height - disk_panel)
  c(cpu = max(1L, column_panel - 6L),
    memory = max(1L, memory_panel - 7L),
    network = max(1L, floor((column_panel - 6L) / 2L)))
}

apply_chart_heights <- function(root, screen_height, disk_count = 2L) {
  heights <- chart_heights(screen_height, disk_count)
  ids <- c(cpu = "cpu-history", memory = "memory-history",
    network_rx = "network-rx-history", network_tx = "network-tx-history")
  for (name in names(ids)) {
    chart <- root$query_one(paste0("#", ids[[name]]))
    if (!is.null(chart)) chart$style <- termr::style(height = heights[[if (startsWith(name, "network")) "network" else name]])
  }
  invisible(heights)
}

rtop_theme <- function() {
  termr::termr_theme("dark", background = "#101521", foreground = "#d8dee9",
    primary = "#33485b", on_primary = "#e5edf5", warning = "#26394a")
}

rtop_stylesheet <- function() termr::stylesheet(paste(
  "Screen { color: #d8dee9; }",
  "#rtop-root { background: #101521; color: #d8dee9; }",
  "Panel { background: #182232; border: round #34445c; padding: 0 1; }",
  "#cpu-panel { border: round #36c8df; }",
  "#memory-panel { border: round #bd83f5; }",
  "#network-panel { border: round #65d98a; }",
  "#disk-panel { border: round #f0a35e; }",
  "#cpu-summary { color: #d8dee9; bold: true; }",
  "#cpu-cores { color: #36c8df; }",
  "#cpu-history { color: #36c8df; }",
  "#memory-summary { color: #bd83f5; bold: true; }",
  "#memory-bar { color: #bd83f5; }",
  "#memory-swap-bar { color: #8263ad; }",
  "#memory-history { color: #bd83f5; }",
  "#network-summary { color: #65d98a; bold: true; }",
  "#network-rx-history { color: #65d98a; }",
  "#network-tx-history { color: #f0a35e; }",
  "#disk-panel { color: #f0c36d; }",
  "#processes-panel { border: round #52647a; }",
  "#process-hint { color: #9baac0; }",
  "#status-text { color: #93a0b4; }",
  "#process-table { background: #182232; color: #d8dee9; }",
  "#process-table:cursor_row { background: #26394a; color: #e5edf5; bold: true; }",
  sep = "\n"
))
