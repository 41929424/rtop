start_rtop <- function() {
  state <- new_rtop_state()
  table <- process_table()
  cpu_previous <- NULL
  net_previous <- NULL
  proc_previous <- NULL
  disk_previous <- NULL
  cpu_time <- net_time <- proc_time <- disk_time <- unname(proc.time()[[3L]])
  timer <- NULL
  application <- NULL
  previous_display <- NULL
  layout_disk_count <- 2L

  refresh <- function(app = NULL) {
    now <- unname(proc.time()[[3L]])
    old_network <- state$network()
    old_memory <- state$memory()
    old_disks <- state$disks()
    old_processes <- state$processes()
    old_cpu <- state$cpu()
    outcome <- tryCatch({
      collected <- collect_independently(
        readers = list(
          cpu = function() collect_cpu(cpu_previous),
          memory = collect_memory,
          network = function() collect_network(net_previous, max(0, now - net_time)),
          disks = function() collect_disks(previous = disk_previous, elapsed = max(0, now - disk_time)),
          processes = function() collect_processes(previous = proc_previous, elapsed = max(0, now - proc_time),
            total_memory = if (isTRUE(old_memory$available)) old_memory$total else NA_real_)
        ),
        previous = list(cpu = old_cpu, memory = old_memory, network = old_network,
          disks = old_disks, processes = old_processes),
        validators = list(
          cpu = function(x) !is.null(x$sample),
          memory = function(x) isTRUE(x$available) && is.finite(x$total) && x$total > 0,
          network = function(x) length(x$interfaces) > 0L,
          disks = function(x) nrow(x$filesystems) > 0L,
          processes = function(x) nrow(x$data) > 0L
        )
      )
      x <- collected$values
      status <- collected$statuses

      cpu <- x$cpu
      if (isTRUE(status$cpu$ok)) {
        cpu_previous <<- cpu$sample
        cpu_time <<- now
        state$cpu(cpu)
        if (!is.null(cpu$usage)) push_history_signal(state$cpu_history, cpu$usage$total)
      }
      mem <- x$memory
      if (isTRUE(status$memory$ok)) {
        if (!identical(old_memory, mem)) state$memory(mem)
        push_history_signal(state$memory_history, mem$percent)
      }
      net <- x$network
      if (isTRUE(status$network$ok)) {
        net_previous <<- net$counters
        net_time <<- now
        selected <- if (!is.null(old_network$selected) && old_network$selected %in% net$interfaces) old_network$selected else net$interfaces[[1L]]
        net$selected <- selected
        state$network(net)
        if (length(net$rates)) {
          histories <- state$network_history()
          for (iface in names(net$rates)) {
            history <- histories[[iface]]
            if (is.null(history)) history <- list(rx = numeric(), tx = numeric())
            history$rx <- append_history(history$rx, net$rates[[iface]][["rx_rate"]], 120L)
            history$tx <- append_history(history$tx, net$rates[[iface]][["tx_rate"]], 120L)
            histories[[iface]] <- history
          }
          if (!identical(state$network_history(), histories)) state$network_history(histories)
        }
      }
      disks <- x$disks
      if (isTRUE(status$disks$ok)) {
        disk_previous <<- disks$counters
        disk_time <<- now
        if (!identical(old_disks, disks)) state$disks(disks)
        new_disk_count <- disk_count(state)
        if (!identical(new_disk_count, layout_disk_count)) {
          layout_disk_count <<- new_disk_count
          if (!is.null(application)) {
            apply_chart_heights(ui, state$terminal_size()[[2L]], new_disk_count)
          }
        }
      }
      proc <- x$processes
      if (isTRUE(status$processes$ok)) {
        proc_previous <<- proc$counters
        proc_time <<- now
        if (isTRUE(status$memory$ok) && is.finite(mem$total) && mem$total > 0) proc$data$mem <- 100 * proc$data$rss / mem$total
        if (!identical(old_processes, proc$data)) state$processes(proc$data)
      }
      state$collector_status(status)
      display <- process_rows(state$processes(), state$sort_by(), state$search())
      if (is.null(previous_display) || !identical(display, previous_display)) {
        selected_pid <- state$selected_pid()
        table$set_data(display)
        if (is.finite(selected_pid) && nrow(display)) {
          row <- match(selected_pid, display$PID)
          if (!is.na(row)) table$move_cursor(row)
        }
        previous_display <<- display
      }
      failed <- names(status)[!vapply(status, function(item) isTRUE(item$ok), logical(1))]
      state$status(if (length(failed)) paste("Collector error:", paste(failed, collapse = ", ")) else "Live")
      state$clock(format(Sys.time(), "%H:%M:%S"))
      TRUE
    }, error = function(e) {
      log_collector_error("refresh", conditionMessage(e))
      state$status("Refresh error")
      FALSE
    })
    invisible(NULL)
  }

  set_refresh_interval <- function(app, ms) {
    ms <- max(250L, min(5000L, as.integer(ms)))
    if (!is.null(timer)) timer$cancel()
    state$interval_ms(ms)
    timer <<- app$set_interval(ms / 1000, function(app) refresh(app))
    state$status("Live")
  }

  bindings <- list(
    termr::bind("q", "quit", "Quit"),
    termr::bind("c", function(app) state$sort_by("cpu"), "Sort by CPU"),
    termr::bind("m", function(app) state$sort_by("mem"), "Sort by memory"),
    termr::bind("p", function(app) state$sort_by("pid"), "Sort by PID"),
    termr::bind("n", function(app) {
      x <- state$network()
      if (is.null(x) || !length(x$interfaces)) return(invisible(NULL))
      current <- match(x$selected, x$interfaces)
      x$selected <- x$interfaces[if (is.na(current) || current >= length(x$interfaces)) 1L else current + 1L]
      state$network(x)
    }, "Next network interface"),
    termr::bind("/", function(app) { app$query_one("#search")$focus(); state$status("Type to filter; Escape clears") }, "Search processes"),
    termr::bind("+", function(app) set_refresh_interval(app, state$interval_ms() + 250L), "Faster updates"),
    termr::bind("-", function(app) set_refresh_interval(app, state$interval_ms() - 250L), "Slower updates"),
    termr::bind("h", function(app) state$status("Keys: q quit; c/m/p sort; / search; Esc clear; n interface; +/- interval"), "Help")
  )

  ui <- system_panel(state, table)
  apply_chart_heights(ui, state$terminal_size()[[2L]], layout_disk_count)
  dashboard_grid <- attr(ui, "rtop_dashboard_grid")
  application <- termr::app(ui, bindings = bindings, theme = rtop_theme(), title = "rtop",
    stylesheet = rtop_stylesheet())
  application$on("resize", handler = function(event, app) {
    state$terminal_size(c(event$width, event$height))
    apply_chart_heights(ui, event$height, layout_disk_count)
    columns <- if (event$width < 72L) 1L else 2L
    dashboard_grid$style <- termr::style(height = "3fr", grid_columns = columns,
      grid_rows = if (columns == 1L) 2L else 1L)
  })
  application$on("input.changed", "#search", function(event, app) state$search(event$data$value))
  application$on("key", "#search", function(event, app) {
    if (event$key == "escape") {
      state$search("")
      app$query_one("#search")$set(value = "")
      app$query_one("#process-table")$focus()
      event$stop()
    }
  })
  application$on("datatable.row_selected", "#process-table", function(event, app) {
    row <- event$data$value
    if (!is.null(row$PID)) state$selected_pid(as.numeric(row$PID))
  })
  refresh()
  application$on("mount", "#process-table", function(event, app) {
    state$terminal_size(app$size)
    apply_chart_heights(ui, app$size[[2L]], layout_disk_count)
    app$query_one("#process-table")$focus()
    set_refresh_interval(app, state$interval_ms())
  })
  termr::run(application)
}
