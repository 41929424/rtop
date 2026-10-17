root <- normalizePath(".", winslash = "/")
modules <- c("R/platform.R", "R/format.R", "R/collectors.R", "R/collectors/cpu.R", "R/collectors/memory.R", "R/collectors/network.R",
             "R/collectors/disk.R", "R/collectors/processes.R", "R/state.R", "R/widgets/history.R",
             "R/widgets/cpu.R", "R/widgets/memory.R", "R/widgets/network.R", "R/widgets/disk.R", "R/widgets/processes.R",
             "R/widgets.R")
for (module in modules) sys.source(file.path(root, module), envir = .GlobalEnv)
stopifnot(isTRUE(check_platform("Linux")))
windows_message <- tryCatch(check_platform("Windows"), error = conditionMessage)
darwin_message <- tryCatch(check_platform("Darwin"), error = conditionMessage)
stopifnot(is.character(windows_message), grepl("Ubuntu WSL", windows_message, fixed = TRUE),
  is.character(darwin_message), grepl("macOS", darwin_message, fixed = TRUE))
fixture <- function(name) file.path(root, "tests", "fixtures", name)

mem <- parse_meminfo(readLines(fixture("meminfo.txt")))
stopifnot(mem[["MemTotal"]] == 1024000000, mem[["MemAvailable"]] == 409600000)

old <- read_proc_stat(fixture("proc-stat.txt"))
new_file <- tempfile()
writeLines(c("cpu 120 20 40 430 10 2 3 0 0 0", "cpu0 60 10 20 215 5 1 1 0 0 0", "cpu1 60 10 20 215 5 1 2 0 0 0"), new_file)
new <- read_proc_stat(new_file)
usage <- cpu_delta(old, new)
stopifnot(is.finite(usage$total), usage$total > 0, usage$total < 100, length(usage$cores) == 2L)
unlink(new_file)

net_a <- parse_net_dev(readLines(fixture("net-dev.txt")))
net_b <- net_a
net_b$eth0 <- net_b$eth0 + c(rx = 200, tx = 400)
rates <- network_delta(net_a, net_b, elapsed = 2)
stopifnot(rates$eth0[["rx_rate"]] == 100, rates$eth0[["tx_rate"]] == 200, !"lo" %in% names(net_a))

h <- numeric()
for (i in seq_len(140)) h <- append_history(h, i)
stopifnot(length(h) == 120L, tail(h, 1L) == 140, length(append_history(h, NA_real_)) == 120L)

error_log_fixture <- tempfile("rtop-errors-")
stopifnot(isTRUE(log_collector_error("fixture", "diagnostic stays in the log", path = error_log_fixture)),
  grepl("[fixture] diagnostic stays in the log", readLines(error_log_fixture, warn = FALSE), fixed = TRUE))
unlink(error_log_fixture)

processes <- data.frame(pid = c(10, 2, 3), cpu = c(1, 5, 2), mem = c(3, 1, 8))
stopifnot(identical(sort_processes(processes, "cpu")$pid, c(2, 3, 10)))
stopifnot(is.null(cpu_delta(NULL, old)), length(network_delta(NULL, net_a, 1)) == 0L)
stopifnot(!collect_memory(path = tempfile())$available)

if (dir.exists("/proc")) {
  live_cpu <- collect_cpu()
  live_memory <- collect_memory()
  live_network <- collect_network()
  live_disks <- collect_disks()
  live_processes <- collect_processes(total_memory = if (isTRUE(live_memory$available)) live_memory$total else NA_real_)
  stopifnot(!is.null(live_cpu$sample), isTRUE(live_memory$available), length(live_network$interfaces) > 0L,
    nrow(live_disks$filesystems) > 0L, nrow(live_processes$data) > 0L)
  cat(sprintf("Live collectors: CPU; RAM %.1f%%; network %s; %d filesystems; %d processes.\n",
    live_memory$percent, paste(live_network$interfaces, collapse = ","),
    nrow(live_disks$filesystems), nrow(live_processes$data)))
}
core_grid <- cpu_core_grid(setNames(rep(25, 12), paste0("cpu", 0:11)), 80L)
stopifnot(length(strsplit(core_grid, "\n", fixed = TRUE)[[1L]]) <= 4L,
  grepl("25%", core_grid, fixed = TRUE), grepl("█", core_grid, fixed = TRUE))
mount_fixture <- data.frame(device = c("overlay", "tmpfs", "proc", "/dev/sda1", "drvfs", "drvfs", "drvfs", "drvfs", "none", "none", "none"),
  total = rep(1000, 11), used = rep(100, 11), free = rep(900, 11), percent = rep(10, 11),
  mount = c("/", "/run", "/proc", "/mnt/data", "/mnt/wsl/docker-desktop", "/mnt/c", "/mnt/wslg/doc",
    "/usr/lib/wsl/lib", "/usr/lib/modules/6.18.54.1-microsoft-standard-WSL2",
    "/usr/lib/linux-tools/6.18.54.1-microsoft-standard-WSL2", "/Docker/host"))
visible_mount_fixture <- visible_filesystems(list(filesystems = mount_fixture))
stopifnot(identical(visible_mount_fixture$mount, c("/", "/mnt/c", "/mnt/data")),
  grepl("…", short_mount("/very/long/mount/path"), fixed = TRUE))
proc_line <- paste(c("42 (worker ) name) S", as.character(seq_len(22))), collapse = " ")
proc <- parse_proc_stat(proc_line)
stopifnot(proc$pid == 42, proc$name == "worker ) name", proc$state == "S", proc$user_ticks == 11)

valid_progress <- list(0, 37.5, 100, -10, 150)
expected_progress <- c(0, 37.5, 100, 0, 100)
stopifnot(identical(vapply(valid_progress, progress_percent, numeric(1)), expected_progress))
invalid_progress <- list(NULL, numeric(), c(1, 2), NA, NA_real_, NaN, Inf, -Inf, "75", list(1))
stopifnot(all(vapply(invalid_progress, function(x) is.na(progress_percent(x)), logical(1))))

for (value in c(0, 1, 50, 100)) {
  lines <- history_plot_lines(value, width = 13L, height = 4L, fixed_max = 100)
  stopifnot(length(lines) == 4L, all(nchar(lines, type = "width") == 13L))
}
one_percent_plot <- history_plot_lines(1, width = 1L, height = 1L, fixed_max = 100)
zero_percent_plot <- history_plot_lines(0, width = 1L, height = 1L, fixed_max = 100)
full_percent_plot <- history_plot_lines(100, width = 1L, height = 1L, fixed_max = 100)
stopifnot(identical(history_scale_label(c(0, 1, 50, 100), 100), "0–100%"),
  one_percent_plot != zero_percent_plot, full_percent_plot == "█",
  grepl("no samples", paste(history_chart_lines(c(NA_real_, NaN), 24L, 2L, 100), collapse = "\n"), fixed = TRUE),
  all(nchar(history_plot_lines(c(NA_real_, NaN), 13L, 4L), type = "width") == 13L),
  nice_axis_max(c(0, 1)) == 1, nice_axis_max(c(0, 1.1)) == 2,
  nice_axis_max(c(0, 50)) == 50, nice_axis_max(c(0, 100)) == 100,
  grepl("RX 0–", history_chart_lines(c(0, 10), 32L, 2L, series = "RX")[[1L]], fixed = TRUE),
  grepl("B/s", history_scale_label(0), fixed = TRUE),
  grepl("KiB/s", history_scale_label(1024), fixed = TRUE),
  grepl("MiB/s", history_scale_label(1024^2), fixed = TRUE),
  grepl("GiB/s", history_scale_label(1024^3), fixed = TRUE),
  grepl("0.0 B/s", format_rate(0), fixed = TRUE),
  grepl("1.0 KiB/s", format_rate(1024), fixed = TRUE),
  grepl("1.0 MiB/s", format_rate(1024^2), fixed = TRUE),
  grepl("1.0 GiB/s", format_rate(1024^3), fixed = TRUE))

if (requireNamespace("termr", quietly = TRUE) && identical(as.character(packageVersion("termr")), "1.0.1")) {
  for (value in c(0, 42, 100, -1, 101, NA_real_, NaN, Inf, -Inf)) {
    safe_value <- progress_percent(value)
    rendered <- termr::render_widget(termr::progress_bar(safe_value, total = 100), width = 1, height = 1)
    stopifnot(inherits(rendered, "ScreenBuffer"))
  }
  count <- termr::signal(1L)
  percentage <- termr::signal(75)
  ui <- termr::app(termr::vertical(
    termr::label(function() paste("tick", count()), id = "tick"),
    termr::progress_bar(function() progress_percent(percentage()), total = 100, id = "percentage"),
    termr::sparkline(function() numeric(), max_length = 120L, id = "empty-history"),
    termr::sparkline(function() c(NA_real_, NaN, Inf), max_length = 120L, id = "invalid-history")
  ))
  timer <- NULL
  ui$on("mount", function(event, app) timer <<- app$set_interval(1, function(app) count(count() + 1L)))
  pilot <- termr::test_app(ui, width = 30, height = 6)
  screen <- paste(pilot$screen_text(), collapse = "\n")
  stopifnot(grepl("tick 1", screen, fixed = TRUE), grepl("75%", screen, fixed = TRUE), timer$active)
  percentage(25)
  pilot$advance(0)
  screen <- paste(pilot$screen_text(), collapse = "\n")
  stopifnot(grepl("25%", screen, fixed = TRUE))
  pilot$advance(2)
  screen <- paste(pilot$screen_text(), collapse = "\n")
  stopifnot(count() > 1L, grepl(paste("tick", count()), screen, fixed = TRUE))
  pilot$resize(10, 4)
  stopifnot(nzchar(pilot$screen_text()))
  pilot$stop()
  stopifnot(!timer$active)

  # Regression: one failing collector must not suppress later panel updates or cancel the timer.
  collector_calls <- new.env(parent = emptyenv())
  collector_calls$memory <- 0L
  errors <- character()
  log_test_error <- function(name, message) errors <<- c(errors, paste(name, message))
  tick <- termr::signal(0L)
  collector_status <- termr::signal(list())
  recovery_app <- termr::app(termr::vertical(
    termr::label(function() paste("Memory panel", tick()), id = "memory-panel-test"),
    termr::label(function() paste("CPU collector", collector_status()$cpu$message), id = "cpu-panel-test")
  ))
  recovery_timer <- NULL
  recovery_app$on("mount", function(event, app) {
    recovery_timer <<- app$set_interval(0.1, function(app) {
      previous <- list(cpu = list(value = "last-good-cpu"), memory = 0L)
      outcome <- collect_independently(
        readers = list(cpu = function() stop("intentional CPU failure"), memory = function() {
          collector_calls$memory <- collector_calls$memory + 1L
          collector_calls$memory
        }),
        previous = previous,
        validators = list(cpu = function(x) TRUE, memory = function(x) is.numeric(x)),
        logger = log_test_error
      )
      collector_calls$last_outcome <- outcome
      collector_status(outcome$statuses)
      tick(outcome$values$memory)
    })
  })
  recovery_pilot <- termr::test_app(recovery_app, width = 50, height = 6)
  recovery_pilot$advance(0.35)
  recovery_screen <- paste(recovery_pilot$screen_text(), collapse = "\n")
  stopifnot(collector_calls$memory >= 2L, tick() >= 2L, recovery_timer$active,
    identical(collector_status()$cpu$ok, FALSE), identical(collector_status()$memory$ok, TRUE),
    identical(collector_calls$last_outcome$values$cpu$value, "last-good-cpu"),
    grepl("CPU collector", recovery_screen, fixed = TRUE), grepl("Memory panel", recovery_screen, fixed = TRUE),
    !grepl("intentional CPU failure", recovery_screen, fixed = TRUE),
    length(errors) >= 2L)
  recovery_pilot$stop()
  stopifnot(!recovery_timer$active)

  state <- new_rtop_state()
  state$terminal_size(c(90L, 24L))
  state$cpu(list(usage = list(total = 74, cores = c(cpu0 = 31, cpu1 = 92)), load = c(0.4, 0.6, 0.8),
    frequency_mhz = 1800, temperature_c = 55))
  state$cpu_history(c(20, 45, 74))
  state$memory(list(available = TRUE, total = 1000, used = 750, free = 250, available_bytes = 250,
    percent = 75, swap_total = 200, swap_used = 25, swap_percent = 12.5))
  state$memory_history(c(60, 72, 75))
  state$network(list(interfaces = "eth0", selected = "eth0", rates = list(eth0 = c(rx_rate = 100, tx_rate = 40)),
    counters = list(eth0 = c(rx = 1000, tx = 400))))
  state$network_history(list(eth0 = list(rx = c(10, 50, 100), tx = c(5, 20, 40))))
  stopifnot(identical(selected_network_history(state, "rx"), c(10, 50, 100)))
  state$network_history(list(eth0 = list(rx = c(10, 50, 100), tx = c(5, 20, 40)),
    wlan0 = list(rx = c(1, 2), tx = c(3, 4))))
  state$network(list(interfaces = c("eth0", "wlan0"), selected = "wlan0", rates = list(), counters = list()))
  stopifnot(identical(selected_network_history(state, "rx"), c(1, 2)))
  state$network(list(interfaces = c("eth0", "wlan0"), selected = "eth0", rates = list(), counters = list()))
  stopifnot(identical(selected_network_history(state, "rx"), c(10, 50, 100)))
  state$network(list(interfaces = "eth0", selected = "eth0", rates = list(eth0 = c(rx_rate = 100, tx_rate = 40)),
    counters = list(eth0 = c(rx = 1000, tx = 400))))
  state$disks(list(filesystems = data.frame(device = "/dev/sda1", total = 1000, used = 700,
    free = 300, percent = 70, mount = "/"), rates = list()))
  state$processes(data.frame(pid = 42, user = "user", name = "worker", cpu = 10, mem = 2, rss = 200, state = "S"))
  state$selected_pid(42)
  dashboard_table <- process_table()
  dashboard_table$set_data(process_rows(state$processes(), "cpu", ""))
  dashboard_ui <- system_panel(state, dashboard_table)
  dashboard_app <- termr::app(dashboard_ui,
    bindings = list(termr::bind("m", function(app) {
      state$sort_by("mem")
      dashboard_table$set_data(process_rows(state$processes(), state$sort_by(), state$search()))
    })), theme = rtop_theme(), stylesheet = rtop_stylesheet())
  dashboard_app$on("resize", handler = function(event, app) {
    state$terminal_size(c(event$width, event$height))
    apply_chart_heights(dashboard_ui, event$height)
  })
  dashboard_app$on("input.changed", "#search", function(event, app) {
    state$search(event$data$value)
    dashboard_table$set_data(process_rows(state$processes(), state$sort_by(), state$search()))
  })
  dashboard <- termr::test_app(dashboard_app, width = 90, height = 24)
  dashboard_text <- paste(dashboard$screen_text(), collapse = "\n")
  for (size in list(c(80L, 24L), c(120L, 40L), c(180L, 50L), c(240L, 60L))) {
    dashboard$resize(size[[1L]], size[[2L]])
    dashboard$advance(0)
    stopifnot(identical(state$terminal_size(), size))
    dashboard_text <- paste(dashboard$screen_text(), collapse = "\n")
    expected_plot_height <- chart_heights(size[[2L]])[["cpu"]]
    stopifnot(dashboard$query_one("#cpu-history")$region$height >= expected_plot_height)
    stopifnot(grepl("CPU", dashboard_text, fixed = TRUE), grepl("Memory", dashboard_text, fixed = TRUE),
      grepl("Network", dashboard_text, fixed = TRUE), grepl("Disks", dashboard_text, fixed = TRUE),
      grepl("Processes", dashboard_text, fixed = TRUE), grepl("74.0%", dashboard_text, fixed = TRUE),
      grepl("75.0%", dashboard_text, fixed = TRUE), grepl("worker", dashboard_text, fixed = TRUE),
      grepl("eth0", dashboard_text, fixed = TRUE), grepl("700/1000B", dashboard_text, fixed = TRUE),
      grepl("Frequency 1800 MHz", dashboard_text, fixed = TRUE), grepl("Temp 55.0", dashboard_text, fixed = TRUE),
      grepl("SWAP", dashboard_text, fixed = TRUE), grepl("CPU%", dashboard_text, fixed = TRUE),
      grepl("MEM%", dashboard_text, fixed = TRUE), grepl("0–100%", dashboard_text, fixed = TRUE),
      !is.null(dashboard$query_one("#memory-swap-bar")),
      dashboard$query_one("#memory-history")$region$height >= 1L,
      dashboard$query_one("#network-rx-history")$region$width >= 12L,
      dashboard$query_one("#network-tx-history")$region$width >= 12L)
  }
  before_graph <- paste(dashboard$screen_text(), collapse = "\n")
  state$cpu_history(c(rep(0, 80), 100))
  dashboard$advance(0)
  after_graph <- paste(dashboard$screen_text(), collapse = "\n")
  stopifnot(!identical(before_graph, after_graph), length(state$cpu_history()) <= 120L)

  sample_processes <- data.frame(pid = c(7, 42, 9), user = c("a", "b", "c"),
    name = c("idle", "worker", "worker-child"), cpu = c(1, 20, 5), mem = c(2, 8, 4),
    rss = c(100, 800, 400), state = c("S", "R", "S"))
  sorted_rows <- process_rows(sample_processes, "cpu", "")
  filtered_rows <- process_rows(sample_processes, "cpu", "worker")
  stopifnot(identical(sorted_rows$PID, c(42, 9, 7)), nrow(filtered_rows) == 2L,
    identical(filtered_rows$PID, c(42, 9)), all(c("CPU%", "MEM%", "RSS", "STATE") %in% names(filtered_rows)))
  state$processes(sample_processes)
  dashboard_table$set_data(process_rows(sample_processes, "cpu", ""))
  dashboard$focus("#process-table")
  dashboard$press("m")
  stopifnot(identical(state$sort_by(), "mem"), identical(dashboard_table$data$PID, c(42, 9, 7)))
  dashboard$focus("#search")
  dashboard$type("worker")
  stopifnot(identical(state$search(), "worker"), identical(dashboard_table$data$PID, c(42, 9)))
  dashboard$stop()

  quit_app <- termr::app(termr::vertical(termr::input(id = "filter"),
    termr::data_table(data.frame(name = "worker"), id = "process-table")),
    bindings = list(termr::bind("q", "quit")))
  quit_pilot <- termr::test_app(quit_app, width = 40, height = 8)
  quit_pilot$focus("#process-table")
  quit_pilot$press("q")
  stopifnot(isTRUE(quit_pilot$exited))
} else {
  message("Skipping termr headless test: installed termr 1.0.1 is required.")
}

cat("rtop checks passed\n")
