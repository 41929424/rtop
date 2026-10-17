new_rtop_state <- function() {
  list(
    interval_ms = termr::signal(1000L), sort_by = termr::signal("cpu"),
    search = termr::signal(""), selected_pid = termr::signal(NA_real_),
    status = termr::signal("Starting"), clock = termr::signal(format(Sys.time(), "%H:%M:%S")),
    cpu = termr::signal(NULL), memory = termr::signal(NULL), terminal_size = termr::signal(c(80L, 24L)),
    network = termr::signal(NULL), disks = termr::signal(NULL), processes = termr::signal(data.frame()),
    collector_status = termr::signal(list(cpu = list(ok = FALSE, message = "waiting"),
      memory = list(ok = FALSE, message = "waiting"), network = list(ok = FALSE, message = "waiting"),
      disks = list(ok = FALSE, message = "waiting"), processes = list(ok = FALSE, message = "waiting"))),
    cpu_history = termr::signal(numeric()), memory_history = termr::signal(numeric()),
    network_history = termr::signal(list())
  )
}

push_history_signal <- function(s, x) termr::update_signal(s, function(old) append_history(old, x, 120L))
