# Collector modules are loaded independently by rtop.R.

collector_log_state <- new.env(parent = emptyenv())

log_collector_error <- function(name, message, path = "/tmp/rtop-errors.log") {
  now <- unname(proc.time()[[3L]])
  key <- paste(name, message, sep = "\r")
  if (exists(key, collector_log_state, inherits = FALSE) && now - get(key, collector_log_state, inherits = FALSE) < 30) return(invisible(FALSE))
  assign(key, now, collector_log_state)
  line <- sprintf("%s [%s] %s\n", format(Sys.time(), "%Y-%m-%d %H:%M:%S %z"), name, message)
  tryCatch({
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    cat(line, file = path, append = TRUE)
    TRUE
  }, error = function(e) FALSE)
}

collect_independently <- function(readers, previous, validators, logger = log_collector_error) {
  values <- previous
  statuses <- setNames(vector("list", length(readers)), names(readers))
  for (name in names(readers)) {
    result <- tryCatch(list(ok = TRUE, value = readers[[name]]()), error = function(e) {
      list(ok = FALSE, error = conditionMessage(e))
    })
    if (!isTRUE(result$ok)) {
      message <- if (nzchar(result$error)) result$error else "collector raised an error without a message"
      logger(name, message)
      statuses[[name]] <- list(ok = FALSE, message = "collection failed")
      next
    }
    valid <- tryCatch(isTRUE(validators[[name]](result$value)), error = function(e) FALSE)
    if (!valid) {
      message <- "collector returned unavailable or invalid data"
      logger(name, message)
      statuses[[name]] <- list(ok = FALSE, message = "no valid data")
      next
    }
    values[[name]] <- result$value
    statuses[[name]] <- list(ok = TRUE, message = "Live")
  }
  list(values = values, statuses = statuses)
}
