system_constant <- local({
  cache <- new.env(parent = emptyenv())
  function(name, fallback) {
    if (exists(name, cache, inherits = FALSE)) return(get(name, cache, inherits = FALSE))
    value <- suppressWarnings(as.numeric(tryCatch(system2("getconf", name, stdout = TRUE, stderr = FALSE)[[1L]], error = function(e) NA_character_)))
    if (!is.finite(value) || value <= 0) value <- fallback
    assign(name, value, cache)
    value
  }
})

parse_proc_stat <- function(line) {
  close <- regexpr("\\) [A-Z] ", line)
  if (close[[1L]] < 0) return(NULL)
  pid <- suppressWarnings(as.numeric(sub(" .*", "", line)))
  comm <- substr(line, regexpr("\\(", line) + 1L, close[[1L]] - 1L)
  fields <- strsplit(trimws(substr(line, close[[1L]] + 1L, nchar(line))), "\\s+")[[1L]]
  if (!is.finite(pid) || pid < 0 || length(fields) < 22L) return(NULL)
  nums <- suppressWarnings(as.numeric(fields[c(12L, 13L, 20L, 22L)]))
  if (anyNA(nums) || any(!is.finite(nums)) || any(nums < 0)) return(NULL)
  list(pid = pid, name = comm, state = substr(line, close[[1L]] + 2L, close[[1L]] + 2L),
       user_ticks = nums[[1L]], system_ticks = nums[[2L]], rss_pages = nums[[3L]], start_ticks = nums[[4L]])
}

read_processes_proc <- function(proc_root = "/proc", page_size = NULL) {
  if (is.null(page_size)) {
    page_size <- system_constant("PAGESIZE", 4096)
  }
  uid_map <- character()
  passwd <- tryCatch(readLines("/etc/passwd", warn = FALSE), error = function(e) character())
  if (length(passwd)) {
    entries <- strsplit(passwd, ":", fixed = TRUE)
    uids <- vapply(entries, function(x) if (length(x) >= 3L) x[[3L]] else "", character(1))
    users <- vapply(entries, function(x) if (length(x)) x[[1L]] else "", character(1))
    uid_map <- setNames(users, uids)
  }
  dirs <- list.dirs(proc_root, recursive = FALSE, full.names = TRUE)
  dirs <- dirs[grepl("^[0-9]+$", basename(dirs))]
  rows <- lapply(dirs, function(dir) {
    stat <- tryCatch(readLines(file.path(dir, "stat"), n = 1L, warn = FALSE), error = function(e) character())
    if (!length(stat)) return(NULL)
    parsed <- parse_proc_stat(stat)
    if (is.null(parsed)) return(NULL)
    status <- tryCatch(readLines(file.path(dir, "status"), warn = FALSE), error = function(e) character())
    uid <- suppressWarnings(as.numeric(sub("^Uid:\\s+([0-9]+).*$", "\\1", grep("^Uid:", status, value = TRUE)[1L])))
    mem <- suppressWarnings(as.numeric(sub("^VmRSS:\\s+([0-9]+).*$", "\\1", grep("^VmRSS:", status, value = TRUE)[1L])))
    cmd <- tryCatch(readBin(file.path(dir, "cmdline"), "raw", n = 8192L), error = function(e) raw())
    if (length(cmd)) cmd[cmd == as.raw(0)] <- as.raw(32)
    cmd <- if (length(cmd)) rawToChar(cmd) else ""
    uid_text <- if (is.finite(uid)) as.character(uid) else "?"
    parsed$user <- if (uid_text %in% names(uid_map)) unname(uid_map[[uid_text]]) else uid_text
    parsed$rss <- if (is.finite(mem)) mem * 1024 else parsed$rss_pages * page_size
    parsed$name <- if (nzchar(trimws(cmd))) basename(strsplit(trimws(cmd), "\\s+")[[1L]][1L]) else parsed$name
    as.data.frame(parsed, stringsAsFactors = FALSE)
  })
  rows <- Filter(Negate(is.null), rows)
  if (!length(rows)) return(data.frame(pid = numeric(), user = character(), name = character(), cpu = numeric(), mem = numeric(), rss = numeric(), state = character()))
  do.call(rbind, rows)
}

collect_processes <- function(proc_root = "/proc", previous = NULL, elapsed = NA_real_, total_memory = NA_real_) {
  x <- read_processes_proc(proc_root)
  if (nrow(x)) {
    x$cpu <- NA_real_
    if (!is.null(previous) && is.finite(elapsed) && elapsed > 0) {
      old <- setNames(previous$user_ticks + previous$system_ticks, previous$pid)
      current <- x$user_ticks + x$system_ticks
      ix <- match(as.character(x$pid), names(old))
      valid <- !is.na(ix) & current >= old[ix]
      ticks <- system_constant("CLK_TCK", 100)
      cores <- parallel::detectCores(logical = TRUE)
      if (!is.finite(cores) || cores < 1) cores <- 1
      x$cpu[valid] <- 100 * (current[valid] - old[ix[valid]]) / ticks / elapsed / cores
    }
    x$mem <- if (is.finite(total_memory) && total_memory > 0) 100 * x$rss / total_memory else NA_real_
  }
  list(data = x, counters = x[c("pid", "user_ticks", "system_ticks")])
}

sort_processes <- function(data, by = "cpu", decreasing = TRUE) {
  if (!nrow(data) || !by %in% names(data)) return(data)
  data[order(data[[by]], na.last = TRUE, decreasing = decreasing), , drop = FALSE]
}
