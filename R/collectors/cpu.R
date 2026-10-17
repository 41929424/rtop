read_proc_stat <- function(path = "/proc/stat") {
  if (!file.exists(path)) return(NULL)
  lines <- tryCatch(readLines(path, warn = FALSE), error = function(e) character())
  parse <- function(line) {
    fields <- strsplit(trimws(line), "\\s+")[[1L]]
    if (length(fields) < 5L) return(NULL)
    values <- suppressWarnings(as.numeric(fields[-1L]))
    if (anyNA(values) || any(!is.finite(values)) || any(values < 0)) return(NULL)
    values <- c(values, rep(0, max(0, 10L - length(values))))[seq_len(min(10L, max(4L, length(values))))]
    names(values) <- c("user", "nice", "system", "idle", "iowait", "irq", "softirq", "steal", "guest", "guest_nice")[seq_along(values)]
    values
  }
  cpu_lines <- grep("^cpu[0-9]*\\s", lines, value = TRUE)
  if (!length(cpu_lines)) return(NULL)
  out <- lapply(cpu_lines, parse)
  names(out) <- sub("\\s.*$", "", cpu_lines)
  out <- Filter(Negate(is.null), out)
  if (!length(out) || is.null(out$cpu)) return(NULL)
  list(counters = out, sampled_at = unname(proc.time()[[3L]]))
}

cpu_delta <- function(previous, current) {
  if (is.null(previous) || is.null(current)) return(NULL)
  ids <- intersect(names(previous$counters), names(current$counters))
  calc <- function(a, b) {
    n <- min(length(a), length(b), 8L); d <- b[seq_len(n)] - a[seq_len(n)]
    if (!n || any(!is.finite(d)) || any(d < 0)) return(NA_real_)
    total <- sum(d)
    if (total <= 0) return(NA_real_)
    idle <- sum(d[intersect(c("idle", "iowait"), names(d))])
    clamp_value(100 * (total - idle) / total)
  }
  usage <- vapply(ids, function(id) calc(previous$counters[[id]], current$counters[[id]]), numeric(1))
  list(total = unname(usage["cpu"]), cores = usage[grep("^cpu[0-9]+$", names(usage))], sampled_at = current$sampled_at)
}

read_loadavg <- function(path = "/proc/loadavg") {
  x <- tryCatch(readLines(path, n = 1L, warn = FALSE), error = function(e) character())
  if (!length(x)) return(rep(NA_real_, 3L))
  vals <- suppressWarnings(as.numeric(strsplit(trimws(x), "\\s+")[[1L]][1:3]))
  if (length(vals) != 3L) rep(NA_real_, 3L) else vals
}

read_cpu_frequency <- function(root = "/sys/devices/system/cpu") {
  files <- Sys.glob(file.path(root, "cpu[0-9]*", "cpufreq", "scaling_cur_freq"))
  if (!length(files)) return(NA_real_)
  x <- suppressWarnings(as.numeric(vapply(files, function(f) tryCatch(readLines(f, n = 1L), error = function(e) NA_character_), character(1))))
  x <- x[is.finite(x) & x > 0]
  if (!length(x)) NA_real_ else mean(x) / 1000
}

read_cpu_temperature <- function(thermal_root = "/sys/class/thermal", hwmon_root = "/sys/class/hwmon") {
  thermal_dirs <- Sys.glob(file.path(thermal_root, "thermal_zone*"))
  thermal_files <- vapply(thermal_dirs, function(dir) {
    type <- tryCatch(readLines(file.path(dir, "type"), n = 1L, warn = FALSE), error = function(e) "")
    if (length(type) && grepl("cpu|package|x86", type, ignore.case = TRUE)) file.path(dir, "temp") else ""
  }, character(1))
  hwmon_dirs <- Sys.glob(file.path(hwmon_root, "hwmon*"))
  hwmon_files <- unlist(lapply(hwmon_dirs, function(dir) {
    inputs <- Sys.glob(file.path(dir, "temp*_input"))
    Filter(function(input) {
      label_path <- sub("_input$", "_label", input)
      label <- tryCatch(readLines(label_path, n = 1L, warn = FALSE), error = function(e) "")
      if (!length(label) || !nzchar(label)) return(FALSE)
      grepl("cpu|package|core|tctl|tccd", label, ignore.case = TRUE)
    }, inputs)
  }), use.names = FALSE)
  files <- c(thermal_files[nzchar(thermal_files)], hwmon_files)
  if (!length(files)) return(NA_real_)
  vals <- suppressWarnings(as.numeric(vapply(files, function(f) tryCatch(readLines(f, n = 1L, warn = FALSE), error = function(e) NA_character_), character(1))))
  vals <- vals[is.finite(vals) & vals > 0]
  if (!length(vals)) return(NA_real_)
  vals <- ifelse(vals > 1000, vals / 1000, vals)
  vals <- vals[vals >= -20 & vals <= 150]
  if (!length(vals)) NA_real_ else max(vals)
}

collect_cpu <- function(previous = NULL, stat_path = "/proc/stat", load_path = "/proc/loadavg") {
  sample <- read_proc_stat(stat_path)
  list(sample = sample, usage = if (is.null(previous)) NULL else cpu_delta(previous, sample),
       load = read_loadavg(load_path), frequency_mhz = read_cpu_frequency(), temperature_c = read_cpu_temperature())
}
