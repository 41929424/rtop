collect_disks <- function(path = "/proc/diskstats", exclude = "^(loop|ram|fd|sr|zram)", previous = NULL, elapsed = NA_real_) {
  lines <- tryCatch(readLines(path, warn = FALSE), error = function(e) character())
  devices <- lapply(lines, function(line) {
    x <- strsplit(trimws(line), "\\s+")[[1L]]
    if (length(x) < 14L) return(NULL)
    nums <- suppressWarnings(as.numeric(x[1:14]))
    if (anyNA(nums) || any(!is.finite(nums)) || any(nums < 0)) return(NULL)
    list(name = x[[3L]], reads = nums[[4L]], read_sectors = nums[[6L]], writes = nums[[8L]], write_sectors = nums[[10L]])
  })
  devices <- Filter(Negate(is.null), devices)
  devices <- Filter(function(x) !grepl(exclude, x$name), devices)
  rates <- list()
  if (!is.null(previous) && is.finite(elapsed) && elapsed > 0) {
    for (device in devices) {
      old <- previous[[device$name]]
      if (is.null(old) || device$read_sectors < old$read_sectors || device$write_sectors < old$write_sectors) next
      rates[[device$name]] <- c(read_bytes_s = (device$read_sectors - old$read_sectors) * 512 / elapsed,
                                write_bytes_s = (device$write_sectors - old$write_sectors) * 512 / elapsed)
    }
  }
  mounts <- tryCatch(system2("df", c("-P", "-k"), stdout = TRUE, stderr = FALSE), error = function(e) character())
  mounts <- mounts[nzchar(mounts)]
  fs <- list()
  if (length(mounts) > 1L) for (line in mounts[-1L]) {
    x <- strsplit(trimws(line), "\\s+")[[1L]]
    if (length(x) < 6L) next
    nums <- suppressWarnings(as.numeric(x[2:4]))
    if (any(!is.finite(nums))) next
    fs[[length(fs) + 1L]] <- data.frame(device = x[[1L]], total = nums[[1L]] * 1024,
      used = nums[[2L]] * 1024, free = nums[[3L]] * 1024, percent = as.numeric(sub("%$", "", x[[5L]])),
      mount = paste(x[6:length(x)], collapse = " "), stringsAsFactors = FALSE)
  }
  filesystems <- if (length(fs)) do.call(rbind, fs) else data.frame()
  if (nrow(filesystems)) {
    pseudo_mount <- grepl("^/(proc|sys|dev|run|init)(/|$)", filesystems$mount)
    pseudo_device <- grepl("^(tmpfs|devtmpfs|proc|sysfs|cgroup2?|mqueue|shm)$", filesystems$device)
    filesystems <- filesystems[!pseudo_mount & !(pseudo_device & filesystems$mount != "/"), , drop = FALSE]
    filesystems <- filesystems[order(filesystems$mount != "/", nchar(filesystems$mount)), , drop = FALSE]
  }
  counters <- setNames(lapply(devices, function(x) x[c("read_sectors", "write_sectors")]), vapply(devices, `[[`, character(1), "name"))
  list(devices = devices, counters = counters, rates = rates, filesystems = filesystems)
}
