parse_meminfo <- function(lines) {
  if (!length(lines)) return(NULL)
  keys <- sub(":.*$", "", lines)
  vals <- suppressWarnings(as.numeric(sub("^[^:]+:\\s*([0-9]+).*$", "\\1", lines))) * 1024
  vals[!is.finite(vals) | vals < 0] <- NA_real_
  names(vals) <- keys
  vals
}

collect_memory <- function(path = "/proc/meminfo", proc_root = "/proc") {
  info <- tryCatch(parse_meminfo(suppressWarnings(readLines(path, warn = FALSE))), error = function(e) NULL)
  if (is.null(info) || is.na(info["MemTotal"])) return(list(available = FALSE))
  total <- unname(info["MemTotal"])
  available <- unname(info["MemAvailable"])
  if (!is.finite(available)) {
    if (!is.finite(info["MemFree"])) return(list(available = FALSE))
    parts <- info[c("MemFree", "Buffers", "Cached", "SReclaimable")]
    available <- sum(parts[is.finite(parts)], na.rm = TRUE)
  }
  if (!is.finite(total) || total <= 0 || !is.finite(available)) return(list(available = FALSE))
  available <- min(total, max(0, available))
  used <- max(0, total - available)
  swap_total <- if (is.finite(info["SwapTotal"])) unname(info["SwapTotal"]) else 0
  swap_free <- if (is.finite(info["SwapFree"])) unname(info["SwapFree"]) else 0
  list(available = TRUE, total = total, used = used, free = max(0, total - used), available_bytes = available,
       percent = if (total > 0) 100 * used / total else NA_real_, swap_total = swap_total,
       swap_used = max(0, swap_total - swap_free), swap_percent = if (swap_total > 0) 100 * (swap_total - swap_free) / swap_total else 0)
}
