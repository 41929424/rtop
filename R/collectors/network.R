parse_net_dev <- function(lines) {
  lines <- lines[grepl(":", lines)]
  rows <- lapply(lines, function(line) {
    parts <- strsplit(line, ":", fixed = TRUE)[[1L]]
    iface <- trimws(parts[[1L]])
    x <- suppressWarnings(as.numeric(strsplit(trimws(parts[[2L]]), "\\s+")[[1L]]))
    if (length(x) < 9L || any(!is.finite(x[c(1L, 9L)])) || any(x[c(1L, 9L)] < 0)) return(NULL)
    c(rx = x[[1L]], tx = x[[9L]])
  })
  names(rows) <- trimws(sub(":.*$", "", lines))
  rows <- Filter(Negate(is.null), rows)
  rows[setdiff(names(rows), "lo")]
}

read_net_dev <- function(path = "/proc/net/dev") {
  x <- tryCatch(parse_net_dev(readLines(path, warn = FALSE)), error = function(e) list())
  x
}

network_delta <- function(previous, current, elapsed) {
  if (is.null(previous) || !length(previous) || !length(current) || length(elapsed) != 1L || !is.finite(elapsed) || elapsed <= 0) return(list())
  ifaces <- intersect(names(previous), names(current))
  rates <- lapply(ifaces, function(iface) {
    before <- previous[[iface]]; after <- current[[iface]]
    rx <- after[["rx"]] - before[["rx"]]; tx <- after[["tx"]] - before[["tx"]]
    if (!is.finite(rx) || !is.finite(tx) || rx < 0 || tx < 0) return(NULL)
    c(rx_rate = rx / elapsed, tx_rate = tx / elapsed, rx_total = after[["rx"]], tx_total = after[["tx"]])
  })
  names(rates) <- ifaces
  Filter(Negate(is.null), rates)
}

collect_network <- function(previous = NULL, elapsed = NA_real_, path = "/proc/net/dev") {
  counters <- read_net_dev(path)
  list(counters = counters, rates = network_delta(previous, counters, elapsed), interfaces = names(counters))
}
