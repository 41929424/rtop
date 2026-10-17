visible_filesystems <- function(x) {
  if (is.null(x) || is.null(x$filesystems) || !nrow(x$filesystems)) return(data.frame())
  fs <- x$filesystems
  pseudo_mount <- grepl("^/(proc|sys|dev|run|init|mnt/wsl(g)?|usr/lib/wsl|usr/lib/modules/[^/]*microsoft-standard-WSL2|usr/lib/linux-tools/[^/]*microsoft-standard-WSL2|Docker/host)(/|$)", fs$mount)
  pseudo_device <- grepl("^(tmpfs|devtmpfs|proc|sysfs|cgroup2?|mqueue|shm)$", fs$device)
  fs <- fs[!pseudo_mount & !(pseudo_device & fs$mount != "/"), , drop = FALSE]
  fs[order(fs$mount != "/", nchar(fs$mount)), , drop = FALSE]
}

short_mount <- function(path, width = 11L) {
  path <- as.character(path)
  width <- max(4L, as.integer(width))
  if (nchar(path, type = "width") <= width) return(path)
  if (width < 8L) return(paste0("…", substr(path, nchar(path) - width + 2L, nchar(path))))
  left <- floor((width - 1L) / 2L)
  right <- width - left - 1L
  paste0(substr(path, 1L, left), "…", substr(path, nchar(path) - right + 1L, nchar(path)))
}

disk_row <- function(state, index) {
  fs <- visible_filesystems(state$disks())
  if (nrow(fs) < index) return(NULL)
  fs[index, , drop = FALSE]
}

disk_summary_lines <- function(state, terminal_width = state$terminal_size()[[1L]]) {
  fs <- visible_filesystems(state$disks())
  status <- collector_status_text(state, "disks")
  if (!nrow(fs)) return(paste0("No user filesystems", status))
  row_width <- max(24L, floor(as.integer(terminal_width) / 2L) - 4L)
  path_width <- max(5L, min(18L, row_width - 25L))
  bar_width <- 6L
  lines <- vapply(seq_len(nrow(fs)), function(i) {
    valid <- is.finite(fs$percent[[i]])
    pct <- if (valid) max(0, min(100, fs$percent[[i]])) else NA_real_
    filled <- if (valid) max(0L, min(bar_width, round(pct / 100 * bar_width))) else 0L
    bar <- if (valid) paste0(strrep("█", filled), strrep("░", bar_width - filled)) else strrep("?", bar_width)
    total <- fs$total[[i]]
    used <- fs$used[[i]]
    units <- c("B", "K", "M", "G", "T", "P")
    unit_index <- 1L
    while (total >= 1024 && unit_index < length(units)) { total <- total / 1024; used <- used / 1024; unit_index <- unit_index + 1L }
    capacity <- sprintf("%.0f/%.0f%s", used, total, units[[unit_index]])
    pct_label <- if (valid) sprintf("%3.0f%%", pct) else "  —"
    paste0(sprintf("%-*s", path_width, short_mount(fs$mount[[i]], path_width)), " ",
      pct_label, " ", bar, " ", capacity)
  }, character(1))
  paste(c(lines, if (nzchar(status)) status), collapse = "\n")
}

disk_panel <- function(state) {
  termr::panel(termr::label(function() disk_summary_lines(state, state$terminal_size()[[1L]]), id = "disk-summary"),
    title = "Disks", id = "disk-panel", style = termr::style(height = "auto"))
}

disk_count <- function(state) nrow(visible_filesystems(state$disks()))
