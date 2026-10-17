check_platform <- function(os = Sys.info()[["sysname"]]) {
  if (length(os) != 1L || is.na(os) || !nzchar(os)) {
    stop("Unable to determine the operating system. rtop currently supports Linux only.", call. = FALSE)
  }

  if (identical(os, "Linux")) return(invisible(TRUE))

  if (identical(os, "Windows")) {
    stop(paste0(
      "rtop currently supports Linux only. On Windows, launch it from an Ubuntu WSL terminal, ",
      "then run `Rscript rtop.R` from the project directory."
    ), call. = FALSE)
  }

  if (identical(os, "Darwin")) {
    stop("rtop currently supports Linux only; macOS (Darwin) is not supported yet.", call. = FALSE)
  }

  stop(sprintf("rtop currently supports Linux only; detected operating system: %s.", os), call. = FALSE)
}
