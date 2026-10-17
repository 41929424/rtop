#!/usr/bin/env Rscript

script_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
script_path <- if (length(script_arg)) sub("^--file=", "", script_arg[[1L]]) else "rtop.R"
root <- dirname(normalizePath(script_path, winslash = "/", mustWork = FALSE))
if (!nzchar(root) || root == ".") root <- getwd()
sys.source(file.path(root, "R/platform.R"), envir = .GlobalEnv)
check_platform()

required_termr <- "1.0.1"
if (!requireNamespace("termr", quietly = TRUE)) {
  stop("Package 'termr' is required. Install it with remotes::install_github('41929424/termr', ref = 'v1.0.1').", call. = FALSE)
}
installed_termr <- as.character(utils::packageVersion("termr"))
if (!identical(installed_termr, required_termr)) {
  stop(sprintf("rtop requires termr %s; found %s. Install the v1.0.1 tag.", required_termr, installed_termr), call. = FALSE)
}

modules <- c("R/format.R", "R/collectors.R", "R/collectors/cpu.R", "R/collectors/memory.R", "R/collectors/network.R",
             "R/collectors/disk.R", "R/collectors/processes.R", "R/state.R", "R/widgets/history.R",
             "R/widgets/cpu.R", "R/widgets/memory.R", "R/widgets/network.R", "R/widgets/disk.R", "R/widgets/processes.R",
             "R/widgets.R", "R/app.R")
for (module in modules) sys.source(file.path(root, module), envir = .GlobalEnv)
start_rtop()
