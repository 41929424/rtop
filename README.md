# rtop

`rtop` is an interactive Linux system monitor written in R and built on the
`termr` 1.0.1 terminal UI framework. It displays CPU, memory, network,
filesystem and process information in a compact dashboard.

## Requirements and installation

- R 4.1 or newer
- Linux with `/proc` mounted (CPU, memory, network and process collectors)
- On Windows, Ubuntu WSL with R installed; native Windows is not supported
- `termr` exactly 1.0.1

Install the pinned framework version from R:

```r
install.packages("remotes")
remotes::install_github("41929424/termr", ref = "v1.0.1")
packageVersion("termr")
```

The final command should print `1.0.1`. Start rtop from this directory in a
real terminal:

```sh
Rscript rtop.R
```

`rtop` currently supports Linux, including Ubuntu running under WSL1 or WSL2.
On Windows, open an Ubuntu WSL terminal, change to the project directory, and
run `Rscript rtop.R` there. Native Windows and macOS are rejected before the
collectors or terminal UI are initialized.

The app refuses to start against another termr version because the TUI API is
version-pinned. Filesystem capacities are read using `df -P`; temperature and
frequency show unavailable when Linux exposes no matching sysfs sensors.

## Keys

| Key | Action |
| --- | --- |
| `q` | Quit |
| `↑` / `↓` | Move through processes |
| `c` / `m` / `p` | Sort by CPU / memory / PID |
| `n` | Select the next network interface |
| `/` | Focus process filter |
| `Esc` | Clear filter and return to processes |
| `Tab` | Move focus |
| `+` / `-` | Change refresh interval by 250 ms (250–5000 ms) |
| `h` | Show key help in the status line |

Refresh defaults to 1000 ms. Graph history is bounded to the latest 120
samples. CPU and network rates are left unavailable until two valid samples
are collected. Some restricted `/proc` process details can be absent.

## Development checks

Run the fixture based tests from the project root with:

```sh
Rscript tests/run-tests.R
```

The headless UI check uses `termr::test_app()` and does not require an
interactive terminal. A real terminal is required to check terminal mode,
keyboard input and restoration.
