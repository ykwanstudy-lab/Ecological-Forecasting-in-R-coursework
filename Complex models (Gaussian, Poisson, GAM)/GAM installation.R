Sys.which("make")
install.packages(c("brms", "collapse", "dplyr", "gratia",
                   "ggplot2", "marginaleffects", "tidybayes", "zoo",
                   "viridis", "remotes"))

install.packages(
  "cmdstanr",
  repos = c('https://stan-dev.r-universe.dev', getOption("repos"))
)
remotes::install_github('nicholasjclark/mvgam', force = TRUE)

library(cmdstanr)
check_cmdstan_toolchain(fix = TRUE)
install_cmdstan()

cmdstan_version()
