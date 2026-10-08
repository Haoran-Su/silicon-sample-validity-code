source("R/00_config.R")
################################################################################
# Weak-prior companion for the simplified Bayesian sensitivity model
# Reuses simple_mcmc_fit.rds and refits the same model with weak priors.
################################################################################

library(brms)
library(cmdstanr)
library(posterior)
library(bayestestR)

cmdstanr::set_cmdstan_path("C:/Users/昊然 苏/.cmdstan/cmdstan-2.40.0")

main_path <- file.path(DG_MCMC_OUTPUT_DIR, "simple_mcmc_fit.rds")
fit_main <- readRDS(main_path)

priors_weak <- c(
  prior(normal(0, 1), class = "Intercept"),
  prior(normal(0, 1), class = "b"),
  prior(cauchy(0, 0.5), class = "sd")
)

fit_weak <- update(
  fit_main,
  prior = priors_weak,
  chains = 4,
  iter = 5000,
  warmup = 2500,
  cores = 4,
  seed = 20260908,
  control = list(adapt_delta = 0.99, max_treedepth = 15),
  backend = "cmdstanr"
)

core <- c("b_is_AI1", "b_is_AI1:takeoption1", "b_is_AI1:recdesv")
d_main <- as.data.frame(fit_main)
d_weak <- as.data.frame(fit_weak)

prior_sensitivity <- do.call(rbind, lapply(core, function(p) {
  data.frame(
    parameter = p,
    main_mean = mean(d_main[[p]]),
    weak_mean = mean(d_weak[[p]]),
    difference = abs(mean(d_main[[p]]) - mean(d_weak[[p]]))
  )
}))

bundle <- list(
  metadata = list(created_at = Sys.time(), formula = formula(fit_main)),
  prior_sensitivity = prior_sensitivity,
  weak_diagnostics = posterior::summarise_draws(posterior::as_draws_df(fit_weak)),
  weak_divergences = brms::nuts_params(fit_weak)[brms::nuts_params(fit_weak)$Parameter == "divergent__", ],
  weak_session_info = sessionInfo()
)

out_dir <- "mcmc_simplified_output"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
saveRDS(fit_weak, file.path(out_dir, "simple_mcmc_weak_fit.rds"), compress = "xz")
saveRDS(bundle, file.path(out_dir, "simple_mcmc_prior_sensitivity_for_codex.rds"), compress = "xz")
write.csv(prior_sensitivity, file.path(out_dir, "simple_mcmc_prior_sensitivity.csv"), row.names = FALSE)

cat("Send this file:", normalizePath(file.path(out_dir, "simple_mcmc_prior_sensitivity_for_codex.rds")), "\n")
