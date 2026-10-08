source("R/00_config.R")
library(brms)
library(cmdstanr)
library(posterior)
library(bayestestR)

cmdstanr::set_cmdstan_path("C:/Users/昊然 苏/.cmdstan/cmdstan-2.40.0")

main_path <- file.path(DG_MCMC_OUTPUT_DIR, "simple_mcmc_fit.rds")
fit_main <- readRDS(main_path)
mcmc_data <- fit_main$data

priors_weak <- c(
  prior(normal(0, 1), class = "Intercept"),
  prior(normal(0, 1), class = "b"),
  prior(cauchy(0, 0.5), class = "sd")
)

fit_weak <- brm(
  mean | se(semean, sigma = TRUE) ~
    is_AI * (takeoption + socialdistance + incentive + recdesv + dictearn) +
    (1 | study_id),
  data = mcmc_data,
  prior = priors_weak,
  family = gaussian(),
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

diag_weak <- posterior::summarise_draws(posterior::as_draws_df(fit_weak))
np <- brms::nuts_params(fit_weak)

out_dir <- DG_MCMC_OUTPUT_DIR
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

saveRDS(fit_weak, file.path(out_dir, "simple_mcmc_weak_fit.rds"), compress = "xz")
saveRDS(
  list(
    metadata = list(created_at = Sys.time(), formula = formula(fit_main)),
    prior_sensitivity = prior_sensitivity,
    weak_diagnostics = diag_weak,
    weak_divergences = np[np$Parameter == "divergent__", ],
    weak_treedepth = np[np$Parameter == "treedepth__", ],
    session_info = sessionInfo()
  ),
  file.path(out_dir, "simple_mcmc_prior_sensitivity_for_codex.rds"),
  compress = "xz"
)
write.csv(prior_sensitivity, file.path(out_dir, "simple_mcmc_prior_sensitivity.csv"), row.names = FALSE)

print(prior_sensitivity)
cat("Saved:", normalizePath(out_dir), "\n")
