source("R/00_config.R")
################################################################################
# Bayesian MCMC diagnostics package for BRM manuscript
# Behavior Research Methods version
#
# Use Part A if fit_mcmc and fit_mcmc_weak already exist in R.
# Use Part B only if they need to be refitted.
################################################################################

library(brms)
library(cmdstanr)
library(posterior)
library(bayesplot)
library(bayestestR)
library(tidyverse)

OUT <- "mcmc_diagnostics_output"
dir.create(OUT, showWarnings = FALSE, recursive = TRUE)

core_pars <- c(
  "b_is_AI1",
  "b_is_AI1:takeoption1",
  "b_is_AI1:recdesv"
)

save_mcmc_diagnostics <- function(fit_main, fit_weak = NULL,
                                  priors_main = NULL, priors_weak = NULL,
                                  out_dir = OUT) {
  cat("\n[1] Summarizing posterior draws...\n")
  draws_main <- posterior::as_draws_df(fit_main)
  diag_all <- posterior::summarise_draws(draws_main)

  cat("[2] Extracting NUTS diagnostics...\n")
  np <- brms::nuts_params(fit_main)
  div_df <- np[np$Parameter == "divergent__", , drop = FALSE]
  td_df  <- np[np$Parameter == "treedepth__", , drop = FALSE]
  acc_df <- np[np$Parameter == "accept_stat__", , drop = FALSE]

  cat("[3] Core posterior summaries...\n")
  posterior_fixed <- as.data.frame(fit_main)
  posterior_summary_core <- do.call(rbind, lapply(core_pars, function(p) {
    x <- posterior_fixed[[p]]
    h <- bayestestR::hdi(x, ci = 0.95)
    data.frame(
      parameter = gsub("^b_", "", p),
      post_mean = mean(x),
      post_sd = sd(x),
      hdi_lb = h$CI_low,
      hdi_ub = h$CI_high,
      p_positive = mean(x > 0),
      p_negative = mean(x < 0)
    )
  }))

  prior_sensitivity <- NULL
  if (!is.null(fit_weak)) {
    cat("[4] Computing prior sensitivity...\n")
    posterior_weak <- as.data.frame(fit_weak)
    prior_sensitivity <- do.call(rbind, lapply(core_pars, function(p) {
      data.frame(
        parameter = gsub("^b_", "", p),
        main_prior_mean = mean(posterior_fixed[[p]]),
        weak_prior_mean = mean(posterior_weak[[p]]),
        absolute_difference = abs(mean(posterior_fixed[[p]]) - mean(posterior_weak[[p]]))
      )
    }))
  }

  cat("[5] Saving tables...\n")
  write.csv(diag_all, file.path(out_dir, "mcmc_diagnostics_all.csv"), row.names = FALSE)
  write.csv(posterior_summary_core, file.path(out_dir, "mcmc_posterior_summary_core.csv"), row.names = FALSE)
  write.csv(div_df, file.path(out_dir, "mcmc_divergences.csv"), row.names = FALSE)
  write.csv(td_df, file.path(out_dir, "mcmc_treedepth.csv"), row.names = FALSE)
  write.csv(acc_df, file.path(out_dir, "mcmc_accept_stat.csv"), row.names = FALSE)
  if (!is.null(prior_sensitivity)) {
    write.csv(prior_sensitivity, file.path(out_dir, "mcmc_prior_sensitivity.csv"), row.names = FALSE)
  }

  cat("[6] Saving plots...\n")
  p_post <- bayesplot::mcmc_areas(
    fit_main, pars = core_pars, prob = 0.95, point_est = "mean"
  ) + ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
    ggplot2::labs(title = "Posterior distributions of core parameters")
  ggplot2::ggsave(file.path(out_dir, "mcmc_posterior_core.png"), p_post,
                  width = 10, height = 6, dpi = 300)

  p_trace <- bayesplot::mcmc_trace(fit_main, pars = core_pars) +
    ggplot2::labs(title = "Trace plots of core parameters")
  ggplot2::ggsave(file.path(out_dir, "mcmc_trace_core.png"), p_trace,
                  width = 12, height = 8, dpi = 300)

  cat("[7] Building single diagnostic bundle for Codex...\n")
  bundle <- list(
    metadata = list(
      created_at = Sys.time(),
      cmdstan_version = tryCatch(cmdstan_version(), error = function(e) NA_character_),
      formula = formula(fit_main),
      core_pars = core_pars
    ),
    diagnostics_all = diag_all,
    posterior_summary_core = posterior_summary_core,
    prior_sensitivity = prior_sensitivity,
    divergences = div_df,
    treedepth = td_df,
    accept_stat = acc_df,
    fixed_effects_summary = summary(fit_main)$fixed,
    random_effects_summary = summary(fit_main)$random,
    priors_main = priors_main,
    priors_weak = priors_weak,
    session_info = sessionInfo()
  )
  saveRDS(bundle, file.path(out_dir, "mcmc_diagnostics_for_codex.rds"), compress = "xz")

  cat("\nSaved diagnostic bundle:\n")
  cat(normalizePath(file.path(out_dir, "mcmc_diagnostics_for_codex.rds")), "\n")
  cat("\nIf file size permits, also save the full fitted objects:\n")
  cat("  saveRDS(fit_main, 'mcmc_fit_main.rds', compress='xz')\n")
  if (!is.null(fit_weak)) {
    cat("  saveRDS(fit_weak, 'mcmc_fit_weak.rds', compress='xz')\n")
  }
}

################################################################################
# PART A: Already have fit_mcmc / fit_mcmc_weak in R
################################################################################
cat("Running Part A if fitted objects already exist...\n")

if (exists("fit_mcmc")) {
  fit_weak_tmp <- if (exists("fit_mcmc_weak")) fit_mcmc_weak else NULL
  save_mcmc_diagnostics(
    fit_main = fit_mcmc,
    fit_weak = fit_weak_tmp,
    priors_main = if (exists("mcmc_priors")) mcmc_priors else NULL,
    priors_weak = if (exists("priors_weak")) priors_weak else NULL
  )
} else {
  cat("fit_mcmc not found. Use Part B to refit the model.\n")
}

################################################################################
# PART B: Refit if necessary
# Uncomment the whole block below only if fit_mcmc does not exist.
################################################################################
# load(DG_COMBINED_META)
#
# cm <- combined_meta
# cm$ai_flag <- ifelse(!is.na(cm$deepthinking), 1L, 0L)
# cm$is_AI <- factor(cm$ai_flag, levels = c(0, 1))
# cm$study_id <- ifelse(cm$ai_flag == 1, cm$study, as.character(cm$studyid))
# cm$treatment_id <- paste0(cm$study_id, "_", cm$treatid)
#
# ai_rows <- which(cm$ai_flag == 1)
# cm$mean[ai_rows] <- cm$mean[ai_rows] / 100
# cm$semean[ai_rows] <- cm$semean[ai_rows] / 100
# cm$vi <- cm$semean^2
#
# cm <- cm[!(cm$ai_flag == 1 & cm$study %in% c("CGSS-DeepSeek-R1", "GSS-qwen-3.6")), ]
#
# mcmc_data <- cm %>%
#   filter(!is.na(mean), !is.na(semean),
#          !is.na(takeoption), !is.na(socialdistance),
#          !is.na(incentive), !is.na(recdesv), !is.na(dictearn)) %>%
#   mutate(
#     is_AI = factor(is_AI, levels = c(0, 1)),
#     takeoption = factor(takeoption),
#     socialdistance = factor(socialdistance),
#     incentive = factor(incentive),
#     study_id = factor(study_id),
#     treat_id = factor(treatment_id)
#   )
#
# mcmc_priors <- c(
#   prior(normal(0.3, 0.3), class = "Intercept"),
#   prior(normal(0, 0.5), class = "b"),
#   prior(cauchy(0, 0.3), class = "sd")
# )
#
# fit_mcmc <- brm(
#   formula = mean | se(semean, sigma = TRUE) ~
#     is_AI * (takeoption + socialdistance + incentive + recdesv + dictearn) +
#     (1 | study_id) + (1 | treat_id),
#   data = mcmc_data,
#   prior = mcmc_priors,
#   family = gaussian(),
#   chains = 4,
#   iter = 5000,
#   warmup = 2500,
#   cores = 4,
#   seed = 20260908,
#   control = list(adapt_delta = 0.95, max_treedepth = 12),
#   backend = "cmdstanr"
# )
#
# priors_weak <- c(
#   prior(normal(0, 1), class = "Intercept"),
#   prior(normal(0, 1), class = "b"),
#   prior(cauchy(0, 0.5), class = "sd")
# )
#
# fit_mcmc_weak <- brm(
#   formula = mean | se(semean, sigma = TRUE) ~
#     is_AI * (takeoption + socialdistance + incentive + recdesv + dictearn) +
#     (1 | study_id) + (1 | treat_id),
#   data = mcmc_data,
#   prior = priors_weak,
#   family = gaussian(),
#   chains = 4,
#   iter = 5000,
#   warmup = 2500,
#   cores = 4,
#   seed = 20260908,
#   control = list(adapt_delta = 0.95, max_treedepth = 12),
#   backend = "cmdstanr"
# )
#
# save_mcmc_diagnostics(
#   fit_main = fit_mcmc,
#   fit_weak = fit_mcmc_weak,
#   priors_main = mcmc_priors,
#   priors_weak = priors_weak
# )
