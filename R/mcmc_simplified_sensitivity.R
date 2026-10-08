source("R/00_config.R")
################################################################################
# Simplified Bayesian sensitivity model for the BRM manuscript
# Purpose: retain a Bayesian check while avoiding the non-identifiable
# unique-treatment random intercept that caused severe divergences.
#
# Model:
#   mean | se(semean) ~ is_AI * contexts + (1 | study_id)
#   - study_id random intercept
#   - residual sigma estimated and interpreted as treatment-level heterogeneity
################################################################################

library(brms)
library(cmdstanr)
library(posterior)
library(bayesplot)
library(bayestestR)
library(tidyverse)

OUT <- "mcmc_simplified_output"
dir.create(OUT, showWarnings = FALSE, recursive = TRUE)

cmdstanr::set_cmdstan_path("C:/Users/昊然 苏/.cmdstan/cmdstan-2.40.0")

load(DG_COMBINED_META)

cm <- combined_meta
cm$ai_flag <- ifelse(!is.na(cm$deepthinking), 1L, 0L)
cm$is_AI <- factor(cm$ai_flag, levels = c(0, 1))
cm$study_id <- ifelse(cm$ai_flag == 1, cm$study, as.character(cm$studyid))
cm$treatment_id <- paste0(cm$study_id, "_", cm$treatid)

ai_rows <- which(cm$ai_flag == 1)
cm$mean[ai_rows] <- cm$mean[ai_rows] / 100
cm$semean[ai_rows] <- cm$semean[ai_rows] / 100
cm$vi <- cm$semean^2

cm <- cm[!(cm$ai_flag == 1 & cm$study %in% c("CGSS-DeepSeek-R1", "GSS-qwen-3.6")), ]

mcmc_data <- cm %>%
  filter(!is.na(mean), !is.na(semean),
         !is.na(takeoption), !is.na(socialdistance),
         !is.na(incentive), !is.na(recdesv), !is.na(dictearn)) %>%
  mutate(
    is_AI = factor(is_AI, levels = c(0, 1)),
    takeoption = factor(takeoption),
    socialdistance = factor(socialdistance),
    incentive = factor(incentive),
    study_id = factor(study_id),
    treatment_id = factor(treatment_id)
  )

priors_simplified <- c(
  prior(normal(0.3, 0.3), class = "Intercept"),
  prior(normal(0, 0.5), class = "b"),
  prior(cauchy(0, 0.3), class = "sd"),
  prior(cauchy(0, 0.2), class = "sigma")
)

fit_simple <- brm(
  formula = mean | se(semean) ~
    is_AI * (takeoption + socialdistance + incentive + recdesv + dictearn) +
    (1 | study_id),
  data = mcmc_data,
  prior = priors_simplified,
  family = gaussian(),
  chains = 4,
  iter = 5000,
  warmup = 2500,
  cores = 4,
  seed = 20260908,
  control = list(adapt_delta = 0.99, max_treedepth = 15),
  backend = "cmdstanr"
)

draws <- posterior::as_draws_df(fit_simple)
diag <- posterior::summarise_draws(draws)
np <- brms::nuts_params(fit_simple)

write.csv(diag, file.path(OUT, "simple_mcmc_diagnostics_all.csv"), row.names = FALSE)
write.csv(np[np$Parameter == "divergent__", ], file.path(OUT, "simple_mcmc_divergences.csv"), row.names = FALSE)
write.csv(np[np$Parameter == "treedepth__", ], file.path(OUT, "simple_mcmc_treedepth.csv"), row.names = FALSE)

core <- c("b_is_AI1", "b_is_AI1:takeoption1", "b_is_AI1:recdesv")
post <- as.data.frame(fit_simple)
post_summary <- do.call(rbind, lapply(core, function(p) {
  x <- post[[p]]; h <- bayestestR::hdi(x, ci = 0.95)
  data.frame(parameter = p, mean = mean(x), sd = sd(x),
             hdi_lb = h$CI_low, hdi_ub = h$CI_high,
             p_positive = mean(x > 0), p_negative = mean(x < 0))
}))
write.csv(post_summary, file.path(OUT, "simple_mcmc_core_summary.csv"), row.names = FALSE)

bundle <- list(
  metadata = list(created_at = Sys.time(), formula = formula(fit_simple)),
  diagnostics_all = diag,
  posterior_summary_core = post_summary,
  divergences = np[np$Parameter == "divergent__", ],
  treedepth = np[np$Parameter == "treedepth__", ],
  fixed_effects_summary = summary(fit_simple)$fixed,
  session_info = sessionInfo()
)
saveRDS(bundle, file.path(OUT, "simple_mcmc_diagnostics_for_codex.rds"), compress = "xz")
saveRDS(fit_simple, file.path(OUT, "simple_mcmc_fit.rds"), compress = "xz")

cat("Saved to:", normalizePath(OUT), "\n")
cat("Send this file:", normalizePath(file.path(OUT, "simple_mcmc_diagnostics_for_codex.rds")), "\n")
