source("R/00_config.R")
# ==============================================================================
# 补充检验脚本：多层元分析 + 缺失机制修复
# ------------------------------------------------------------------------------
# 用途：接在原脚本之后运行。核心用现有对象 mod_data（完整案例，483 行）。
# 缺失机制部分需要你另准备 full_data（过滤前，658 行，含被排除的 175 条人类记录）。
#
# 依赖：
#   metafor, clubSandwich, mice, dplyr, tidyr, naniar
#
# 说明：
#   1) 多层部分可以直接用 mod_data 跑。
#   2) AI 方差膨胀敏感性可以直接用 mod_data 跑。
#   3) 缺失机制部分必须用 full_data 跑，否则无法检验 175 条被排除的原因。
# ==============================================================================

suppressPackageStartupMessages({
  library(metafor)
  library(clubSandwich)
  library(mice)
  library(dplyr)
  library(tidyr)
  library(naniar)
})

cat("\n==================== 补充检验开始 ====================\n")

# ------------------------------------------------------------------------------
# 0. 准备分析数据（保留原变量名）
# ------------------------------------------------------------------------------

# mod_data：原脚本中已经完成完整案例过滤的数据
if (!exists("mod_data")) {
  stop("找不到 mod_data，请先运行原脚本。")
}

mod_data <- mod_data %>%
  mutate(
    treatment_id = dplyr::row_number(),
    study_f      = factor(study_id),
    treat_f      = factor(treatment_id),
    is_AI_f      = factor(is_AI, levels = c(0, 1))
  )

# 情境变量名（与原脚本保持一致）
ctx_vars <- c("takeoption", "socialdistance", "incentive", "recdesv", "dictearn")

# 公式
f_main <- as.formula(paste0("~ is_AI_f + ", paste(ctx_vars, collapse = " + ")))
f_int  <- as.formula(paste0("~ is_AI_f * (", paste(ctx_vars, collapse = " + "), ")"))

# 取系数的小工具
get_coef <- function(model, name) {
  i <- which(names(coef(model)) == name)
  if (length(i) == 0) return(c(b = NA, se = NA, z = NA, p = NA))
  c(b  = coef(model)[i],
    se = model$se[i],
    z  = model$zval[i],
    p  = 2 * pnorm(-abs(model$zval[i])))
}

# ==============================================================================
# 1. 多层元分析：研究内 treatment 不独立
# ==============================================================================
cat("\n[1] 多层元分析\n")
cat(rep("-", 60), "\n", sep = "")

mod_ml <- rma.mv(
  yi     = mean,
  V      = vi,
  mods   = f_int,
  random = ~ 1 | study_f / treat_f,
  data   = mod_data,
  method = "REML"
)

cat("\n--- 三层随机效应模型 ---\n")
print(summary(mod_ml))

# 聚类稳健方差：即使随机效应结构不完美，也给出更保守的标准误
mod_ml_rob <- robust(mod_ml, cluster = mod_data$study_f, clubSandwich = TRUE)
cat("\n--- 聚类稳健标准误 ---\n")
print(summary(mod_ml_rob))

# 与原始单层 rma 对比
mod_rma_int <- rma(
  yi = mean, vi = vi,
  mods = f_int,
  data = mod_data, method = "REML"
)

cat("\n--- 单层 rma vs 多层 rma.mv：is_AI1 系数对比 ---\n")
cmp <- data.frame(
  model = c("rma_old", "rma.mv_multilevel", "rma.mv_robust"),
  b     = c(get_coef(mod_rma_int, "is_AI_f1")["b"],
            get_coef(mod_ml, "is_AI_f1")["b"],
            get_coef(mod_ml_rob, "is_AI_f1")["b"]),
  se    = c(get_coef(mod_rma_int, "is_AI_f1")["se"],
            get_coef(mod_ml, "is_AI_f1")["se"],
            get_coef(mod_ml_rob, "is_AI_f1")["se"])
)
print(cmp, row.names = FALSE)

# ==============================================================================
# 2. AI 方差膨胀敏感性：AI 的 vi 可能被低估
# ==============================================================================
cat("\n[2] AI 方差膨胀敏感性\n")
cat(rep("-", 60), "\n", sep = "")

ai_rows <- which(mod_data$is_AI == 1)
inflate_grid <- c(1, 2, 3, 4, 5)

sens_ai <- lapply(inflate_grid, function(k) {
  d <- mod_data
  d$vi[ai_rows] <- mod_data$vi[ai_rows] * k
  m <- tryCatch(
    rma.mv(yi = mean, V = vi, mods = f_int,
           random = ~ 1 | study_f / treat_f,
           data = d, method = "REML"),
    error = function(e) NULL
  )
  if (is.null(m)) return(c(inflate = k, b = NA, se = NA, p = NA))
  out <- get_coef(m, "is_AI_f1")
  c(inflate = k, b = out["b"], se = out["se"], p = out["p"])
})

sens_ai <- as.data.frame(do.call(rbind, sens_ai))
cat("\n--- is_AI1 随 AI 方差膨胀因子的变化 ---\n")
print(sens_ai, row.names = FALSE)

# ==============================================================================
# 3. 缺失机制诊断（需要 full_data，658 行）
# ==============================================================================
cat("\n[3] 缺失机制诊断\n")
cat(rep("-", 60), "\n", sep = "")

if (!exists("full_data")) {
  cat("⚠️  未检测到 full_data。请提供过滤前的 658 行数据后再运行本部分。\n")
  cat("    full_data 至少应包含：mean, vi, study_id, is_AI,\n")
  cat("    takeoption, socialdistance, incentive, recdesv, dictearn\n")
  cat("    如还有 year / n 请一并提供，用于缺失回归。\n")
} else {
  full_data <- full_data %>%
    mutate(
      treatment_id = dplyr::row_number(),
      study_f      = factor(study_id),
      treat_f      = factor(treatment_id)
    )

  # 3.1 缺失指标
  full_data$miss_any <- rowSums(
    is.na(full_data[, c("mean", "vi", ctx_vars)])
  ) > 0
  full_data$miss_outcome <- is.na(full_data$mean) | is.na(full_data$vi)
  full_data$miss_mod <- rowSums(is.na(full_data[, ctx_vars])) > 0

  cat("\n--- 缺失按主体类型分布 ---\n")
  print(table(AI = full_data$is_AI, any_missing = full_data$miss_any))

  cat("\n--- 缺失模式 ---\n")
  print(md.pattern(
    full_data[, c("mean", "vi", ctx_vars)],
    rotate.names = TRUE
  ))

  # 3.2 缺失是否与可观测变量相关
  if (all(c("year", "n") %in% names(full_data))) {
    miss_glm <- glm(
      miss_any ~ is_AI + year + n,
      data = full_data, family = binomial
    )
    cat("\n--- 缺失 logistic 回归 ---\n")
    print(summary(miss_glm))
  } else {
    cat("\n⚠️  未提供 year / n，跳过缺失 logistic 回归。\n")
  }

  # ============================================================================
  # 4. 多重插补敏感性
  # ============================================================================
  cat("\n[4] 多重插补\n")
  cat(rep("-", 60), "\n", sep = "")

  imp_data <- full_data %>%
    mutate(logvi = log(vi)) %>%
    select(mean, logvi, all_of(ctx_vars), is_AI)

  if (all(c("year", "n") %in% names(full_data))) {
    imp_data$year <- full_data$year
    imp_data$n    <- full_data$n
  }

  imp <- mice(imp_data, m = 20, maxit = 10,
              seed = 20260915, print = FALSE)

  # Rubin 合并工具
  pool_rma <- function(model_list) {
    m  <- length(model_list)
    B  <- sapply(model_list, coef)          # 行=系数, 列=插补
    Vs <- lapply(model_list, vcov)
    theta_bar <- rowMeans(B)
    W  <- Reduce("+", Vs) / m
    Bm <- var(t(B))                          # 组间协方差
    T  <- W + (1 + 1 / m) * Bm
    se <- sqrt(diag(T))
    z  <- theta_bar / se
    data.frame(
      term = names(theta_bar),
      b    = theta_bar,
      se   = se,
      z    = z,
      p    = 2 * pnorm(-abs(z))
    )
  }

  imp_list <- complete(imp, "all")
  mi_models <- lapply(imp_list, function(d) {
    d$study_id    <- full_data$study_id
    d$treatment_id <- full_data$treatment_id
    d$study_f     <- factor(d$study_id)
    d$treat_f     <- factor(d$treatment_id)
    d$vi          <- exp(d$logvi)
    tryCatch(
      rma.mv(yi = mean, V = vi, mods = f_int,
             random = ~ 1 | study_f / treat_f,
             data = d, method = "REML"),
      error = function(e) NULL
    )
  })
  mi_models <- mi_models[!vapply(mi_models, is.null, logical(1))]

  if (length(mi_models) >= 5) {
    mi_summary <- pool_rma(mi_models)
    cat("\n--- 多重插补合并结果：is_AI_f1 ---\n")
    print(mi_summary[mi_summary$term == "is_AI_f1", ], row.names = FALSE)
  } else {
    cat("⚠️  多重插补模型收敛失败，跳过。\n")
  }

  # ============================================================================
  # 5. 逆概率加权（IPW）
  # ============================================================================
  cat("\n[5] 逆概率加权（IPW）\n")
  cat(rep("-", 60), "\n", sep = "")

  if (all(c("year", "n") %in% names(full_data))) {
    full_data$complete_flag <- as.integer(!full_data$miss_any)
    ps_fit <- glm(complete_flag ~ is_AI + year + n,
                  data = full_data, family = binomial)
    full_data$ipw <- 1 / fitted(ps_fit)

    cc <- full_data %>%
      filter(!miss_any) %>%
      filter(!is.na(ipw), !is.na(vi))

    W_ipw <- diag(cc$ipw / cc$vi)
    m_ipw <- tryCatch(
      rma.mv(yi = mean, V = vi, mods = f_int,
             random = ~ 1 | study_f / treat_f,
             W = W_ipw, data = cc, method = "REML"),
      error = function(e) NULL
    )
    if (!is.null(m_ipw)) {
      cat("\n--- IPW 模型：is_AI_f1 ---\n")
      print(get_coef(m_ipw, "is_AI_f1"))
    }
  } else {
    cat("⚠️  缺少 year / n，跳过 IPW。\n")
  }

  # ============================================================================
  # 6. 模式混合（缺失人类均值的 MNAR 敏感性）
  # ============================================================================
  cat("\n[6] 模式混合敏感性\n")
  cat(rep("-", 60), "\n", sep = "")

  if (exists("imp_list") && length(imp_list) >= 1) {
    d0 <- imp_list[[1]]
    d0$study_id    <- full_data$study_id
    d0$treatment_id <- full_data$treatment_id
    d0$study_f     <- factor(d0$study_id)
    d0$treat_f     <- factor(d0$treatment_id)
    d0$vi          <- exp(d0$logvi)

    miss_mean <- is.na(full_data$mean)
    delta_grid <- seq(-0.15, 0.15, 0.05)

    pm_res <- lapply(delta_grid, function(delta) {
      dd <- d0
      dd$mean_adj <- dd$mean
      dd$mean_adj[miss_mean] <- dd$mean[miss_mean] + delta
      m <- tryCatch(
        rma.mv(yi = mean_adj, V = vi, mods = f_int,
               random = ~ 1 | study_f / treat_f,
               data = dd, method = "REML"),
        error = function(e) NULL
      )
      if (is.null(m)) return(c(delta = delta, b = NA, se = NA, p = NA))
      out <- get_coef(m, "is_AI_f1")
      c(delta = delta, b = out["b"], se = out["se"], p = out["p"])
    })

    pm_res <- as.data.frame(do.call(rbind, pm_res))
    cat("\n--- is_AI_f1 随 delta 的变化 ---\n")
    print(pm_res, row.names = FALSE)
  }
}

cat("\n==================== 补充检验结束 ====================\n")
