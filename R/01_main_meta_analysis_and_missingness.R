source("R/00_config.R")
suppressPackageStartupMessages({
  library(metafor); library(sandwich); library(dplyr); library(tidyr); library(mice)
})

logfile <- file.path(DG_OUTPUT_DIR, "analysis_log.txt")
dir.create("outputs", showWarnings=FALSE, recursive=TRUE)
sink(logfile, split=FALSE)
cat("=== 固定面板 + 元分析完整分析日志 ===\n")
cat("开始:", as.character(Sys.time()), "\n\n")

p <- DG_COMBINED_META
e <- new.env(); load(p, envir=e)

# ---------- 数据准备 ----------
cm <- e$combined_meta
cm$ai_flag <- ifelse(!is.na(cm$deepthinking), 1L, 0L)
cm$is_AI <- factor(cm$ai_flag, levels=c(0,1))
cm$study_id <- ifelse(cm$ai_flag==1, cm$study, as.character(cm$studyid))
cm$treatment_id <- paste0(cm$study_id, "_", cm$treatid)
ai_rows <- which(cm$ai_flag==1)
cm$mean[ai_rows] <- cm$mean[ai_rows]/100
cm$semean[ai_rows] <- cm$semean[ai_rows]/100
cm$vi <- cm$semean^2
cm$takeoption <- factor(cm$takeoption)
cm$socialdistance <- factor(cm$socialdistance)
cm$incentive <- factor(cm$incentive)
cm$study_f <- factor(cm$study_id)
cm$treat_f <- factor(cm$treatment_id)

full_data <- cm
mod_data <- full_data %>%
  filter(!is.na(mean), !is.na(vi), !is.na(takeoption), !is.na(socialdistance),
         !is.na(incentive), !is.na(recdesv), !is.na(dictearn))

human_data <- full_data %>% filter(ai_flag==0)
ai_data <- full_data %>% filter(ai_flag==1)

cat("数据规模: full=", nrow(full_data), " mod=", nrow(mod_data),
    " human=", nrow(human_data), " ai=", nrow(ai_data), "\n")
cat("AI 模型面板:", paste(sort(unique(ai_data$study)), collapse=" | "), "\n\n")

# ---------- 1. 缺失机制 ----------
cat("[1] 缺失机制\n")
cat("人类 semean 缺失:", sum(human_data$ai_flag==0 & is.na(human_data$semean)), "\n")
cat("人类 mean 缺失:", sum(human_data$ai_flag==0 & is.na(human_data$mean)), "\n")
full_data$miss_semean <- as.integer(is.na(full_data$semean))
m_miss <- glm(miss_semean ~ ai_flag + year, data=full_data, family=binomial)
cat("缺失 logistic:\n"); print(summary(m_miss)$coefficients)

# ---------- 2. 人类元分析 ----------
f_ctx <- ~ takeoption + socialdistance + incentive + recdesv + dictearn
human_cc <- human_data %>% filter(!is.na(mean), !is.na(vi), !is.na(takeoption),
                                  !is.na(socialdistance), !is.na(incentive),
                                  !is.na(recdesv), !is.na(dictearn))
human_mv <- rma.mv(yi=mean, V=vi, mods=f_ctx,
                   random=~1|study_f/treat_f, data=human_cc, method="REML")
cat("\n[2] 人类元回归\n"); print(summary(human_mv))
write.csv(as.data.frame(summary(human_mv)$b), file.path(DG_OUTPUT_DIR, "human_meta_effects.csv"))

# ---------- 3. AI 固定面板情境效应 ----------
# model 作为固定效应
f_ai <- ~ study + takeoption + socialdistance + incentive + recdesv + dictearn
ai_fe <- rma(yi=mean, vi=vi, mods=f_ai, data=ai_data, method="FE")
cat("\n[3] AI 固定面板情境效应（model 固定效应）\n"); print(summary(ai_fe))
write.csv(as.data.frame(summary(ai_fe)$b), file.path(DG_OUTPUT_DIR, "ai_context_effects.csv"))

# ---------- 4. 面板跨模型一致性 ----------
cat("\n[4] 面板跨模型一致性\n")
ctxs <- c("takeoption","socialdistance","incentive","recdesv","dictearn")
cons <- lapply(ctxs, function(v){
  d <- ai_data[!is.na(ai_data[[v]]),]
  d$lev <- d[[v]]
  d %>% group_by(study) %>% summarise(m0=mean(mean[lev==0]), m1=mean(mean[lev==1]), .groups="drop") %>%
    mutate(ctx=v, diff=m1-m0, direction=ifelse(diff>0,"+",ifelse(diff<0,"-","0")))
})
cons <- bind_rows(cons)
cons_sum <- cons %>% group_by(ctx) %>%
  summarise(n_models=n(), n_positive=sum(diff>0), n_negative=sum(diff<0), median_diff=median(diff), .groups="drop")
cat("每情境方向一致统计:\n"); print(as.data.frame(cons_sum))
write.csv(as.data.frame(cons), file.path(DG_OUTPUT_DIR, "panel_consistency.csv"), row.names=FALSE)

# ---------- 5. 人机对照 + 门控 ----------
f_int <- ~ is_AI * (takeoption + socialdistance + incentive + recdesv + dictearn)
mod_int <- rma.mv(yi=mean, V=vi, mods=f_int,
                  random=~1|study_f/treat_f, data=mod_data, method="REML")
cat("\n[5] 人机交互模型\n"); print(summary(mod_int))
write.csv(as.data.frame(summary(mod_int)$b), file.path(DG_OUTPUT_DIR, "human_ai_interaction.csv"))

sum_eff <- function(model, b1, b2){
  nm <- names(coef(model)); i1 <- which(nm==b1); i2 <- which(nm==b2)
  if(length(i1)==0 || length(i2)==0) return(c(b=NA,se=NA))
  b <- coef(model)[i1] + coef(model)[i2]
  V <- vcov(model); se <- sqrt(V[i1,i1] + V[i2,i2] + 2*V[i1,i2])
  c(b=b, se=se)
}
gates <- list(
  takeoption = list(h="takeoption1", int="is_AI1:takeoption1"),
  socialdistance = list(h="socialdistance1", int="is_AI1:socialdistance1"),
  incentive = list(h="incentive1", int="is_AI1:incentive1"),
  recdesv = list(h="recdesv", int="is_AI1:recdesv"),
  dictearn = list(h="dictearn", int="is_AI1:dictearn")
)
gate_res <- lapply(names(gates), function(g){
  h <- getc <- NULL
  gg <- gates[[g]]
  hh <- getc2 <- function(model,nm){i<-which(names(coef(model))==nm); if(length(i)==0)return(c(b=NA,se=NA)); c(b=coef(model)[i],se=model$se[i])}
  h <- hh(mod_int, gg$h); inter <- hh(mod_int, gg$int)
  if(any(is.na(inter))) return(data.frame(context=g, human_b=h[1], human_se=h[2], ai_b=NA, ai_se=NA, diff=NA, diff_se=NA, gate="无法判定"))
  ai <- c(b=h[1]+inter[1], se=NA)
  V <- vcov(mod_int); i1<-which(names(coef(mod_int))==gg$h); i2<-which(names(coef(mod_int))==gg$int)
  ai_se <- sqrt(V[i1,i1]+V[i2,i2]+2*V[i1,i2])
  ci90 <- c(inter[1]-1.645*inter[2], inter[1]+1.645*inter[2])
  gate <- if(ci90[1] > 0.05) "不通过(差异>+0.05)" else if(ci90[2] < -0.05) "不通过(差异<-0.05)" else if(ci90[2] <= 0.05 && ci90[1] >= -0.05) "通过(90%CI在±0.05内)" else "证据不足"
  data.frame(context=g, human_b=h[1], human_se=h[2], ai_b=ai[1], ai_se=ai_se,
             diff=inter[1], diff_se=inter[2], ci90_lb=ci90[1], ci90_ub=ci90[2], gate=gate)
})
gate_res <- bind_rows(gate_res)
cat("\n门控判定（δ=0.05，90% CI TOST）:\n"); print(as.data.frame(gate_res))
write.csv(gate_res, file.path(DG_OUTPUT_DIR, "gate_judgments.csv"), row.names=FALSE)

# ---------- 6. 敏感性 ----------
cat("\n[6] 敏感性\n")
# 6.1 AI 方差膨胀
inflate <- 1:5
sens <- lapply(inflate, function(k){
  d <- mod_data; d$vi[d$ai_flag==1] <- mod_data$vi[mod_data$ai_flag==1]*k
  m <- rma.mv(yi=mean,V=vi,mods=~ is_AI + takeoption + socialdistance + incentive + recdesv + dictearn,
              random=~1|study_f/treat_f, data=d, method="REML")
  i<-which(names(coef(m))=="is_AI1"); data.frame(inflate=k,b=coef(m)[i],se=m$se[i],p=m$pval[i])
})
sens <- bind_rows(sens); cat("AI 方差膨胀:\n"); print(as.data.frame(sens))
write.csv(sens, file.path(DG_OUTPUT_DIR, "AI_variance_sensitivity.csv"), row.names=FALSE)

# 6.2 IPW
full_data$complete_flag <- as.integer(!full_data$miss_semean & !is.na(full_data$mean))
ps <- glm(complete_flag ~ ai_flag + year, data=full_data, family=binomial)
full_data$ipw <- 1/fitted(ps)
cc <- full_data %>% filter(complete_flag==1, !is.na(ipw), !is.na(vi))
W_ipw <- diag(cc$ipw/cc$vi)
m_ipw <- rma.mv(yi=mean,V=vi,mods=~ is_AI + takeoption + socialdistance + incentive + recdesv + dictearn,
                random=~1|study_f/treat_f, W=W_ipw, data=cc, method="REML")
i<-which(names(coef(m_ipw))=="is_AI1"); cat("IPW is_AI1:", coef(m_ipw)[i], "se", m_ipw$se[i], "p", m_ipw$pval[i], "\n")

# 6.3 MICE (m=5)
imp_base <- full_data %>% mutate(log_semean=log(semean)) %>%
  select(mean, log_semean, ai_flag, year, n, takeoption, socialdistance, incentive, recdesv, dictearn)
imp <- mice(imp_base, m=5, maxit=5, seed=20260917, printFlag=FALSE)
imp_list <- lapply(complete(imp,"all"), function(d){
  d$study_id<-full_data$study_id; d$treatment_id<-full_data$treatment_id
  d$is_AI<-factor(d$ai_flag,levels=c(0,1)); d$takeoption<-factor(d$takeoption)
  d$socialdistance<-factor(d$socialdistance); d$incentive<-factor(d$incentive)
  d$study_f<-factor(d$study_id); d$treat_f<-factor(d$treatment_id); d$vi<-exp(d$log_semean)^2; d
})
fit_mi <- lapply(imp_list, function(d){
  d2<-d[!is.na(d$mean)&!is.na(d$vi),]
  tryCatch(rma.mv(yi=mean,V=vi,mods=~ is_AI + takeoption + socialdistance + incentive + recdesv + dictearn,
                  random=~1|study_f/treat_f, data=d2, method="REML"), error=function(e)NULL)
})
fit_mi <- fit_mi[!vapply(fit_mi,is.null,logical(1))]
ref <- names(coef(fit_mi[[1]]))
B <- sapply(fit_mi, function(m){cf<-coef(m); out<-setNames(rep(NA,length(ref)),ref); out[names(cf)]<-cf; out})
Vlist <- lapply(fit_mi, function(m){v<-vcov(m); V<-matrix(NA,length(ref),length(ref),dimnames=list(ref,ref)); idx<-match(rownames(v),ref); V[idx,idx]<-v; V})
Wm <- Reduce("+", Vlist)/length(fit_mi)
Bm <- var(t(B), na.rm=TRUE)
Tm <- Wm + (1+1/length(fit_mi))*Bm
mi_summary <- data.frame(term=ref, b=rowMeans(B,na.rm=TRUE), se=sqrt(diag(Tm)), stringsAsFactors=FALSE)
mi_summary$p <- 2*pnorm(-abs(mi_summary$b/mi_summary$se))
cat("MICE is_AI1:\n"); print(mi_summary[mi_summary$term=="is_AI1",])
write.csv(mi_summary, file.path(DG_OUTPUT_DIR, "MI_additive_results.csv"), row.names=FALSE)

cat("\n完成:", as.character(Sys.time()), "\n")
sink()
cat("LOG WRITTEN\n")
quit(save="no")
