source("R/00_config.R")
suppressPackageStartupMessages({library(metafor); library(sandwich); library(dplyr); library(tidyr); library(mice)})
p <- DG_COMBINED_META
e <- new.env(); load(p, envir=e)
cm <- e$combined_meta
cm$ai_flag <- ifelse(!is.na(cm$deepthinking),1L,0L)
cm$is_AI <- factor(cm$ai_flag, levels=c(0,1))
cm$study_id <- ifelse(cm$ai_flag==1, cm$study, as.character(cm$studyid))
cm$treatment_id <- paste0(cm$study_id,"_",cm$treatid)
ai_rows <- which(cm$ai_flag==1)
cm$mean[ai_rows] <- cm$mean[ai_rows]/100; cm$semean[ai_rows] <- cm$semean[ai_rows]/100
cm$vi <- cm$semean^2
cm$takeoption <- factor(cm$takeoption); cm$socialdistance <- factor(cm$socialdistance); cm$incentive <- factor(cm$incentive)
cm$study_f <- factor(cm$study_id); cm$treat_f <- factor(cm$treatment_id)
mod_data <- cm %>% filter(!is.na(mean),!is.na(vi),!is.na(takeoption),!is.na(socialdistance),!is.na(incentive),!is.na(recdesv),!is.na(dictearn))
f_add <- ~ is_AI + takeoption + socialdistance + incentive + recdesv + dictearn

# ---------- 1. True wild cluster bootstrap (WLS fixed-effect + Webb weights) ----------
X <- model.matrix(f_add, data=mod_data)
y <- mod_data$mean; v <- mod_data$vi
wls <- lm(y ~ X - 1, weights = 1/v)
set.seed(20260917)
wcb <- vcovBS(wls, cluster = mod_data$study_f, R = 999, type = "webb")
b <- coef(wls); se_wcb <- sqrt(diag(wcb))
names_wcb <- colnames(X)
wcb_df <- data.frame(term=names_wcb, b=b, se=se_wcb,
                     ci_lb=b-1.96*se_wcb, ci_ub=b+1.96*se_wcb,
                     p=2*pnorm(-abs(b/se_wcb)))
wcb_isAI <- wcb_df[wcb_df$term=="is_AI1",]
cat("wild cluster bootstrap (WLS) is_AI1:\n"); print(wcb_isAI)
write.csv(wcb_df, file.path(DG_OUTPUT_DIR, "wild_cluster_bootstrap_WLS.csv"), row.names=FALSE)

# ---------- 2. Full MCMC Bayesian (two-level random effects, known sampling variances) ----------
stud <- as.integer(factor(mod_data$study_id)); G <- max(stud); n <- nrow(mod_data); pcols <- ncol(X)
u <- rep(0, G); w <- rep(0, n); tau2s <- var(y); sigma2 <- var(y)/2
iter <- 2000; burn <- 1000; thin <- 2; keep <- floor((iter-burn)/thin)
beta_draws <- matrix(NA, keep, pcols); colnames(beta_draws) <- colnames(X)
Qprior <- diag(1/100^2, pcols)
Xv <- X / v
cnt <- 0
set.seed(20260917)
for(it in 1:iter){
  # beta
  r <- y - u[stud] - w
  Q <- crossprod(Xv, X) + Qprior
  bhat <- solve(Q, crossprod(Xv, r))
  R <- chol(Q)
  beta <- bhat + solve(R, rnorm(pcols))
  # study random effects u
  r <- y - X %*% beta - w
  prec_u <- 1/tau2s
  for(g in 1:G){
    idx <- which(stud==g)
    vi <- v[idx]; ri <- r[idx]
    psum <- sum(1/vi) + prec_u
    mu <- sum(ri/vi)/psum
    u[g] <- mu + rnorm(1)/sqrt(psum)
  }
  # treatment random effects w
  r <- y - X %*% beta - u[stud]
  pw <- 1/v + 1/sigma2
  mw <- (r/v)/pw
  w <- mw + rnorm(n)/sqrt(pw)
  # variances
  tau2s <- 1/rgamma(1, 0.001 + G/2, rate = 0.001 + 0.5*sum(u^2))
  sigma2 <- 1/rgamma(1, 0.001 + n/2, rate = 0.001 + 0.5*sum(w^2))
  if(it > burn && ((it-burn) %% thin == 1)) { cnt <- cnt+1; beta_draws[cnt,] <- beta }
}
beta_draws <- beta_draws[1:cnt,,drop=FALSE]
bayes_sum <- data.frame(term=colnames(X), post_mean=colMeans(beta_draws), post_sd=apply(beta_draws,2,sd),
                        ci_lb=apply(beta_draws,2,function(x)quantile(x,.025)),
                        ci_ub=apply(beta_draws,2,function(x)quantile(x,.975)))
cat("\nFull MCMC Bayesian (Gibbs) key terms:\n")
print(bayes_sum[bayes_sum$term %in% c("is_AI1","takeoption1","socialdistance1","socialdistance2","incentive1","incentive2","recdesv","dictearn"),])
write.csv(bayes_sum, file.path(DG_OUTPUT_DIR, "bayesian_MCMC_gibbs.csv"), row.names=FALSE)

# ---------- 3. MICE m=20 ----------
full_data <- cm
full_data$miss_semean <- as.integer(is.na(full_data$semean))
imp_base <- full_data %>% mutate(log_semean=log(semean)) %>%
  select(mean, log_semean, ai_flag, year, n, takeoption, socialdistance, incentive, recdesv, dictearn)
imp20 <- mice(imp_base, m=20, maxit=10, seed=20260917, printFlag=FALSE)
imp20_list <- lapply(complete(imp20,"all"), function(d){
  d$study_id<-full_data$study_id; d$treatment_id<-full_data$treatment_id
  d$is_AI<-factor(d$ai_flag,levels=c(0,1)); d$takeoption<-factor(d$takeoption)
  d$socialdistance<-factor(d$socialdistance); d$incentive<-factor(d$incentive)
  d$study_f<-factor(d$study_id); d$treat_f<-factor(d$treatment_id); d$vi<-exp(d$log_semean)^2; d
})
fit20 <- lapply(imp20_list, function(d){
  d2<-d[!is.na(d$mean)&!is.na(d$vi),]
  tryCatch(rma.mv(yi=mean,V=vi,mods=f_add,random=~1|study_f/treat_f,data=d2,method="REML"), error=function(e)NULL)
})
fit20 <- fit20[!vapply(fit20,is.null,logical(1))]
ref <- names(coef(fit20[[1]]))
B <- sapply(fit20, function(m){cf<-coef(m); out<-setNames(rep(NA,length(ref)),ref); out[names(cf)]<-cf; out})
Vlist <- lapply(fit20, function(m){v<-vcov(m); V<-matrix(NA,length(ref),length(ref),dimnames=list(ref,ref)); idx<-match(rownames(v),ref); V[idx,idx]<-v; V})
Wm <- Reduce("+", Vlist)/length(fit20); Bm <- var(t(B), na.rm=TRUE); Tm <- Wm + (1+1/length(fit20))*Bm
mi20 <- data.frame(term=ref, b=rowMeans(B,na.rm=TRUE), se=sqrt(diag(Tm)), p=2*pnorm(-abs(rowMeans(B,na.rm=TRUE)/sqrt(diag(Tm)))))
cat("\nMICE m=20 is_AI1:\n"); print(mi20[mi20$term=="is_AI1",])
write.csv(mi20, file.path(DG_OUTPUT_DIR, "MI_additive_m20.csv"), row.names=FALSE)
cat("saved\n"); quit(save="no")
