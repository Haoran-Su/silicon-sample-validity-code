source("R/00_config.R")
suppressPackageStartupMessages({library(metafor); library(dplyr); library(tidyr)})
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
f_int <- ~ is_AI * (takeoption + socialdistance + incentive + recdesv + dictearn)
mod_int <- rma.mv(yi=mean,V=vi,mods=f_int,random=~1|study_f/treat_f,data=mod_data,method="REML")
getc <- function(m,nm){i<-which(names(coef(m))==nm); if(length(i)==0)return(NA_real_); unname(coef(m)[i])}
set.seed(20260917); B <- 50
studies <- as.character(unique(mod_data$study_id))
boot_ai <- boot_take <- boot_rec <- numeric(B)
for(b in 1:B){
  s <- sample(studies, size=length(studies), replace=TRUE)
  dat <- do.call(rbind, lapply(seq_along(s), function(j){z<-mod_data[mod_data$study_id==s[j],,drop=FALSE]; z$boot_id<-paste0(s[j],"_",j); z$treatment_id<-paste0(z$boot_id,"_",z$treatid); z$study_f<-factor(z$boot_id); z$treat_f<-factor(z$treatment_id); z}))
  m <- tryCatch(rma.mv(yi=mean,V=vi,mods=f_int,random=~1|study_f/treat_f,data=dat,method="REML",control=list(iter.max=80,stepadj=0.5)), error=function(e)NULL)
  if(!is.null(m)){boot_ai[b]<-getc(m,"is_AI1"); boot_take[b]<-getc(m,"is_AI1:takeoption1"); boot_rec[b]<-getc(m,"is_AI1:recdesv")}
  cat("boot",b,"\n")
}
boot_ai<-boot_ai[is.finite(boot_ai)]; boot_take<-boot_take[is.finite(boot_take)]; boot_rec<-boot_rec[is.finite(boot_rec)]
boot_summary<-data.frame(term=c("is_AI1","is_AI1:takeoption1","is_AI1:recdesv"),
 obs=c(getc(mod_int,"is_AI1"),getc(mod_int,"is_AI1:takeoption1"),getc(mod_int,"is_AI1:recdesv")),
 boot_mean=c(mean(boot_ai),mean(boot_take),mean(boot_rec)),
 ci_lb=c(quantile(boot_ai,.025),quantile(boot_take,.025),quantile(boot_rec,.025)),
 ci_ub=c(quantile(boot_ai,.975),quantile(boot_take,.975),quantile(boot_rec,.975)))
print(boot_summary); write.csv(boot_summary,file.path(DG_OUTPUT_DIR, "cluster_bootstrap.csv"),row.names=FALSE)
bayes_coef<-function(m,nm,prior_sd=1){i<-which(names(coef(m))==nm); if(length(i)==0)return(c(post_mean=NA,post_sd=NA,lb=NA,ub=NA)); b<-coef(m)[i]; se<-m$se[i]; prec<-1/se^2+1/prior_sd^2; pm<-(b/se^2)/prec; psd<-sqrt(1/prec); c(post_mean=pm,post_sd=psd,lb=pm-1.96*psd,ub=pm+1.96*psd)}
terms<-c("is_AI1","is_AI1:takeoption1","is_AI1:recdesv"); bayes<-as.data.frame(t(sapply(terms,function(nm)bayes_coef(mod_int,nm)))); bayes$term<-rownames(bayes)
print(bayes); write.csv(bayes,file.path(DG_OUTPUT_DIR, "bayesian_normal_approx.csv"),row.names=FALSE)
cat("saved\n"); quit(save="no")
