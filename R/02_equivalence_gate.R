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
gate <- function(h, inter, label){
  i1 <- which(names(coef(mod_int))==h); i2 <- which(names(coef(mod_int))==inter)
  if(length(i1)==0 || length(i2)==0) return(data.frame(label=label, human=NA, ai=NA, diff=NA, ci90_lb=NA, ci90_ub=NA, gate="无法判定"))
  hb<-coef(mod_int)[i1]; ab<-hb+coef(mod_int)[i2]
  V<-vcov(mod_int); diff<-coef(mod_int)[i2]; se<-mod_int$se[i2]
  ci<-diff+c(-1.645,1.645)*se
  g<-if(ci[1] > 0.05) "不通过(diff>+0.05)" else if(ci[2] < -0.05) "不通过(diff<-0.05)" else if(ci[2]<=0.05 & ci[1]>=-0.05) "通过" else "证据不足"
  data.frame(label=label, human=hb, ai=ab, diff=diff, ci90_lb=ci[1], ci90_ub=ci[2], gate=g)
}
res <- rbind(
  gate("incentive1","is_AI1:incentive1","incentive"),
  gate("dictearn","is_AI1:dictearn","dictearn"),
  gate("socialdistance2","is_AI1:socialdistance2","socialdistance2")
)
print(res)
write.csv(res, file.path(DG_OUTPUT_DIR, "gate_evidence_insufficient.csv"), row.names=FALSE)
cat("saved\n"); quit(save="no")
