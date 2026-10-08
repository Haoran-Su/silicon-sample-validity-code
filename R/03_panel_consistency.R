source("R/00_config.R")
suppressPackageStartupMessages({library(dplyr); library(tidyr)})
p <- DG_COMBINED_META
e <- new.env(); load(p, envir=e)
cm <- e$combined_meta
cm$ai_flag <- ifelse(!is.na(cm$deepthinking),1L,0L)
ai_rows <- which(cm$ai_flag==1)
cm$mean[ai_rows] <- cm$mean[ai_rows]/100
ai <- cm[cm$ai_flag==1,]
ctxs <- c("takeoption","socialdistance","incentive","recdesv","dictearn")
out <- list()
for(v in ctxs){
  lev <- as.numeric(as.character(ai[[v]]))
  d <- data.frame(study=ai$study, y=ai$mean, lev=lev)
  sm <- d %>% filter(!is.na(lev)) %>% group_by(study, lev) %>% summarise(my=mean(y), .groups="drop_last") %>%
    summarise(lo=min(lev), hi=max(lev), m_lo=mean(my[lev==min(lev)]), m_hi=mean(my[lev==max(lev)]), .groups="drop") %>%
    mutate(ctx=v, diff=m_hi-m_lo, direction=sign(diff))
  out[[v]] <- sm
}
res <- bind_rows(out)
summ <- res %>% group_by(ctx) %>% summarise(
  n_models = sum(!is.na(diff)),
  n_positive = sum(diff > 0, na.rm=TRUE),
  n_negative = sum(diff < 0, na.rm=TRUE),
  n_zero = sum(diff == 0, na.rm=TRUE),
  median_diff = median(diff, na.rm=TRUE),
  .groups="drop"
)
print(as.data.frame(summ))
write.csv(res, file.path(DG_OUTPUT_DIR, "panel_consistency.csv"), row.names=FALSE)
write.csv(as.data.frame(summ), file.path(DG_OUTPUT_DIR, "panel_consistency_summary.csv"), row.names=FALSE)
cat("saved\n"); quit(save="no")
