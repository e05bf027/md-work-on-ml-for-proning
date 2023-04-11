# retained-vs-28-v2.R
# ----------------
# this script examines if there is an association between retained response to
# proning. PF ratio did not suggest any differences. This will use other
# indices of oxygenation.

# setup
rm(list = ls())
set.seed(3.14)
library(tidyverse)
load(file = 'data/pre_post_changes.Rda')

# refine the dataset - proning sessions over 6 hours, and mod-severe ARDS
pre_post_sess1 <- pre_post_changes %>% 
  filter(proning_session == 1,
         time_between_abg >= 0.25,
         pf_ratio_supine <= 20)


# VENTILATORY RATIO
# =================
# add if patient responded (was the 'retained' VR LOWER than before proning)
pre_post_sess1$vr_responder <- if_else(condition = (pre_post_sess1$ventilatory_ratio_supine_post - pre_post_sess1$ventilatory_ratio_supine) < 0,
                                       true = 'responder',
                                       false = 'non-responder')

pre_post_sess1$vr_responder <- factor(pre_post_sess1$vr_responder)

pre_post_sess1 <- pre_post_sess1 %>% 
  filter(is.na(pre_post_sess1$vr_responder) == F)


# see if any signal here
vr_table <- table(pre_post_sess1$mortality_28, pre_post_sess1$vr_responder)
rownames(vr_table) <- c('lived for 28', 'died within 28')

stats::chisq.test(pre_post_sess1$vr_responder, pre_post_sess1$mortality_28, correct = F)
# no significant difference 


# PF RATIO
# ========
# # add if patient responded
pre_post_sess1$pf_responder <- if_else(condition = (pre_post_sess1$pf_ratio_supine_post - pre_post_sess1$pf_ratio_supine) > 0,
                                       true = 'responder',
                                       false = 'non-responder')

# see if any signal here
pf_table <- table(pre_post_sess1$mortality_28, pre_post_sess1$pf_responder)
rownames(pf_table) <- c('lived for 28', 'died within 28')

stats::chisq.test(pre_post_sess1$pf_responder, pre_post_sess1$mortality_28, correct = F)
  # no significant difference



