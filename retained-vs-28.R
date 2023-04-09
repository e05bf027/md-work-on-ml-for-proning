# retained-vs-28.R
# ----------------
# this script examines if there is an association between retained response to
# proning (as measured by PF ratio being higher after returning supine than
# before turning prone)

# setup
set.seed(3.14)
library(tidyverse)
load(file = 'data/pre_post_changes.Rda')

# add if patient responded
pre_post_changes$pf_responder <- if_else(condition = (pre_post_changes$pf_ratio_supine_post - pre_post_changes$pf_ratio_supine) > 0,
                                         true = 'responder', 
                                         false = 'non-responder')

# look at association between 28 day mortality and proning change retention
changes_sess_01 <- pre_post_changes %>% 
  filter(proning_session == 1)

t <- table(changes_sess_01$mortality_28, changes_sess_01$pf_responder)
rownames(t) <- c('lived for 28', 'died within 28')

base::chisq.test(changes_sess_01$pf_responder, changes_sess_01$mortality_28, correct = F)
   # no significant difference 