# determination_of_retained_response.R
# this script decides if a patient has 'responded' to being placed in the
# prone positioning by analyzing if their PF ratio after returning to the 
# supine position is GREATER THAN the PF ratio before proning.
# -----------------------------------------------------------------------

# setup
set.seed(3.14)
library(tidyverse)
load(file = 'data/pre_post_changes.Rda')

# add if patient responded
pre_post_changes$pf_responder <- if_else(condition = (pre_post_changes$pf_ratio_supine_post - pre_post_changes$pf_ratio_supine) > 0,
                                         true = T, 
                                         false = F)
