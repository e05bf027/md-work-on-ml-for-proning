pre_post_raw['position'][pre_post_raw['position'] == 'supine'] <- 'supine_at_admission'
pre_post_raw['position'][pre_post_raw['position'] == 'supine_pre'] <- 'supine_pre_proning'
pre_post_raw['position'][pre_post_raw['position'] == 'prone'] <- 'prone_end_of_session'
pre_post_raw['position'][pre_post_raw['position'] == 'supine_post'] <- 'supine_post_proning'