# s02_prePost.R
# ==================
# This script reads in both the 'demographics.Rda' data-frame and the 'pre_post_01.xlsx'
# file (which consists of manually selected ABGs, etc. from before and after instances
# of prone-positioning) and reassembles them to generate a new dataframe that is a 
# modified wide version of the 'pre_post' table.
#
# The resulting dataframe can be used to start analysis and ML attempts.

# setup
library(tidyverse)
library(readxl)
library(janitor)
library(lubridate)

# read in dataframe of physiological measures (and remove old demographics)
path <- '~/Documents/02. Medicine/Med_Programming/00. Patient DB/ai_df/pre_post_01.xlsx'
pre_post_raw <- read_xlsx(path = path, col_names = T, guess_max = 500) %>% 
  clean_names() %>% 
  select(-c('age_years',
            'height_cm',
            'weight_kg',
            'gender',
            'los_days',
            'apache_ii',
            'adm_location',
            'pf_ratio',
            'patient_positioning',
            'patient_positioning_abg'))

# read in demographics info
setwd('~/Documents/02. Medicine/Med_Programming/00. Patient DB/ai_df/interim_dfs')
load('demographics.Rda')

# coalesce the various methods of minute volume measurement into one col
pre_post_raw <- pre_post_raw %>% 
  mutate(minute_volume_coalesced = coalesce(minute_volume_pb, 
                                            minute_volume_expired_s, 
                                            minute_volume_d, 
                                            expired_minute_volume_measured_a),
         mean_airway_pressure_coalesced = coalesce(mean_airway_pressure_pb,
                                                   mean_airway_pressure_s,
                                                   mean_airway_pressure_d,
                                                   mean_airway_pressure_measured_a),
         peak_pressure_coalesced = coalesce(peak_inspiratory_pressure_measured_pb,
                                            peak_inspiratory_pressure_measured_s,
                                            peak_inspiratory_pressure_measured_d,
                                            peak_pressure_measured_a),
         .keep = 'unused')

# add hydrogen ion concentration (rather than just pH)
pre_post_raw <- pre_post_raw %>% 
  mutate(hydrogen_ion_abg_nmolL = 10^(ph_abg*-1) * 1000000000)


## ADD INDICES OF OXYGENATION
# ==========================================
# Predicted body weight is needed
# 1. add gender and height
pre_post_raw <- pre_post_raw %>% 
  left_join(select(demo_raw, gender, patient_id, height_cm), by = 'patient_id')

# 2. split to df and add PBW by gender
pre_post_m <- pre_post_raw %>% 
  filter(gender == 'm') %>% 
  mutate(predicted_weight = 50 + (0.91 * (height_cm - 152.4)))

pre_post_f <- pre_post_raw %>% 
  filter(gender == 'f') %>% 
  mutate(predicted_weight = 45.5 + (0.91 * (height_cm - 152.4)))

# 3. unite the dfs and tidy
pre_post_raw <- bind_rows(pre_post_m, pre_post_f) %>% 
  arrange(patient_id, proning_session) %>% 
  select(-c(gender, height_cm))

# 4. manually modify for patients with height < 150cm (the above formula doesn't work here)
# inspection shows patients 143 & 167 have height < 150cm
pre_post_raw$predicted_weight[pre_post_raw$patient_id == 'patient_143'] <- 46.0
pre_post_raw$predicted_weight[pre_post_raw$patient_id == 'patient_167'] <- 46.0

# Now add the indices PF ratio, Oxygenation Factor, Ventilatory Ratio
pre_post_raw <- pre_post_raw %>%
  mutate(oxy_factor = pa_o2 / fi_o2 * mean_airway_pressure_coalesced,
         ventilatory_ratio = (minute_volume_coalesced * 100) * pa_co2 / (100 * predicted_weight) * 5,
         pf_ratio = pa_o2 / fi_o2)

# replace and recalculate the aa gradient column
pre_post_raw <- pre_post_raw %>% 
  mutate(aa_gradient_paco2 = ((fi_o2 * (101.3 - 6.3)) - (pa_co2 / 0.8)) - pa_o2)

## SEPARATE INTO PRE AND POST PRONING, AND ADMISSION DATA
# isolate supines but NOT reading 0 (ABG from admission)
pre_post_supine <- pre_post_raw %>% 
  filter(position == 'supine_pre' & proning_session != 0) %>% 
  select(-position)

# isolate prones
pre_post_prone <- pre_post_raw %>% 
  filter(position == 'prone') %>% 
  select(-position)

# isolate supines after proning session
pre_post_supine_post <- pre_post_raw %>% 
  filter(position == 'supine_post') %>% 
  select(-position)

# isolate data from admission
admission_data <- pre_post_raw %>% 
  filter(proning_session == 0) %>% 
  select(-position)


## ALTER COLNAMES TO REFLECT PATIENT POSITION
# ==========================================
# set patient_id and proning_session columns aside
id_and_session <- select(pre_post_prone, 'patient_id', 'proning_session')

# and remove them from the pre_post dfs
pre_post_supine <- select(pre_post_supine, -c('patient_id', 'proning_session'))
pre_post_prone <- select(pre_post_prone, -c('patient_id', 'proning_session'))
pre_post_supine_post <- select(pre_post_supine_post, -c('patient_id', 'proning_session'))

# create new names for columns 
supine_names <- str_c(names(pre_post_supine), '_supine')
prone_names <- str_c(names(pre_post_prone), '_prone')
supine_post_names <- str_c(names(pre_post_supine_post), '_supine_post')

# rename the columns
colnames(pre_post_supine) <- supine_names
colnames(pre_post_prone) <- prone_names
colnames(pre_post_supine_post) <- supine_post_names

# rejoin it all
pre_post_wide <- bind_cols(id_and_session, pre_post_supine) %>% 
  bind_cols(pre_post_prone) %>% 
  bind_cols(pre_post_supine_post)


## ADD DEMOGRAPHICS
# ========================================
# start by cleaning the predicted weight column
pre_post_wide <- pre_post_wide %>% 
  select(-c(predicted_weight_prone, predicted_weight_supine_post)) %>% 
  rename(predicted_weight = predicted_weight_supine)

# add demographic details
pre_post_wide <- pre_post_wide %>% 
  left_join(demo_raw, by = 'patient_id') 


# ADD NEW COLS TO DESCRIBE ABSOLUTE AND PERCENT DIFFERENCES POST PRONING
# ========================================
pre_post_changes <- pre_post_wide %>% 
  mutate(time_between_abg = time_since_adm_days_prone - time_since_adm_days_supine,
         sa_o2_change_absolute = sa_o2_systemic_prone - sa_o2_systemic_supine,
         sa_o2_retain_absolute = sa_o2_systemic_supine_post - sa_o2_systemic_supine,
         
         ph_change_absolute = ph_abg_prone - ph_abg_supine,
         ph_retain_absolute = ph_abg_supine_post - ph_abg_supine,
         hydrogen_ion_absolute = hydrogen_ion_abg_nmolL_prone - hydrogen_ion_abg_nmolL_supine,
         hydrogen_ion_retain_absolute = hydrogen_ion_abg_nmolL_supine_post - hydrogen_ion_abg_nmolL_supine,
         
         pa_o2_change_absolute = pa_o2_prone - pa_o2_supine,
         pa_o2_retain_absolute = pa_o2_supine_post - pa_o2_supine,
         
         pa_co2_change_absolute = pa_co2_prone - pa_co2_supine,
         pa_co2_retain_absolute = pa_co2_supine_post - pa_co2_supine,
         
         bicarbonate_change_absolute = bicarbonate_abg_a_prone - bicarbonate_abg_a_supine,
         bicarbonate_retain_absolute = bicarbonate_abg_a_supine_post - bicarbonate_abg_a_supine,
         
         lactate_abg_change_absolute = lactate_abg_prone - lactate_abg_supine,
         lactate_abg_retain_absolute = lactate_abg_supine_post - lactate_abg_supine,
         
         base_excess_change_absolute = base_excess_vt_prone - base_excess_vt_supine,
         base_excess_retain_absolute = base_excess_vt_supine_post - base_excess_vt_supine,
         
         potassium_abg_change_absolute = potassium_abg_prone - potassium_abg_supine,
         potassium_abg_retain_absolute = potassium_abg_supine_post - potassium_abg_supine,
         
         sodium_abg_change_absolute = sodium_abg_prone - sodium_abg_supine,
         sodium_abg_retain_absolute = sodium_abg_supine_post - sodium_abg_supine,
         
         ionised_calcium_abg_change_absolute = ionised_calcium_abg_prone - ionised_calcium_abg_supine,
         ionised_calcium_abg_retain_absolute = ionised_calcium_abg_supine_post - ionised_calcium_abg_supine,
         
         anion_gap_change_absolute = anion_gap_abg_prone - anion_gap_abg_supine,
         anion_gap_retain_absolute = anion_gap_abg_supine_post - anion_gap_abg_supine,
         
         glucose_change_absolute = glucose_abg_prone - glucose_abg_supine,
         glucose_retain_absolute = glucose_abg_supine_post - glucose_abg_supine,
         
         fi_o2_change_absolute = fi_o2_prone - fi_o2_supine,
         fi_o2_retain_absolute = fi_o2_supine_post - fi_o2_supine,
         
         et_co2_change_absolute = end_tidal_co2_marquette_prone - end_tidal_co2_marquette_supine,
         et_co2_retain_absolute = end_tidal_co2_marquette_supine_post - end_tidal_co2_marquette_supine,
         
         peep_change_absolute = peep_prone - peep_supine,
         peep_retain_absolute = peep_supine_post - peep_supine,
         
         resp_rate_change_absolute = resp_rate_prone - resp_rate_supine,
         resp_rate_retain_absolute = resp_rate_supine_post - resp_rate_supine,
         
         mean_airway_pressure_change_absolute = mean_airway_pressure_coalesced_prone - mean_airway_pressure_coalesced_supine,
         mean_airway_pressure_retain_absolute = mean_airway_pressure_coalesced_supine_post - mean_airway_pressure_coalesced_supine,
         
         peak_pressure_change_absolute = peak_pressure_coalesced_prone - peak_pressure_coalesced_supine,
         peak_pressure_retain_absolute = peak_pressure_coalesced_supine_post - peak_pressure_coalesced_supine,
         
         pfr_change_absolute = pf_ratio_prone - pf_ratio_supine,
         pfr_retain_absolute = pf_ratio_supine_post - pf_ratio_supine,
         
         oxy_factor_change_absolute = oxy_factor_prone - oxy_factor_supine,
         oxy_factor_retain_absolute = oxy_factor_prone - oxy_factor_supine,
         
         vent_ratio_change_absolute = ventilatory_ratio_prone - ventilatory_ratio_supine,
         vent_ratio_retain_absolute = ventilatory_ratio_supine_post - ventilatory_ratio_supine,
         
         aa_p_aco2_change_absolute = aa_gradient_p_aco2_prone - aa_gradient_p_aco2_supine,
         aa_p_aco2_retain_absolute = aa_gradient_p_aco2_supine_post - aa_gradient_p_aco2_supine,
         
         aa_paco2_change_absolute = aa_gradient_paco2_prone - aa_gradient_paco2_supine,
         aa_paco2_retain_absolute = aa_gradient_paco2_supine_post - aa_gradient_paco2_supine,
         
         minute_volume_change_absolute = minute_volume_coalesced_prone - minute_volume_coalesced_supine,
         minute_volume_retain_absolute = minute_volume_coalesced_supine_post - minute_volume_coalesced_supine,
         
         .keep = ('all'))

# coerce some resulting variables
pre_post_changes <- pre_post_changes %>% 
  mutate(adm_location = factor(adm_location),
         ards_risk_factor = factor(ards_risk_factor),
         ards_type = factor(ards_type),
         gender = factor(gender),
         age_years = as.integer(age_years),
         outcome = factor(outcome))


## add if the patient died within 3, 5, 7 days of that episode of proning
# ===========================
# Die within 72 hours (3 days)
pre_post_changes$die_in_72 <-  if_else(pre_post_changes$outcome == 'rip' & (pre_post_changes$dc_date_time - pre_post_changes$time_prone < 72),
                                       TRUE, FALSE)

# Die within 120 hours (5 days)
pre_post_changes$die_in_120 <-  if_else(pre_post_changes$outcome == 'rip' & (pre_post_changes$dc_date_time - pre_post_changes$time_prone < 120),
                                        TRUE, FALSE)

# Die within 168 hours (7 days)
pre_post_changes$die_in_168 <-  if_else(pre_post_changes$outcome == 'rip' & (pre_post_changes$dc_date_time - pre_post_changes$time_prone < 168),
                                        TRUE, FALSE)

pre_post_changes <- pre_post_changes %>% 
  mutate(die_in_72 = as.logical(die_in_72),
         die_in_120 = as.logical(die_in_120),
         die_in_168 = as.logical(die_in_168))

# clean the result to remove many NA entries that have resulted
pre_post_changes <- pre_post_changes %>% 
  select(-c(total_haemoglobin_prone,
            set_tv_pb_supine,
            set_tv_pb_prone,
            set_tv_pb_supine_post,
            set_tidal_volume_servo_supine,
            set_tidal_volume_servo_prone,
            set_tidal_volume_servo_supine_post,
            set_tv_d_supine,
            set_tv_d_prone,
            set_tv_d_supine_post,
            white_cell_count_prone,
            white_cell_count_supine_post,
            neutrophils_prone,
            neutrophils_supine_post,
            lymphocytes_prone,
            lymphocytes_supine_post,
            c_reactive_protein_prone,
            c_reactive_protein_supine_post,
            urea_prone,
            urea_supine_post,
            pcre_prone,
            pcre_supine_post,
            gfr_prone,
            gfr_supine_post,
            haematocrit_prone,
            haematocrit_supine_post,
            platelet_count_prone,
            platelet_count_supine_post,
            albumin_prone,
            albumin_supine_post,
            expiratory_tidal_volume_d_supine,
            expiratory_tidal_volume_d_prone,
            expiratory_tidal_volume_d_supine_post,
            expired_tidal_volume_measured_avea_supine,
            expired_tidal_volume_measured_avea_prone,
            expired_tidal_volume_measured_avea_supine_post,
            time_since_adm_days_supine,
            time_since_adm_days_prone,
            time_since_adm_days_supine_post,
            s_tidal_volume_inspired_supine,
            s_tidal_volume_inspired_prone,
            s_tidal_volume_inspired_supine_post,
            inotropic_vasoactive_agents_in_progress_supine,
            inotropic_vasoactive_agents_in_progress_prone,
            inotropic_vasoactive_agents_in_progress_supine_post,
            plateau_airway_pressure_d_prone,
            plateau_airway_pressure_d_supine_post,
            resistance_d_prone,
            resistance_d_supine_post,
            adm_date,
            dc_date_time))

# create versions 

# save the resulting df
setwd(dir = '~/Documents/02. Medicine/04. MD/Strand 03 - ML/md-work-on-ml-for-proning/data')
save(data = pre_post_changes, file = 'pre_post_changes.Rda')
save(data = admission_data, file = 'data_at_admission.Rda')
save(data = pre_post_raw, file = 'dataset_before_manipulation.Rda')
setwd('~/Documents/02. Medicine/04. MD/Strand 03 - ML/md-work-on-ml-for-proning')

# tidy up
rm(list = ls())
