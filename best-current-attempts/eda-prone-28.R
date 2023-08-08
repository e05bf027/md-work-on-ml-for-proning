# eda-prone-28.R
# EDA for the proning dataset (only looking at episode 1)

# setup
set.seed(3.14)
library(tidyverse)
library(DataExplorer)
library(correlationfunnel)   # needs no NAs it seems


# load data & refine to include only proning session 1
load('data/pre_post_changes.Rda')
prone_session_1 <- pre_post_changes %>% filter(proning_session == 1)
rm(pre_post_changes)


# refine variable selection
prone_session_1 <- prone_session_1 %>% 
  select(patient_id,
         age_years,
         ards_type, 
         apache_ii, 
         bmi, # bimodal distribution, I'm suspicious of its accuracy
         weight_kg,
         mortality_28,
         time_between_abg,
         sa_o2_systemic_supine,
         pa_o2_supine,
         pa_co2_supine, 
         bicarbonate_abg_a_supine, 
         lactate_abg_supine,
         base_excess_vt_supine,
         sodium_abg_supine,
         ionised_calcium_abg_supine,
         anion_gap_abg_supine,
         fi_o2_supine, 
         end_tidal_co2_marquette_supine,
         peep_supine, 
         total_haemoglobin_supine,
         haematocrit_supine,
         white_cell_count_supine, 
         neutrophils_supine, 
         lymphocytes_supine, 
         c_reactive_protein_supine, 
         urea_supine, 
         pcre_supine, 
         albumin_supine, 
         aa_gradient_paco2_supine, 
         aa_gradient_paco2_prone,
         aa_gradient_paco2_supine_post,
         aa_paco2_change_absolute, 
         aa_paco2_retain_absolute,
         minute_volume_coalesced_supine, 
         peak_pressure_coalesced_supine,
         peak_pressure_coalesced_prone,
         peak_pressure_coalesced_supine_post,
         peak_pressure_change_absolute,
         peak_pressure_retain_absolute, 
         pf_ratio_supine, 
         pf_ratio_prone, 
         pf_ratio_supine_post, 
         pfr_change_absolute, 
         pfr_retain_absolute)


# explore missingness, and histograms to show distributions
plot_histogram(prone_session_1)
plot_missing(prone_session_1)


# rm vars with >10% missing (except bmi)
prone_session_1 <- prone_session_1 %>% 
  select(-c(ionised_calcium_abg_supine,
            anion_gap_abg_supine,
            end_tidal_co2_marquette_supine))


# save resulting df
save(data = prone_session_1, file = 'sketches-ideas/prone_session_1_cleaned.Rda')
save(data = prone_session_1, file = 'data/prone_session_1_cleaned.Rda')