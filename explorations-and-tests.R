# explorations-and-tests.R
# a place to test ideas about associations of variables with 28 day mortality

# setup
library(tidyverse)
library(janitor)
library(readxl)
library(plotrix)
library(reshape2) # for 'melt' fx

# read in data
load(file = 'data/pre_post_changes.Rda')
load(file = 'demographics/demographics.Rda')

# refine to isolate first proning
pre_post_changes %>% filter(proning_session == 1) -> pre_post_first_exp 
rm(pre_post_changes)

# ASSOCIATIONS - looking for differences
# 1. Age - small difference
pre_post_first_exp %>% 
  ggplot(aes(x = mortality_28, y = age_years)) +
  geom_boxplot(notch = T, na.rm = T, outlier.colour = 'red') +
  theme_light()

pre_post_first_exp %>% 
  select(age_years, mortality_28) %>% 
  melt(id.vars = 'mortality_28', na.rm = T) %>% 
  group_by(mortality_28) %>% 
  summarise(age_mean = mean(value),
            se = std.error(value),
            ci_95_min = age_mean - (1.96 * se),
            ci_95_max = age_mean + (1.96 * se)) %>% 
  ggplot(aes(x = mortality_28, y = age_mean)) +
  geom_point() +
  geom_errorbar(aes(ymin = ci_95_min,
                    ymax = ci_95_max),
                width = 0.2)


# 2. BMI - no difference plus many NAs
pre_post_first_exp %>% 
  filter(!is.na(bmi)) %>% 
  ggplot(aes(x = mortality_28, y = bmi)) +
  geom_boxplot(notch = T, na.rm = T, outlier.colour = 'red') +
  theme_light()


# 3. Apache II - diff and no overlap in 95% CIs
pre_post_first_exp %>% 
  ggplot(aes(x = mortality_28, y = apache_ii)) +
  geom_boxplot(notch = T, na.rm = T, outlier.colour = 'red') +
  theme_light()

pre_post_first_exp %>% 
  select(apache_ii, mortality_28) %>% 
  melt(id.vars = 'mortality_28', na.rm = T) %>% 
  group_by(mortality_28) %>% 
  summarise(apache_mean = mean(value),
            se = std.error(value),
            ci_95_min = apache_mean - (1.96 * se),
            ci_95_max = apache_mean + (1.96 * se)) %>% 
  ggplot(aes(x = mortality_28, y = apache_mean)) +
  geom_point() +
  geom_errorbar(aes(ymin = ci_95_min,
                    ymax = ci_95_max),
                width = 0.2)


# 4. WCC - no significant difference
pre_post_first_exp %>% 
  ggplot(aes(x = mortality_28, y = white_cell_count_supine)) +
  geom_boxplot(notch = T, na.rm = T, outlier.colour = 'red') +
  theme_light()


# 5. neuts - no significant difference
pre_post_first_exp %>% 
  ggplot(aes(x = mortality_28, y = neutrophils_supine)) +
  geom_boxplot(notch = T, na.rm = T, outlier.colour = 'red') +
  theme_light()


# 6. lymphs - no significant difference
pre_post_first_exp %>% 
  ggplot(aes(x = mortality_28, y = lymphocytes_supine)) +
  geom_boxplot(notch = T, na.rm = T, outlier.colour = 'red') +
  theme_light()


# 7. CRP - no difference
pre_post_first_exp %>% 
  ggplot(aes(x = mortality_28, y = c_reactive_protein_supine)) +
  geom_boxplot(notch = T, na.rm = T, outlier.colour = 'red') +
  theme_light()


# 8. lactate - no difference
pre_post_first_exp %>% 
  ggplot(aes(x = mortality_28, y = lactate_abg_supine)) +
  geom_boxplot(notch = T, na.rm = T, outlier.colour = 'red') +
  theme_light()


# 9. pH - small difference but not significant
pre_post_first_exp %>% 
  ggplot(aes(x = mortality_28, y = hydrogen_ion_abg_nmolL_supine)) +
  geom_boxplot(notch = T, na.rm = T, outlier.colour = 'red') +
  theme_light()

pre_post_first_exp %>% 
  select(hydrogen_ion_abg_nmolL_supine, mortality_28) %>% 
  melt(id.vars = 'mortality_28', na.rm = T) %>% 
  group_by(mortality_28) %>% 
  summarise(hydrogen_mean = mean(value),
            se = std.error(value),
            ci_95_min = hydrogen_mean - (1.96 * se),
            ci_95_max = hydrogen_mean + (1.96 * se)) %>% 
  ggplot(aes(x = mortality_28, y = hydrogen_mean)) +
  geom_point() +
  geom_errorbar(aes(ymin = ci_95_min,
                    ymax = ci_95_max),
                width = 0.2)


# 10. mean airway pressure - no significant diff
pre_post_first_exp %>% 
  ggplot(aes(x = mortality_28, y = mean_airway_pressure_coalesced_supine)) +
  geom_boxplot(notch = T, na.rm = T, outlier.colour = 'red') +
  theme_light()


# 11. peak airway pressure - no significant diff
pre_post_first_exp %>% 
  ggplot(aes(x = mortality_28, y = peak_pressure_coalesced_supine)) +
  geom_boxplot(notch = T, na.rm = T, outlier.colour = 'red') +
  theme_light()


# 12. urea - small significant difference
pre_post_first_exp %>% 
  ggplot(aes(x = mortality_28, y = urea_supine)) +
  geom_boxplot(notch = T, na.rm = T, outlier.colour = 'red') +
  theme_light()

pre_post_first_exp %>% 
  select(urea_supine, mortality_28) %>% 
  melt(id.vars = 'mortality_28', na.rm = T) %>% 
  group_by(mortality_28) %>% 
  summarise(urea_mean = mean(value),
            se = std.error(value),
            ci_95_min = urea_mean - (1.96 * se),
            ci_95_max = urea_mean + (1.96 * se)) %>% 
  ggplot(aes(x = mortality_28, y = urea_mean)) +
  geom_point() +
  geom_errorbar(aes(ymin = ci_95_min,
                    ymax = ci_95_max),
                width = 0.2)


# 13. creatinine - small diff but 95% CIs overlap
pre_post_first_exp %>% 
  ggplot(aes(x = mortality_28, y = pcre_supine)) +
  geom_boxplot(notch = T, na.rm = T, outlier.colour = 'red') +
  theme_light()

pre_post_first_exp %>% 
  select(pcre_supine, mortality_28) %>% 
  melt(id.vars = 'mortality_28', na.rm = T) %>% 
  group_by(mortality_28) %>% 
  summarise(pcre_mean = mean(value),
            se = std.error(value),
            ci_95_min = pcre_mean - (1.96 * se),
            ci_95_max = pcre_mean + (1.96 * se)) %>% 
  ggplot(aes(x = mortality_28, y = pcre_mean)) +
  geom_point() +
  geom_errorbar(aes(ymin = ci_95_min,
                    ymax = ci_95_max),
                width = 0.2)


# 14. base excess
pre_post_first_exp %>% 
  ggplot(aes(x = mortality_28, y = base_excess_vt_supine)) +
  geom_boxplot(notch = T, na.rm = T, outlier.colour = 'red') +
  theme_light()


# 15. haematocrit - small diff but 95% CIs overlap
pre_post_first_exp %>% 
  ggplot(aes(x = mortality_28, y = haematocrit_supine)) +
  geom_boxplot(notch = T, na.rm = T, outlier.colour = 'red') +
  theme_light()

pre_post_first_exp %>% 
  select(haematocrit_supine, mortality_28) %>% 
  melt(id.vars = 'mortality_28', na.rm = T) %>% 
  group_by(mortality_28) %>% 
  summarise(hct_mean = mean(value),
            se = std.error(value),
            ci_95_min = hct_mean - (1.96 * se),
            ci_95_max = hct_mean + (1.96 * se)) %>% 
  ggplot(aes(x = mortality_28, y = hct_mean)) +
  geom_point() +
  geom_errorbar(aes(ymin = ci_95_min,
                    ymax = ci_95_max),
                width = 0.2)


# 16. platelets - small but significant diff
pre_post_first_exp %>% 
  ggplot(aes(x = mortality_28, y = platelet_count_supine)) +
  geom_boxplot(notch = T, na.rm = T, outlier.colour = 'red') +
  theme_light()

pre_post_first_exp %>% 
  select(platelet_count_supine, mortality_28) %>% 
  melt(id.vars = 'mortality_28', na.rm = T) %>% 
  group_by(mortality_28) %>% 
  summarise(plt_mean = mean(value),
            se = std.error(value),
            ci_95_min = plt_mean - (1.96 * se),
            ci_95_max = plt_mean + (1.96 * se)) %>% 
  ggplot(aes(x = mortality_28, y = plt_mean)) +
  geom_point() +
  geom_errorbar(aes(ymin = ci_95_min,
                    ymax = ci_95_max),
                width = 0.2)


# 16. albumen - no significant difference
pre_post_first_exp %>% 
  ggplot(aes(x = mortality_28, y = albumin_supine)) +
  geom_boxplot(notch = T, na.rm = T, outlier.colour = 'red') +
  theme_light()






