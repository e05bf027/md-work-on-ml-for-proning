# 06_wrangle_script.R
# =========================
# compiles what had been individual scripts into one, to enable wrangling of 
# metavision outputs into a single unified script
# =========================

# STEP 1
# list all input files in the relevant directory, and asks for your
# selection (which it names 'i'). 'i' will then be used in other scripts.
# =============

rm(list = ls())

# warning to ensure files are pre-processed
print('The .xls files that are generated from the Metavision Query wizard')
print('require pre-processing. Have you:')
print('1. changed to extension to .xlsx from .xls?')
print('2. removed the empty rows from the top of the excel file?')
print('====================================================================')
print('If the file(s) has been preprocessed correctly, proceed.')
print('====================================================================')

# List of raw input files
mv_location <- "/Users/davidhannon/Documents/02. Medicine/Med_Programming/00. Patient DB/metavision_outputs"
mv_files <- list.files(mv_location, full.names = TRUE)
view(mv_files)

# choose which file you want to import and process
i <- as.numeric(readline("enter the index of the data file you want to import: "))

# also decide what number you wish to designate for this patient in the final database
j <- as.character(readline('What number do you wish to designate for this patient (use 3 digits): '))
file_name <- sprintf('/Users/davidhannon/Documents/02. Medicine/Med_Programming/00. Patient DB/wrangled_outputs/patient_%s.xlsx', j)

# ========================================================================

# STEP 2
# read and imports the demographics data for the selected patient.
# it also asks the user to select which admission date they wish to read in.

# Enter the path to the demographics file and import it
demo_file_location <- mv_files[i-1]
demo_df <- read_xlsx(demo_file_location, guess_max = 1000000) %>% 
  clean_names()

# select only parameters of interest
demo_df <- demo_df %>% 
  select(admission_date, parameter_name, value, validation_time) %>% 
  pivot_wider(id_cols = admission_date,
              names_from = parameter_name,
              values_from = value) %>% 
  clean_names()
view(demo_df)

# get DOB and admission date, then calculate patient age
birthday <- dmy(readline('Enter the date of birth (DD-MM-YYYY): '))
admission_date <- ymd_hms(readline('Enter the admission date you wish to process (paste from console): '))
patient_age <- as.integer(time_length(difftime(admission_date, birthday), "years"))

# get inputs for height, weight, gender
patient_height <- as.numeric(readline('Enter the patients height (cm): '))
patient_weight <- as.numeric(readline('Enter the patients weight (kg): '))
patient_gender <- as.character(readline('Enter the patients gender (M/F): '))

# wait for user to CLOSE EXCEL before hitting enter (if Excel is open it will
# cause the total file number to change which creates problems in the next script)
readline(prompt="CLOSE EXCEL, then press [enter] to continue")

# Tidy up
rm(demo_file_location,
   demo_df,
   birthday)

# ========================================================================

# STEP 3
# imports patient-specific data to create wrangled data and then 
# turns it into a large tidy tibble.
# =============

# Enter the path to the file you want. Must reset mv_files to give full path
metavision_file_specific <- mv_files[i] 

# reads file at that location. guess_max tells the command to look 1000000
# rows into the file, and see what unit format suits best/fits them all
untidy_tibble <- read_xlsx(metavision_file_specific, guess_max = 1000000) %>% 
  filter(`Admission Date` == admission_date)

# now, isolate out only the columns for the parameter name, value, and time
# that the value was recorded
untidy_tibble <-  untidy_tibble %>% 
  select(Time, `Parameter Name`, Value)

# next, factorize the parameter names
untidy_tibble$`Parameter Name` <- as.factor(untidy_tibble$`Parameter Name`)

# The vales in the 'cardiac rhythm;' column are awkward, and often read as
# list_cols. To get around this, isolate the cardiac rhythm values and remove
# them from the larger data frame.
cardiac_rhythm <- filter(untidy_tibble,
                         untidy_tibble$`Parameter Name` == 'Cardiac Rhythm')

untidy_tibble <- filter(untidy_tibble,
                        untidy_tibble$`Parameter Name` != 'Cardiac Rhythm')

# Remove other parameters that are causing an issue with coercion to lists
untidy_tibble <- filter(untidy_tibble,
                        untidy_tibble$`Parameter Name` != 'GCS Eye Response' &
                          untidy_tibble$`Parameter Name` != 'GCS Motor Response' &
                          untidy_tibble$`Parameter Name` != 'GCS Verbal Response' &
                          untidy_tibble$`Parameter Name` != 'Speech' &
                          untidy_tibble$`Parameter Name` != 'Diet Type' &
                          untidy_tibble$`Parameter Name` != 'CT Pupil size L' &
                          untidy_tibble$`Parameter Name` != 'CT Pupil size R' &
                          untidy_tibble$`Parameter Name` != 'Current Insulin Rate' &
                          untidy_tibble$`Parameter Name` != 'L Pupil Reaction' &
                          untidy_tibble$`Parameter Name` != 'R Pupil Reaction' &
                          untidy_tibble$`Parameter Name` != 'Inotropic/Vasoactive Agents in Progress' &
                          untidy_tibble$`Parameter Name` != 'LeftLegT°' &
                          untidy_tibble$`Parameter Name` != 'RightLegT°' &
                          #untidy_tibble$`Parameter Name` != 'PB Spontaneous  Type' &
                          #untidy_tibble$`Parameter Name` != 'PB Mandatory Mode Type' &
                          untidy_tibble$`Parameter Name` != 'O2 Administration mode' )
#untidy_tibble$`Parameter Name` != 'Summary' )

# Now manipulate the cardiac data independently. The initial pivot gives 
# list_cols that are then turned to characters before the original column
# is effectively removed.
cardiac_rhythm <- pivot_wider(cardiac_rhythm, 
                              names_from = `Parameter Name`, 
                              values_from = Value)

cardiac_rhythm$Cardiac_rhythm <- sapply(cardiac_rhythm$`Cardiac Rhythm`, toString)
cardiac_rhythm <- select(cardiac_rhythm, Time, Cardiac_rhythm)

### PIVOT THE UNTIDY TIBBLE INTO WIDE FORMAT and remove the untidy tibble
### replace INIT with the study ID number
tidy_tibble <- pivot_wider(untidy_tibble, 
                           id_cols = Time, 
                           names_from = `Parameter Name`, 
                           values_from = Value)

# Rejoin the cardiac data and arrange everything chronologically
tidy_tibble <- left_join(tidy_tibble, cardiac_rhythm, by = 'Time') %>%
  clean_names() %>% 
  arrange(time)

# add columns for age, height, weight (we will, later, come up with a method
# to designate absent BMI values, and impute them
tidy_tibble$age <- patient_age
tidy_tibble$weight <- patient_weight
tidy_tibble$height <- patient_height
tidy_tibble$gender <- patient_gender

# finally, add a unique identifier for each observation (using datetime objects)
# introduces issues in further scripts
tidy_tibble$observation <- 1:nrow(tidy_tibble)

# Tidy up ================================================================
# checks if there was a height entered for the patient before deleting old
# variables

Sys.sleep(1)

if (tidy_tibble$height[1] > 0) {
  rm(cardiac_rhythm,
     untidy_tibble,
     mv_files,
     mv_location,
     metavision_file_specific,
     i,
     patient_age,
     patient_height,
     patient_weight,
     admission_date,
     patient_gender)
} else {
  rm(cardiac_rhythm,
     untidy_tibble,
     mv_files,
     mv_location,
     metavision_file_specific,
     i,
     patient_age,
     patient_weight,
     admission_date,
     patient_gender)
}


# STEP 4
# this script splits the wide dataframe into two. One will remain in character
# format, but the other has all variables coerced to numeric. They are then 
# rejoined.

# ================= CREATE VECTOR OF VENTILATOR PARAMETERS ================

pb_char <- c('pb_mandatory_mode_type',
             'pb_mode_of_ventilation',
             'pb_spontaneous_type',
             'pb_vent_type',
             'pb_ventilation_mode',
             'set_i_e_ratio_pb',
             'trigger_type_setting')

avea_char <- c('avea_modes',
               'set_i_e_ratio_avea') # 'Set I:E Ratio(Avea)' in MVQ

drager_char <- c('drager_modes') # MVQ also has 'other', 'secondary' and 'third' modes

servo_char <- c('servo_i_modes') 

niv_char <- c('airvo_mode',
              'respironics_mode')

# ================= CREATE VECTOR OF STANDARD PARAMETERS ==================

constant_char <- c('time',
                   'gender',
                   'cardiac_rhythm',
                   'gcs_manual_entry',
                   'patient_positioning',
                   'patient_positioning_abg',
                   'ventilation_type', # 'ventilation_type' may be independent of equipment
                   'summary')

coerce_char <- c(constant_char, 
                 pb_char, 
                 avea_char, 
                 drager_char, 
                 servo_char, 
                 niv_char) 

# all variables that might need coercion
rm(constant_char,
   pb_char, 
   avea_char, 
   drager_char, 
   servo_char, 
   niv_char)

# ==================== COERCE TO NUMERIC ==================================

# create vector of variable names that will be coerced to character, and 
# initialise df
tidy_tibble_char <- select(.data = tidy_tibble, observation)
variables_present <- 'observation'

# select columns that will become character
for (y in 1:length(coerce_char)) {
  if (coerce_char[y] %in% colnames(tidy_tibble)) {
    tidy_tibble_char <-  left_join(tidy_tibble_char,
                                   tidy_tibble[, c('observation', coerce_char[y])],
                                   by = 'observation')
    variables_present <- c(variables_present, coerce_char[y])
  }
}

# remove these columns from the main df (but keep 'observation' for later joining)
tidy_tibble <- select(tidy_tibble, -variables_present, observation)

# use sapply to coerce remaining columns to numeric, then rejoin
tidy_tibble_nums <-  as_tibble(sapply(tidy_tibble[, 1:ncol(tidy_tibble)],
                                      as.numeric))

# reform tidy_tibble by joining the coerced and character dfs
tidy_tibble <-  full_join(tidy_tibble_char, tidy_tibble_nums, by = 'observation')

# tidy up
rm(coerce_char, variables_present, tidy_tibble_char, tidy_tibble_nums)

# ========================================================================

# STEP 5
# this script takes the wide tidy_tibble and divides it into smaller tibbles
# that are useful for analysis by the engineers/mathematicians.

# Create subtibbles
# 1. demographics
demo_tibble <- tidy_tibble %>% 
  select(age,
         gender,
         height,
         weight)

if (!is.na(tidy_tibble$height[1])) {
  demo_tibble$height <- tidy_tibble$height
}

demo_tibble <- demo_tibble[1,]

# 2. ABG
ABG_variables <- c('time',
                   'patient_positioning',
                   'patient_positioning_abg',
                   'ph_abg',
                   'pa_o2',
                   'pa_co2',
                   'bicarbonate_abg_a',
                   'lactate_abg',
                   'base_excess_vt',
                   'potassium_abg',
                   'sodium_abg',
                   'anion_gap_abg',
                   'glucose_abg',
                   'total_haemoglobin',
                   'fi_o2',
                   'tympanic_temperature',
                   'sa_o2_systemic',
                   'sp_o2')

abg_tibble <- tibble(observation = tidy_tibble$observation)
for (k in 1:length(ABG_variables)) {
  if (ABG_variables[k] %in% colnames(tidy_tibble)) {
    abg_tibble <- left_join(abg_tibble,
                            tidy_tibble[, c('observation', ABG_variables[k])],
                            by = 'observation')
  }
}

# 3. ventilator
vent_variables <- c('time',
                    'patient_positioning',
                    'fi_o2',
                    'set_fraction_inspired_oxygen_pb', # ================ PB
                    'end_tidal_co2_marquette',
                    'pb_mode_of_ventilation',
                    'set_respiratory_rate_pb',
                    'set_tv_pb',
                    'set_peep_pb',
                    'peep',
                    'set_i_e_ratio_pb',
                    'minute_volume_pb',
                    'measured_fi02_pb',
                    'measured_peep_pb',
                    'total_respiratory_rate_pb',
                    'expiratory_tidal_volume_pb',
                    'peak_inspiratory_pressure_measured_pb',
                    'plateau_airway_pressure_pb',
                    'mean_airway_pressure_pb',
                    'peak_inspiratory_pressure_measured_pb',
                    'dynamic_characteristics_pb',
                    'peak_flow_vmax_pb',
                    'pb_mandatory_mode_type',
                    'pb_spontaneous_type',
                    'pb_vent_type',
                    'pb_ventilation_mode',
                    'set_i_of_i_e_ratio',
                    'set_e_of_i_e_ratio',
                    'servo_i_modes',   # ======================== SERVO
                    'delivered_percent_o2',
                    'ipap',
                    'epap',
                    'mean_airway_pressure_s',
                    'minute_volume_expired_s',
                    'pause_airway_pressure_a_s',
                    'peak_inspiratory_pressure_measured_s',
                    'positive_end_expiratory_pressure',
                    'resp_rate',
                    's_expired_tidal_vol_breath',
                    'set_flow_trigger_s',
                    'set_peep_servo',
                    'set_pressure_control_level_above_peep_s',
                    'set_pressure_support_above_peep_s',
                    'set_pressure_trigger_s',
                    'set_rate_cmv_or_simv',
                    'set_tidal_volume_servo',   # ============ AVEA
                    'exhalation_time_a',
                    'expired_minute_volume_measured_a',
                    'expired_tidal_volume_measured_avea',
                    'mean_airway_pressure_measured_a',
                    'peak_pressure_measured_a',
                    'pressure_support_a',
                    'respiratory_rate_measured_a',
                    'set_bias_flow_advanced_a',
                    'set_fi_o2_a',
                    'set_flow_trigger_a',
                    'set_peak_flow_a',
                    'set_peep_a',
                    'set_pressure_trigger_advanced_a',
                    'set_respiratory_rate_a',
                    'set_tidal_volume_a',
                    'viasys_avea_modes',
                    'total_respioratory_rate',
                    'set_time_high', # ============ RESPIRONICS
                    'respironics_mode',
                    'bi_pap_vt',
                    'measured_cpap_respironics',
                    'set_breath_rate_respironics',
                    'set_ipap_rise_time_respironics',
                    'set_o2_percent_respironics',
                    'total_rr_respironics', # ============ DRAGER
                    'drager_modes',
                    'dynamic_characteristics_d',
                    'expiratory_tidal_volume_d',
                    'i_e_e_part',
                    'i_e_e_part_measured',
                    'i_e_i_part',
                    'i_e_i_part_measured',
                    'inspiratory_tidal_volume_d',
                    'inspiratory_time_d',
                    'leakage_rel_d',
                    'mean_airway_pressure_d',
                    'measured_peep_d',
                    'measured_respiratory_rate_total_d',
                    'minimum_airway_pressure_d',
                    'minute_volume_d',
                    'peak_inspiratory_pressure_measured_d',
                    'plateau_airway_pressure_d',
                    'resistance_d',
                    'set_fraction_inspired_oxygen_d',
                    'set_peep_d',
                    'set_respiratory_rate_d',
                    'set_tv_d',
                    'slope_d',
                    'inspiratory_pressure_limit_d',
                    'set_pressure_support_d',
                    'aprv_high_pressure',
                    'aprv_high_time',
                    'aprv_low_pressure',
                    'aprv_low_time',
                    'drager_secondary_modes',
                    'rapid_shallow_breathing_index_d',
                    'intrinsic_peep'
                    
)

vent_tibble <- tibble(observation = tidy_tibble$observation)
for (k in 1:length(vent_variables)) {
  if (vent_variables[k] %in% colnames(tidy_tibble)) {
    vent_tibble <- left_join(vent_tibble, 
                             tidy_tibble[, c('observation', vent_variables[k])],
                             by = 'observation')
  }
}

# 4. Cardiovascular
# The initial columns always exist, after that we must check if cardiac output
# columns exist, and then select them.
cardio_tibble <- tidy_tibble %>% 
  select(time,
         patient_positioning,
         cardiac_rhythm,
         heart_rate,
         arterial_pressure_systolic,
         arterial_pressure_diastolic,
         arterial_pressure_mean,
         non_invasive_arterial_pressure_systolic,
         non_invasive_arterial_pressure_diastolic,
         non_invasive_arterial_pressure_mean
  )

# check if advanced CO parameters are present and add them if they do
adv_co <- c('central_venous_pressure',
            'sv_o2_venous',
            'cardiac_output)_vigileo',
            'stroke_volume_vigileo',
            'stroke_volume_variation_vigileo',
            'systemic_vascular_resistance_vigileo')

for (z in 1:length(adv_co)) {
  if (adv_co[z] %in% colnames(tidy_tibble)) {
    left_join(cardio_tibble, 
              tidy_tibble[, c('time', adv_co[z])],
              by = 'time')
  }
}

# the final step is to create a list of these that will be passed to the
# write.xlsx function to give a file with different output sheets
all_data <- tidy_tibble

output_sheets <- list(demographics = demo_tibble,
                      ABG = abg_tibble,
                      Ventilator = vent_tibble,
                      Cardiovascular = cardio_tibble,
                      All_recorded = all_data)

# tidy up
rm(demo_tibble,
   abg_tibble,
   ABG_variables,
   vent_variables,
   vent_tibble,
   cardio_tibble,
   adv_co,
   k,
   y,
   z)

# ========================================================================

# STEP 6
# This script generates output from the wrangled df from the previous series of 
# scripts. This output consists of saving .xlsx of all data to disk

# Save a copy of the file
write_xlsx(x = output_sheets, format_headers = T, path = file_name)

# Save the tibble
save(tidy_tibble, file = sprintf('/Users/davidhannon/Documents/02. Medicine/Med_Programming/00. Patient DB/dfs/patient_%s.Rda', j))

# tidy
rm(all_data, 
   output_sheets, 
   # drive_location,
   # local_location,
   j,
   file_name)