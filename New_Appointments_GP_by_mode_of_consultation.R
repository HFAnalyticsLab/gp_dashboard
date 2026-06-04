library(tidyverse)


#directory <- "P:/opiemars/Documents/gp_dashboard/data"

# Function to load and process regional data files
load_regional_data <- function(directory, dataset_id) {
  setwd(paste0("P:/opiemars/Documents/gp_dashboard/data/appointment_data/", directory))
  list.files(pattern = "*.csv") %>%
    map_df(~read_csv(., col_types = cols(.default = "c"))) %>%
    mutate(dataset = dataset_id)
}

# Load all datasets
regional_GP_data_jun19 <- load_regional_data("regional_tabs_jun2019", 0)
regional_GP_data_dec21 <- load_regional_data("regional_tabs_dec2021", 1)
regional_GP_data_mar_24 <- load_regional_data("regional_tabs_mar2024", 2)
regional_GP_data_apr_24 <- load_regional_data("regional_tabs_apr2024", 3)
regional_GP_data_jul_24 <- load_regional_data("regional_tabs_jul2024", 4)
regional_GP_data_oct_24 <- load_regional_data("regional_tabs_oct2024", 5)
regional_GP_data_jan_25 <- load_regional_data("regional_tabs_jan2025", 6)
regional_GP_data_apr_25 <- load_regional_data("regional_tabs_apr2025", 7) # add new data here and add +1 to the dataset ID
regional_GP_data_may_25 <- load_regional_data("regional_tabs_may2025", 8)
regional_GP_data_aug_25 <- load_regional_data("regional_tabs_aug2025", 9)
regional_GP_data_dec_25 <- load_regional_data("regional_tabs_dec2025", 10)
regional_GP_data_mar_26 <- load_regional_data("regional_tabs_mar2026", 11)

# tidying two historic datasets
regional_GP_data_jun19 <- regional_GP_data_jun19 %>%
  rename(APPOINTMENT_MONTH = Appointment_Month) %>%
  mutate(APPOINTMENT_MONTH = str_to_upper(APPOINTMENT_MONTH),
         APPOINTMENT_MONTH = str_remove_all(APPOINTMENT_MONTH, '-'),
         HCP_TYPE = str_replace_all(HCP_TYPE, 'HCP Type Not Provided', 'Unknown'))

regional_GP_data_jun19$APPOINTMENT_MONTH <- sub("(^[[:alpha:]]{3})([[:digit:]]{2}$)", "\\1\\U20\\2", regional_GP_data_jun19$APPOINTMENT_MONTH, perl = TRUE)

regional_GP_data_dec21 <- regional_GP_data_dec21 %>%
  rename(APPOINTMENT_MONTH = Appointment_Month) %>%
  mutate(APPOINTMENT_MONTH = str_to_upper(APPOINTMENT_MONTH),
         APPOINTMENT_MONTH = str_remove_all(APPOINTMENT_MONTH, '-'),
         HCP_TYPE = str_replace_all(HCP_TYPE, 'HCP Type Not Provided', 'Unknown'))


# Combine all datasets into one
complete_GP_appointment_df <- bind_rows(
  regional_GP_data_jun19, 
  regional_GP_data_dec21, 
  regional_GP_data_mar_24, 
  regional_GP_data_apr_24, 
  regional_GP_data_jul_24,
  regional_GP_data_oct_24,
  regional_GP_data_jan_25,
  regional_GP_data_apr_25,
  regional_GP_data_may_25,
  regional_GP_data_aug_25,
  regional_GP_data_dec_25,
  regional_GP_data_mar_26 # add new data here
)

# Remove duplicates from data overlaps between new datasets ###################
# and create appointment date variable 

complete_JAN2018_JUL24_no_dups <- complete_GP_appointment_df %>%
  select(APPOINTMENT_MONTH, HCP_TYPE, APPT_MODE, COUNT_OF_APPOINTMENTS, dataset) %>%
  group_by(APPOINTMENT_MONTH, HCP_TYPE, APPT_MODE) %>%
  mutate(year = as.numeric(substr(APPOINTMENT_MONTH,4,7)), # creating year variable from string date 
         month = match(str_to_sentence(substr(APPOINTMENT_MONTH,0,3)),month.abb), # separating date string to create appointment month  
         date = make_date(year, month, 1), # creating date vvariable
         HCP_TYPE = str_replace_all(HCP_TYPE, ' ', '_')) %>% #checking that there's no spaces e.g. HCP type -> HCP_type
  ungroup() %>%
  group_by(date, APPT_MODE) %>%
  mutate(correct_dataset = max(dataset)) %>% # within date & APP_MODE grouping, select the highest dataset
  filter(dataset == correct_dataset) %>% # only keep rows with the highest (latest) dataset number
  select(-dataset, -correct_dataset) %>%
  ungroup()


#  Calculate total appointments by month #######################################

regional_GP_data_JAN18_JUL24_df <- complete_JAN2018_JUL24_no_dups %>% # change the date from JUL24 when new data added 
  select(HCP_TYPE, APPT_MODE, COUNT_OF_APPOINTMENTS, date) %>%
  group_by(date, HCP_TYPE, APPT_MODE) %>%
  summarise(appts = sum(as.numeric(COUNT_OF_APPOINTMENTS)), .groups='keep') %>% # summing all count of appointments 
  group_by(date, HCP_TYPE) %>%
  mutate(tot = sum(appts)) %>%  
  ungroup() %>%
  filter(HCP_TYPE == 'GP') %>%
  dplyr::select(date, HCP_TYPE, APPT_MODE, appts, tot, date) %>%
  arrange(date) 

# Load and process PCN data
setwd("P:/opiemars/Documents/gp_dashboard/data/pcn_data")

pcn_granular_dec2025 <- read_csv("pcn_granular_mar2026.csv") # load new pcn data here

pcn_dec2025_df <- pcn_granular_dec2025 %>%
  select(APPOINTMENT_MONTH, HCP_TYPE, APPT_MODE, COUNT_OF_APPOINTMENTS) %>%
  group_by(APPOINTMENT_MONTH, HCP_TYPE, APPT_MODE) %>%
  summarise(appts = sum(as.numeric(COUNT_OF_APPOINTMENTS)), .groups='keep') %>%
  group_by(APPOINTMENT_MONTH, HCP_TYPE) %>%
  mutate(
    tot = sum(appts),
    year = as.numeric(substr(APPOINTMENT_MONTH, 6, 9)),
    month = match(str_to_sentence(substr(APPOINTMENT_MONTH, 3, 5)), month.abb),
    date = make_date(year, month, 1),
    HCP_TYPE = str_replace_all(HCP_TYPE, ' ', '_')
  ) %>%
  ungroup() %>%
  filter(HCP_TYPE == 'GP') %>%
  select(date, HCP_TYPE, APPT_MODE, appts, tot) %>%
  arrange(date) %>%
  rename(appts_pcn = appts, tot_pcn = tot) 


# Append PCN appointments to the main dataset
final_results <- left_join(regional_GP_data_JAN18_JUL24_df, pcn_dec2025_df, by = c('date', 'HCP_TYPE', 'APPT_MODE')) %>% # amend df name here
    mutate_all(~replace(., is.na(.), 0)) %>%
    mutate(
    apps_all = appts + appts_pcn,
    tot_all = tot + tot_pcn,
    final_percentage = round((apps_all / tot_all*100), 2)
  )


# Save outputs - remember to add the date range in 
setwd("P:/opiemars/Documents/gp_dashboard/output")
write.csv(final_results, paste0('appointments_GP_by_mode_of_consultations_',format(today(),'%d%m%y'),'.csv'))

final_results_flourish <- final_results %>%
  select(date, APPT_MODE, final_percentage) %>%
  pivot_wider(names_from = APPT_MODE, values_from = final_percentage) %>%
  view()

write.csv(final_results_flourish, paste0('appointments_GP_by_mode_of_consultations_flourish_',format(today(),'%d%m%y'),'.csv'))

