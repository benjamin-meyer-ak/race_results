
# w/ autostamp
z <- readxl::read_xlsx("data/race_entrant_data_example_v2.xlsx",
                       sheet = "race_end_data") %>%
  remove_empty()


z1 <- readxl::read_xlsx("data/race_entrant_data_example_v2.xlsx",
                       sheet = "race_start_data") %>%
  remove_empty()


# w/o autostamp
z2 <- readxl::read_xlsx("data/race_entrant_data_example.xlsx",
                        sheet = "race_end_data") %>%
  remove_empty()

z3 <- readxl::read_xlsx("data/race_entrant_data_example.xlsx",
                              sheet = "race_start_data") %>%
  remove_empty()







# NOTES & tasks

# 1.) make corrections to time format upon import, now that auto time stamp is available


# 2.) make corrections described to jehangeer
# currently the app will not correctly calculate if a race lasts longer than 24 hours:In the "race_end_data" tab, there is a column labeled "Start Time." 
# This should instead be labeled "Finish Time", and the computation for time duration should be adjusted to calculate the difference between two 
# date-time, rather than just two times

# For example, start_time = "2023-06-09 12:02:00" and finish_time = "2023-06-10 12:43:01", time difference = "HH"MM:SS"







