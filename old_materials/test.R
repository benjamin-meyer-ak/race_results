z <- readxl::read_xlsx("data/race_entrant_data_example_v2.xlsx",
                       sheet = "race_end_data") %>%
  remove_empty()

z1 <- readxl::read_xlsx("data/race_entrant_data_example.xlsx",
                       sheet = "race_end_data") %>%
  remove_empty()


# NOTES & tasks
# 1.) make corrections described to jehangeer
# 2.) make corrections to time format upon import, now that auto time stamp is available
