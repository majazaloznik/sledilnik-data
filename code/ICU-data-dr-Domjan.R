# sledinlnik = gov data + NIJZ daily entry data
library(tidyr)
library(dplyr)
library(readr)
download.file(paste0("https://raw.githubusercontent.com/",
                     "sledilnik/data/master/csv/patients.csv"),
              "data/patients.csv")

df <- read_csv( "data/patients.csv")

df %>% 
  select(date, 
         state.in_hospital.in,
         state.in_hospital.out,
         state.in_hospital,
         state.deceased.hospital,
         state.icu.in,
         state.icu.out,
         state.icu,
         state.deceased.hospital.icu,
         state.critical) %>% 
  mutate(state.in_acute = state.in_hospital - state.icu,
         state.deceased.hospital.acute = state.deceased.hospital - state.deceased.hospital.icu) %>% 
  select(date, 
         state.in_hospital.in,
         state.in_hospital.out,
         state.in_hospital,
         state.deceased.hospital,
         state.in_acute,
         state.deceased.hospital.acute,
         state.icu.in,
         state.icu.out,
         state.icu,
         state.deceased.hospital.icu,
         state.critical) -> rtv

write_csv(rtv, "data/rtv-bolnice.csv")         


