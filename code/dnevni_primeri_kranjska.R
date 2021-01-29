download.file(paste0("https://raw.githubusercontent.com/",
                     "sledilnik/data/master/csv/municipality-confirmed.csv"),
              "data/municipalities.csv")
library(readr)
library(dplyr)
library(zoo)
# import data
sledilnik <- read_csv("data/municipalities.csv")

sledilnik %>% 
  select(date, region.kr.kranjska_gora) -> df
  mutate(dif = c(0, diff(region.kr.)),
         d7 = rollmean(dif, 7, na.pad = TRUE)) -> df
         
plot(df$date, df$d7, type = "l")
plot(df$date, df$dif, type = "b")

write_csv(df, "data/kranjska.csv")
