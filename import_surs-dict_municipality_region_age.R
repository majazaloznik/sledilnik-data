###############################################################################
#'
#' Script to import data from SURS on the population by 
#' municipality
#' region
#' age group
#' 
#' from https://pxweb.stat.si/SiStatData/pxweb/sl/Data/-/05C4002S.px
#' (link to saved query in code)
#' Export to
#' dict-age-groups.csv
#' dict-municipalities.csv
#' dict-region.csv
#' 
#' with timetamps
#' 
###############################################################################

library(readr)
library(tidyr)
library(dplyr)

# download and import population data
download.file("https://pxweb.stat.si:443/SiStatData/sq/2210",
              "data/raw.csv")

raw <- read_csv("data/raw.csv",
                locale = locale(encoding = "Windows-1250"),
                skip = 1)

# download and import municipality dict
download.file(paste0("https://raw.githubusercontent.com/",
                     "sledilnik/data/master/csv/dict-municipality.csv"),   
              "data/dict-municipality.csv")

dict.municipality <- read_csv("data/dict-municipality.csv", 
                              locale = locale(decimal_mark = ","))

# download and import region dict
download.file(paste0("https://raw.githubusercontent.com/",
                     "sledilnik/data/master/csv/dict-region.csv"),   
              "data/dict-region.csv")

dict.region <- read_csv("data/dict-region.csv")

# download and import age group dict
download.file(paste0("https://raw.githubusercontent.com/",
                     "sledilnik/data/master/csv/dict-age-groups.csv"),   
              "data/dict-age-groups.csv")

dict.age.groups <- read_csv("data/dict-age-groups.csv")


# get municipality to region mapping 
dict.municipality %>% 
  select(name, region) -> dict
 
# get age groups
dict.age.groups %>% 
  pull(id) -> age.groups

# population by municipality

raw %>% 
  filter(SPOL == "Spol - SKUPAJ") %>% 
  mutate(population = rowSums(.[3:ncol(raw)])) %>% 
  select(OBČINE, population) %>% 
  rename("name" = "OBČINE") -> pop.municipality


# populaiton  by region

pop.municipality %>% 
  left_join(dict) %>% 
  group_by(region) %>% 
  rename("id" = "region") %>% 
  summarise(population = sum(population)) -> pop.region

# population by sex and age groups 

raw %>% 
  filter(OBČINE == "SLOVENIJA") %>% 
  group_by(SPOL) %>% 
  # summarise_if(is.numeric, sum) %>% 
  gather(age, population, `2020H2 0 let`: `2020H2 85 + let`) %>% 
  select(-OBČINE) %>% 
  spread(SPOL, population) %>% 
  `colnames<-`(c("age", "male", "population", "female")) %>% 
  separate(age, c("x", "age", "xx")) %>% 
  select(age, male, female, population) %>% 
  mutate(age = as.numeric(age)) %>% 
  arrange(age) %>% 
  mutate(groups = cut(age, breaks = c(0, 4.5, 14.5, 24.5, 34.5, 44.5, 54.5, 64.5, 74.5, 84.5, 100),
    labels = age.groups[1:10], include.lowest = TRUE)) %>% 
  rename("id" = "groups") %>% 
  select(id, male, female, population) %>% 
  group_by(id) %>% 
  summarise_if(is.numeric, sum) -> pop.age


# merge with original dicitonaries

# municipality
colnames(dict.municipality) -> col.order
dict.municipality %>% 
  select(-population) %>% 
  left_join(pop.municipality, by = c("name" = "name")) %>% 
  select(col.order) %>% 
  mutate_at(vars(starts_with("geo_")), function(x) 
    format(x, format = "f", decimal.mark = ",", digits = 10)) -> dict.municipality


# region
colnames(dict.region) -> col.order
dict.region %>% 
 select(-population) %>% 
  left_join(pop.region, by = c("id" = "id"))  %>% 
  select(col.order) -> dict.region

# age.groups
pop.age %>% 
  bind_rows(pop.age %>% 
              summarise_if(is.numeric, sum) %>% 
              mutate(id = "population") ) -> dict.age.groups


# export 
write_csv(dict.municipality, "../../data/csv/dict-municipality.csv", na = "")
write.table(round(as.numeric(Sys.time()), 0), "../../data/csv/dict-municipality.csv.timestamp",
            row.names = FALSE, col.names = FALSE)

write_csv(dict.region, "../../data/csv/dict-region.csv", na = "")
write.table(round(as.numeric(Sys.time()), 0), "../../data/csv/dict-region.csv.timestamp",
            row.names = FALSE, col.names = FALSE)

write_csv(dict.age.groups, "../../data/csv/dict-age-groups.csv", na = "")
write.table(round(as.numeric(Sys.time()), 0), "../../data/csv/dict-age-groups.csv.timestamp",
            row.names = FALSE, col.names = FALSE)


