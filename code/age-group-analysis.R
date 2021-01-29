library(readxl)
library(readr)
library(dplyr)
library(tidyr)
library(colorspace)
###############################################################################
# download and import age group dict
download.file(paste0("https://raw.githubusercontent.com/",
                     "sledilnik/data/master/csv/dict-age-groups.csv"),   
              "data/dict-age-groups.csv")

dict.age.groups <- read_csv("data/dict-age-groups.csv")

# get deaths 
download.file(paste0("https://raw.githubusercontent.com/",
                     "sledilnik/data/master/csv/stats.csv"),   
              "data/stats.csv")

stats <- read_csv("data/stats.csv")



# import testing datafrom excel file
testing.jan <- read_xlsx("data/imi-testi-22-januar-starost.xlsx")
testing.dec <- read_xlsx("data/imi-testi-21-november-2020-starost.xlsx")

# import hospital data from excel file
hos <- read_xlsx("data/UKC- COVID_poročilo po starostni strukturi in spolu.xlsx",
                 sheet = "df")

# set colour pallate
colz <- c("#FFEEBA", "#FFDA6B","#E9B825","#AEEFDB","#52C4A2","#33AB87","#189A73","#F4B2E0","#D559B0","#B01C83")
colz.light <- lighten(colz, 0.5)

###############################################################################
## clean up population age groups
###############################################################################


# get age group vector
dict.age.groups %>% 
  pull(id) -> age.groups

# get age group proportions
dict.age.groups %>% 
  filter(id != "population") %>% 
  mutate(pop.prop = population/sum(population) * 100) %>% 
  pull(pop.prop) -> age.prop

age.prop %>% 
  cbind(c(0, cumsum(age.prop[1:9]))) %>% 
  cbind(cumsum(age.prop)) %>% 
  t()  %>% 
  .[2:3,] %>% 
  as.data.frame()-> age.prop

###############################################################################
## clean up data december
###############################################################################

testing.dec %>% 
  mutate(Starost = as.numeric(Starost)) %>% 
  mutate(groups = cut(Starost, breaks = c(0, 4.5, 14.5, 24.5, 34.5, 44.5, 54.5, 64.5, 74.5, 84.5, 110),
                      labels = age.groups[1:10], include.lowest = TRUE))  %>% 
  group_by(groups) %>% 
  summarise(n = n()) %>% 
  mutate(prop.tested = n/sum(n) * 100) %>% 
  pull(prop.tested) -> tested.dec

testing.dec %>% 
  mutate(result = ifelse(`Eks. int.` == "NEGATIVNO", 0, 1)) %>% 
  mutate(Starost = as.numeric(Starost)) %>% 
  filter(result == 1) %>% 
  mutate(groups = cut(Starost, breaks = c(0, 4.5, 14.5, 24.5, 34.5, 44.5, 54.5, 64.5, 74.5, 84.5, 110),
                      labels = age.groups[1:10], include.lowest = TRUE))  %>% 
  group_by(groups) %>% 
  summarise(n = n()) %>% 
  mutate(prop.positive = n/sum(n) * 100) %>% 
  pull(prop.positive) -> positive.dec


###############################################################################
## clean up data  january
###############################################################################
testing.jan %>% 
  mutate(Starost = as.numeric(Starost)) %>% 
  mutate(groups = cut(Starost, breaks = c(0, 4.5, 14.5, 24.5, 34.5, 44.5, 54.5, 64.5, 74.5, 84.5, 110),
                      labels = age.groups[1:10], include.lowest = TRUE))  %>% 
  group_by(groups) %>% 
  summarise(n = n()) %>% 
  mutate(prop.tested = n/sum(n) * 100) %>% 
  pull(prop.tested) -> tested.jan

testing.jan %>% 
  mutate(result = ifelse(`Eks. int.` == "NEGATIVNO", 0, 1)) %>% 
  mutate(Starost = as.numeric(Starost)) %>% 
  filter(result == 1) %>% 
  mutate(groups = cut(Starost, breaks = c(0, 4.5, 14.5, 24.5, 34.5, 44.5, 54.5, 64.5, 74.5, 84.5, 100),
                      labels = age.groups[1:10], include.lowest = TRUE))  %>% 
  group_by(groups) %>% 
  summarise(n = n()) %>% 
  mutate(prop.positive = n/sum(n) * 100) %>% 
  pull(prop.positive) -> positive.jan

###############################################################################
## celan up hos and icu data
###############################################################################
hos %>% 
  mutate_if(is.numeric, function(x) ifelse(is.na(x/sum(x, na.rm= TRUE) * 100), 0, 
                                           x/sum(x, na.rm= TRUE) * 100))-> hos
  

###############################################################################
## clean up death data
###############################################################################
stats %>% 
  select(date | starts_with("deceased") & !contains("female") & !contains("male")) -> deaths

deaths %>% 
  summarise_if(is.numeric, function(x) sum(x, na.rm = TRUE)) %>% 
  select(contains("-") | contains("+")) %>% 
  gather(age, n)  %>% 
  mutate(n = n/sum(n)* 100) %>% 
  pull(n) -> dead

dead <- c(0,0,0, dead)
  
###############################################################################
## plot  testing only
###############################################################################

png(file = "figures/imi.test.dec.png", height = 480, width = 480)
par(mar = c(4,4,3,5), xpd = TRUE)
cbind(tested.dec, positive.dec) %>% 
  barplot(horiz = FALSE, col = viridis(10),
          names.arg = c("tested", "positive"),
          main = "Age structure, IMI, 21.12.2020  N = 3029 (712+) 23.51 %",
          width = c(3029, 712))
legend(4610, 80, legend = rev(age.groups[1:10]), fill = rev(viridis(10)), bty = "n")
dev.off()

png(file = "figures/imi.test.jan.png", height = 480, width = 480)
par(mar = c(4,4,3,5), xpd = TRUE)
cbind(tested.jan, positive.jan) %>% 
  barplot(horiz = FALSE, col = viridis(10),
          names.arg = c("tested", "positive"),
          main = "Age structure, IMI, 22.1.2021  N = 2099 (496+) 23.63 %", 
          width = c(2099, 496))
legend(3200, 80, legend = rev(age.groups[1:10]), fill = rev(viridis(10)), bty = "n")
dev.off()

###############################################################################
## plot  testing and hos
###############################################################################
myfunction<- function(x, i){ 
  rect(0.3, x[1], 7.7, x[2], col = colz.light[i], border = NA)
}

png(file = "figures/gif/age.str.01.png", height = 480, width = 800)
par(mar = c(4,4,3,5), xpd = TRUE)
cbind(NA, NA, NA, NA, NA) %>% 
  barplot(horiz = FALSE, col = colz,
          names.arg = c("", "", "", "", ""),
          # width = c(2099, 496, 269, 50),
          main = "Age structure", space = 0.5, ylim = c(0,100))

invisible(mapply(myfunction, age.prop, seq_along(age.prop)))
legend(7.8, 80, legend = rev(age.groups[1:10]), fill = rev(colz), bty = "n")

dev.off()
png(file = "figures/gif/age.str.02.png", height = 480, width = 800)
par(mar = c(4,4,3,5), xpd = TRUE)
cbind(NA, NA, NA, NA, NA) %>% 
  barplot(horiz = FALSE, col = colz,
          names.arg = c("", "", "", "", ""),
          # width = c(2099, 496, 269, 50),
          main = "Age structure", space = 0.5, ylim = c(0,100))

invisible(mapply(myfunction, age.prop, seq_along(age.prop)))
legend(7.8, 80, legend = rev(age.groups[1:10]), fill = rev(colz), bty = "n")

cbind(tested.jan, NA, NA, NA, NA) %>% 
  barplot(horiz = FALSE, col = colz,
          names.arg = c("tested", "", "", "", ""),
          # width = c(2099, 496, 269, 50),
          main = "Age structure", add = TRUE, space = 0.5)
dev.off()

png(file = "figures/gif/age.str.03.png", height = 480, width = 800)
par(mar = c(4,4,3,5), xpd = TRUE)
cbind(NA, NA, NA, NA, NA) %>% 
  barplot(horiz = FALSE, col = colz,
          names.arg = c("", "", "", "", ""),
          # width = c(2099, 496, 269, 50),
          main = "Age structure", space = 0.5, ylim = c(0,100))

invisible(mapply(myfunction, age.prop, seq_along(age.prop)))
legend(7.8, 80, legend = rev(age.groups[1:10]), fill = rev(colz), bty = "n")

cbind(tested.jan, positive.jan, NA, NA, NA) %>% 
  barplot(horiz = FALSE, col = colz,
          names.arg = c("tested", "positive", "", "", ""),
          # width = c(2099, 496, 269, 50),
          main = "Age structure", add = TRUE, space = 0.5)
dev.off()

png(file = "figures/gif/age.str.04.png", height = 480, width = 800)
par(mar = c(4,4,3,5), xpd = TRUE)
cbind(NA, NA, NA, NA, NA) %>% 
  barplot(horiz = FALSE, col = colz,
          names.arg = c("", "", "", "", ""),
          # width = c(2099, 496, 269, 50),
          main = "Age structure", space = 0.5, ylim = c(0,100))

invisible(mapply(myfunction, age.prop, seq_along(age.prop)))
legend(7.8, 80, legend = rev(age.groups[1:10]), fill = rev(colz), bty = "n")

cbind(tested.jan, positive.jan, hos$hos, NA, NA) %>% 
  barplot(horiz = FALSE, col = colz,
          names.arg = c("tested", "positive", "hospitalised", "", ""),
          # width = c(2099, 496, 269, 50),
          main = "Age structure", add = TRUE, space = 0.5)
dev.off()

png(file = "figures/gif/age.str.05.png", height = 480, width = 800)
par(mar = c(4,4,3,5), xpd = TRUE)
cbind(NA, NA, NA, NA, NA) %>% 
  barplot(horiz = FALSE, col = colz,
          names.arg = c("", "", "", "", ""),
          # width = c(2099, 496, 269, 50),
          main = "Age structure", space = 0.5, ylim = c(0,100))

invisible(mapply(myfunction, age.prop, seq_along(age.prop)))
legend(7.8, 80, legend = rev(age.groups[1:10]), fill = rev(colz), bty = "n")

cbind(tested.jan, positive.jan, hos$hos, hos$icu, NA) %>% 
  barplot(horiz = FALSE, col = colz,
          names.arg = c("tested", "positive", "hospitalised", "intensive care", ""),
          # width = c(2099, 496, 269, 50),
          main = "Age structure", add = TRUE, space = 0.5)
dev.off()

png(file = "figures/gif/age.str.06.png", height = 480, width = 800)
par(mar = c(4,4,3,5), xpd = TRUE)
cbind(NA, NA, NA, NA, NA) %>% 
  barplot(horiz = FALSE, col = colz,
          names.arg = c("", "", "", "", ""),
          # width = c(2099, 496, 269, 50),
          main = "Age structure", space = 0.5, ylim = c(0,100))

invisible(mapply(myfunction, age.prop, seq_along(age.prop)))
legend(7.8, 80, legend = rev(age.groups[1:10]), fill = rev(colz), bty = "n")

cbind(tested.jan, positive.jan, hos$hos, hos$icu, dead) %>% 
  barplot(horiz = FALSE, col = colz,
          names.arg = c("tested", "positive", "hospitalised", "intensive care", "death"),
          # width = c(2099, 496, 269, 50),
          main = "Age structure", add = TRUE, space = 0.5)
dev.off()

png(file = "figures/gif/age.str.07.png", height = 480, width = 800)
par(mar = c(4,4,3,5), xpd = TRUE)
cbind(NA, NA, NA, NA, NA) %>% 
  barplot(horiz = FALSE, col = colz,
          names.arg = c("", "", "", "", ""),
          # width = c(2099, 496, 269, 50),
          main = "Age structure", space = 0.5, ylim = c(0,100))

invisible(mapply(myfunction, age.prop, seq_along(age.prop)))
legend(7.8, 80, legend = rev(age.groups[1:10]), fill = rev(colz), bty = "n")

cbind(NA, positive.jan, NA, NA, dead) %>% 
  barplot(horiz = FALSE, col = colz,
          names.arg = c("tested", "positive", "hospitalised", "intensive care", "death"),
          # width = c(2099, 496, 269, 50),
          main = "Age structure", add = TRUE, space = 0.5)
dev.off()


###############################################################################
# data export
###############################################################################

testing.jan %>% 
  bind_rows(testing.dec) %>% 
  select(-`Dat. spr.`, -Zdravnik, -Št.) %>% 
  `colnames<-` (c("date", "age", "sending.org", "swab.type", "result")) %>% 
  mutate(result = ifelse(result == "NEGATIVNO", 0, 1)) %>% 
  mutate(age = as.numeric(age)) %>% 
  mutate(groups = cut(age, breaks = c(0, 4.5, 14.5, 24.5, 34.5, 44.5, 54.5, 64.5, 74.5, 84.5, 110),
                      labels = age.groups[1:10], include.lowest = TRUE))  %>% 
  select(-swab.type) -> export.testing

write_csv(export.testing, "data/imi-testing-age_groups.csv")
unique(export.testing$sending.org)
