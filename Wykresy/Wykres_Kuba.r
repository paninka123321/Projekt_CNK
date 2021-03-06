library(dplyr)
library(haven)
library(fmsb)
library(ggplot2)
library(stringi)
library(tidyverse)
library(lubridate)


Rosesa <-read_sav("~/OneDrive - Politechnika Warszawska/R_studio/ROSES master quest PL_November 9, 2021_07.50 (1).sav")
View(Roses)

#wywalamy fake odpowiedzi
Roses <- Rosesa %>% 
  mutate(czas_ankiety = as_datetime(EndDate) - as_datetime(StartDate)) %>% 
  filter(czas_ankiety >= 420)

szkolne_geo <- Roses %>% 
  select(StartDate,IPAddress,Q5_2,Q5_3,Q5_4, Q5_5,Q5_15, Q21_2, Q7_10, Q7_11, Q9_2, Q9_3, Q9_4, Q9_16, Q9_19, Q9_20)


geo <- szkolne_geo %>% 
  mutate(geo_mean_ciek= apply(szkolne_geo[,-c(1,2)],1,function(x){mean(x,na.rm = TRUE)})) %>% 
  filter(geo_mean_ciek != 'NaN', geo_mean_ciek >= 3.5)

szkolne_fiz <- Roses %>% 
  select(StartDate, IPAddress,Q5_10,Q5_11,Q21_5,Q21_8,Q21_9,Q21_18,Q21_17,Q7_8,Q9_1,Q5_12,Q5_13,Q21_16,Q7_2,Q7_7,Q9_22)

fiz <- szkolne_fiz %>% 
  mutate(fiz_mean_ciek= apply(szkolne_fiz[,-c(1,2)],1,function(x){mean(x,na.rm = TRUE)})) %>% 
  filter(fiz_mean_ciek != 'NaN', fiz_mean_ciek >= 3.5)

szkolne_chem <- Roses %>% 
  select(StartDate,EndDate, IPAddress, Q5_1, Q5_8, Q21_5, Q21_6, Q7_9, Q9_17, Q9_21)

chem <- szkolne_chem %>% 
  mutate(chem_mean_ciek= apply(szkolne_chem[,-c(1,2,3)],1,function(x){mean(x,na.rm = TRUE)})) %>% 
  filter(chem_mean_ciek != 'NaN', chem_mean_ciek >= 3.5)

szkolne_bio <-Roses %>% 
  select(StartDate,IPAddress, Q5_6, Q5_7, Q5_9, Q21_1, Q21_3, Q21_7, Q21_10, Q21_11, Q21_12, Q21_13, Q21_14,
         Q21_15, Q21_17, Q7_9, Q9_5, Q9_6, Q9_7, Q9_8, Q9_9, Q9_11, Q9_12, Q9_13, Q9_14, Q9_18, Q9_24)

bio <- szkolne_bio %>% 
  mutate(bio_mean_ciek= apply(szkolne_bio[,-c(1,2)],1,function(x){mean(x,na.rm = TRUE)})) %>% 
  filter(bio_mean_ciek != 'NaN', bio_mean_ciek >= 3.5)


## uczę się gdy:
fiz_int_szkola <- Roses %>% 
  select(StartDate, IPAddress, Q8_1:Q15_14) %>% 
  right_join(fiz, c('StartDate', 'IPAddress'))
interere_fiz <- fiz_int_szkola %>% 
  mutate(muzeum_sciencecentrum_planetaryx = apply(fiz_int_szkola[,c('Q15_2', 'Q15_3', 'Q15_4')],1,function(x){mean(x,na.rm = TRUE)})) %>%
  mutate(zoo_garden_festivalx= apply(fiz_int_szkola[,c('Q15_1', 'Q15_5', 'Q15_6')],1,function(x){mean(x,na.rm=TRUE)})) %>% 
  mutate(internet_TVx = apply(
    fiz_int_szkola[,c('Q15_8', 'Q15_9', 'Q15_14', 'Q15_12', 'Q15_13')],1,function(x){mean(x, na.rm=TRUE)})) %>% 
  filter(muzeum_sciencecentrum_planetaryx != 'NaN', zoo_garden_festivalx != 'NaN',
         internet_TVx != 'NaN') %>% 
  summarize(educational_centres = mean(muzeum_sciencecentrum_planetaryx, na.rm=TRUE),
            outdoor_learning = mean(zoo_garden_festivalx, na.rm=TRUE),
            media = mean(internet_TVx, na.rm=TRUE)) %>% 
  mutate(nazwa = 'Physics', .before = 1)


bio_int_szkola <- Roses %>% 
  select(StartDate, IPAddress, Q8_1:Q15_14) %>% 
  right_join(bio, c('StartDate', 'IPAddress'))
interere_bio <- bio_int_szkola %>% 
  mutate(muzeum_sciencecentrum_planetaryx = apply(bio_int_szkola[,c('Q15_2', 'Q15_3', 'Q15_4')],1,function(x){mean(x,na.rm = TRUE)})) %>%
  mutate(zoo_garden_festivalx= apply(bio_int_szkola[,c('Q15_1', 'Q15_5', 'Q15_6')],1,function(x){mean(x,na.rm=TRUE)})) %>% 
  mutate(internet_TVx = apply(
    bio_int_szkola[,c('Q15_8', 'Q15_9', 'Q15_14', 'Q15_12', 'Q15_13')],1,function(x){mean(x, na.rm=TRUE)})) %>% 
  filter(muzeum_sciencecentrum_planetaryx != 'NaN', zoo_garden_festivalx != 'NaN',
         internet_TVx != 'NaN') %>% 
  summarize(educational_centres = mean(muzeum_sciencecentrum_planetaryx, na.rm=TRUE),
            outdoor_learning = mean(zoo_garden_festivalx, na.rm=TRUE),
            media = mean(internet_TVx, na.rm=TRUE)) %>% 
  mutate(nazwa = 'Biology', .before = 1)


chem_int_szkola <- Roses %>% 
  select(StartDate, IPAddress, Q8_1:Q15_14) %>% 
  right_join(chem, c('StartDate', 'IPAddress'))
interere_chem <- chem_int_szkola %>% 
  mutate(muzeum_sciencecentrum_planetaryx = apply(chem_int_szkola[,c('Q15_2', 'Q15_3', 'Q15_4')],1,function(x){mean(x,na.rm = TRUE)})) %>%
  mutate(zoo_garden_festivalx= apply(chem_int_szkola[,c('Q15_1', 'Q15_5', 'Q15_6')],1,function(x){mean(x,na.rm=TRUE)})) %>% 
  mutate(internet_TVx = apply(
    chem_int_szkola[,c('Q15_8', 'Q15_9', 'Q15_14', 'Q15_12', 'Q15_13')],1,function(x){mean(x, na.rm=TRUE)})) %>% 
  filter(muzeum_sciencecentrum_planetaryx != 'NaN', zoo_garden_festivalx != 'NaN',
         internet_TVx != 'NaN') %>% 
  summarize(educational_centres = mean(muzeum_sciencecentrum_planetaryx, na.rm=TRUE),
            outdoor_learning = mean(zoo_garden_festivalx, na.rm=TRUE),
            media = mean(internet_TVx, na.rm=TRUE)) %>% 
  mutate(nazwa = 'Chemistry', .before = 1)

# media = intenet + tv
# educational centres = muzeum_scienceńcentrum_planetary
# outdoor_learning = zoo_garden_festival


geo_int_szkola <- Roses %>% 
  select(StartDate, IPAddress, Q8_1:Q15_14) %>% 
  right_join(geo, c('StartDate', 'IPAddress'))
interere_geo <- geo_int_szkola %>% 
  mutate(muzeum_sciencecentrum_planetaryx = apply(geo_int_szkola[,c('Q15_2', 'Q15_3', 'Q15_4')],1,function(x){mean(x,na.rm = TRUE)})) %>%
  mutate(zoo_garden_festivalx= apply(geo_int_szkola[,c('Q15_1', 'Q15_5', 'Q15_6')],1,function(x){mean(x,na.rm=TRUE)})) %>% 
  mutate(internet_TVx = apply(
    geo_int_szkola[,c('Q15_8', 'Q15_9', 'Q15_14', 'Q15_12', 'Q15_13')],1,function(x){mean(x, na.rm=TRUE)})) %>% 
  filter(muzeum_sciencecentrum_planetaryx != 'NaN', zoo_garden_festivalx != 'NaN',
         internet_TVx != 'NaN') %>% 
  summarize(educational_centres = mean(muzeum_sciencecentrum_planetaryx, na.rm=TRUE),
            outdoor_learning = mean(zoo_garden_festivalx, na.rm=TRUE),
            media = mean(internet_TVx, na.rm=TRUE)) %>% 
  mutate(nazwa = 'Geography', .before = 1)

interere <- Roses %>% 
  select(StartDate, IPAddress, Q8_1:Q15_14)
interere2 <- interere %>% 
  mutate(muzeum_sciencecentrum_planetaryx = apply(interere[,c('Q15_2', 'Q15_3', 'Q15_4')],1,function(x){mean(x,na.rm = TRUE)})) %>%
  mutate(zoo_garden_festivalx= apply(interere[,c('Q15_1', 'Q15_5', 'Q15_6')],1,function(x){mean(x,na.rm=TRUE)})) %>% 
  mutate(internet_TVx = apply(
    interere[,c('Q15_8', 'Q15_9', 'Q15_14', 'Q15_12', 'Q15_13')],1,function(x){mean(x, na.rm=TRUE)})) %>% 
  filter(muzeum_sciencecentrum_planetaryx != 'NaN', zoo_garden_festivalx != 'NaN',
         internet_TVx != 'NaN') %>% 
  summarize(educational_centres = mean(muzeum_sciencecentrum_planetaryx, na.rm=TRUE),
            outdoor_learning = mean(zoo_garden_festivalx, na.rm=TRUE),
            media = mean(internet_TVx, na.rm=TRUE)) %>% 
  mutate(nazwa = 'ALL', .before = 1)

#tworze tabele wąską aby stworzyć wykres kolumnowy pogrupowany


zainteresowania<- bind_rows(interere_bio, interere_fiz, interere_chem, interere_geo, interere2, id=NULL)

zainteresowania_waskie<- zainteresowania %>% 
  gather(przedmiot, wartosci, -nazwa)

zainteresowania_waskie[ ,2] <- stri_replace_all_regex(zainteresowania_waskie$przedmiot,"_", "\n")

zainteresowania_waskie[ ,2] <- stri_replace_all_regex(zainteresowania_waskie$przedmiot,"ń", " ")


ggplot(zainteresowania_waskie, aes(x=przedmiot, y=wartosci, fill = nazwa))+
  geom_col(aes(fill = nazwa), position = position_dodge()) +
  labs(title = "What is your preferred way of learning?",y = "mean", fill = "subject")+
  xlab("The way of learning")+
  theme_bw()


