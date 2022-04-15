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
View(Roses)

#wykres zainteresowanie a trudnosć

Trudnosc_vs_Zainteresowanie <- Roses %>% 
  select(IPAddress, Q10_1, Q10_2, Progress, Q2) %>% 
  filter(Progress >= 50)
Trudnosc_vs_Zainteresowanie <- na.omit(Trudnosc_vs_Zainteresowanie)
View(Trudnosc_vs_Zainteresowanie)


Trudne <- Trudnosc_vs_Zainteresowanie %>% 
  group_by(Q2, Q10_1) %>% 
  summarise(Difficulty = n()) %>% 
  rename(How_much = Q10_1, Gender = Q2)

Interesujace <- Trudnosc_vs_Zainteresowanie %>% 
  group_by(Q2, Q10_2) %>% 
  summarise(Interest = n()) %>% 
  rename(How_much = Q10_2, Gender = Q2)


Trudne_vs_Interesujace <- Trudne %>% 
  inner_join(Interesujace, by = c("Q2", "Q10_1" = "Q10_2")) %>% 
  rename(How_much = Q10_1, Gender = Q2)
Trudne_vs_Interesujace <- Trudne_vs_Interesujace








Wykres <- Wykres %>% 
  gather(Zgadzam, Number_of_people, - c(Gender, How_much))

Wykres %>% ggplot(aes(x = Zgadzam, y = Number_of_people), fill = Gender)  
  geom_col()


  





#zliczenie chlopcow i dziewczyn






a <- data_frame(table(Trudnosc_vs_Zainteresowanie[,c(Q2, Q10_1 = Q10_2)]))
?inner_join



Trudnosc_vs_Zainteresowanie %>% 
  select(Q10_1, Q2) %>% 
  










#pytania zwi?zane z chemi?. Licz? ?rednie zainteresowanie os?b tymi zagadnieniami
Chemia<- Roses %>% 
  select(StartDate,IPAddress,Q5_1,Q5_8,Q21_6,Q9_21)

Chemia<- Roses %>% 
  select(StartDate,IPAddress,Q5_1,Q5_8,Q21_6,Q9_21) %>% 
  mutate(Cikaw_chem = apply(Chemia[,-c(1,2)],1,function(x){mean(x,na.rm = TRUE)}))


#pytania zwi?zane z geografi?
Geografia<- Roses %>% 
  select(StartDate,IPAddress,Q5_2,Q5_3,Q5_4,Q5_14)

Geografia<- Roses %>% 
  select(StartDate,IPAddress,Q5_2,Q5_3,Q5_4,Q5_14) %>% 
  mutate(Cikaw_geo= apply(Geografia[,-c(1,2)],1,function(x){mean(x,na.rm = TRUE)}))


Cikawosc_srednia<-left_join(Geografia,Chemia,c('IPAddress','StartDate'))

# biologia
Biologia<-Roses %>% 
  select(StartDate,IPAddress,Q5_5,Q5_6,Q5_7,Q5_9,Q21_1,Q21_3,Q21_4,Q7_2,Q21_8,Q9_11,Q9_12,Q9_13,Q9_14,Q9_19,Q9_20,Q9_24)

Biologia<- Roses %>% 
  select(StartDate,IPAddress,Q5_5,Q5_6,Q5_7,Q5_9,Q21_1,Q21_3,Q21_4,Q7_2,Q21_8,Q9_11,Q9_12,Q9_13,Q9_14,Q9_19,Q9_20,Q9_24) %>% 
  mutate(Cikaw_bio= apply(Biologia[,-c(1,2)],1,function(x){mean(x,na.rm = TRUE)}))

Cikawosc_srednia<-left_join(Cikawosc_srednia,Biologia,c('IPAddress','StartDate'))

#fizyka
Fizyka<-Roses %>% 
  select(StartDate,IPAddress,Q5_10,Q5_11,Q21_5,Q21_8,Q21_18,Q7_1,Q7_7,Q7_8,Q9_1,Q5_4,Q21_2,Q9_1)

Fizyka<- Roses %>% 
  select(StartDate,IPAddress,Q5_10,Q5_11,Q21_5,Q21_8,Q21_18,Q7_1,Q7_7,Q7_8,Q9_1,Q5_4,Q21_2,Q9_1) %>% 
  mutate(Cikaw_fiz= apply(Fizyka[,-c(1,2)],1,function(x){mean(x,na.rm = TRUE)}))

Cikawosc_srednia<-left_join(Cikawosc_srednia,Fizyka,c('IPAddress','StartDate'))


#astronomia
Astronomia<-Roses %>% 
  select(StartDate,IPAddress,Q5_12,Q5_13,Q21_16,Q7_2,Q7_7,Q9_22)

Astronomia<- Roses %>% 
  select(StartDate,IPAddress,Q5_12,Q5_13,Q21_16,Q7_2,Q7_7,Q9_22) %>% 
  mutate(Cikaw_astro= apply(Astronomia[,-c(1,2)],1,function(x){mean(x,na.rm = TRUE)}))

Cikawosc_srednia<-left_join(Cikawosc_srednia,Astronomia,c('IPAddress','StartDate'))


#medycyna
Medycyna_Cialo_czlowieka<- Roses %>% 
  select(StartDate,IPAddress,Q21_7,Q21_9,Q21_10,Q21_11,Q21_12,Q21_13,Q21_14,Q21_15,Q21_17,Q9_18)

Medycyna_Cialo_czlowieka<- Roses %>% 
  select(StartDate,IPAddress,Q21_7,Q21_9,Q21_10,Q21_11,Q21_12,Q21_13,Q21_14,Q21_15,Q21_17,Q9_18) %>% 
  mutate(Cikaw_med= apply(Medycyna_Cialo_czlowieka[,-c(1,2)],1,function(x){mean(x,na.rm = TRUE)}))

Cikawosc_srednia<-left_join(Cikawosc_srednia,Medycyna_Cialo_czlowieka,c('IPAddress','StartDate'))




Ekologia<-Roses %>% 
  select(StartDate,IPAddress,Q7_11,Q9_2,Q9_3,Q9_4,Q9_14,Q9_15,Q9_16) 

Ekologia<- Roses %>% 
  select(StartDate,IPAddress,Q7_11,Q9_2,Q9_3,Q9_4,Q9_14,Q9_15,Q9_16) %>% 
  mutate(Cikaw_eko= apply(Ekologia[,-c(1,2)],1,function(x){mean(x,na.rm = TRUE)}))


Cikawosc_srednia<-left_join(Cikawosc_srednia,Ekologia,c('IPAddress','StartDate'))


#tworzy radarplota

CI<-Cikawosc_srednia %>% 
  select(-c('StartDate','IPAddress')) %>% 
  summarise(across(1:9,mean)) 

gg<-Cikawosc_srednia %>% 
  select(c('Cikaw_geo', 'Cikaw_fiz', 'Cikaw_chem', 'Cikaw_bio','Cikaw_med', 'Cikaw_eko', 'Cikaw_astro'))%>% 
  filter(Cikaw_geo != 'NaN', Cikaw_fiz != 'NaN', Cikaw_chem != 'NaN', Cikaw_bio != 'NaN', Cikaw_med != 'NaN', Cikaw_eko != 'NaN', Cikaw_astro != 'NaN') %>% 
  summarise(across(1:7,mean))

gg <- rbind(rep(4,length(gg)) , rep(1,length(gg)) , gg)
radarchart(gg)













##niestworzone rzeczy (duchy itp)
# Niewiadome<-Roses %>% 
#   select(StartDate,IPAddress,Q7_3,Q7_4,Q7_5,Q7_6,Q7_9,Q9_31)
# 
# Niewiadome<-Niewiadome %>% 
#   mutate(Cikaw_niew = apply(Niewiadome[,3:length(Niewiadome)],1,mean)) %>% 
#   select(StartDate,IPAddress,Cikaw_niew)
# 
# Cikawosc_srednia<-left_join(Cikawosc_srednia,Niewiadome,c('IPAddress','StartDate'))
# 
# Nauka<-Roses %>% 
#   select(StartDate,IPAddress,Q9_25,Q9_26,Q9_27,Q9_28,Q9_29,Q9_30,Q9_31)
# Nauka<-Nauka %>% 
#   mutate(Cikaw_nauka = apply(Nauka[,3:length(Nauka)],1,mean)) %>% 
#   select(StartDate,IPAddress,Cikaw_nauka)
# Cikawosc_srednia<-left_join(Cikawosc_srednia,Nauka,c('IPAddress','StartDate')) %>% 
#   na.omit()





#wyb?r pyta? "co chcia?by? wiedzie? wi?cej?", kt?re odnosz? si? do ?cis?ych przedmiot?W szkolnych
# nast?pnie policzone zosta?o ?rednie zainteresowanie ka?dej osoby danym przedmiotem i wybranie
# os?b, kt?rych zainteresowanie przekracza 3.5

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




#tutaj wybieramy zagadnienie np. zainteresowanie przedmiotami szkolnymi i sprawdzamy 
#jak bardzo osoby zainteresowane poszczeg?lnym przedmiotem ?cis?ym interesuj? si? przedmiotami szkolnymi
# nowe pomys?y na ciekawe zapytania mo?na dodawa? do mutate, trzeba to zrobi? w ramkach
# interere_fiz,interere_chem, interere_biol, interere_geo oraz interere2


fiz_int_szkola <- Roses %>% 
  select(StartDate, IPAddress, Q8_1:Q15_14) %>% 
  mutate(Q8_1bis = 4-Q8_1) %>%
  right_join(fiz, c('StartDate', 'IPAddress'))
interere_fiz <- fiz_int_szkola %>% 
  mutate(srodowisko= apply(fiz_int_szkola[,c('Q8_1bis', 'Q8_2', 'Q8_3', 'Q8_4', 'Q8_5', 'Q8_6', 'Q8_9', 'Q8_10')],1,function(x){mean(x,na.rm = TRUE)})) %>%
  mutate(Czy_lubi_przedmioty_scisle= apply(fiz_int_szkola[,c('Q10_2', 'Q10_3', 'Q10_4', 'Q10_7')],1,function(x){mean(x,na.rm=TRUE)})) %>% 
  mutate(waznosc_technologii = apply(
    fiz_int_szkola[,c('Q11_1', 'Q11_2', 'Q11_3', 'Q11_5', 'Q11_6', 'Q11_7',
                      'Q11_8', 'Q11_10','Q11_13')],1,function(x){mean(x, na.rm=TRUE)})) %>% 
  mutate(nauka_z_mediow = apply(fiz_int_szkola[,c('Q12_1', 'Q14_1', 'Q14_2')],1,function(x){mean(x, na.rm=TRUE)})) %>% 
  filter(srodowisko != 'NaN', Czy_lubi_przedmioty_scisle != 'NaN',
         waznosc_technologii != 'NaN', nauka_z_mediow != 'NaN') %>% 
  summarize(srednie_srodowisko = mean(srodowisko, na.rm=TRUE),
            Czy_lubi_przedmioty_scisle_mean = mean(Czy_lubi_przedmioty_scisle, na.rm=TRUE),
            waznosc_technologii_mean = mean(waznosc_technologii, na.rm=TRUE),
            nauka_z_mediow_mean = mean(nauka_z_mediow, na.rm=TRUE)) %>% 
  mutate(nazwa = 'Fizyka', .before = 1)

?mutate





bio_int_szkola <- Roses %>% 
  select(StartDate, IPAddress, Q8_1:Q15_14) %>% 
  mutate(Q8_1bis = 4-Q8_1) %>%
  right_join(bio, c('StartDate', 'IPAddress'))
interere_bio <- bio_int_szkola %>% 
  mutate(srodowisko= apply(bio_int_szkola[,c('Q8_1bis', 'Q8_2', 'Q8_3', 'Q8_4', 'Q8_5', 'Q8_6', 'Q8_9', 'Q8_10')],1,function(x){mean(x,na.rm = TRUE)})) %>%
  mutate(Czy_lubi_przedmioty_scisle= apply(bio_int_szkola[,c('Q10_2', 'Q10_3', 'Q10_4', 'Q10_7')],1,function(x){mean(x,na.rm=TRUE)})) %>% 
  mutate(waznosc_technologii = apply(
    bio_int_szkola[,c('Q11_1', 'Q11_2', 'Q11_3', 'Q11_5', 'Q11_6', 'Q11_7',
                      'Q11_8', 'Q11_10','Q11_13')],1,function(x){mean(x, na.rm=TRUE)})) %>% 
  mutate(nauka_z_mediow = apply(bio_int_szkola[,c('Q12_1', 'Q14_1', 'Q14_2')],1,function(x){mean(x, na.rm=TRUE)})) %>% 
  filter(srodowisko != 'NaN', Czy_lubi_przedmioty_scisle != 'NaN',
         waznosc_technologii != 'NaN', nauka_z_mediow != 'NaN') %>% 
  summarize(srednie_srodowisko = mean(srodowisko, na.rm=TRUE),
            Czy_lubi_przedmioty_scisle_mean = mean(Czy_lubi_przedmioty_scisle, na.rm=TRUE),
            waznosc_technologii_mean = mean(waznosc_technologii, na.rm=TRUE),
            nauka_z_mediow_mean = mean(nauka_z_mediow, na.rm=TRUE)) %>% 
  mutate(nazwa = 'Biologia', .before = 1)

chem_int_szkola <- Roses %>% 
  select(StartDate, IPAddress, Q8_1:Q15_14) %>% 
  mutate(Q8_1bis = 4-Q8_1) %>%
  right_join(chem, c('StartDate', 'IPAddress'))
interere_chem <- chem_int_szkola %>% 
  mutate(srodowisko= apply(chem_int_szkola[,c('Q8_1bis', 'Q8_2', 'Q8_3', 'Q8_4', 'Q8_5', 'Q8_6', 'Q8_9', 'Q8_10')],1,function(x){mean(x,na.rm = TRUE)})) %>%
  mutate(Czy_lubi_przedmioty_scisle= apply(chem_int_szkola[,c('Q10_2', 'Q10_3', 'Q10_4', 'Q10_7')],1,function(x){mean(x,na.rm=TRUE)})) %>% 
  mutate(waznosc_technologii = apply(
    chem_int_szkola[,c('Q11_1', 'Q11_2', 'Q11_3', 'Q11_5', 'Q11_6', 'Q11_7',
                      'Q11_8', 'Q11_10','Q11_13')],1,function(x){mean(x, na.rm=TRUE)})) %>% 
  mutate(nauka_z_mediow = apply(chem_int_szkola[,c('Q12_1', 'Q14_1', 'Q14_2')],1,function(x){mean(x, na.rm=TRUE)})) %>% 
  filter(srodowisko != 'NaN', Czy_lubi_przedmioty_scisle != 'NaN',
         waznosc_technologii != 'NaN', nauka_z_mediow != 'NaN') %>% 
  summarize(srednie_srodowisko = mean(srodowisko, na.rm=TRUE),
            Czy_lubi_przedmioty_scisle_mean = mean(Czy_lubi_przedmioty_scisle, na.rm=TRUE),
            waznosc_technologii_mean = mean(waznosc_technologii, na.rm=TRUE),
            nauka_z_mediow_mean = mean(nauka_z_mediow, na.rm=TRUE)) %>% 
  mutate(nazwa = 'Chemia', .before = 1)


geo_int_szkola <- Roses %>% 
  select(StartDate, IPAddress, Q8_1:Q15_14) %>% 
  mutate(Q8_1bis = 4-Q8_1) %>%
  right_join(geo, c('StartDate', 'IPAddress'))
interere_geo <- geo_int_szkola %>% 
  mutate(srodowisko= apply(geo_int_szkola[,c('Q8_1bis', 'Q8_2', 'Q8_3', 'Q8_4', 'Q8_5', 'Q8_6', 'Q8_9', 'Q8_10')],1,function(x){mean(x,na.rm = TRUE)})) %>%
  mutate(Czy_lubi_przedmioty_scisle= apply(geo_int_szkola[,c('Q10_2', 'Q10_3', 'Q10_4', 'Q10_7')],1,function(x){mean(x,na.rm=TRUE)})) %>% 
  mutate(waznosc_technologii = apply(
    geo_int_szkola[,c('Q11_1', 'Q11_2', 'Q11_3', 'Q11_5', 'Q11_6', 'Q11_7',
                      'Q11_8', 'Q11_10','Q11_13')],1,function(x){mean(x, na.rm=TRUE)})) %>% 
  mutate(nauka_z_mediow = apply(geo_int_szkola[,c('Q12_1', 'Q14_1', 'Q14_2')],1,function(x){mean(x, na.rm=TRUE)})) %>% 
  filter(srodowisko != 'NaN', Czy_lubi_przedmioty_scisle != 'NaN',
         waznosc_technologii != 'NaN', nauka_z_mediow != 'NaN') %>% 
  summarize(srednie_srodowisko = mean(srodowisko, na.rm=TRUE),
            Czy_lubi_przedmioty_scisle_mean = mean(Czy_lubi_przedmioty_scisle, na.rm=TRUE),
            waznosc_technologii_mean = mean(waznosc_technologii, na.rm=TRUE),
            nauka_z_mediow_mean = mean(nauka_z_mediow, na.rm=TRUE)) %>% 
  mutate(nazwa = 'Geografia', .before = 1)

interere<- Roses %>% 
  select(StartDate, IPAddress, Q8_1:Q15_14) %>% 
  mutate(Q8_1bis = 4-Q8_1)
interere2<- Roses %>% 
  mutate(srodowisko= apply(interere[,c('Q8_1bis', 'Q8_2', 'Q8_3', 'Q8_4', 'Q8_5', 'Q8_6', 'Q8_9', 'Q8_10')],1,function(x){mean(x,na.rm = TRUE)})) %>%
  mutate(Czy_lubi_przedmioty_scisle= apply(interere[,c('Q10_2', 'Q10_3', 'Q10_4', 'Q10_7')],1,function(x){mean(x,na.rm=TRUE)})) %>% 
  mutate(waznosc_technologii = apply(
    interere[,c('Q11_1', 'Q11_2', 'Q11_3', 'Q11_5', 'Q11_6', 'Q11_7',
                      'Q11_8', 'Q11_10','Q11_13')],1,function(x){mean(x, na.rm=TRUE)})) %>% 
  mutate(nauka_z_mediow = apply(interere[,c('Q12_1', 'Q14_1', 'Q14_2')],1,function(x){mean(x, na.rm=TRUE)})) %>% 
  filter(srodowisko != 'NaN', Czy_lubi_przedmioty_scisle != 'NaN',
         waznosc_technologii != 'NaN', nauka_z_mediow != 'NaN') %>% 
  summarize(srednie_srodowisko = mean(srodowisko, na.rm=TRUE),
            Czy_lubi_przedmioty_scisle_mean = mean(Czy_lubi_przedmioty_scisle, na.rm=TRUE),
            waznosc_technologii_mean = mean(waznosc_technologii, na.rm=TRUE),
            nauka_z_mediow_mean = mean(nauka_z_mediow, na.rm=TRUE)) %>% 
  mutate(nazwa = 'Wszyscy', .before = 1)

#tworze tabele w?sk? aby stworzy? wykres kolumnowy pogrupowany


zainteresowania<- bind_rows(interere_bio, interere_fiz, interere_chem, interere_geo, interere2, id=NULL)

zainteresowania_waskie<- zainteresowania %>% 
  gather(przedmiot, wartosci, -nazwa)


zainteresowania_waskie[ ,2] <- stri_replace_all_regex(zainteresowania_waskie$przedmiot,"_", "\n")

ggplot(zainteresowania_waskie, aes(x=przedmiot, y=wartosci, group = nazwa))+
  geom_col(aes(fill = nazwa), position = position_dodge())




