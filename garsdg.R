library(ggplot2)
library(ggthemes)
library(patchwork)
library(dplyr)
library(haven)
library(fmsb)
library(ggplot2)
library(stringi)
library(tidyverse)
library(lubridate)
options(scipen = 999)  # turns of scientific notations like 1e+40

Chemia2<- Roses %>% 
  select(StartDate,IPAddress,Q2,Q5_1,Q5_8,Q21_6,Q9_21)

Chemia2<- Roses %>% 
  select(StartDate,IPAddress,Q2,Q5_1,Q5_8,Q21_6,Q9_21) %>% 
  mutate(Cikaw_chem = apply(Chemia2[,-c(1,2,3)],1,function(x){mean(x,na.rm = TRUE)}))


#pytania zwi?zane z geografi?
Geografia2<- Roses %>% 
  select(StartDate,IPAddress,Q2,Q5_2,Q5_3,Q5_4,Q5_14)

Geografia2<- Roses %>% 
  select(StartDate,IPAddress,Q2,Q5_2,Q5_3,Q5_4,Q5_14) %>% 
  mutate(Cikaw_geo= apply(Geografia2[,-c(1,2,3)],1,function(x){mean(x,na.rm = TRUE)}))


Cikawosc_srednia2<-left_join(Geografia2,Chemia2,c('IPAddress','StartDate'))

# biologia
Biologia2<-Roses %>% 
  select(StartDate,IPAddress,Q2,Q5_5,Q5_6,Q5_7,Q5_9,Q21_1,Q21_3,Q21_4,Q7_2,Q21_8,Q9_11,Q9_12,Q9_13,Q9_14,Q9_19,Q9_20,Q9_24)

Biologia2<- Roses %>% 
  select(StartDate,IPAddress,Q2,Q5_5,Q5_6,Q5_7,Q5_9,Q21_1,Q21_3,Q21_4,Q7_2,Q21_8,Q9_11,Q9_12,Q9_13,Q9_14,Q9_19,Q9_20,Q9_24) %>% 
  mutate(Cikaw_bio= apply(Biologia2[,-c(1,2,3)],1,function(x){mean(x,na.rm = TRUE)}))

Cikawosc_srednia2<-left_join(Cikawosc_srednia2,Biologia2,c('IPAddress','StartDate'))

#fizyka
Fizyka2<-Roses %>% 
  select(StartDate,IPAddress,Q2,Q5_10,Q5_11,Q21_5,Q21_8,Q21_18,Q7_1,Q7_7,Q7_8,Q9_1,Q5_4,Q21_2,Q9_1)

Fizyka2<- Roses %>% 
  select(StartDate,IPAddress,Q2,Q5_10,Q5_11,Q21_5,Q21_8,Q21_18,Q7_1,Q7_7,Q7_8,Q9_1,Q5_4,Q21_2,Q9_1) %>% 
  mutate(Cikaw_fiz= apply(Fizyka2[,-c(1,2,3)],1,function(x){mean(x,na.rm = TRUE)}))

Cikawosc_srednia2<-left_join(Cikawosc_srednia2,Fizyka2,c('IPAddress','StartDate'))


#astronomia
Astronomia2<-Roses %>% 
  select(StartDate,IPAddress,Q2,Q5_12,Q5_13,Q21_16,Q7_2,Q7_7,Q9_22)

Astronomia2<- Roses %>% 
  select(StartDate,IPAddress,Q2,Q5_12,Q5_13,Q21_16,Q7_2,Q7_7,Q9_22) %>% 
  mutate(Cikaw_astro= apply(Astronomia2[,-c(1,2,3)],1,function(x){mean(x,na.rm = TRUE)}))

Cikawosc_srednia2<-left_join(Cikawosc_srednia2,Astronomia2,c('IPAddress','StartDate'))


#medycyna
Medycyna_Cialo_czlowieka2<- Roses %>% 
  select(StartDate,IPAddress,Q2,Q21_7,Q21_9,Q21_10,Q21_11,Q21_12,Q21_13,Q21_14,Q21_15,Q21_17,Q9_18)

Medycyna_Cialo_czlowieka2<- Roses %>% 
  select(StartDate,IPAddress,Q2,Q21_7,Q21_9,Q21_10,Q21_11,Q21_12,Q21_13,Q21_14,Q21_15,Q21_17,Q9_18) %>% 
  mutate(Cikaw_med= apply(Medycyna_Cialo_czlowieka2[,-c(1,2,3)],1,function(x){mean(x,na.rm = TRUE)}))

Cikawosc_srednia2<-left_join(Cikawosc_srednia2,Medycyna_Cialo_czlowieka2,c('IPAddress','StartDate'))




Ekologia2<-Roses %>% 
  select(StartDate,IPAddress,Q2,Q7_11,Q9_2,Q9_3,Q9_4,Q9_14,Q9_15,Q9_16) 

Ekologia2<- Roses %>% 
  select(StartDate,IPAddress,Q2,Q7_11,Q9_2,Q9_3,Q9_4,Q9_14,Q9_15,Q9_16) %>% 
  mutate(Cikaw_eko= apply(Ekologia2[,-c(1,2,3)],1,function(x){mean(x,na.rm = TRUE)}))


Cikawosc_srednia2<-left_join(Cikawosc_srednia2,Ekologia2,c('IPAddress','StartDate'))

gg2<-Cikawosc_srednia2 %>% 
  select(c('Q2.x','Cikaw_geo', 'Cikaw_chem', 'Cikaw_bio', 'Cikaw_fiz','Cikaw_med', 'Cikaw_eko', 'Cikaw_astro'))%>% 
  filter(Cikaw_geo != 'NaN', Cikaw_fiz != 'NaN', Cikaw_chem != 'NaN', Cikaw_bio != 'NaN', Cikaw_med != 'NaN', Cikaw_eko != 'NaN', Cikaw_astro != 'NaN') %>% 
  group_by(Q2.x) %>% 
  summarise(across(1:7,mean)) %>% 
  select(-c('Q2.x')) 

gg2_dziew<-Cikawosc_srednia2 %>% 
  select(c('Q2.x','Cikaw_geo', 'Cikaw_chem', 'Cikaw_bio', 'Cikaw_fiz','Cikaw_med', 'Cikaw_eko', 'Cikaw_astro'))%>% 
  filter(Cikaw_geo != 'NaN', Cikaw_fiz != 'NaN', Cikaw_chem != 'NaN', Cikaw_bio != 'NaN', Cikaw_med != 'NaN', Cikaw_eko != 'NaN', Cikaw_astro != 'NaN')%>% 
  group_by(Q2.x)%>% 
  summarise(across(1:7,mean)) %>% 
  select(-c('Q2.x')) %>% 
  slice(1) %>% 
  rbind(rep('female',length(as.data.frame(gg2)[1])))


gg2_chlop<-Cikawosc_srednia2 %>% 
  select(c('Q2.x','Cikaw_geo', 'Cikaw_chem', 'Cikaw_bio', 'Cikaw_fiz','Cikaw_med', 'Cikaw_eko', 'Cikaw_astro'))%>% 
  filter(Cikaw_geo != 'NaN', Cikaw_fiz != 'NaN', Cikaw_chem != 'NaN', Cikaw_bio != 'NaN', Cikaw_med != 'NaN', Cikaw_eko != 'NaN', Cikaw_astro != 'NaN') %>% 
  group_by(Q2.x) %>% 
  summarise(across(1:7,mean)) %>% 
  select(-c('Q2.x')) %>% 
  slice(2)%>% 
  rbind(rep('male',length(as.data.frame(gg2)[1])))


gg2_chlop<-as.data.frame(t(gg2_chlop))
gg2_chlop<-gg2_chlop %>% 
  cbind(przedmiot = row.names(gg2_chlop)) 
row.names(gg2_chlop)<- 1:dim(gg2_chlop)[1]
gg2_chlop$V1<-as.numeric(gg2_chlop$V1)

gg2_dziew<-as.data.frame(t(gg2_dziew))
gg2_dziew<-gg2_dziew %>% 
  cbind(przedmiot = row.names(gg2_dziew)) 
row.names(gg2_dziew)<- 1:dim(gg2_dziew)[1]
gg2_dziew$V1<-as.numeric(gg2_dziew$V1)*(-1)

gg2t<- gg2_chlop %>% 
  rbind(gg2_dziew) %>% 
  group_by(przedmiot)

gg2t<- as.data.frame(gg2t)

# Plot
p1<-ggplot(gg2_chlop, aes(x = przedmiot, y = V1,fill = V2)) + 
  geom_col(fill = "darkgreen", width = .4)+  
  scale_y_continuous(breaks = seq(0,3,0.2))+
  coord_flip()+
  theme(axis.text.y=element_blank(),axis.title.y = element_blank(),axis.title.x = element_blank())+
  labs(title = 'Male')
  

 
p2<-ggplot(gg2_dziew, aes(x = przedmiot, y = V1,fill = V2)) +  
  geom_col( width = .4)+ 
  scale_y_continuous(breaks = seq(-3,0,0.2))+
  coord_flip()+
  theme(axis.title.y = element_blank(),axis.title.x = element_blank())+
  labs(title = 'Female')+
  theme(legend.position = 'none')


p1

p2

p2+p1  +  # Flip axes
  plot_annotation(title = "Average interest in a subject",
                  subtitle = 'by gender')
