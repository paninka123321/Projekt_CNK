library(dplyr)
library(haven)
library(fmsb)
library(ggplot2)
library(stringi)
library(tidyverse)
library(lubridate)
library(patchwork)


Rosesa <-read_sav("~/OneDrive - Politechnika Warszawska/R_studio/ROSES master quest PL_November 9, 2021_07.50 (1).sav")
View(Roses)

#wywalamy fake odpowiedzi
Roses <- Rosesa %>% 
  mutate(czas_ankiety = as_datetime(EndDate) - as_datetime(StartDate)) %>% 
  filter(czas_ankiety >= 420)
View(Roses)

#wykres zainteresowanie a trudnosć

Trudnosc_vs_Zainteresowanie <- Roses %>% 
  select(IPAddress, "Difficulty" = Q10_1, "Interest" = Q10_2, Progress, "Gender" = Q2) %>% 
  filter(Progress >= 50)
Trudnosc_vs_Zainteresowanie <- na.omit(Trudnosc_vs_Zainteresowanie)

Trudnosc_vs_Zainteresowanie[ ,5] <- stri_replace_all_regex(Trudnosc_vs_Zainteresowanie$Gender,"1", "Female")
Trudnosc_vs_Zainteresowanie[ ,5] <- stri_replace_all_regex(Trudnosc_vs_Zainteresowanie$Gender,"2", "Male")

Trudnosc_vs_Zainteresowanie$Difficulty <- as.numeric(Trudnosc_vs_Zainteresowanie$Difficulty)
Trudnosc_vs_Zainteresowanie$Interest <- as.numeric(Trudnosc_vs_Zainteresowanie$Interest)

#chyba to drop dobrze dziala

Trudnosc_1 <- Trudnosc_vs_Zainteresowanie %>% 
  select(Difficulty, Interest, Gender) %>% 
  filter(Difficulty == 1) %>% 
  group_by(Gender, Interest) %>% 
  summarise(Amount = n(), .groups = "drop") 

#195 dzieci
#74 dziewczynki, 121 chlopcow

#-----------------------------------------------------------

Trudnosc_2 <- Trudnosc_vs_Zainteresowanie %>% 
  select(Difficulty, Interest, Gender) %>% 
  filter(Difficulty == 2) %>% 
  group_by(Gender, Interest) %>% 
  summarise(Amount = n(), .groups = "drop")
#438 dzieci
#234 dziewczynki i 204 chlopców

#-----------------------------------------------------------

Trudnosc_3 <- Trudnosc_vs_Zainteresowanie %>% 
  select(Difficulty, Interest, Gender) %>% 
  filter(Difficulty == 3) %>% 
  group_by(Gender, Interest) %>% 
  summarise(Amount = n(), .groups = "drop")
#445 dzieci
#247 dziewczynki i 198 chlopcow

#---------------------------------------------------------

Trudnosc_4 <- Trudnosc_vs_Zainteresowanie %>% 
  select(Difficulty, Interest, Gender) %>% 
  filter(Difficulty == 4) %>% 
  group_by(Gender, Interest) %>% 
  summarise(Amount = n(), .groups = "drop")
#554 dzieci
#324 dziewczynki i 230 chlopcow


chart1 <- Trudnosc_1 %>% ggplot(aes(x = Interest, y = Amount, fill = Gender)) +
  geom_col(position = position_dodge()) +
  labs(title = "Very Easy") +
  theme(legend.position = "none") +
  theme_bw() +
  ylim(0, 115)

chart2 <- Trudnosc_2 %>% ggplot(aes(x = Interest, y = Amount, fill = Gender)) +
  geom_col(position = position_dodge()) +
  labs(title = "Easy") +
  theme(legend.position = "none")+
  theme_bw() +
  ylim(0, 115)
chart3 <- Trudnosc_3 %>% ggplot(aes(x = Interest, y = Amount, fill = Gender)) +
  geom_col(position = position_dodge())+
  labs(title = "Not so hard") +
  theme(legend.position = "none") + 
  theme_bw() +
  ylim(0, 115)
chart4 <- Trudnosc_4 %>% ggplot(aes(x = Interest, y = Amount, fill = Gender)) +
  geom_col(position = position_dodge())+
  labs(title = "Hard") + 
  theme_bw()+
  ylim(0, 115)


chart1 + chart2 + chart3 + chart4 + plot_annotation(title = "Measure of difficulty") +
  plot_layout(guides = 'collect')



