####==========================================================================####
# 1. Initialization
####==========================================================================###

#setworking directory
setwd("D:/Rafael/Dropbox/4 diversificación Open Evidence/10_ETravel Insurance")

library(tidyverse)
library(stargazer)
library(estimatr)
library(readxl)
library(janitor)
library(flextable)

####==========================================================================####
# 2. Database
####==========================================================================###

df <- read_excel("./2_Data/etravel_clean.xlsx")
####==========================================================================####
# 3. Testing for errors or inconsistencies
####==========================================================================###

####==========================================================================####
# 3.1 Check if there are any problems with the age and edu_cat variables
####==========================================================================###

df %>% 
  mutate(my_edad_cat = case_when(
    25 <= age & age <= 34 ~ "1",
    35 <= age & age <= 54 ~ "2",
    55 <= age & age <= 75 ~ "3",
    TRUE ~ "error!"
  )) %>%
  group_by(my_edad_cat, edad_cat) %>%
  summarise(n = n())
# Conclusión: No hay problemas con las variables de edad

####==========================================================================####
# 3.2 Check if there are any problems with the risk preference variables
####==========================================================================###

risk_test <- df %>% 
  mutate(b3_1 = as.numeric(b3_1),
         b3_2 = as.numeric(b3_2),
         b3_3 = as.numeric(b3_3),
         error_b3_1 = ifelse(b3_1 < 0 | b3_1 > 10, 1, 0),
         error_b3_2 = ifelse(b3_2 < 0 | b3_2 > 10, 1, 0),
         error_b3_3 = ifelse(b3_3 < 0 | b3_3 > 10, 1, 0))
risk_test %>%
  group_by(error_b3_1,
           error_b3_2,
           error_b3_3) %>%
  summarise(n = n())

summary(risk_test$b3_1)
summary(risk_test$b3_2)
summary(risk_test$b3_3)
# Conclusión: No hay problemas con las variables de risk

####==========================================================================####
# 3.3 Check if there are any problems with the bias questions
####==========================================================================###

####==========================================================================####
# 3.3.1 Pregunta de framing
####==========================================================================###

df %>%
  select(m1a1, m1a2, m2a1, m2a2, random) %>%
  group_by(m1a1, m1a2, m2a1, m2a2,random) %>%
  summarise(n = n()) %>%
  View()

### M1a1 y M1a2 son la misma pregunta y M2a1 y M2a2 son la misma pregunta. La diferencia está en el orden en que se presentaron. Las preguntas que tienen a1  fueron preguntadas primero. 
### las que tienen el a2 al final se preguntaron en segundo lugar.
### M1 señala que la pregunta fue la de "program A vs Program B. 
### Mientras que M2 son las preguntas de Program c vs program d.

####==========================================================================####
# 3.3.2 Pregunta sunk cost
####==========================================================================###

df %>%
  select(m1b1, m1b2, m2b1, m2b2, random) %>%
group_by(m1b1, m1b2, m2b1, m2b2,random) %>%
  summarise(n = n()) %>%
  View()
####==========================================================================####
# 3.3.3 Regrest Question
####==========================================================================###

df %>%
  select(m1c1, m1c2, random) %>%
group_by(m1c1, m1c2,random) %>%
  summarise(n = n()) %>%
  View()
  
####==========================================================================####
# 3.4 Explorar si las variables de WTP tienen sentido
####==========================================================================###

df %>% 
  select(country, wtp_ch_normalized, wtp_exp_normalized) %>%
  group_by(country) %>% 
  summarise(mean_ch = mean(wtp_ch_normalized, na.rm = TRUE),
            mean_exp = mean(wtp_exp_normalized, na.rm = TRUE),
            sd_ch =  sd(wtp_ch_normalized, na.rm = TRUE),
            sd_exp = sd(wtp_exp_normalized, na.rm = TRUE))
#Conclusión: La verdad no sé -> hablar con el fran

####==========================================================================####
# 4. Check whether the framing function was well constructed
####==========================================================================###

df %>% 
  group_by(framing) %>% 
  count()

df %>%
  group_by(sunkcost_bias) %>% 
  count()

####==========================================================================####
# 5. Check if the wtp variables are well constructed:
# - Make them again each of them separetly and try them
####==========================================================================###

not_euro <- df %>% 
  mutate(
         wtp_exp_hu = ifelse(country == "HU", wtp_iceland_exp/140000, NA),
         wtp_exp_ro = ifelse(country == "RO", wtp_iceland_exp/2000, NA),
         wtp_ch_hu = ifelse(country == "HU", wtp_lisbon_ch/35000, NA),
         wtp_ch_ro = ifelse(country == "RO", wtp_lisbon_ch/500, NA),
         ) %>% 
  group_by(country) %>%
  summarise(
            mean_ch_ro = mean(wtp_ch_ro, na.rm = TRUE),
            mean_exp_ro = mean(wtp_exp_ro, na.rm = TRUE),
            mean_ch_hu = mean(wtp_ch_hu, na.rm = TRUE),
            mean_exp_hu = mean(wtp_exp_hu, na.rm = TRUE))

# CONSLUSIÓN: HAY UN PROBLEMA EN COMO CONSTRUÍ LAS VARIABLES

####==========================================================================####
# 5.1 Is the mean the same if I exclude the no other euro countries? 
# - It has to because RO and HU have values there.
# - I should exclude these countries for the mean calculation.
####==========================================================================###
# This is the correct calculation for euro countries
df_euro_only <- df %>% 
  filter(country != "RO" & country != "HU")

df_euro_only %>% 
  summarise(mean_ch = mean(wtp_lisbon_ch, na.rm = TRUE),
            mean_exp = mean(wtp_iceland_exp, na.rm = TRUE))
#This is wrong
df %>%
  summarise(mean_ch = mean(wtp_lisbon_ch, na.rm = TRUE),
            mean_exp = mean(wtp_iceland_exp, na.rm = TRUE))
df_euro_only %>% 
  group_by(country) %>%
  summarise(mean_ch = mean(wtp_lisbon_ch, na.rm = TRUE),
            mean_exp = mean(wtp_iceland_exp, na.rm = TRUE))
#This calculates correctly the value for each ocuntry
df %>% 
  group_by(country) %>%
  summarise(mean_ch = mean(wtp_lisbon_ch, na.rm = TRUE),
            mean_exp = mean(wtp_iceland_exp, na.rm = TRUE),
            #no sirve para ro ni HU
            mean_ch_norm = mean_ch /400,
            mean_exp_norm = mean_exp /100,
            )

####==========================================================================####
# 6. Ultima pregunta
####==========================================================================###

df %>% 
  group_by(rationality_violation) %>%
  summarise(n = n()) %>%
  flextable()

df %>% 
  select(rationality_violation, l5, wtp, country, diff_last_question_claims) %>%
  mutate(l5 = ifelse(l5 ==1, "x", "z")) %>%
  group_by(l5, rationality_violation) %>% 
  View()

  
#  31 participantes tienen rationality violations

df %>%
  #TODO: Acá la comparación debería ser con respecto a un número diferente
  # para RO y HU
  mutate(price_x_higher_z = ifelse(wtp < 45, 1,0)) %>%
  group_by(price_x_higher_z, diff_last_question_claims, l5) %>%
  summarise(n = n()) %>%
  flextable()

####==========================================================================####
# 7. Zip code per country
####==========================================================================###


df %>%
  select(country, zipcode) %>% 
  mutate(country = case_when(
    country == "DE" ~ "germany",
    country == "IT" ~ "italy",
    country == "RO" ~ "romania",
    country == "GR" ~ "greece",
    country == "FL" ~ "finland",
    country == "HU" ~ "hungary",
    TRUE ~ "problem"
  )) %>%
  writexl::write_xlsx(., path = "./2_Data/country_zipcodes.xlsx")