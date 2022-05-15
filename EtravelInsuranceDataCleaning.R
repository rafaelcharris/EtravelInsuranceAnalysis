####==========================================================================####
# 1. Initialization
####==========================================================================###
setwd("D:/Rafael/Dropbox/4 diversificación Open Evidence/10_ETravel Insurance")

library(tidyverse)
library(stargazer)
library(estimatr)
library(readxl)
library(janitor)

####==========================================================================####
# 2. Database
####==========================================================================###

df <- read_excel("./2_Data/TRAVEL INSURANCE n4800 OE V1.xlsx") %>%
  clean_names() %>%
  rename("treatment" = "concept",
         "age" = "a1",
         "gender" = "a2",
         "education" = "a3",
         "marital_status" = "a4",
         "income" = "a5",
         "labor_market_status" = "a6",
         "household_size" = "a7",
         "wtp_lisbon_ch" = "mt_1ch",
         "wtp_iceland_exp" = "mt_1exp",
         "zipcode" = "a9") %>%
  mutate(treatment = tolower(treatment),
         price = tolower(price),
         module_order = ifelse(random == "M1-M2", 1,0),
         age = as.numeric(age),
         mt_2ar1 = as.numeric(mt_2ar1), 
         mt_2ar2 = as.numeric(mt_2ar2),
         mt_2ar3 = as.numeric(mt_2ar3),
         mt_2ar4 = as.numeric(mt_2ar4),
         mt_2ar5 = as.numeric(mt_2ar5),
         mt_2ar11 = as.numeric(mt_2ar11), 
         mt_2ar12 = as.numeric(mt_2ar12),
         mt_2ar13 = as.numeric(mt_2ar13),
         mt_2ar14 = as.numeric(mt_2ar14),
         mt_2ar15 = as.numeric(mt_2ar15),
         mt_2br11 = as.numeric(mt_2br11), 
         mt_2br12 = as.numeric(mt_2br12),
         mt_2br13 = as.numeric(mt_2br13),
         mt_2br14 = as.numeric(mt_2br14),
         mt_2br15 = as.numeric(mt_2br15),
         mt_2br11 = ifelse(!is.na(mt_2br11), mt_2br11, 0), 
         mt_2br12 = ifelse(!is.na(mt_2br12), mt_2br12, 0),
         mt_2br13 = ifelse(!is.na(mt_2br13), mt_2br13, 0),
         mt_2br14 = ifelse(!is.na(mt_2br14), mt_2br14, 0),
         mt_2br15 = ifelse(!is.na(mt_2br15), mt_2br15, 0),
         covid_top3 = ifelse((1 <= mt_2br11 & mt_2br11 <= 3)|
                               (1 <= mt_2br12 & mt_2br12 <= 3)|
                               (1 <= mt_2br13 & mt_2br13 <= 3)|
                               (1 <= mt_2br14 & mt_2br14 <= 3)|
                               (1 <= mt_2br15 & mt_2br15 <= 3 ), 1, 0),
         framing1 = ifelse(m1a1 == m2a2, 1, 0),
         framing2 = ifelse(m1a2 == m2a1, 1, 0),
         framing = ifelse(!is.na(framing1), framing1, framing2),
         sunk_cost1 = ifelse(m1b1 == m2b2, 1, 0),
         sunk_cost2 = ifelse(m1b2 == m2b1, 1, 0),
         sunkcost_bias = ifelse(!is.na(sunk_cost1), sunk_cost1, sunk_cost2), #esta variable sigue las instrucciones del documento
         sunkcost = ifelse(m1b1 == 1 & m2b2 == 2, 1, 0), #Esta variable indica si hay sunk cost 
         wtp_lisbon_ch = as.numeric(wtp_lisbon_ch),
         wtp_iceland_exp = as.numeric(wtp_iceland_exp),
         wtp_ch_normalized = case_when(
           country == "RO" ~ wtp_lisbon_ch/500,
           country == "HU"~ wtp_lisbon_ch/35000,
           TRUE ~ wtp_lisbon_ch/100),
         wtp_exp_normalized = case_when(
           country == "RO" ~ wtp_iceland_exp/2000,
           country == "HU" ~ wtp_iceland_exp/140000,
           TRUE ~ wtp_iceland_exp/400),
         wtp = ifelse(!is.na(wtp_lisbon_ch), wtp_lisbon_ch, wtp_iceland_exp),
         wtp_norm = ifelse(!is.na(wtp_ch_normalized), wtp_ch_normalized, wtp_exp_normalized),
         # Crear una variable,
         mt_2ar6  =as.numeric(mt_2ar6),
         mt_2ar7  =as.numeric(mt_2ar7),
         mt_2ar8  =as.numeric(mt_2ar8),
         mt_2ar9  =as.numeric(mt_2ar9),
         mt_2ar10 = as.numeric(mt_2ar10),
         less_well_off = ifelse(a8 == 1, 1, 0)
) %>%
  rowwise() %>% 
  mutate(
    health_related = ifelse(sum(mt_2ar1,mt_2ar2,mt_2ar3,mt_2ar4,mt_2ar5) > 0, 1, 0),
    covid_related = ifelse(sum(mt_2ar11, mt_2ar12, mt_2ar13, mt_2ar14, mt_2ar15, na.rm =TRUE) > 0, 1, 0),
    num_covid_related = sum(mt_2ar11, mt_2ar12, mt_2ar13, mt_2ar14, mt_2ar15, na.rm =TRUE),
    last_question_claims = sum(mt_2ar2,
                               mt_2ar3,
                               mt_2ar4,
                               mt_2ar5,
                               mt_2ar6,
                               mt_2ar7,
                               mt_2ar8,
                               mt_2ar9,
                               mt_2ar10, na.rm = TRUE),
    locus_control_total = sum(q11 == 2,q12 == 2,q13 == 2,q16 == 1,q17 == 1,q18 == 1,q22 == 2,q25 == 1,q28 == 2,q29 == 1)) %>%
  ungroup() %>%
  mutate(
    # Are the claims in the last question different than the ones the participant choose?
    diff_last_question_claims = ifelse(last_question_claims != 7, 1, 0),
    rationality_violation = case_when(
      country != "RO" & country != "HU" & l5 == 2 & (wtp <= 45 | diff_last_question_claims == 1) ~ 0,
      country == "RO" & l5 == 2 & (wtp <= 222 | diff_last_question_claims == 1) ~ 0,
      country == "HU" & wtp <= 15570 &l5 == 2 & (wtp <= 222 | diff_last_question_claims == 1) ~ 0,
      TRUE ~ 1
      ),
    b3_1 = as.numeric(b3_1),
    b3_2 = as.numeric(b3_2),
    b3_3 = as.numeric(b3_3),
    at_least2covid  = ifelse(num_covid_related >= 2, 1, 0),
    three_claims = ifelse(num_covid_related == 3, 1, 0),
    risk_averse = ifelse(b3_1 <= mean(b3_1),"averse", "seeking"),
    intertemporal_discount = ifelse(b3_2 <= mean(b3_2), "high", "low"),
    trust = ifelse(b3_3 <= mean(b3_3), "high", "low"),
    locus_control = ifelse(locus_control_total <= mean(locus_control_total), "internal", "external"),
    regret_averse1 = ifelse(m1c1 == 1, 1, 0),
    regret_averse2 = ifelse(m1c2 == 1, 1, 0),
    regret_averse = ifelse(!is.na(regret_averse1), regret_averse1, regret_averse2)
    ) %>%
  select(-c(framing1, framing2, sunk_cost1, sunk_cost2, regret_averse1, regret_averse2)) %>%
  # add id row
  rowid_to_column() %>% 
  writexl::write_xlsx("./2_Data/etravel_clean.xlsx")