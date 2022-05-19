####==========================================================================####
# 1. Initialization
####==========================================================================###
library(tidyverse)
library(ggpubr)
library(stargazer)
library(estimatr)
library(readxl)
library(flextable)
library(tools)
library(table1)
library(scales)
library(stringr)
library(janitor)

#Auxiliary function
personal_pvalue <- function(x){
  if (x == 0){
    better_p = "0.000"
    return (better_p)
  } else{
    return(format(x, nsmall = 3))
  }
}

####==========================================================================####
# 2. Database
####==========================================================================###

df <- read_excel("./data/df_clean.xlsx")

mortality_df <- read_excel("./data/mortality_data_merged.xlsx") %>% 
  select(rowid, above_mortality, mortality_rate, median_mortality,
         percentil_60, percentil_75, above_p60, above_p75)

df <- left_join(df, mortality_df, by = c("rowid"))

####==========================================================================####
# 3. Tables
####==========================================================================###
####==========================================================================####
# 3.1: Table 1. Descriptive statistics
####==========================================================================###
df <- df %>%
  mutate(age_table = ifelse(age <= 40, "25-40", "41-65"),
         female = as.factor(ifelse(gender == 1, 1, 0)),
         edu_table = ifelse(education <= 2, "High School or less", "At least tertiary education"), 
         marital_table = as.factor(ifelse(marital_status == 1, "Single", 0)),
         labor_market_table = case_when(
           labor_market_status == 1 | labor_market_status == 2 ~ "Employed/ In search",
           labor_market_status == 3 | labor_market_status == 4 |labor_market_status == 5 ~ "Student/Retired/Other"),
         household_size = as.numeric(household_size)
         )

table1(~age_table + female + edu_table + marital_table + income + labor_market_table + household_size | country, data = df )

####==========================================================================####
# 3.2: Table 2. During your last trip abroad for personal not business travel where did you go?
####==========================================================================###
# Create Folder
dir.create('./FlexTables')
#Para tener el valor de overall 
df_overall <- df
df_overall <- df_overall %>%
  mutate(country = "Total")
df_tables <- rbind(df, df_overall)
df_tables %>% 
  select(rowid, country, q1r1,q1r2,q1r3,q1r4,q1r5,q1r6,q1r7) %>%
  mutate(q1r1 = as.numeric(q1r1),
         q1r2 = as.numeric(q1r2),
         q1r3 = as.numeric(q1r3),
         q1r4 = as.numeric(q1r4),
         q1r5 = as.numeric(q1r5),
         q1r6 = as.numeric(q1r6),
         q1r7 = as.numeric(q1r7)) %>%
  pivot_longer(cols = contains("q1"), names_to = "travel_dest", values_to  = "selected") %>% 
  group_by(country, travel_dest) %>%
  summarise(
    n = n(),
    suma = sum(selected),
    per = suma/n*100,
    cell = paste0(suma,"\n(", round(per,2),")%")) %>%
  mutate(country_tab = paste0(country, "\n(N = ", n, ")")) %>%
  ungroup() %>%
  select(country_tab, travel_dest, cell) %>%
  group_by(country_tab, travel_dest) %>%
  pivot_wider(
    names_from = country_tab, 
    values_from = c(cell)
  ) %>% 
  mutate(travel_dest = case_when(
    travel_dest == "q1r1" ~ "Europe",
    travel_dest == "q1r2" ~ "North America",
    travel_dest == "q1r3" ~ "South America",
    travel_dest == "q1r4" ~ "Middle East",
    travel_dest == "q1r5" ~ "Far East",
    travel_dest == "q1r6" ~ "Australia/New Zealand",
    travel_dest == "q1r7" ~ "Africa")
  ) %>%
  flextable() %>%
  theme_vanilla() %>%
  save_as_docx(., path = "./FlexTables/table2.docx")


####==========================================================================####
# 3.3: Table 3. Did you have travel insurance for that trip? Which one?
####==========================================================================###

df <- df %>% 
  mutate(q2 = ifelse(q2 == 1, "yes", "no")
         )
table1(~q2 | country, data = df)

df_tables %>% 
  mutate(q2br1 = ifelse(q2br1 ==1, 1, 0),
        q2br2 = ifelse(q2br2 == 1, 1, 0),
        q2br3 = ifelse(q2br3 == 1, 1, 0),
        q2br4 = ifelse(q2br4 == 1, 1, 0),
        q2br5 = ifelse(q2br5 == 1, 1, 0),
        q2br6 = ifelse(q2br6 == 1, 1, 0),
        q2br7 =ifelse(q2 == 2, 1, 0) # esta opción representa que no tuvo insurance
  ) %>% 
  select(rowid, country, q2br1 ,q2br2 ,q2br3 ,q2br4 ,q2br5 ,q2br6, q2br7) %>%
  pivot_longer(cols = contains("q2"), names_to = "insurance", values_to  = "selected") %>% 
  group_by(country, insurance) %>%
  summarise(
    n = n(),
    suma = sum(selected, na.rm = TRUE),
    per = suma/n*100,
    cell = paste0(suma,"\n(", round(per,2),")%")) %>%
  mutate(country_tab = paste0(country, "\n(N = ", n, ")")) %>%
  ungroup() %>%
  select(country_tab, insurance, cell) %>%
  group_by(country_tab, insurance) %>%
  pivot_wider(
    names_from = country_tab, 
    values_from = c(cell)
  ) %>% 
  mutate(insurance = case_when(
    insurance == "q2br1" ~ "An annual policy",
    insurance == "q2br2" ~ "A single trip policy bought separately from hotel reservation/travel ticket",
    insurance == "q2br3" ~ "Purchased as an ‘add on to travel",
    insurance == "q2br4" ~ "Came with my credit card or bank acc",
    insurance == "q2br5" ~ "I shopped around to find a good deal",
    insurance == "q2br6" ~ "A single trip policy bought purchased with the hotel reservation/travel ticket",
    insurance == "q2br7" ~ "I did not buy travel insurance"
      )) %>%
  flextable() %>%
  autofit() %>% 
  theme_vanilla() %>%
  save_as_docx(., path = "./FlexTables/table3.docx")


####==========================================================================####
# 3.4: Table 4 Do you remember what was insured in that policy?
####==========================================================================###

table1(~ as.factor(q3) | country, data = df[!is.na(df$q3),])
df %>% 
  filter(!is.na(q3)) %>% 
  group_by(country, q3) %>% 
  summarise(n = n()) %>%
  ungroup() %>%
  mutate(total_col = sum(n))

df_tables %>% 
  #filter(q3 == 1) %>%
  mutate(
         q3br2 = ifelse(q3br2 == 1, 1, 0),
         q3br3 = ifelse(q3br3 == 1, 1, 0),
         q3br4 = ifelse(q3br4 == 1, 1, 0),
         q3br5 = ifelse(q3br5 == 1, 1, 0),
         q3br6 = ifelse(q3br6 == 1, 1, 0),
         q3br7 = ifelse(q3br7 ==1, 1, 0),
         q3br8 = ifelse(q3br8 ==1, 1, 0),
         q3br9 = ifelse(q3 == 2 | q2 == 2, 1, 0)
  ) %>% 
  select(rowid, country, q3br2,q3br3,q3br4,q3br5,q3br6,q3br7,q3br8, q3br9) %>%
  pivot_longer(cols = contains("q3"), names_to = "insurance", values_to  = "selected") %>% 
  group_by(country, insurance) %>%
  summarise(
    n = n(),
    suma = sum(selected, na.rm = TRUE),
    per = suma/n*100,
    cell = paste0(suma,"\n(", round(per,2),")%")) %>%
  mutate(country_tab = paste0(country, "\n(N = ", n, ")")) %>%
  ungroup() %>%
  select(country_tab, insurance, cell) %>%
  group_by(country_tab, insurance) %>%
  pivot_wider(
    names_from = country_tab, 
    values_from = c(cell)
  ) %>% 
  mutate(insurance = case_when(
    #insurance == "q3br1" ~ "The normal things for travellers",
    insurance == "q3br2" ~ "Cancellation/ interruption",
    insurance == "q3br3" ~ "Emergency medical treatment",
    insurance == "q3br4" ~ "Personal possessions",
    insurance == "q3br5" ~ "Medical Expenses",
    insurance == "q3br6" ~ "Baggage Cover",
    insurance == "q3br7" ~  "Personal Accident",
    insurance == "q3br8" ~  "Travel Delay",
    insurance == "q3br9" ~  "Do not remember/ did not buy insurance",
    TRUE ~ "m"
  )) %>%
  flextable() %>%
  theme_vanilla() %>%
  autofit() %>% 
  save_as_docx(., path = "./FlexTables/table4.docx")


####==========================================================================####
# 3.5: Table 5 Has the COVID-19 pandemic changed your view of travel insurance. Share of participants answering yes to each statement.
####==========================================================================###

df_tables %>% 
  #filter(b2_1a == 1) %>%
  mutate(
    b2_1br1 = ifelse(b2_1br1 == 1, 1, 0),
    b2_1br2 = ifelse(b2_1br2 == 1, 1, 0),
    b2_1br3 = ifelse(b2_1br3 == 1, 1, 0),
    b2_1br4 = ifelse(b2_1br4 == 1, 1, 0),
    b2_1br5 = ifelse(b2_1br5 == 1, 1, 0),
    b2_1br6 = ifelse(b2_1br6 == 1, 1, 0)
  ) %>% 
  select(rowid, country, b2_1br1,
         b2_1br2,
         b2_1br3,
         b2_1br4,
         b2_1br5,
         b2_1br6) %>%
  pivot_longer(cols = contains("b2"), names_to = "statement", values_to  = "selected") %>% 
  group_by(country, statement) %>%
  summarise(
    n = n(),
    suma = sum(selected, na.rm = TRUE),
    per = suma/n*100,
    cell = paste0(suma,"\n(", round(per,2),")%")) %>%
  mutate(country_tab = paste0(country, "\n(N = ", n, ")")) %>%
  ungroup() %>%
  select(country_tab, statement, cell) %>%
  group_by(country_tab, statement) %>%
  pivot_wider(
    names_from = country_tab, 
    values_from = c(cell)
  ) %>% 
  mutate(statement = case_when(
    statement == "b2_1br1" ~ "I would be more likely to purchase travel insurance than in the past",
    statement == "b2_1br2" ~ "I would make sure I know what is covered and what is excluded",
    statement == "b2_1br3" ~ "I would want to have risks related to pandemics covered",
    statement == "b2_1br4" ~ "I would shop around/check comparison websites to get a good deal",
    statement == "b2_1br5" ~ "I will check out annual travel insurance",
    statement == "b2_1br6" ~ "I would expect to pay more"
  )) %>%
  flextable() %>%
  theme_vanilla() %>%
  autofit() %>%
  save_as_docx(., path = "./FlexTables/table5.docx")


####==========================================================================####
# 3.6  Table 6 Treatment effects: willingness to pay
####==========================================================================###

df %>%
  select(wtp_ch_normalized, wtp_exp_normalized, treatment, wtp_norm) %>%
  group_by(treatment) %>%
  summarise(mean_ch = round(mean(wtp_ch_normalized, na.rm = TRUE), 2),
            sd_ch = round(sd(wtp_ch_normalized, na.rm = TRUE),2),
            mean_exp = round(mean(wtp_exp_normalized, na.rm = TRUE),2),
            sd_exp =   round(sd(wtp_exp_normalized, na.rm = TRUE),2 ),
            `t-test\n(p-value)` = 
              paste0(t = "t = ", t.test(wtp_ch_normalized, wtp_exp_normalized)$statistic,
              "\np = ", personal_pvalue(t.test(wtp_ch_normalized, wtp_exp_normalized)$p.value))
            ) %>%
  mutate(Treatment = toTitleCase(treatment),
         Cheap = paste0(mean_ch, "\n(", sd_ch, ")"),
         Expensive = paste0(mean_exp, "\n(", sd_exp, ")")) %>%
  select(Treatment, Cheap, Expensive, `t-test\n(p-value)`) %>%
  flextable() %>% 
  bold(part = "header") %>% 
  add_body(top = F, Treatment = "t-test\n(p-value)", 
           Cheap = paste0(
             "t = ", t.test(data = df[df$price == "cheap", ], wtp_norm~ treatment)$statistic,
             "\np = ", personal_pvalue(t.test(data = df[df$price == "cheap", ], wtp_norm~ treatment)$p.value)),
           Expensive = 
             paste0(
               "t = ", t.test(data = df[df$price == "expensive", ], wtp_norm~ treatment)$statistic,
               "\np = ", t.test(data = df[df$price == "expensive", ], wtp_norm~ treatment)$p.value),
           `t-test\n(p-value)`= "") %>%
  bold(j = 1, bold = TRUE) %>%
  theme_vanilla() %>%
  save_as_docx(., path = "./FlexTables/table6.docx")


####==========================================================================####
# 3.7 Table 8 WTP by exposure to covid first measure
####==========================================================================###
df %>%
  group_by(a8) %>%
  mutate(`As a result of the covid pandemic` = case_when(
    a8 == 1 ~ "Less Well off", 
    a8 == 2 ~ "About the same", 
    a8 == 3 ~ "More well off",
    TRUE ~ "n"), 
    t_test = paste0("t = ", format(round(t.test(wtp_norm ~ price)$statistic, 2), nsmall = 2),
                    "\np = ", format(round( t.test(wtp_norm ~ price)$p.value, 3), nsmall = 3))) %>%
  group_by(`As a result of the covid pandemic`, price) %>% 
  summarise(mean_wtp = round(mean(wtp_norm), 2),
            t_test = t_test) %>%
  distinct() %>% 
  pivot_wider(names_from = price,
              values_from = c(mean_wtp, t_test)) %>%
  ungroup() %>%
  select(`As a result of the covid pandemic`, mean_wtp_cheap, mean_wtp_expensive, t_test_cheap) %>%
  flextable() %>%
    theme_vanilla()  %>%
  add_body(top = F,
           `As a result of the covid pandemic` = "Kruskal Wallis\n(p-value)",
           mean_wtp_cheap = paste0(
             "KW = ",
             round(kruskal.test(data = df[df$price == "cheap", ], wtp_norm ~ a8)$statistic,2),
             "\np = ", personal_pvalue(round(kruskal.test(data = df[df$price == "cheap", ], wtp_norm ~ a8)$p.value,3))),
           mean_wtp_expensive = 
             paste0(
               "KW = ", 
               round(kruskal.test(data = df[df$price == "expensive", ], wtp_norm ~ a8)$statistic,2),
               "\np = ", personal_pvalue(round(kruskal.test(data = df[df$price == "expensive", ], wtp_norm ~ a8)$p.value,3)))
           ) %>%
  set_header_labels(`As a result of the covid pandemic...` = "As a result of the covid pandemic I'm...",
                    mean_wtp_cheap = "Cheap",
                    mean_wtp_expensive = "Expensive",
                    t_test_cheap = "T-test (p-value)") %>% 
  save_as_docx(., path = "./FlexTables/table8.docx")

####==========================================================================####
# 3.8 Table 9 WTP by mortality
####==========================================================================###

df %>%
  group_by(above_p75) %>%
  mutate(`Mortality by Covid19 in the area of residence` = case_when(
    above_p75 == 1 ~ "Above 75% percentile Mortality", 
    above_p75 == 0 ~ "Below 75% percentile Mortality",
    TRUE ~ "n"), 
    t_test = paste0("t = ", format(round(t.test(wtp_norm ~ price)$statistic, 2), nsmall = 2),
                    "\np = ", format(round( t.test(wtp_norm ~ price)$p.value, 3), nsmall = 3))) %>%
  filter(`Mortality by Covid19 in the area of residence` != "n") %>% #sacar los que no pudimos
  group_by(`Mortality by Covid19 in the area of residence`, price) %>% 
  summarise(mean_wtp = round(mean(wtp_norm), 2),
            t_test = t_test) %>%
  distinct() %>% 
  pivot_wider(names_from = price,
              values_from = c(mean_wtp, t_test)) %>%
  ungroup() %>%
  select(`Mortality by Covid19 in the area of residence`, mean_wtp_cheap, mean_wtp_expensive, t_test_cheap) %>%
  flextable() %>%
  theme_vanilla()  %>%
  add_body(top = F,
           `Mortality by Covid19 in the area of residence` = "t-test\n(p-value)",
           mean_wtp_cheap = paste0(
             "t = ",
             round(t.test(data = df[df$price == "cheap", ], wtp_norm ~ above_mortality)$statistic,2),
             "\np = ", personal_pvalue(round(t.test(data = df[df$price == "cheap", ], wtp_norm ~ above_mortality)$p.value,3))),
           mean_wtp_expensive = 
             paste0(
               "t = ", 
               round(t.test(data = df[df$price == "expensive", ], wtp_norm ~ above_mortality)$statistic,2),
               "\np = ", personal_pvalue(round(t.test(data = df[df$price == "expensive", ], wtp_norm ~ above_mortality)$p.value,3)))
  ) %>%
  set_header_labels(`Mortality by Covid19 in the area of residence` = "Mortality by Covid19 in the area of residence",
                    mean_wtp_cheap = "Cheap",
                    mean_wtp_expensive = "Expensive",
                    t_test_cheap = "T-test (p-value)") %>%
save_as_docx(., path = "./FlexTables/table9.docx")

####========================================================================####
# 3.8 Table 11 Violation of rationality
####=========================================================================###

rationality_df <- df %>%
  select(price, treatment, rationality_violation) %>%
  group_by(price, treatment) %>%
  summarise(rv = sum(rationality_violation)) %>% 
       pivot_wider(names_from = price,
                   values_from = rv) 
rationality_df %>%
      rowwise() %>% 
    mutate(chi_res = paste0(
        "chi= ", round(chisq.test(c(cheap,expensive))$statistic, 2),
        "\np = ", personal_pvalue(round(chisq.test(c(cheap,expensive))$p.value,3))),
        cheap = paste0(round(cheap/12, 2), "%"),
        expensive = paste0(round(expensive/12, 2),"%")) %>%
flextable() %>%
  add_body(top = F, treatment = "Chi squared", 
           cheap = paste0("chi = ", round(chisq.test(rationality_df$cheap)$statistic, 2),
                          "\n(p = ", personal_pvalue(round(chisq.test(rationality_df$cheap)$p.value, 3)), ")"),
         expensive = paste0("chi = ", round(chisq.test(rationality_df$expensive)$statistic, 2),
                            "\n(p = ", personal_pvalue(round(chisq.test(rationality_df$expensive)$p.value, 3)), ")")
           ) %>%
  set_header_labels(treatment = "Treatment", cheap = "Cheap", expensive = "Expensive",
                    chi_res = "Chi Squared") %>%
  save_as_docx(., path = "./FlexTables/table11.docx")
####========================================================================####
# 3.81 CHECK IF TABLE 11 WAS PROPERLY CALCULATED
####=========================================================================###
chisq.test(rationality_df$cheap) #compares negative vs positive per cheap cell (4,2)
chisq.test(rationality_df$expensive) #compares negative vs positive per expensive cell(4,3)

###========================================================================####
# Result: It was!
####=========================================================================###


####========================================================================####
# 3.9 Table 12 Classification of participants in different subgroups
####=========================================================================###
  
df %>% 
    table1(data = ., ~as.factor(regret_averse) + trust + locus_control + as.factor(sunkcost_bias) + risk_averse + intertemporal_discount + as.factor(framing))

####========================================================================####
# 3.10 Table 13 Treatment effect by subgroup. Outcome variable: willingness to pay
####=========================================================================###

df %>%
  select(rowid,
         regret_averse,
         trust, 
         locus_control,
         sunkcost_bias, 
         risk_averse,
         intertemporal_discount,
         framing, 
         price, 
         treatment, 
         wtp_norm) %>%
  mutate(regret_averse = ifelse(regret_averse ==1, "yes", "no"),
         sunkcost_bias = ifelse(sunkcost_bias == 1, "yes", "no"),
         framing = ifelse(framing == 1, "yes", "no")) %>% 
  pivot_longer(names_to = "bias",
               values_to = "value",
               cols = !contains("price") & !contains("wtp_norm") & !contains("rowid") & !contains("treatment") & !contains("covid")) %>%
  pivot_wider(names_from = treatment, 
              values_from = wtp_norm) %>%
  group_by(price, bias, value) %>% 
  summarise(cell = paste0("delta = ", format(round(mean(negative, na.rm = TRUE) - mean(positive, na.rm = TRUE), 2), nsmall = 2), 
                          "\n(t = ", format(round(t.test(negative,positive, na.rm = TRUE)$statistic, 3), nsmall = 2),
                          ",\np = ", personal_pvalue(round(t.test(negative, positive, na.rm = TRUE)$p.value, 3)), ")"))  %>%
  pivot_wider(names_from = price,
              values_from = cell) %>%
  select(bias, value, expensive, cheap) %>%
  flextable() %>%
  merge_v(j = "bias") %>% 
  theme_vanilla()%>%
  autofit() %>%
  save_as_docx(., path = "./FlexTables/table13.docx")
  
####========================================================================####
# 3.11 Table 14 Treatment effect by subgroup. Outcome variable: likelihood to select 2 covid claims
####=========================================================================###

df %>%
  select(rowid,
         regret_averse,
         trust, 
         locus_control,
         sunkcost_bias, 
         risk_averse,
         intertemporal_discount,
         framing, 
         price, treatment, at_least2covid) %>%
  mutate(regret_averse = ifelse(regret_averse ==1, "yes", "no"),
         sunkcost_bias = ifelse(sunkcost_bias == 1, "yes", "no"),
         framing = ifelse(framing == 1, "yes", "no")) %>% 
  pivot_longer(names_to = "bias",
               values_to = "value",
               cols = !contains("price") &  !contains("rowid") & !contains("treatment") & !contains("covid")) %>%
  pivot_wider(names_from = treatment, 
              values_from = at_least2covid) %>%
  group_by(price, bias, value) %>% 
  summarise(cell = paste0("delta = ", format(round(mean(negative, na.rm = TRUE) - mean(positive, na.rm = TRUE), 2), nsmall = 2), 
                          "\n(t = ", format(round(t.test(negative,positive, na.rm = TRUE)$statistic, 3), nsmall = 2),
                          ",\np = ", personal_pvalue(round(t.test(negative, positive, na.rm = TRUE)$p.value, 3)), ")"))  %>%
  pivot_wider(names_from = price,
              values_from = cell) %>%
  select(bias, value, expensive, cheap) %>%
  flextable() %>%
  merge_v(j = "bias") %>% 
  theme_vanilla()%>%
  save_as_docx(., path = "./FlexTables/table14.docx")
  
####========================================================================####
# 3.12 Table 15 Treatment effect by subgroup. Outcome variable: likelihood to select a covid among top 3
####=========================================================================###
df %>%
  select(rowid,
         regret_averse,
         trust, 
         locus_control,
         sunkcost_bias, 
         risk_averse,
         intertemporal_discount,
         framing, 
         price, treatment, covid_top3) %>%
  mutate(regret_averse = ifelse(regret_averse ==1, "yes", "no"),
         sunkcost_bias = ifelse(sunkcost_bias == 1, "yes", "no"),
         framing = ifelse(framing == 1, "yes", "no")) %>% 
  pivot_longer(names_to = "bias",
               values_to = "value",
               cols = !contains("price") &  !contains("rowid") & !contains("treatment") & !contains("covid")) %>%
  pivot_wider(names_from = treatment, 
              values_from = covid_top3) %>%
  group_by(price, bias, value) %>% 
  summarise(cell = paste0("delta = ", format(round(mean(negative, na.rm = TRUE) - mean(positive, na.rm = TRUE),2), nsmall = 2), 
                          "\n(t = ", format(round(t.test(negative,positive, na.rm = TRUE)$statistic, 3), nsmall = 2),
                          ",\np = ", personal_pvalue(round(t.test(negative, positive, na.rm = TRUE)$p.value, 3)), ")"))  %>%
  pivot_wider(names_from = price,
              values_from = cell) %>%
  select(bias, value, expensive, cheap) %>%
  flextable() %>%
  merge_v(j = "bias") %>% 
  theme_vanilla()   %>%
  save_as_docx(., path = "./FlexTables/table15.docx")


####========================================================================####
# 3.13 Table 16 Treatment effect by subgroup. Outcome variable: likelihood to select 3 covid claims
####=========================================================================###
df %>%
  select(rowid,
         regret_averse,
         trust, 
         locus_control,
         sunkcost_bias, 
         risk_averse,
         intertemporal_discount,
         framing, 
         price, treatment, three_claims) %>%
  mutate(regret_averse = ifelse(regret_averse ==1, "yes", "no"),
         sunkcost_bias = ifelse(sunkcost_bias == 1, "yes", "no"),
         framing = ifelse(framing == 1, "yes", "no")) %>% 
  pivot_longer(names_to = "bias",
               values_to = "value",
               cols = !contains("price") &  
                 !contains("rowid") & !contains("three_claims") & !contains("treatment") & !contains("covid")) %>%
  pivot_wider(names_from = treatment, 
              values_from = three_claims) %>%
  group_by(price, bias, value) %>% 
  summarise(cell = paste0("delta = ", format(round(mean(negative, na.rm = TRUE) - mean(positive, na.rm = TRUE), 2), nsmall = 2), 
                          "\n(t = ", format(round(t.test(negative,positive, na.rm = TRUE)$statistic, 3), nsmall = 2),
                          ",\np = ", personal_pvalue(round(t.test(negative, positive, na.rm = TRUE)$p.value, 3)), ")"))  %>%
  pivot_wider(names_from = price,
              values_from = cell) %>%
  select(bias, value, expensive, cheap) %>%
  flextable() %>%
  merge_v(j = "bias") %>% 
  theme_vanilla()   %>%
  save_as_docx(., path = "./FlexTables/table16.docx")

####==========================================================================####
# 4. Regressions
####==========================================================================###
#Create folder to store regression outputs
dir.create('./RegOutput')
####==========================================================================####
# 4.1 Table 7 
####==========================================================================###
df_reg <- df %>%
  mutate(treatment = ifelse(treatment == "negative", 1, 0))  
reg1 <- lm(data = df_reg, at_least2covid ~ as.factor(treatment) + as.factor(price) + age + gender +
             country + education +income + marital_status + labor_market_status )
reg2 <- lm(data = df_reg, covid_top3 ~ as.factor(treatment) + as.factor(price) + age + gender +
             country + education +income + marital_status + labor_market_status )
reg3 <- lm(data = df_reg, three_claims ~ as.factor(treatment) + as.factor(price)+ age + gender +
             country + education +income + marital_status + labor_market_status )
####==========================================================================####
# 4.1.1 Output
####==========================================================================###

stargazer(reg1, reg2, reg3,
          title = "Table 7",
          covariate.labels = c("Negative Priming", "Expensive Package"),
          #dep.var.caption = "Dependent Variable:",
          column.labels = c("Likelihood to select 2 Covid19 claims", 
          "Likelihood to select a Covid19 claim as 1st 2nd or 3rd claim", 
          "Likelihood to select 3 Covid19 claims"),
          omit = c("age", "gender", "country", "education",
                   "income", "marital_status","labor_market_status"),
          table.layout = "=ldc-t-a-s-n",
          model.names = TRUE,
          model.numbers = TRUE, 
          add.lines = list(c("Controls",  "Yes", "Yes", "Yes")),
          se = starprep(reg1, reg2, reg3, se_type = "HC1"),
          omit.stat= c("adj.rsq", "ser", "f"),
          type = "html",
          out = "./RegOutput/Table7_likelihoodcovid.doc")

####==========================================================================####
# 4.2 Table 10 
####==========================================================================###

reg1 <-lm(data = df, wtp_norm ~ as.factor(less_well_off)+ age + gender +
            country + education +income + marital_status + labor_market_status  )
reg2<- lm(data = df, at_least2covid ~ as.factor(less_well_off) + age + gender +
            country  + education +income + marital_status + labor_market_status )
reg3<- lm(data = df, covid_top3 ~ as.factor(less_well_off) + age + gender +
            country + education +income + marital_status + labor_market_status )
reg4<- lm(data = df, three_claims ~ as.factor(less_well_off) + age + gender +
            country  + education +income + marital_status + labor_market_status )

reg5 <-lm(data = df, wtp_norm ~ as.factor(above_p75) + age + gender +
            country + education +income + marital_status + labor_market_status )
reg6<- lm(data = df, at_least2covid ~ as.factor(above_p75)+ age + gender +
            country + education +income + marital_status + labor_market_status )
reg7<- lm(data = df, covid_top3 ~ as.factor(above_p75)+ age + gender +
            country + education +income + marital_status + labor_market_status )
reg8<- lm(data = df, three_claims ~ as.factor(above_p75)+ age + gender +
            country + education +income + marital_status + labor_market_status )


stargazer(reg1,reg2,reg3,reg4,
          reg5,reg6,reg7,reg8,
          title = "Table 10",
          dep.var.labels.include = FALSE, #-> quitarlo cuando sepa que está bien
          covariate.labels = c("Less well of because of covid", 
                               "Above the 75 percentile Covid19mortality in the area of residence"
                               ),
          column.labels = c("Willigness to Pay", "Likelihood to select 2 Covid19 claims",
                             "Likelihood to select a Covid19 claim as 1st 2nd or 3rd claim",
                             "Likelihood to select 3 Covid19 claims", 
                            "Willigness to Pay",
                             "Likelihood to select 2 Covid19 claims",
                             "Likelihood to select a Covid19 claim as 1st 2nd or 3rd claim",
                             "Likelihood to select 3 Covid19 claims"
                             ),
          se = starprep(reg1,reg2,reg3,reg4,reg5,reg6,reg7,reg8, se_type = "HC1"),
          omit.stat= c("adj.rsq", "ser", "f"),
          omit = c("age", "gender", "country", "education",
                   "income", "marital_status","labor_market_status"),
          add.lines = list(c("Controls",  "Yes", "Yes", "Yes","Yes", "Yes", "Yes", "Yes", "Yes")),
          type = "html",
          out = "./RegOutput/Table10regp75.doc")

####==========================================================================####
# 5. Graph
####==========================================================================###

# En esta versión lo que hago es crear el data set para plotear. Es mejor
se <- function(x) sqrt(var(x)/length(x))

plot_df <- df %>% 
  select(rowid, treatment, price, mt_2ar1 ,mt_2ar2 ,mt_2ar3 ,mt_2ar4 ,mt_2ar5,mt_2ar6 ,mt_2ar7 ,mt_2ar8 ,mt_2ar9 ,mt_2ar10,mt_2ar11,mt_2ar12,mt_2ar13,mt_2ar14,mt_2ar15) %>%
  pivot_longer(cols = contains("mt_"), names_to = "claim", values_to  = "count") %>%
  group_by(price, treatment, claim) %>%
  summarise(prob = mean(count),
            se = se(count)) %>%
  arrange(desc(prob)) %>%
  group_by(price, treatment) %>% 
  slice_head(n = 7) %>%
  mutate(claim = case_when(
   claim == "mt_2ar1"~ "Dental",
   claim == "mt_2ar2"~ "Hospitalization",
   claim == "mt_2ar3"~ "Ambulance\ntransportation",
   claim == "mt_2ar4"~ "Medical\nevacuation\n/repatriation",
   claim == "mt_2ar5"~ "Prescribed\nmedicines",
   claim == "mt_2ar6"~ "Personal\naccident",
   claim == "mt_2ar7"~ "Travel delay -\n(expenses for\naccommodation\nand meals)",
   claim == "mt_2ar8"~ "Baggage\ncover",
   claim == "mt_2ar9"~ "Security and\nlegal assistance",
   claim == "mt_2ar10" ~  "Trip cancellation\n(by costumer)",
   claim == "mt_2ar11" ~  "Hospitalization\nCosts for\nCovid19",
   claim == "mt_2ar12" ~  "Visa required covid test",
   claim == "mt_2ar13" ~  "Curtailment cover",
   claim == "mt_2ar14" ~  "Costs related to\nquarantine at\nthe arrival",
   claim == "mt_2ar15" ~  "Mandatory or\nrecommended\ncovid testing\nbefore travel",
   TRUE ~ " "
  ))

####==========================================================================####
# 5.1 Panel A: Negative + Expensive 
####==========================================================================###

A <- plot_df %>%
  filter(treatment == "negative" & price == "expensive") %>%
  mutate(claim = fct_reorder(claim, desc(prob))) %>%
  ggplot(aes(x = claim, y = prob)) + 
  geom_bar(stat = "identity",
           fill = "white", 
           col = "black") +
  geom_errorbar(aes(x = claim, 
                ymin = prob - se,
                ymax = prob + se,
                width = 0.2)) + 
  scale_y_continuous(labels = scales::percent) + 
  labs(y = "Likelihood to be selected", x = "Claim", title = "Negative Primming & Expensive Package") + 
  theme_bw()

####==========================================================================####
# 5.2 Panel B:  Negative + Cheap 
####==========================================================================####
B <- plot_df %>%
  filter(treatment == "negative" & price == "cheap") %>%
  mutate(claim = fct_reorder(claim, desc(prob))) %>%
  ggplot(aes(x = claim, y = prob)) + 
  geom_bar(stat = "identity",
           fill = "white", 
           col = "black") +
  geom_errorbar(aes(x = claim, 
                    ymin = prob - se,
                    ymax = prob + se,
                    width = 0.2)) + 
  scale_y_continuous(labels = scales::percent) + 
  labs(y = "Likelihood to be selected", x = "Claim", title = "Negative Primming & Cheap Package") + 
  theme_bw()
####==========================================================================####
# 5.3 Panel C: Positive + Expensive 
####==========================================================================###
C <- plot_df %>%
  filter(treatment == "positive" & price == "expensive") %>%
  mutate(claim = fct_reorder(claim, desc(prob))) %>%
  ggplot(aes(x = claim, y = prob)) + 
  geom_bar(stat = "identity",
           fill = "white", 
           col = "black") +
  geom_errorbar(aes(x = claim, 
                    ymin = prob - se,
                    ymax = prob + se,
                    width = 0.2)) + 
  scale_y_continuous(labels = scales::percent) + 
  labs(y = "Likelihood to be selected", x = "Claim", title = "Positive Primming & Expensive Package") + 
  theme_bw()


####==========================================================================####
# 5.4 Panel D: Positive + Expensive 
####==========================================================================###

D <- plot_df %>%
  filter(treatment == "positive" & price == "cheap") %>%
  mutate(claim = fct_reorder(claim, desc(prob))) %>%
  ggplot(aes(x = claim, y = prob)) + 
  geom_bar(stat = "identity",
           fill = "white", 
           col = "black") +
  geom_errorbar(aes(x = claim, 
                    ymin = prob - se,
                    ymax = prob + se,
                    width = 0.2)) + 
  scale_y_continuous(labels = scales::percent) + 
  labs(y = "Likelihood to be selected", x = "Claim", title = "Positive Primming & Cheap Package") + 
  theme_bw()

####==========================================================================####
# 5.5: Organize output
####==========================================================================###

ggarrange(A, B, C, D,
          labels = c("A", "B", "C", "D"),
          ncol = 2, nrow = 2) %>%
  ggsave(filename = "claims_ranking.png",
         width = 40, height = 27, units = "cm",
         dpi = 300,
         path = "./Plots")

####==========================================================================####
# 6. Text
####==========================================================================####
####==========================================================================####
# 6.1 Page 4 when buying travel insurance what do you think is the likelihood of you having to make a claim?
####==========================================================================####

df %>%
  select(country, q4, q5) %>%
  mutate(q4 = as.numeric(q4),
         q5 = as.numeric(q5)) %>%
  group_by(country) %>%
  summarise(
    mean_q4 = round(mean(q4),2),
    sd_q4 = round(sd(q4),2),
    mean_q5 = round(mean(q5),2),
    sd_q5 = round(sd(q5),2)) %>%
  flextable() %>% 
  theme_vanilla() %>%
  save_as_docx(., path = "./FlexTables/page4.docx")

df <- df %>% 
  mutate(q6_str = case_when(
    q6 == 1 ~ "That is what you would expect",
    q6 == 2 ~ "It is more than you would expect",
    q6 == 3 ~ "It is less than you would expect",
    TRUE~ "error"))

table1(~ q6_str | country, data = df)

####==========================================================================####
# 6.2 Page 7 Compraing between covid and precovid times
####==========================================================================####

df <- df %>%
  mutate(b2_2 = as.factor(b2_2),
         b2_3 = as.factor(b2_3))
label(df$b2_2) <- "If you compare your behaviour nowadays with pre-covid times would you be"
label(df$b2_3) <- "If you compare your behaviour nowadays with pre-covid times would it be"

table1(~ b2_2 + b2_3 | country, data = df)

####==========================================================================####
# 6.3 Page 9 Graph description
####==========================================================================####

plot_df %>%
  mutate(covid_related = ifelse(claim == "Hospitalization\nCosts for\nCovid19"| 
                                claim == "Visa required covid test"|
                                claim == "Curtailment cover"| 
                                claim == "Costs related to\nquarantine at\nthe arrival"|
                                claim == "Mandatory or\nrecommended\ncovid testing\nbefore travel", 1, 0)) %>%
  group_by(treatment, price, claim) %>%
  summarise(probability = prob,
            is_covid_related = sum(covid_related)) %>%
  arrange(treatment, price,
    desc(probability)) %>%
  mutate(total_covid_related = sum(is_covid_related),
        probability = paste0(round(probability,4)*100, "%")) %>% 
  flextable() %>%
  merge_v(j = c("treatment", "price", "total_covid_related")) %>%
  theme_vanilla() %>%
  save_as_docx(., path = "./FlexTables/graph_description.docx")

####==========================================================================####
# 7 Sentiment Analysis
####==========================================================================####

sent_df <- read_excel("./data/sentiment_data.xlsx")

####==========================================================================####
# 7.1 Is there a difference in the polarity of words by treatment? 
####==========================================================================####

sent_df %>%
  select(treatment, w1_polarity, w2_polarity, w3_polarity) %>%
  pivot_longer(cols = contains("_polarity"),
               names_to = "word_order",
               values_to = "polarity") %>%
  mutate(negative_priming = ifelse(treatment == "negative", 1,0)) %>%
  summarise(t_test = paste0(
    "t = ", round(t.test(polarity ~ treatment)$statistic,2),
    "\np = ", round(t.test(polarity ~ treatment)$p.value, 3)),
    correlation = paste0("rho = ",round(cor.test(polarity, negative_priming, method = "spearman", exact=FALSE)$estimate,2),
                         "\np = ", personal_pvalue(round(cor.test(polarity, negative_priming, method = "spearman", exact=FALSE)$p.value,3)))) %>%
  flextable() %>%
  theme_vanilla() %>% 
  save_as_docx(., path = "./FlexTables/page9_sent_rho.docx")

####==========================================================================####
# 7.1 Is there a difference in the polarity of words by treatment? 
####==========================================================================####

sent_df %>%
  select(treatment, w1_trans, w2_trans, w3_trans) %>% 
  pivot_longer(cols = contains("_trans"),
               names_to = "word_order",
               values_to = "words") %>%
  mutate(words = tolower(words),
    covid_related = ifelse(str_detect(words, "covid|virus|sick|death|corona|vaccine|pandemic"), 1,0)) %>%
  group_by(treatment) %>%
  summarise(covid_words = sum(covid_related)) %>%
  mutate(chi_test = paste0("Chi = ", round(chisq.test(covid_words)$statistic,2),
                           "\n(p = ", personal_pvalue(round(chisq.test(covid_words)$p.value,2)), ")")) %>%
  flextable() %>%
  merge_v(j = "chi_test") %>%
  theme_vanilla() %>%
  save_as_docx(., path = "./FlexTables/page9_sent_chi.docx")



