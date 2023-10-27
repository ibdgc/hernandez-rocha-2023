library(tidyverse)
library(naniar)
library(table1)
library(ggplot2)
library(ggpubr)
library(sjPlot)
library(lme4)
library(car)
library(ggalluvial)
library(dplyr)
library(lubridate)


rm(list=ls())
set.seed(1)

#####################################
# LOADING FORMATTED AND MERGED FILES
#####################################

setwd("/Users/cristian/Desktop/Rcharger/NIDDK")

dfep.all <- as.data.frame(readRDS("NIDDK_merged_data.rds"))

nrow(dfep.all)
nrow(distinct(dfep.all, consortium_id))
summary(dfep.all$Rutgeerts_bin)


summary(dfep.all$surg_findings)
dfep.all <- mutate(dfep.all,
                   penetrating_compl = case_when(
                     surg_findings == "Penetrating" ~ "Yes",
                     surg_findings == "Only_Stricturing" | surg_findings == "Others" ~ "No",
                     TRUE ~ NA_character_)
)
dfep.all$penetrating_compl <- as.factor(dfep.all$penetrating_compl)
summary(dfep.all$penetrating_compl)


###################################################
# LINEAR MIXED-EFFECTS MODEL FOR FIRST COLONOSCOPY
###################################################

###########################################################
# SELECTING FIRST COLONOSCOPIES AND VARIABLES FOR ANALYSIS
##########################################################

summary(dfep.all$redcap_event_name)

summary(dfep.all$grc.a)

dfep.first <- dfep.all %>% filter(redcap_event_name == "First_Endoscopy")

dfep.first <- subset(dfep.first, select = c("consortium_id", "grc.a", "country", "index_date", "age_surg", "sex",
                                          "race", "race_bin", "jewish", "hispanic",
                                          "smoking_presurg", "smoking_presurg_bin",
                                          "age_diag", "age_montreal", "duration", 
                                          "montreal_loc", "dis_behavior", "dis_behavior_bin", "disloc_peri",
                                          "surg_smbow", "surg_findings", "surg_findings2", "penetrating_compl",
                                          "other_sbow_res", "other_stric_plasty", "other_lbow_res",  
                                          "anastomosis_type", "sewn_stapled", "resection_length", "margin",
                                          "immuno", "immuno_1yr", "immuno_3mos",
                                          "mtx", "mtx_1yr", "mtx_3mos",
                                          "tnf_any", "tnf_any_1yr", "tnf_any_3mos",
                                          "infliximab", "infliximab_1yr", "infliximab_3mos",
                                          "infliximab_drug_level", "infliximab_antibody",
                                          "adalimumab", "adalimumab_1yr", "adalimumab_3mos",
                                          "adalimumab_drug_level", "adalimumab_antibody",
                                          "certolizumab", "certolizumab_1yr", "certolizumab_3mos",
                                          "certolizumab_drug_level", "certolizumab_antibody",
                                          "vedolizumab", "vedolizumab_1yr", "vedolizumab_3mos",
                                          "ustekinumab", "ustekinumab_1yr", "ustekinumab_3mos",
                                          "inter_surg_endo", "redcap_event_name", "redcap_event_name2", "endoscopy_date",
                                          "Rutgeerts", "Modified_Rutgeerts", 
                                          "Rutgeerts_bin", "Modified_Rutgeerts_bin",
                                          "Rutgeerts_bin2", "Rutgeerts_bin3",
                                          "anyster_since_last", "anyster_current_upd",
                                          "antibio_since_last", "antibio_current_upd",       
                                          "immuno_since_last", "immuno_current_upd",
                                          "mtx_since_last", "mtx_current_upd",
                                          "tnf_any_since_last", "tnf_any_current_upd",
                                          "infliximab_since_last", "infliximab_current_upd",
                                          "adalimumab_since_last", "adalimumab_current_upd",
                                          "certolizumab_since_last", "certolizumab_current_upd",
                                          "vedolizumab_since_last", "vedolizumab_current_upd",
                                          "ustekinumab_since_last", "ustekinumab_current_upd",
                                          "combo_since_last", "combo_current_upd",
                                          "no_meds_since_last", "no_meds_current_upd",
                                          "smoking_status"))


dfep.first <- dfep.first %>%
  mutate(across(where(is.factor), ~na_if(., "Unknown")))

dfep.first <- droplevels(dfep.first)

dfep.first.c <- dfep.first

#####################################################
# BIVARIATE ANALYSIS (RECURRENCE AT FIRST ENDOSCOPY)
#####################################################

pvalue <- function(x, ...) {
  # Construct vectors of data y, and groups (strata) g
  y <- unlist(x)
  g <- factor(rep(1:length(x), times=sapply(x, length)))
  if (is.numeric(y)) {
    # For numeric variables, perform a standard 2-sample t-test
    p <- kruskal.test(y ~ g)$p.value
  } else {
    # For categorical variables, perform a chi-squared test of independence
    p <- chisq.test(table(y, g))$p.value
  }
  # Format the p-value, using an HTML entity for the less-than sign.
  # The initial empty string places the output on the line below the variable label.
  c("", sub("<", "&lt;", format.pval(p, digits=3, eps=0.001)))
}


table1(~ Rutgeerts_bin + Rutgeerts, data= dfep.first.c,
       topclass="Rtable1-grid Rtable1-shade Rtable1-times", overall = "Total",
       render.continuous = c(.="median [Q1 - Q3]"))


table1(~ race, data= dfep.first.c,
       topclass="Rtable1-grid Rtable1-shade Rtable1-times", overall = "Total",
       render.continuous = c(.="median [Q1 - Q3]"))


table1(~ other_sbow_res + other_lbow_res + other_stric_plasty, data= dfep.first.c,
       topclass="Rtable1-grid Rtable1-shade Rtable1-times", overall = "Total",
       render.continuous = c(.="median [Q1 - Q3]"))


#######################################################
# TABLE 1 (Demographic, presurgical and surgical data)
#######################################################

hist(dfep.first.c$age_surg)
shapiro.test(dfep.first.c$age_surg)


table1(~ age_surg + sex + country + race_bin + jewish + hispanic + smoking_presurg, data= dfep.first.c,
       topclass="Rtable1-grid Rtable1-shade Rtable1-times", overall = "Total",
       render.continuous = c(.="median [Q1 - Q3]"))


table1(~ age_surg + sex + country + race_bin + jewish + hispanic + smoking_presurg | Rutgeerts_bin, data= dfep.first.c,
       topclass="Rtable1-grid Rtable1-shade Rtable1-times", overall = F, extra.col = list("P-value" = pvalue),
       render.continuous = c(.="median [Q1 - Q3]"))


table1(~ other_sbow_res + other_lbow_res + other_stric_plasty| Rutgeerts_bin, data= dfep.first.c,
       topclass="Rtable1-grid Rtable1-shade Rtable1-times", overall = F, extra.col = list("P-value" = pvalue),
       render.continuous = c(.="median [Q1 - Q3]"))


hist(dfep.first.c$duration)
shapiro.test(dfep.first.c$duration)


table1(~  age_diag + duration + 
         montreal_loc + dis_behavior + disloc_peri + surg_smbow, data= dfep.first.c,
       topclass="Rtable1-grid Rtable1-shade Rtable1-times", overall = "Total",
       render.continuous = c(.="median [Q1 - Q3]"))


table1(~ age_diag + duration + 
         montreal_loc + dis_behavior + penetrating_compl + disloc_peri + surg_smbow | Rutgeerts_bin, data= dfep.first.c,
       topclass="Rtable1-grid Rtable1-shade Rtable1-times", overall = F, extra.col = list("P-value" = pvalue),
       render.continuous = c(.="median [Q1 - Q3]"))


ggscatter(dfep.first.c, x = "age_diag", y = "duration", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman")

cor.test(dfep.first.c$age_diag, dfep.first.c$duration, 
                  method = "spearman")


table1(~ surg_findings2 + anastomosis_type +sewn_stapled + resection_length + margin, data= dfep.first.c,
       topclass="Rtable1-grid Rtable1-shade Rtable1-times", overall = "Total",
       render.continuous = c(.="median [Q1 - Q3]"))


table1(~ surg_findings2 + anastomosis_type + sewn_stapled + resection_length + margin | Rutgeerts_bin, data= dfep.first.c,
       topclass="Rtable1-grid Rtable1-shade Rtable1-times", overall = F, extra.col = list("P-value" = pvalue),
       render.continuous = c(.="median [Q1 - Q3]"))


table1(~  immuno + mtx + tnf_any, data= dfep.first.c,
       topclass="Rtable1-grid Rtable1-shade Rtable1-times", overall = "Total",
       render.continuous = c(.="median [Q1, Q3]"))

table1(~  immuno + mtx + tnf_any | Rutgeerts_bin, data= dfep.first.c,
       topclass="Rtable1-grid Rtable1-shade Rtable1-times", overall = F, extra.col = list("P-value" = pvalue),
       render.continuous = c(.="median [Q1, Q3]"))


table1(~  infliximab + adalimumab + certolizumab, data= dfep.first.c,
       topclass="Rtable1-grid Rtable1-shade Rtable1-times", overall = "Total",
       render.continuous = c(.="median [Q1, Q3]"))


table1(~  infliximab + adalimumab + certolizumab | Rutgeerts_bin, data= dfep.first.c,
       topclass="Rtable1-grid Rtable1-shade Rtable1-times", overall = F, extra.col = list("P-value" = pvalue),
       render.continuous = c(.="median [Q1, Q3]"))


table1(~ vedolizumab + ustekinumab, data= dfep.first.c,
       topclass="Rtable1-grid Rtable1-shade Rtable1-times", overall = "Total", 
       render.continuous = c(.="median [Q1, Q3]"))


table1(~ vedolizumab + ustekinumab | Rutgeerts_bin, data= dfep.first.c,
       topclass="Rtable1-grid Rtable1-shade Rtable1-times", overall = F, extra.col = list("P-value" = pvalue),
       render.continuous = c(.="median [Q1, Q3]"))


##############################
# TABLE 2 (Postsurgical data)
##############################

summary(dfep.all$redcap_event_name)


table1(~  inter_surg_endo | redcap_event_name, data= dfep.all,
       topclass="Rtable1-grid Rtable1-shade Rtable1-times", overall = "Total",
       render.continuous = c(.="median [Q1, Q3]"))


table1(~  inter_surg_endo + smoking_status + antibio_current_upd + anyster_current_upd, data= dfep.first.c,
       topclass="Rtable1-grid Rtable1-shade Rtable1-times", overall = "Total",
       render.continuous = c(.="median [Q1, Q3]"))


table1(~  inter_surg_endo + smoking_status + antibio_current_upd + anyster_current_upd | Rutgeerts_bin, data= dfep.first.c,
       topclass="Rtable1-grid Rtable1-shade Rtable1-times", overall = F, extra.col = list("P-value" = pvalue),
       render.continuous = c(.="median [Q1, Q3]"))

dfep.first.c <- mutate(dfep.first.c,
                       inter_surg_endo_cat = case_when(
                         inter_surg_endo < 6 ~ "less_6",
                         inter_surg_endo >= 6 & inter_surg_endo < 12 ~ "bet_6_12",
                         inter_surg_endo >= 12 ~ "greater_12",
                         TRUE ~ as.character("No")))

dfep.first.c$inter_surg_endo_cat <- as.factor(dfep.first.c$inter_surg_endo_cat)
summary(dfep.first.c$inter_surg_endo_cat)

dfep.first.c$inter_surg_endo_cat <- relevel(dfep.first.c$inter_surg_endo_cat, ref = "less_6")  

table1(~  inter_surg_endo_cat, data= dfep.first.c,
       topclass="Rtable1-grid Rtable1-shade Rtable1-times", overall = "Total",
       render.continuous = c(.="median [Q1, Q3]"))

table1(~  inter_surg_endo_cat | Rutgeerts_bin, data= dfep.first.c,
       topclass="Rtable1-grid Rtable1-shade Rtable1-times", overall = F, extra.col = list("P-value" = pvalue),
       render.continuous = c(.="median [Q1, Q3]"))

table1(~  Rutgeerts_bin | inter_surg_endo_cat, data= dfep.first.c,
       topclass="Rtable1-grid Rtable1-shade Rtable1-times", overall = F, extra.col = list("P-value" = pvalue),
       render.continuous = c(.="median [Q1, Q3]"))


table1(~  Rutgeerts_bin3 | inter_surg_endo_cat, data= dfep.first.c,
       topclass="Rtable1-grid Rtable1-shade Rtable1-times", overall = F, extra.col = list("P-value" = pvalue),
       render.continuous = c(.="median [Q1, Q3]"))


table1(~ immuno_current_upd + mtx_current_upd, data= dfep.first.c,
       topclass="Rtable1-grid Rtable1-shade Rtable1-times", overall = "Total",
       render.continuous = c(.="median [Q1, Q3]"))


table1(~ immuno_current_upd +  mtx_current_upd | Rutgeerts_bin, data= dfep.first.c,
       topclass="Rtable1-grid Rtable1-shade Rtable1-times", overall = F, extra.col = list("P-value" = pvalue),
       render.continuous = c(.="median [Q1, Q3]"))

table1(~  tnf_any_current_upd, data= dfep.first.c,
       topclass="Rtable1-grid Rtable1-shade Rtable1-times", overall = "Total",
       render.continuous = c(.="median [Q1, Q3]"))

table1(~  tnf_any_current_upd | Rutgeerts_bin, data= dfep.first.c,
       topclass="Rtable1-grid Rtable1-shade Rtable1-times", overall = F, extra.col = list("P-value" = pvalue),
       render.continuous = c(.="median [Q1, Q3]"))

table1(~  infliximab_current_upd + adalimumab_current_upd + certolizumab_current_upd, data= dfep.first.c,
       topclass="Rtable1-grid Rtable1-shade Rtable1-times", overall = "Total",
       render.continuous = c(.="median [Q1, Q3]"))

table1(~  infliximab_current_upd + adalimumab_current_upd + certolizumab_current_upd | Rutgeerts_bin, data= dfep.first.c,
       topclass="Rtable1-grid Rtable1-shade Rtable1-times", overall = F, extra.col = list("P-value" = pvalue),
       render.continuous = c(.="median [Q1, Q3]"))

table1(~  combo_current_upd, data= dfep.first.c,
       topclass="Rtable1-grid Rtable1-shade Rtable1-times", overall = "Total",
       render.continuous = c(.="median [Q1, Q3]"))


dfep.first.c <- mutate(dfep.first.c,
              mono_immuno_current_upd = case_when(
                combo_current_upd == "Mono_immuno" ~ "Yes",
                TRUE ~ as.character("No")))

dfep.first.c$mono_immuno_current_upd <- as.factor(dfep.first.c$mono_immuno_current_upd)
summary(dfep.first.c$mono_immuno_current_upd)


dfep.first.c <- mutate(dfep.first.c,
                       mono_mtx_current_upd = case_when(
                         combo_current_upd == "Mono_mtx" ~ "Yes",
                         TRUE ~ as.character("No")))

dfep.first.c$mono_mtx_current_upd <- as.factor(dfep.first.c$mono_mtx_current_upd)
summary(dfep.first.c$mono_mtx_current_upd)

dfep.first.c <- mutate(dfep.first.c,
                       mono_tnf_current_upd = case_when(
                         combo_current_upd == "Mono_TNF" ~ "Yes",
                         TRUE ~ as.character("No")))

dfep.first.c$mono_tnf_current_upd <- as.factor(dfep.first.c$mono_tnf_current_upd)
summary(dfep.first.c$mono_tnf_current_upd)


dfep.first.c <- mutate(dfep.first.c,
                       combo_tnf_current_upd = case_when(
                         combo_current_upd == "Combo" ~ "Yes",
                         TRUE ~ as.character("No")))

dfep.first.c$combo_tnf_current_upd <- as.factor(dfep.first.c$combo_tnf_current_upd)
summary(dfep.first.c$combo_tnf_current_upd)


table1(~  mono_immuno_current_upd + mono_mtx_current_upd + mono_tnf_current_upd + combo_tnf_current_upd, data= dfep.first.c,
       topclass="Rtable1-grid Rtable1-shade Rtable1-times", overall = "Total",
       render.continuous = c(.="median [Q1, Q3]"))

table1(~  mono_immuno_current_upd + mono_mtx_current_upd + mono_tnf_current_upd + combo_tnf_current_upd | Rutgeerts_bin, data= dfep.first.c,
       topclass="Rtable1-grid Rtable1-shade Rtable1-times", overall = F, extra.col = list("P-value" = pvalue),
       render.continuous = c(.="median [Q1, Q3]"))



table1(~ vedolizumab_current_upd +  ustekinumab_current_upd, data= dfep.first.c,
       topclass="Rtable1-grid Rtable1-shade Rtable1-times", overall = "Total",
       render.continuous = c(.="median [Q1, Q3]"))


table1(~ vedolizumab_current_upd + ustekinumab_current_upd | Rutgeerts_bin, data= dfep.first.c,
       topclass="Rtable1-grid Rtable1-shade Rtable1-times", overall = F, extra.col = list("P-value" = pvalue),
       render.continuous = c(.="median [Q1, Q3]"))



#######################################
# LOGISTIC REGRESSION (FIRST ENDOSCOPY)
######################################

dfep.first.c$Rutgeerts_bin.num <- ifelse(dfep.first.c$Rutgeerts_bin == "i0_i1", 0, 1)

table(dfep.first.c$Rutgeerts_bin)

# Variables included a priori

summary(dfep.first.c$sex)

summary(dfep.first.c$race_bin)

summary(dfep.first.c$age_diag)

summary(dfep.first.c$surg_smbow)

summary(dfep.first.c$age_surg)

summary(dfep.first.c$margin)

summary(dfep.first.c$inter_surg_endo)

summary(dfep.first.c$smoking_status)

summary(dfep.first.c$tnf_any_current_upd)


# Additional variables with p < 0.1 in the univariable analysis

ggscatter(dfep.first.c, x = "age_surg", y = "duration", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman")

ggscatter(dfep.first.c, x = "age_diag", y = "duration", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman")

ggscatter(dfep.first.c, x = "age_diag", y = "age_surg", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman")

ggscatter(dfep.first.c, x = "age_surg", y = "inter_surg_endo", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman")

ggscatter(dfep.first.c, x = "age_diag", y = "inter_surg_endo", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman")

ggscatter(dfep.first.c, x = "duration", y = "inter_surg_endo", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman")


#################
# Model Fitting
#################

dfep.first.c$race_bin <- relevel(dfep.first.c$race_bin, ref = "White")  

# Model without GRC
m.glm = glm(Rutgeerts_bin.num ~ sex + race_bin + age_diag + duration +
              disloc_peri + penetrating_compl +
              surg_smbow + margin + resection_length + inter_surg_endo + 
              smoking_status + tnf_any_current_upd + ustekinumab_current_upd,
                 data = dfep.first.c, family = binomial) 

summary(m.glm)

tab_model(m.glm, show.aic = T)


# Model with GRC as fixed effect

m.glm.grc = glm(Rutgeerts_bin.num ~ sex + race_bin + age_diag + duration +
                  disloc_peri + penetrating_compl +
                  surg_smbow + margin + resection_length + inter_surg_endo + 
                  smoking_status + tnf_any_current_upd + ustekinumab_current_upd +
                  grc.a,
            data = dfep.first.c, family = binomial) 

summary(m.glm.grc)

tab_model(m.glm.grc, show.aic = T)


############################################
# TABLE 3 (Model with GRC as random effect)
###########################################

m.glmer = glmer(Rutgeerts_bin.num ~ sex + race_bin + age_diag + duration +
                  disloc_peri + penetrating_compl +
                  surg_smbow + margin + resection_length + inter_surg_endo + 
                  smoking_status + tnf_any_current_upd + ustekinumab_current_upd +
                  (1|grc.a),
                data = dfep.first.c, family = binomial) 

summary(m.glmer)

tab_model(m.glmer, show.aic = T, digits = 2)

isSingular(m.glmer)

ifelse(max(car::vif(m.glmer)) <= 10,  "VIFs okay", "WARNING: high VIFs!")


#############################################
# ANTI-TNF AND MULTIPLE LOGISTIC REGRESSION
############################################

# Mono vs combo and different type of anti-TNF

summary(dfep.first.c$combo_tnf_current_upd)

m.glmer = glmer(Rutgeerts_bin.num ~  sex + race_bin + age_diag + duration + disloc_peri + penetrating_compl +
                  surg_smbow + margin + resection_length + inter_surg_endo +
                  smoking_status + combo_tnf_current_upd + ustekinumab_current_upd  +
                  + (1|grc.a),
                data = dfep.first.c, family = binomial) 
summary(m.glmer)

tab_model(m.glmer, show.aic = T)


m.glmer = glmer(Rutgeerts_bin.num ~  sex + race_bin + age_diag + duration + disloc_peri + penetrating_compl +
                  surg_smbow + margin + resection_length + inter_surg_endo +
                  smoking_status + mono_tnf_current_upd + ustekinumab_current_upd + 
                  (1|grc.a),
                data = dfep.first.c, family = binomial) 
summary(m.glmer)

tab_model(m.glmer, show.aic = T)


dfep.first.c.ifx <- dfep.first.c %>% filter(adalimumab_current_upd != "Yes" & certolizumab_current_upd != "Yes")

m.glmer = glmer(Rutgeerts_bin.num ~ sex + race_bin + age_diag + duration + disloc_peri + penetrating_compl +
                  surg_smbow + margin + resection_length + inter_surg_endo +
                  smoking_status + infliximab_current_upd + ustekinumab_current_upd +
                  (1|grc.a),
                data = dfep.first.c.ifx, family = binomial) 
summary(m.glmer)

tab_model(m.glmer, show.aic = T)


dfep.first.c.ada <- dfep.first.c %>% filter(infliximab_current_upd != "Yes" & certolizumab_current_upd != "Yes")

m.glmer = glmer(Rutgeerts_bin.num ~ sex + race_bin + age_diag + duration + disloc_peri + penetrating_compl +
                  surg_smbow + margin + resection_length + inter_surg_endo +
                  smoking_status + adalimumab_current_upd + ustekinumab_current_upd + (1|grc.a),
                data = dfep.first.c.ada, family = binomial) 
summary(m.glmer)

tab_model(m.glmer, show.aic = T)


dfep.first.c.ctz <- dfep.first.c %>% filter(infliximab_current_upd != "Yes" & adalimumab_current_upd != "Yes")

m.glmer = glmer(Rutgeerts_bin.num ~ sex + race_bin + age_diag + duration + disloc_peri + penetrating_compl +
                  surg_smbow + margin + resection_length + inter_surg_endo +
                  smoking_status + certolizumab_current_upd + ustekinumab_current_upd + (1|grc.a),
                data = dfep.first.c.ctz, family = binomial) 
summary(m.glmer)

tab_model(m.glmer, show.aic = T)


# Anti-TNF experienced vs naive 

summary(dfep.first.c$tnf_any)

dfep.first.c.naive <- dfep.first.c %>% filter(tnf_any == "No")

summary(dfep.first.c.naive$tnf_any_current_upd)

summary(dfep.first.c.naive$Rutgeerts_bin)

m.glmer = glmer(Rutgeerts_bin.num ~ sex + race_bin + age_diag + duration + disloc_peri + penetrating_compl +
                  surg_smbow + margin + resection_length + inter_surg_endo +
                  smoking_status + tnf_any_current_upd + ustekinumab_current_upd + (1|grc.a),
                data = dfep.first.c.naive, family = binomial) 
summary(m.glmer)

tab_model(m.glmer, show.aic = T)


table1(~ Rutgeerts_bin | tnf_any_current_upd, data= dfep.first.c.naive,
       topclass="Rtable1-grid Rtable1-shade Rtable1-times", overall = F, extra.col = list("P-value" = pvalue),
       render.continuous = c(.="median [Q1 - Q3]"))



summary(dfep.first.c$tnf_any)

dfep.first.c.exp <- dfep.first.c %>% filter(tnf_any == "Yes")

summary(dfep.first.c.exp$tnf_any_current_upd)

summary(dfep.first.c.exp$Rutgeerts_bin)

m.glmer = glmer(Rutgeerts_bin.num ~ sex + race_bin + age_diag + duration + disloc_peri + penetrating_compl +
                  surg_smbow + margin + resection_length + inter_surg_endo +
                  smoking_status + tnf_any_current_upd + ustekinumab_current_upd +
                  (1|grc.a),
                data = dfep.first.c.exp, family = binomial) 
summary(m.glmer)

tab_model(m.glmer, show.aic = T)


#######################################################
# ANALYZING DIFFERENCES BETWEEN VARIABLES DISTRIBUTION
#######################################################

##############################
# SUPPLEMENTARY TABLES (S1-S6)
##############################


# S1
table1(~ age_surg +  race_bin + 
         age_diag + duration + dis_behavior + disloc_peri + surg_smbow +
         penetrating_compl + resection_length + margin + inter_surg_endo +
         inter_surg_endo_cat +  smoking_status + tnf_any_current_upd + combo_tnf_current_upd +
         ustekinumab_current_upd | sex, data= dfep.first.c,
       topclass="Rtable1-grid Rtable1-shade Rtable1-times", overall = F, extra.col = list("P-value" = pvalue),
       render.continuous = c(.="median [Q1, Q3]"))


#S2
table1(~ age_surg +  sex + 
         age_diag + duration + dis_behavior + disloc_peri + surg_smbow +
         penetrating_compl + resection_length + margin + inter_surg_endo +
         inter_surg_endo_cat +  smoking_status + tnf_any_current_upd + combo_tnf_current_upd +
         ustekinumab_current_upd  | race_bin, data= dfep.first.c,
       topclass="Rtable1-grid Rtable1-shade Rtable1-times", overall = F, extra.col = list("P-value" = pvalue),
       render.continuous = c(.="median [Q1, Q3]"))

#S3
table1(~ age_surg + sex + race_bin + 
         age_diag + duration + dis_behavior + disloc_peri + surg_smbow +
         penetrating_compl + resection_length + margin + inter_surg_endo +
         inter_surg_endo_cat + tnf_any_current_upd + combo_tnf_current_upd +
         ustekinumab_current_upd  | smoking_status, data= dfep.first.c,
       topclass="Rtable1-grid Rtable1-shade Rtable1-times", overall = F, extra.col = list("P-value" = pvalue),
       render.continuous = c(.="median [Q1, Q3]"))


#S4
table1(~ age_surg + sex + race_bin + 
         age_diag + duration + dis_behavior + disloc_peri + surg_smbow +
         penetrating_compl + resection_length + margin + inter_surg_endo +
         inter_surg_endo_cat +  smoking_status +
         ustekinumab_current_upd | tnf_any_current_upd, data= dfep.first.c,
       topclass="Rtable1-grid Rtable1-shade Rtable1-times", overall = F, extra.col = list("P-value" = pvalue),
       render.continuous = c(.="median [Q1, Q3]"))


#S5
table1(~ age_surg +  sex + race_bin + 
         age_diag + duration + dis_behavior + disloc_peri + surg_smbow +
         penetrating_compl + resection_length + margin + 
         smoking_status + tnf_any_current_upd + combo_tnf_current_upd +
         ustekinumab_current_upd | inter_surg_endo_cat, data= dfep.first.c,
       topclass="Rtable1-grid Rtable1-shade Rtable1-times", overall = F, extra.col = list("P-value" = pvalue),
       render.continuous = c(.="median [Q1, Q3]"))


#S6
table1(~ age_surg + sex + race_bin + 
         age_diag + duration + dis_behavior + disloc_peri + surg_smbow +
         penetrating_compl + resection_length + margin + inter_surg_endo +
         inter_surg_endo_cat +  smoking_status + tnf_any_current_upd + combo_tnf_current_upd +
         ustekinumab_current_upd | tnf_any, data= dfep.first.c,
       topclass="Rtable1-grid Rtable1-shade Rtable1-times", overall = F, extra.col = list("P-value" = pvalue),
       render.continuous = c(.="median [Q1, Q3]"))



####################################
# SENSITIVITY AND SUBGROUP ANALYSIS
####################################

#######################
# DATE OF SURGERY 
#######################

summary(dfep.first.c$index_date)

dfep.first.c$index_year <- as.numeric(format(dfep.first.c$index_date, format="%Y"))

summary(dfep.first.c$index_year)


dfep.first.c <- mutate(dfep.first.c,
                       index_time = case_when(
                         index_year <= 2016 ~ "early",
                         index_year > 2016 ~ "late",
                         TRUE ~ NA_character_)
)
dfep.first.c$index_time <- as.factor(dfep.first.c$index_time)
summary(dfep.first.c$index_time)


table1(~  Rutgeerts_bin + age_surg +  sex + race_bin + 
         age_diag + surg_smbow + resection_length +
         dis_behavior + margin + duration + disloc_peri + penetrating_compl + surg_findings2 +
         inter_surg_endo + inter_surg_endo_cat +  smoking_status + tnf_any_current_upd + combo_tnf_current_upd +
         ustekinumab_current_upd + vedolizumab_current_upd | index_time, data= dfep.first.c,
       topclass="Rtable1-grid Rtable1-shade Rtable1-times", overall = F, extra.col = list("P-value" = pvalue),
       render.continuous = c(.="median [Q1, Q3]"))



# Model with GRC as random effect

m.glmer = glmer(Rutgeerts_bin.num ~ sex + race_bin + duration +
                  dis_behavior_bin + index_time +
                  surg_smbow + margin + inter_surg_endo + 
                  smoking_status + tnf_any_current_upd + ustekinumab_current_upd + index_time +
                  (1|grc.a),
                data = dfep.first.c, family = binomial) 
summary(m.glmer)

tab_model(m.glmer, show.aic = T, digits = 2)

isSingular(m.glmer)

ifelse(max(car::vif(m.glmer)) <= 10,  "VIFs okay", "WARNING: high VIFs!")



###############################################################################################################################################################################
# LOGISTIC REGRESSION OF FIRST COLONOSCOPY (INCLUDING ONLY PATIENTS WITH FIRST ENDO < 12 MONTHS)
#########################################################################################################

summary(dfep.first.c$inter_surg_endo)

length(which(dfep.first.c$inter_surg_endo > 12))

dfep.first.c.12.range <- dfep.first.c %>% filter(inter_surg_endo <= 12)

table(dfep.first.c.12.range$Rutgeerts_bin)


m.glmer.12.range = glmer(Rutgeerts_bin.num ~ sex + race_bin + age_diag + duration + disloc_peri + penetrating_compl +
                           surg_smbow + margin + resection_length + inter_surg_endo +
                           smoking_status + tnf_any_current_upd + ustekinumab_current_upd  +
                           (1|grc.a),
                         data = dfep.first.c.12.range, family = binomial) 
summary(m.glmer.12.range)

tab_model(m.glmer.12.range, show.aic = T)


##########################################################################################
# LOGISTIC REGRESSION OF FIRST COLONOSCOPY (INCLUDING ONLY PATIENTS WITH FIRST RESECTION)
###########################################################################################

summary(dfep.first.c$surg_smbow)

dfep.first.c.first <- dfep.first.c %>% filter(surg_smbow == "No")

table(dfep.first.c.first$Rutgeerts_bin)


m.glmer.first = glmer(Rutgeerts_bin.num ~ sex + race_bin + age_diag + duration + disloc_peri + penetrating_compl +
                           margin + resection_length + inter_surg_endo +
                           smoking_status + tnf_any_current_upd + ustekinumab_current_upd  +
                           (1|grc.a),
                         data = dfep.first.c.first, family = binomial) 
summary(m.glmer.first)

tab_model(m.glmer.first, show.aic = T)



#################################################################
# LOGISTIC REGRESSION OF FIRST COLONOSCOPY (MODIFIED RUTGEERTS)
#################################################################

summary(dfep.first.c$Rutgeerts_bin)

summary(dfep.first.c$Modified_Rutgeerts_bin)

dfep.first.c$Modified_Rutgeerts_bin.num <- ifelse(dfep.first.c$Modified_Rutgeerts_bin == "i0_i2a", 0, 1)

table(dfep.first.c$Rutgeerts_bin)

table(dfep.first.c$Modified_Rutgeerts_bin)

table1(~ Modified_Rutgeerts_bin, data= dfep.first.c,
       topclass="Rtable1-grid Rtable1-shade Rtable1-times", overall = "Total",
       render.continuous = c(.="median [Q1, Q3]"))


m.glmer.mod = glmer(Modified_Rutgeerts_bin.num ~ sex + race_bin + age_diag + duration + disloc_peri + penetrating_compl +
                      surg_smbow + margin + resection_length + inter_surg_endo +
                      smoking_status + tnf_any_current_upd + ustekinumab_current_upd  + (1|grc.a),
                data = dfep.first.c, family = binomial) 
summary(m.glmer.mod)

tab_model(m.glmer.mod, show.aic = T, digits = 2)


###############################################################################
# LOGISTIC REGRESSION OF FIRST COLONOSCOPY (STRICTER DEFINITION OF REMISSION)
###############################################################################

summary(dfep.first.c$Rutgeerts_bin2)

dfep.first.c$Rutgeerts_bin2.num <- ifelse(dfep.first.c$Rutgeerts_bin2 == "i0", 0, 1)

table(dfep.first.c$Rutgeerts_bin)

table(dfep.first.c$Rutgeerts_bin2)


m.glmer.strict = glmer(Rutgeerts_bin2.num ~ sex + race_bin + age_diag + duration + disloc_peri + penetrating_compl +
                         surg_smbow + margin + resection_length + inter_surg_endo +
                         smoking_status + tnf_any_current_upd + ustekinumab_current_upd + (1|grc.a),
                    data = dfep.first.c, family = binomial)  
summary(m.glmer.strict)

tab_model(m.glmer.strict, show.aic = T)


##############################################################
# LOGISTIC REGRESSION OF FIRST COLONOSCOPY (SEVERE RECURRENCE)
##############################################################

summary(dfep.first.c$Rutgeerts_bin3)

dfep.first.c$Rutgeerts_bin3.num <- ifelse(dfep.first.c$Rutgeerts_bin3 == "i0_i2", 0, 1)

table(dfep.first.c$Rutgeerts_bin)

table(dfep.first.c$Rutgeerts_bin3)

m.glmer.sev = glmer(Rutgeerts_bin3.num ~ sex + race_bin + age_diag + duration + disloc_peri + penetrating_compl +
                      surg_smbow + margin + resection_length + inter_surg_endo +
                      smoking_status + tnf_any_current_upd + ustekinumab_current_upd + (1|grc.a),
                    data = dfep.first.c, family = binomial) 
summary(m.glmer.sev)

tab_model(m.glmer.sev, show.aic = T, digits = 2)


##################################################################################################
# LOGISTIC REGRESSION OF FIRST COLONOSCOPY (INCLUDING ONLY PATIENT ON NO MEDICATION AFTER SURGERY)
##################################################################################################

summary(dfep.first.c$immuno_current_upd)
summary(dfep.first.c$mtx_current_upd)
summary(dfep.first.c$tnf_any_current_upd)
summary(dfep.first.c$vedolizumab_current_upd)
summary(dfep.first.c$ustekinumab_current_upd)

summary(dfep.first.c$no_meds_current_upd)

dfep.first.c.no.meds <- dfep.first.c %>% filter(immuno_current_upd == "No" & mtx_current_upd == "No" &
                                                  tnf_any_current_upd == "No" & vedolizumab_current_upd == "No" &
                                                  ustekinumab_current_upd == "No")

table(dfep.first.c.no.meds$Rutgeerts_bin.num)

dfep.first.c.no.meds <- dfep.first.c %>% filter(no_meds_current_upd == "Yes")

table(dfep.first.c.no.meds$Rutgeerts_bin.num)

summary(dfep.first.c.no.meds$sex)
summary(dfep.first.c.no.meds$race_bin)
summary(dfep.first.c.no.meds$age_montreal)
summary(dfep.first.c.no.meds$surg_smbow)
summary(dfep.first.c.no.meds$dis_behavior_bin)
summary(dfep.first.c.no.meds$inter_surg_endo)
summary(dfep.first.c.no.meds$smoking_status)
summary(dfep.first.c.no.meds$grc.a)

m.glmer.nomeds = glmer(Rutgeerts_bin.num ~ sex + race_bin + age_diag + duration + disloc_peri + penetrating_compl +
                         surg_smbow + margin + resection_length + inter_surg_endo +
                         smoking_status + (1|grc.a),
                       data = dfep.first.c.no.meds, family = binomial) 
summary(m.glmer.nomeds)

tab_model(m.glmer.nomeds, show.aic = T, digits = 2)



######################################
# ANALYZING SUBSEQUENT COLONOSCOPIES
######################################

summary(dfep.all$redcap_event_name)

summary(dfep.all$Rutgeerts_bin)

table1(~ Rutgeerts_bin | redcap_event_name, data= dfep.all,
       topclass="Rtable1-grid Rtable1-shade Rtable1-times", overall = "Total",
       render.continuous = c(.="median [Q1 - Q3]"))

summary(dfep.all$Rutgeerts)

###########################
# FIGURE 1 (Alluvial Plot)
###########################

dfep.all.alluv <- dfep.all

summary(dfep.all.alluv$Rutgeerts)

dfep.all.alluv$Rutgeerts2 <- plyr::revalue(dfep.all.alluv$Rutgeerts, 
                                           c("i4" = "i3-i4", "i3" = "i3-i4",
                                             "i2" = "i2", "i1" = "i1", "i0" = "i0"))

dfep.all.alluv$Rutgeerts2 <- factor(dfep.all.alluv$Rutgeerts2, 
                                          levels = c("i3-i4", "i2", "i1", "i0"))


summary(dfep.all.alluv$Rutgeerts2)


g.alluv <- ggplot(dfep.all.alluv %>% filter(redcap_event_name2 == "First_Endoscopy" |
                                              redcap_event_name2 == "Second_Endoscopy"),
                  aes(x = redcap_event_name2, stratum = Rutgeerts2,
                      alluvium = consortium_id, fill = Rutgeerts2)) +
  scale_fill_manual(values = c("#993404", "#FED976", 
                               "#52854C", "#C3D7A4")) +
  scale_x_discrete(labels= c("First colonoscopy\n(n = 365)", "Second colonoscopy\n(n = 208)")) +
  labs(x = "", y = "Number of patients", fill = "Rutgeerts score") +
  scale_y_continuous(breaks=seq(0,400,50)) +
  geom_flow() +
  geom_stratum(alpha = .7) + 
  geom_text(stat = "stratum", aes(label = paste0(after_stat(n), " ", "(", round(after_stat(prop)*100, 1),"%)"))) +
  geom_text(stat = "flow", nudge_x = 0.21, size = 3,  aes(label = after_stat(n),
                                                          hjust = (after_stat(flow) == "from"))) +
  theme(legend.position = "bottom",
        panel.background = element_rect(fill = "white"),
        axis.title.x = element_blank(), 
        axis.text.x = element_text(size = 18), 
        axis.title.y =element_text(size = 18),
        axis.text.y = element_text(size = 18), 
        legend.text = element_text(size = 18),
        legend.title = element_text(size = 18),
        axis.line = element_line(colour = "black", size = 1, linetype = "solid"))

#setwd("/Users/cristian/Library/CloudStorage/Dropbox/Temas\ gastroenterología/Inflammatory\ Bowel\ Diseases/Postoperative\ recurrence\ of\ Crohn\'s\ disease/NIDDK\ Ileal\ CD\ Post\ Op/Clinical\ predictors\ of\ postoperative\ CD\ recurrence/Manuscript/JCC_submission/Revised_versionJCC")

# Figure 1
#tiff("Figure 1.tiff", units="in", width=12, height=8, res=300)

g.alluv

#dev.off()



########################
# SECOND COLONOSCOPIES
########################

# Selecting only patients with no recurrence at first colonoscopy
dfep.all2 <- dfep.all %>% filter(!(Rutgeerts_bin == "i2_i4" & redcap_event_name == "First_Endoscopy"))

no_rec <- dfep.all %>% filter(Rutgeerts_bin == "i2_i4" & redcap_event_name == "First_Endoscopy")

dfep.all2 <- dfep.all2[!(dfep.all2$consortium_id %in% no_rec$consortium_id),]

summary(dfep.all2$redcap_event_name)

dfep.all2 <- dfep.all2 %>% filter(redcap_event_name == "Second_Endoscopy")

dfep.all2 <- droplevels(dfep.all2)


table1(~ Rutgeerts_bin | redcap_event_name2, data= dfep.all2,
       topclass="Rtable1-grid Rtable1-shade Rtable1-times", overall = "Total",
       render.continuous = c(.="median [Q1 - Q3]"))


dfep.first.c.rut <- subset(dfep.first.c, select = c(1,64:67,82))

colnames(dfep.first.c.rut) <- paste0(colnames(dfep.first.c.rut), ".f", sep = "")

dfep.all2 <- dfep.all2 %>% left_join(dfep.first.c.rut, by = c("consortium_id" = "consortium_id.f"))

summary(dfep.all2$endoscopy_date) 

dfep.all2$endoscopy_date <- as.Date(dfep.all2$endoscopy_date, format="%Y-%m-%d")

summary(dfep.all2$endoscopy_date.f) 

dfep.all2$endoscopy_date.f <- as.Date(dfep.all2$endoscopy_date.f, format="%Y-%m-%d")

dfep.all2$inter_endo_endo <- interval(dfep.all2$endoscopy_date.f, dfep.all2$endoscopy_date) %/% months(1) 

summary(dfep.all2$inter_endo_endo)

##############################################
# SUPPLEMENTARY TABLE S7-S8 (second endoscopy)
##############################################

dfep.all2 <- dfep.all2 %>%
  mutate(across(where(is.factor), ~na_if(., "Unknown")))

dfep.all2 <- droplevels(dfep.all2)

summary(dfep.all2$redcap_event_name)


table1(~ age_surg + sex + country + race_bin + jewish + hispanic + smoking_presurg | Rutgeerts_bin,
       data= dfep.all2,
       topclass="Rtable1-grid Rtable1-shade Rtable1-times", overall = F, extra.col = list("P-value" = pvalue),
       render.continuous = c(.="median [Q1 - Q3]"))


table1(~ age_diag + duration +
         montreal_loc + dis_behavior + disloc_peri + surg_smbow | Rutgeerts_bin,
       data= dfep.all2,
       topclass="Rtable1-grid Rtable1-shade Rtable1-times", overall = F, extra.col = list("P-value" = pvalue),
       render.continuous = c(.="median [Q1 - Q3]"))


table1(~ surg_findings2 + anastomosis_type + sewn_stapled + resection_length + margin | Rutgeerts_bin,
       data= dfep.all2,
       topclass="Rtable1-grid Rtable1-shade Rtable1-times", overall = F, extra.col = list("P-value" = pvalue),
       render.continuous = c(.="median [Q1 - Q3]"))


table1(~  immuno + mtx | Rutgeerts_bin,
       data= dfep.all2,
       topclass="Rtable1-grid Rtable1-shade Rtable1-times", overall = F, extra.col = list("P-value" = pvalue),
       render.continuous = c(.="median [Q1, Q3]"))


table1(~ tnf_any | Rutgeerts_bin,
       data= dfep.all2,
       topclass="Rtable1-grid Rtable1-shade Rtable1-times", overall = F, extra.col = list("P-value" = pvalue),
       render.continuous = c(.="median [Q1, Q3]"))

table1(~ vedolizumab +  ustekinumab | Rutgeerts_bin,
       data= dfep.all2,
       topclass="Rtable1-grid Rtable1-shade Rtable1-times", overall = F, extra.col = list("P-value" = pvalue),
       render.continuous = c(.="median [Q1, Q3]"))


table1(~  Rutgeerts.f + inter_endo_endo + smoking_status + antibio_current_upd +
         anyster_current_upd | Rutgeerts_bin, 
       data= dfep.all2,
       topclass="Rtable1-grid Rtable1-shade Rtable1-times", overall = F, extra.col = list("P-value" = pvalue),
       render.continuous = c(.="median [Q1, Q3]"))


table1(~ immuno_current_upd +  mtx_current_upd | Rutgeerts_bin,
       data= dfep.all2,
       topclass="Rtable1-grid Rtable1-shade Rtable1-times", overall = F, extra.col = list("P-value" = pvalue),
       render.continuous = c(.="median [Q1, Q3]"))


table1(~  tnf_any_current_upd | Rutgeerts_bin,
       data= dfep.all2,
       topclass="Rtable1-grid Rtable1-shade Rtable1-times", overall = F, extra.col = list("P-value" = pvalue),
       render.continuous = c(.="median [Q1, Q3]"))


table1(~ vedolizumab_current_upd  + ustekinumab_current_upd | Rutgeerts_bin,
       data= dfep.all2,
       topclass="Rtable1-grid Rtable1-shade Rtable1-times", overall = F, extra.col = list("P-value" = pvalue),
       render.continuous = c(.="median [Q1, Q3]"))



#########################
# SUPPLEMENTARY TABLE S9
#########################

# Model with GRC as random effect

dfep.all2$Rutgeerts_bin.num <- ifelse(dfep.all2$Rutgeerts_bin == "i0_i1", 0, 1)

table(dfep.all2$Rutgeerts_bin)

dfep.all2$Rutgeerts.f <- relevel(dfep.all2$Rutgeerts.f, ref = "i0")  

summary(dfep.all2$race_bin)

dfep.all2$race_bin <- relevel(dfep.all2$race_bin, ref = "White")  

m.glmer.sec = glmer(Rutgeerts_bin.num ~ Rutgeerts.f + sex + race_bin +
                      inter_endo_endo + smoking_status + tnf_any_current_upd + (1|grc.a),
                    data = dfep.all2, family = binomial) 
summary(m.glmer.sec)

tab_model(m.glmer.sec, show.aic = T)

isSingular(m.glmer)



#########################
# THIRD COLONOSCOPIES
########################


dfep.all3 <- dfep.all %>% filter(!(Rutgeerts_bin == "i2_i4" & redcap_event_name == "First_Endoscopy"))

dfep.all3 <- dfep.all3 %>% filter(!(Rutgeerts_bin == "i2_i4" & redcap_event_name == "Second_Endoscopy"))

no_rec3 <- dfep.all %>% filter(Rutgeerts_bin == "i2_i4" & redcap_event_name == "First_Endoscopy" |
                                 Rutgeerts_bin == "i2_i4" & redcap_event_name == "Second_Endoscopy")

dfep.all3 <- dfep.all3[!(dfep.all3$consortium_id %in% no_rec3$consortium_id),]


summary(dfep.all3$redcap_event_name)

dfep.all3 <- dfep.all3 %>% filter(redcap_event_name == "Third (or Greater) Endoscopies")

dfep.all3 <- droplevels(dfep.all3)


table1(~ Rutgeerts_bin | redcap_event_name, data= dfep.all3,
       topclass="Rtable1-grid Rtable1-shade Rtable1-times", overall = "Total",
       render.continuous = c(.="median [Q1 - Q3]"))


dfep.sec <- dfep.all %>% filter(redcap_event_name == "Second_Endoscopy")

dfep.sec <- subset(dfep.sec, select = c(1,118,119,122))

colnames(dfep.sec) <- paste0(colnames(dfep.sec), ".s", sep = "")

dfep.all3 <- dfep.all3 %>% left_join(dfep.sec, by = c("consortium_id" = "consortium_id.s"))

summary(dfep.all3$endoscopy_date) 

summary(dfep.all3$endoscopy_date.s) 

dfep.all3$endoscopy_date <- as.Date(dfep.all3$endoscopy_date, format="%Y-%m-%d")

summary(dfep.all3$endoscopy_date.s) 

dfep.all3$endoscopy_date.s <- as.Date(dfep.all3$endoscopy_date.s, format="%Y-%m-%d")

dfep.all3$inter_endo_endo <- interval(dfep.all3$endoscopy_date.s, dfep.all3$endoscopy_date) %/% months(1) 

summary(dfep.all3$inter_endo_endo)


##############################################
# SUPPLEMENTARY TABLE S7-S8 (third endoscopy)
##############################################

# Participants data

dfep.all3 <- dfep.all3 %>%
  mutate(across(where(is.factor), ~na_if(., "Unknown")))

dfep.all3 <- droplevels(dfep.all3)

summary(dfep.all3$redcap_event_name)


table1(~ age_surg + sex + country + race_bin + jewish + hispanic + smoking_presurg | Rutgeerts_bin,
       data= dfep.all3,
       topclass="Rtable1-grid Rtable1-shade Rtable1-times", overall = F, extra.col = list("P-value" = pvalue),
       render.continuous = c(.="median [Q1 - Q3]"))


table1(~ age_diag + duration +
         montreal_loc + dis_behavior + disloc_peri + surg_smbow | Rutgeerts_bin,
       data= dfep.all3,
       topclass="Rtable1-grid Rtable1-shade Rtable1-times", overall = F, extra.col = list("P-value" = pvalue),
       render.continuous = c(.="median [Q1 - Q3]"))


table1(~ surg_findings2 + anastomosis_type + sewn_stapled + resection_length + margin | Rutgeerts_bin,
       data= dfep.all3,
       topclass="Rtable1-grid Rtable1-shade Rtable1-times", overall = F, extra.col = list("P-value" = pvalue),
       render.continuous = c(.="median [Q1 - Q3]"))


table1(~  immuno + mtx | Rutgeerts_bin,
       data= dfep.all3,
       topclass="Rtable1-grid Rtable1-shade Rtable1-times", overall = F, extra.col = list("P-value" = pvalue),
       render.continuous = c(.="median [Q1, Q3]"))


table1(~ tnf_any | Rutgeerts_bin,
       data= dfep.all3,
       topclass="Rtable1-grid Rtable1-shade Rtable1-times", overall = F, extra.col = list("P-value" = pvalue),
       render.continuous = c(.="median [Q1, Q3]"))

table1(~ vedolizumab +  ustekinumab | Rutgeerts_bin,
       data= dfep.all3,
       topclass="Rtable1-grid Rtable1-shade Rtable1-times", overall = F, extra.col = list("P-value" = pvalue),
       render.continuous = c(.="median [Q1, Q3]"))


table1(~  Rutgeerts.s + inter_endo_endo + smoking_status + antibio_current_upd +
         anyster_current_upd | Rutgeerts_bin, 
       data= dfep.all3,
       topclass="Rtable1-grid Rtable1-shade Rtable1-times", overall = F, extra.col = list("P-value" = pvalue),
       render.continuous = c(.="median [Q1, Q3]"))


table1(~ immuno_current_upd +  mtx_current_upd | Rutgeerts_bin,
       data= dfep.all3,
       topclass="Rtable1-grid Rtable1-shade Rtable1-times", overall = F, extra.col = list("P-value" = pvalue),
       render.continuous = c(.="median [Q1, Q3]"))


table1(~  tnf_any_current_upd | Rutgeerts_bin,
       data= dfep.all3,
       topclass="Rtable1-grid Rtable1-shade Rtable1-times", overall = F, extra.col = list("P-value" = pvalue),
       render.continuous = c(.="median [Q1, Q3]"))


table1(~ vedolizumab_current_upd  + ustekinumab_current_upd | Rutgeerts_bin,
       data= dfep.all3,
       topclass="Rtable1-grid Rtable1-shade Rtable1-times", overall = F, extra.col = list("P-value" = pvalue),
       render.continuous = c(.="median [Q1, Q3]"))


table1(~ no_meds_current_upd | Rutgeerts_bin,
       data= dfep.all3,
       topclass="Rtable1-grid Rtable1-shade Rtable1-times", overall = F, extra.col = list("P-value" = pvalue),
       render.continuous = c(.="median [Q1, Q3]"))



# Model with GRC as random effect
dfep.all3$Rutgeerts_bin.num <- ifelse(dfep.all3$Rutgeerts_bin == "i0_i1", 0, 1)

table(dfep.all3$Rutgeerts_bin)

dfep.all3$Rutgeerts.s <- relevel(dfep.all3$Rutgeerts.s, ref = "i0")  

m.glmer.third = glmer(Rutgeerts_bin.num ~ Rutgeerts.s + (1|grc.a),
                    data = dfep.all3, family = binomial) 
summary(m.glmer.third)

tab_model(m.glmer.third, show.aic = T)

isSingular(m.glmer.third)


#######################################
# PREPARING DATA FOR SURVIVAL ANALYSIS
######################################

summary(dfep.all$redcap_event_name)

summary(dfep.all$inter_surg_endo)

dfep.all.sur <- dfep.all 

# Use this in case of analyzing patients without prophilactic medication after surgery
#dfep.all.sur <- dfep.all %>% filter(no_meds_since_last == "Yes")

#dfep.all.sur <- subset(dfep.all, select = c(1,116,117,114,118,122))

summary(dfep.all.sur$redcap_event_name)

table1(~ Rutgeerts_bin | redcap_event_name2, data= dfep.all.sur,
       topclass="Rtable1-grid Rtable1-shade Rtable1-times", overall = "Total", 
       render.continuous = c(.="median [Q1, Q3]"))

table1(~ redcap_event_name | Rutgeerts_bin, data= dfep.all.sur,
       topclass="Rtable1-grid Rtable1-shade Rtable1-times", overall = "Total", 
       render.continuous = c(.="median [Q1, Q3]"))



setdiff(dfep.first.c$consortium_id, dfep.all.sur$consortium_id)


dfep.all.sur <- dfep.all.sur %>% group_by(consortium_id) %>% mutate(count = n())


dfep.all.sur$ind <- ave(dfep.all.sur$Rutgeerts_bin == "i2_i4",
                        dfep.all.sur$consortium_id, FUN = function(x) !cumsum(c(0, head(x, -1)))) 


dfep.all.sur <- dfep.all.sur %>% filter(ind == TRUE)


dfep.all.sur <- dfep.all.sur %>% group_by(consortium_id) %>% top_n(1, inter_surg_endo)

#dfep.all.check <- subset(dfep.all, select = c(1,116,117,114,118,122))


table1(~ Rutgeerts_bin | redcap_event_name2, data= dfep.all.sur,
       topclass="Rtable1-grid Rtable1-shade Rtable1-times", overall = "Total", 
       render.continuous = c(.="median [Q1, Q3]"))


nrow(dfep.first.c)
nrow(dfep.all.sur)


setdiff(dfep.first.c$consortium_id, dfep.all.sur$consortium_id)

nrow(distinct(dfep.all.sur, consortium_id))

dfep.all.sur$consortium_id[duplicated(dfep.all.sur$consortium_id)]


#####################
# SURVIVAL ANALYSIS
#####################

dfep.all.sur <- as.data.frame(dfep.all.sur)

rownames(dfep.all.sur) <- dfep.all.sur$consortium_id

summary(dfep.all.sur$Rutgeerts_bin3)

dfep.all.sur$status <- ifelse(dfep.all.sur$Rutgeerts_bin == "i0_i1", 1, 2)

table(dfep.all.sur$status)

table(dfep.all.sur$status, dfep.all.sur$count)

summary(dfep.all.sur$inter_surg_endo)

hist(dfep.all.sur$inter_surg_endo)

nrow(dfep.all.sur)

summary(dfep.all.sur$inter_surg_endo)

dfep.all.sur %>%
  group_by(Rutgeerts_bin) %>%
  summarise(median = median(inter_surg_endo),
            q1 = quantile(inter_surg_endo, 0.25),
            q3 = quantile(inter_surg_endo, 0.75))


table1(~ Rutgeerts_bin | redcap_event_name, data= dfep.all.sur,
       topclass="Rtable1-grid Rtable1-shade Rtable1-times", overall = "Total", 
       render.continuous = c(.="median [Q1, Q3]"))


length(which(dfep.all.sur$inter_surg_endo >= 48))

dfep.all.sur2 <- dfep.all.sur %>% filter(inter_surg_endo < 48)

#dfep.all.sur2 <- dfep.all.sur

nrow(dfep.all.sur2)

summary(dfep.all.sur2$Rutgeerts_bin)

table1(~ Rutgeerts_bin | redcap_event_name, data= dfep.all.sur2,
       topclass="Rtable1-grid Rtable1-shade Rtable1-times", overall = "Total", 
       render.continuous = c(.="median [Q1, Q3]"))

table1(~ redcap_event_name | Rutgeerts_bin, data= dfep.all.sur2,
       topclass="Rtable1-grid Rtable1-shade Rtable1-times", overall = "Total", 
       render.continuous = c(.="median [Q1, Q3]"))


library(survival)
library(survminer)
library(lubridate)
library(cmprsk)
library(gtsummary)
library(ggplotify)


Surv(dfep.all.sur2$inter_surg_endo, dfep.all.sur2$status)

f1 <- survfit(Surv(inter_surg_endo, status) ~ 1, data = dfep.all.sur2)
names(f1)

summary(f1)

range(dfep.all.sur2$inter_surg_endo)

summary(f1, times=seq(0, 101, 12))

cuminc(dfep.all.sur2$inter_surg_endo, dfep.all.sur2$status, cencode = 1)


##########################
# SUPPLEMENTARY FIGURE S2
##########################

ggsurvplot(f1, color = "#2E9FDF", risk.table = T, break.time.by = 3,
           ncensor.plot = T, ncensor.plot.height = 0.4)

plot(survfit(Surv(inter_surg_endo, status) ~ 1, data = dfep.all.sur2), mark.time = T,
     xlab = "Months", 
     ylab = "Recurrence-free survival")

summary(survfit(Surv(inter_surg_endo, status) ~ 1, data = dfep.all.sur2), times = 12)

summary(survfit(Surv(inter_surg_endo, status) ~ 1, data = dfep.all.sur2), times = 24)

summary(survfit(Surv(inter_surg_endo, status) ~ 1, data = dfep.all.sur2), times = 36)


ci_fit <- 
  cuminc(
    ftime = dfep.all.sur2$inter_surg_endo, 
    fstatus = dfep.all.sur2$status, 
    cencode = 2
  )

plot(ci_fit, xlab = "Months")


table(dfep.all.sur2$status)

dfep.all.sur2 <- dfep.all.sur2 %>%
  mutate(across(where(is.factor), ~na_if(., "Unknown")))

dfep.all.sur2 <- droplevels(dfep.all.sur2)

###########################################
# TABLE 4 (Uni and multivariabe cox model)
###########################################


coxph(Surv(inter_surg_endo, status) ~ sex, data = dfep.all.sur2)
gtsummary::tbl_regression(coxph(Surv(inter_surg_endo, status) ~ sex, data = dfep.all.sur2), exp = T)

dfep.all.sur2$race_bin <- relevel(dfep.all.sur2$race_bin, ref = "White")  

coxph(Surv(inter_surg_endo, status) ~ race_bin, data = dfep.all.sur2)
gtsummary::tbl_regression(coxph(Surv(inter_surg_endo, status) ~ race_bin, data = dfep.all.sur2), exp = T)

coxph(Surv(inter_surg_endo, status) ~ age_diag, data = dfep.all.sur2)
gtsummary::tbl_regression(coxph(Surv(inter_surg_endo, status) ~ age_diag, data = dfep.all.sur2), exp = T)

coxph(Surv(inter_surg_endo, status) ~ duration, data = dfep.all.sur2)
gtsummary::tbl_regression(coxph(Surv(inter_surg_endo, status) ~ duration, data = dfep.all.sur2), exp = T)

coxph(Surv(inter_surg_endo, status) ~ disloc_peri, data = dfep.all.sur2)
gtsummary::tbl_regression(coxph(Surv(inter_surg_endo, status) ~ disloc_peri, data = dfep.all.sur2), exp = T)

coxph(Surv(inter_surg_endo, status) ~ penetrating_compl, data = dfep.all.sur2)
gtsummary::tbl_regression(coxph(Surv(inter_surg_endo, status) ~ penetrating_compl, data = dfep.all.sur2), exp = T)

coxph(Surv(inter_surg_endo, status) ~ surg_smbow, data = dfep.all.sur2)
gtsummary::tbl_regression(coxph(Surv(inter_surg_endo, status) ~ surg_smbow, data = dfep.all.sur2), exp = T)

coxph(Surv(inter_surg_endo, status) ~ margin, data = dfep.all.sur2)
gtsummary::tbl_regression(coxph(Surv(inter_surg_endo, status) ~ margin, data = dfep.all.sur2), exp = T)

coxph(Surv(inter_surg_endo, status) ~ resection_length, data = dfep.all.sur2)
gtsummary::tbl_regression(coxph(Surv(inter_surg_endo, status) ~ resection_length, data = dfep.all.sur2), exp = T)

coxph(Surv(inter_surg_endo, status) ~ smoking_status, data = dfep.all.sur2)
gtsummary::tbl_regression(coxph(Surv(inter_surg_endo, status) ~ smoking_status, data = dfep.all.sur2), exp = T)

coxph(Surv(inter_surg_endo, status) ~ tnf_any_current_upd, data = dfep.all.sur2)
gtsummary::tbl_regression(coxph(Surv(inter_surg_endo, status) ~ tnf_any_current_upd, data = dfep.all.sur2), exp = T)

coxph(Surv(inter_surg_endo, status) ~ ustekinumab_current_upd, data = dfep.all.sur2)
gtsummary::tbl_regression(coxph(Surv(inter_surg_endo, status) ~ ustekinumab_current_upd, data = dfep.all.sur2), exp = T)


table(dfep.all.sur2$status)

summary(dfep.all.sur2$inter_surg_endo)


cox.model <- coxph(Surv(inter_surg_endo, status) ~ sex + race_bin + age_diag + duration +
                     disloc_peri + penetrating_compl +
                     surg_smbow + margin + resection_length + 
                     smoking_status + tnf_any_current_upd + ustekinumab_current_upd +
                     cluster(grc.a),
      data = dfep.all.sur2)


gtsummary::tbl_regression(cox.model, exp = T)


# ANALIZING PATIENTS WITH NO POSTOP MEDICATION

summary(dfep.all.sur2$no_meds_since_last)

dfep.all.sur2.nomeds <- dfep.all.sur2 %>% filter(no_meds_since_last == "Yes")

summary(dfep.all.sur2.nomeds$Rutgeerts_bin)

dfep.sec2 <- dfep.all %>% filter(redcap_event_name == "Second_Endoscopy")

dfep.sec2 <- subset(dfep.sec2, select = c(1,116,145))

colnames(dfep.sec2) <- paste0(colnames(dfep.sec2), ".s", sep = "")

dfep.all.sur2.nomeds <- dfep.all.sur2.nomeds %>% left_join(dfep.sec2, by = c("consortium_id" = "consortium_id.s"))

summary(dfep.all.sur2.nomeds$no_meds_since_last.s)

dfep.all.sur2.nomeds <- dfep.all.sur2.nomeds %>% filter(no_meds_since_last.s == "Yes" | is.na(no_meds_since_last.s))

summary(dfep.all.sur2.nomeds$Rutgeerts_bin)


dfep.third <- dfep.all %>% filter(redcap_event_name == "Third (or Greater) Endoscopies")

dfep.third <- subset(dfep.third, select = c(1,116,145))

colnames(dfep.third) <- paste0(colnames(dfep.third), ".t", sep = "")

dfep.all.sur2.nomeds <- dfep.all.sur2.nomeds %>% left_join(dfep.third, by = c("consortium_id" = "consortium_id.t"))

summary(dfep.all.sur2.nomeds$no_meds_since_last.t)

dfep.all.sur2.nomeds <- dfep.all.sur2.nomeds %>% filter(no_meds_since_last.t == "Yes" | is.na(no_meds_since_last.t))

summary(dfep.all.sur2.nomeds$Rutgeerts_bin)

summary(dfep.all.sur2.nomeds$inter_surg_endo)



coxph(Surv(inter_surg_endo, status) ~ sex, data = dfep.all.sur2.nomeds)
gtsummary::tbl_regression(coxph(Surv(inter_surg_endo, status) ~ sex, data = dfep.all.sur2.nomeds), exp = T)

coxph(Surv(inter_surg_endo, status) ~ race_bin, data = dfep.all.sur2.nomeds)
gtsummary::tbl_regression(coxph(Surv(inter_surg_endo, status) ~ race_bin, data = dfep.all.sur2.nomeds), exp = T)

coxph(Surv(inter_surg_endo, status) ~ age_diag, data = dfep.all.sur2.nomeds)
gtsummary::tbl_regression(coxph(Surv(inter_surg_endo, status) ~ age_diag, data = dfep.all.sur2.nomeds), exp = T)

coxph(Surv(inter_surg_endo, status) ~ duration, data = dfep.all.sur2.nomeds)
gtsummary::tbl_regression(coxph(Surv(inter_surg_endo, status) ~ duration, data = dfep.all.sur2.nomeds), exp = T)

coxph(Surv(inter_surg_endo, status) ~ disloc_peri, data = dfep.all.sur2.nomeds)
gtsummary::tbl_regression(coxph(Surv(inter_surg_endo, status) ~ disloc_peri, data = dfep.all.sur2.nomeds), exp = T)

coxph(Surv(inter_surg_endo, status) ~ penetrating_compl, data = dfep.all.sur2.nomeds)
gtsummary::tbl_regression(coxph(Surv(inter_surg_endo, status) ~ penetrating_compl, data = dfep.all.sur2.nomeds), exp = T)

coxph(Surv(inter_surg_endo, status) ~ surg_smbow, data = dfep.all.sur2.nomeds)
gtsummary::tbl_regression(coxph(Surv(inter_surg_endo, status) ~ surg_smbow, data = dfep.all.sur2.nomeds), exp = T)

coxph(Surv(inter_surg_endo, status) ~ margin, data = dfep.all.sur2.nomeds)
gtsummary::tbl_regression(coxph(Surv(inter_surg_endo, status) ~ margin, data = dfep.all.sur2.nomeds), exp = T)

coxph(Surv(inter_surg_endo, status) ~ resection_length, data = dfep.all.sur2.nomeds)
gtsummary::tbl_regression(coxph(Surv(inter_surg_endo, status) ~ resection_length, data = dfep.all.sur2.nomeds), exp = T)

coxph(Surv(inter_surg_endo, status) ~ smoking_status, data = dfep.all.sur2.nomeds)
gtsummary::tbl_regression(coxph(Surv(inter_surg_endo, status) ~ smoking_status, data = dfep.all.sur2.nomeds), exp = T)




cox.model.nomeds <- coxph(Surv(inter_surg_endo, status) ~ sex + race_bin + age_diag + duration + disloc_peri + penetrating_compl +
                            surg_smbow + margin + resection_length +
                            smoking_status +
                     cluster(grc.a),
                   data = dfep.all.sur2.nomeds)


gtsummary::tbl_regression(cox.model.nomeds, exp = T)


#######################################
# ADDITIVE RISK FACTORS (ALL PATIENTS)
######################################

dfep.first.c2 <- dfep.first.c

dfep.first.c2 <- dfep.first.c2 %>% filter(!is.na(smoking_status))
dfep.first.c2 <- dfep.first.c2 %>% filter(!is.na(race_bin))
dfep.first.c2 <- dfep.first.c2 %>% filter(!is.na(sex))
dfep.first.c2 <- dfep.first.c2 %>% filter(!is.na(tnf_any_current_upd))

summary(dfep.first.c2$smoking_presurg_bin)

summary(dfep.first.c2$sex)
dfep.first.c2$sex_num <- ifelse(dfep.first.c2$sex == "Female", 0, 1)
table(dfep.first.c2$sex_num, useNA = "always")

summary(dfep.first.c2$race_bin)
dfep.first.c2$race_bin_num <- ifelse(dfep.first.c2$race_bin == "White", 0, 1)
table(dfep.first.c2$race_bin_num)

summary(dfep.first.c2$smoking_status)
dfep.first.c2$smoking_status_num <- ifelse(dfep.first.c2$smoking_status == "No", 0, 1)
table(dfep.first.c2$smoking_status_num, useNA = "always")

summary(dfep.first.c$tnf_any_current_upd)
dfep.first.c2$tnf_any_current_upd_num <- ifelse(dfep.first.c2$tnf_any_current_upd == "Yes", 0, 1)
table(dfep.first.c2$tnf_any_current_upd_num)


dfep.first.c2$num_risk <- dfep.first.c2$sex_num + dfep.first.c2$race_bin_num + dfep.first.c2$smoking_status_num

summary(dfep.first.c2$num_risk)

summary(dfep.first.c2$Rutgeerts_bin)

dfep.first.c2 <- mutate(dfep.first.c2,
               num_risk.cat = case_when(
                num_risk == 0 ~ "No_RF",
                num_risk == 1 ~ "One_RF",
                num_risk == 2 ~ "Two_RF",
                num_risk == 3 ~ "Three_RF",
                TRUE ~ NA_character_)
)
dfep.first.c2$num_risk.cat <- factor(dfep.first.c2$num_risk.cat, levels = c("No_RF", "One_RF", "Two_RF", "Three_RF"),
                                     labels = c("No risk factor", "1 risk factor",
                                                "2 risk factors", "3 or more risk factors"))
summary(dfep.first.c2$num_risk.cat)


summary(dfep.first.c$Rutgeerts)

dfep.first.c2$Rutgeerts <- factor(dfep.first.c2$Rutgeerts, levels = c("i4", "i3", "i2", "i1", "i0"))


table1(~ Rutgeerts_bin | num_risk.cat, data= dfep.first.c2,
       topclass="Rtable1-grid Rtable1-shade Rtable1-times", overall = F,
       render.continuous = c(.="median [Q1, Q3]"))

table1(~ Rutgeerts | num_risk.cat, data= dfep.first.c2,
       topclass="Rtable1-grid Rtable1-shade Rtable1-times", overall = F,
       render.continuous = c(.="median [Q1, Q3]"))


library(dplyr)

summary(dfep.first.c2$num_risk.cat)

df.plot1 <- dfep.first.c2 %>% 
  group_by(num_risk.cat, Rutgeerts) %>% 
  summarise(count = n()) %>% 
  mutate(percent = count/sum(count))


stacked1 <- ggplot(df.plot1, aes(x = factor(num_risk.cat), y = percent*100,  fill= factor(Rutgeerts))) + 
  geom_bar(stat = "identity", width = 0.7, color = "black") +
  geom_text(aes(label=paste0(sprintf("%1.1f", percent*100),"%")),
            position=position_stack(vjust=0.5), colour="black", size = 5) +
  labs(x = "", y = "Percent", fill = "Rutgeerts score") +
  ggtitle("All patients") +
  scale_fill_manual(values = c("#993404", "#FB6A4A", "#FED976", 
                               "#52854C", "#C3D7A4")) +
  scale_x_discrete(labels=c("No risk factor" = "No risk factor\n(n = 138)", "1 risk factor" = "1 risk factor\n(n = 176)",
                            "2 risk factors" = "2 risk factors\n(n= 36)")) +
  theme(legend.position = "right",
        legend.text = element_text(size = 18),
        legend.title = element_text(size = 18),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(size = 18), 
        axis.text.y = element_text(size = 18), 
        axis.title.x = element_text(size = 18), 
        axis.title.y =element_text(size = 18),
        plot.title = element_text(size = 18, hjust = 0.5),
        axis.line = element_line(colour = "black", size = 1, linetype = "solid"))


stacked1


m.glmer.risk1 = glmer(Rutgeerts_bin.num ~ num_risk.cat + age_diag + duration + disloc_peri + penetrating_compl +
                  surg_smbow + margin + resection_length + inter_surg_endo +
                  tnf_any_current_upd + ustekinumab_current_upd  + (1|grc.a),
                data = dfep.first.c2, family = binomial) 

gtsummary::tbl_regression(m.glmer.risk1, exp = T)


################################################
# ADDITIVE RISK FACTORS (ONLY NO MEDS PATIENTS)
###############################################

dfep.first.c2.nomeds <- dfep.first.c2[dfep.first.c2$consortium_id %in% dfep.first.c.no.meds$consortium_id,]


dfep.first.c2.nomeds$num_risk <- dfep.first.c2.nomeds$sex_num + dfep.first.c2.nomeds$race_bin_num + dfep.first.c2.nomeds$smoking_status_num 

summary(dfep.first.c2.nomeds$num_risk)

summary(dfep.first.c2$Rutgeerts_bin)

summary(dfep.first.c2.nomeds$num_risk.cat)


dfep.first.c2.nomeds <- mutate(dfep.first.c2.nomeds,
                        num_risk.cat = case_when(
                          num_risk == 0 ~ "No_RF",
                          num_risk == 1 ~ "One_RF",
                          num_risk == 2 ~ "Two_RF",
                          num_risk == 3 ~ "Three_RF",
                          TRUE ~ NA_character_)
)
dfep.first.c2.nomeds$num_risk.cat <- factor(dfep.first.c2.nomeds$num_risk.cat, levels = c("No_RF", "One_RF", "Two_RF", "Three_RF"),
                                     labels = c("No risk factor", "1 risk factor",
                                                "2 risk factors", "3 or more risk factors"))
summary(dfep.first.c2.nomeds$num_risk.cat)


m.glmer.risk1.nomeds = glmer(Rutgeerts_bin.num ~ num_risk.cat + age_diag + duration + disloc_peri + penetrating_compl +
                        surg_smbow + margin + resection_length + inter_surg_endo + (1|grc.a),
                      data = dfep.first.c2.nomeds, family = binomial) 

gtsummary::tbl_regression(m.glmer.risk1.nomeds, exp = T)



table1(~ Rutgeerts_bin | num_risk.cat, data= dfep.first.c2.nomeds,
       topclass="Rtable1-grid Rtable1-shade Rtable1-times", overall = F,
       render.continuous = c(.="median [Q1, Q3]"))

table1(~ Rutgeerts | num_risk.cat, data= dfep.first.c2.nomeds,
       topclass="Rtable1-grid Rtable1-shade Rtable1-times", overall = F,
       render.continuous = c(.="median [Q1, Q3]"))


df.plot2 <- dfep.first.c2.nomeds %>% 
  group_by(num_risk.cat, Rutgeerts) %>% 
  summarise(count = n()) %>% 
  mutate(percent = count/sum(count))


summary(dfep.first.c2.nomeds$num_risk.cat)

stacked2 <- ggplot(df.plot2, aes(x = factor(num_risk.cat), y = percent*100,  fill= factor(Rutgeerts))) + 
  geom_bar(stat = "identity", width = 0.7, color = "black") +
  geom_text(aes(label=paste0(sprintf("%1.1f", percent*100),"%")),
            position=position_stack(vjust=0.5), colour="black", size = 5) +
  labs(x = "", y = "Percent", fill = "Rutgeerts score") +
  ggtitle("Only patients on no postop medication") +
  scale_fill_manual(values = c("#993404", "#FB6A4A", "#FED976", 
                               "#52854C", "#C3D7A4")) +
  scale_x_discrete(labels=c("No risk factor" = "No risk factor\n(n = 54)", "1 risk factor" = "1 risk factor\n(n = 66)",
                            "2 risk factors" = "2 risk factors\n(n= 10)")) +
  theme(legend.position = "right",
        legend.text = element_text(size = 18),
        legend.title = element_text(size = 18),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(size = 18), 
        axis.text.y = element_text(size = 18), 
        axis.title.x = element_text(size = 18), 
        axis.title.y =element_text(size = 18),
        plot.title = element_text(size = 18, hjust = 0.5),
        axis.line = element_line(colour = "black", size = 1, linetype = "solid"))

stacked2



################################################
# ADDITIVE RISK FACTORS (ONLY ON MED USERS)
###############################################

dfep.first.c2.meds <- dfep.first.c2[!(dfep.first.c2$consortium_id %in% dfep.first.c.no.meds$consortium_id),]


dfep.first.c2.meds$num_risk <- dfep.first.c2.meds$sex_num + dfep.first.c2.meds$race_bin_num + dfep.first.c2.meds$smoking_status_num 

summary(dfep.first.c2.meds$num_risk)

summary(dfep.first.c2.meds$Rutgeerts_bin)

summary(dfep.first.c2.meds$num_risk.cat)


dfep.first.c2.meds <- mutate(dfep.first.c2.meds,
                               num_risk.cat = case_when(
                                 num_risk == 0 ~ "No_RF",
                                 num_risk == 1 ~ "One_RF",
                                 num_risk == 2 ~ "Two_RF",
                                 num_risk == 3 ~ "Three_RF",
                                 TRUE ~ NA_character_)
)
dfep.first.c2.meds$num_risk.cat <- factor(dfep.first.c2.meds$num_risk.cat, levels = c("No_RF", "One_RF", "Two_RF", "Three_RF"),
                                            labels = c("No risk factor", "1 risk factor",
                                                       "2 risk factors", "3 or more risk factors"))
summary(dfep.first.c2.meds$num_risk.cat)


table1(~ Rutgeerts_bin | num_risk.cat, data= dfep.first.c2.meds,
       topclass="Rtable1-grid Rtable1-shade Rtable1-times", overall = F,
       render.continuous = c(.="median [Q1, Q3]"))

table1(~ Rutgeerts | num_risk.cat, data= dfep.first.c2.meds,
       topclass="Rtable1-grid Rtable1-shade Rtable1-times", overall = F,
       render.continuous = c(.="median [Q1, Q3]"))


m.glmer.risk1.meds = glmer(Rutgeerts_bin.num ~ num_risk.cat + age_diag + duration + disloc_peri + penetrating_compl +
                               surg_smbow + margin + resection_length + inter_surg_endo +
                             tnf_any_current_upd + ustekinumab_current_upd + (1|grc.a),
                             data = dfep.first.c2.meds, family = binomial) 

gtsummary::tbl_regression(m.glmer.risk1.meds, exp = T)



df.plot3 <- dfep.first.c2.meds %>% 
  group_by(num_risk.cat, Rutgeerts) %>% 
  summarise(count = n()) %>% 
  mutate(percent = count/sum(count))

summary(dfep.first.c2.meds$num_risk.cat)

stacked3 <- ggplot(df.plot3, aes(x = factor(num_risk.cat), y = percent*100,  fill= factor(Rutgeerts))) + 
  geom_bar(stat = "identity", width = 0.7, color = "black") +
  geom_text(aes(label=paste0(sprintf("%1.1f", percent*100),"%")),
            position=position_stack(vjust=0.5), colour="black", size = 5) +
  labs(x = "", y = "Percent", fill = "Rutgeerts score") +
  ggtitle("Only patients on postop medication") +
  scale_fill_manual(values = c("#993404", "#FB6A4A", "#FED976", 
                               "#52854C", "#C3D7A4")) +
  scale_x_discrete(labels=c("No risk factor" = "No risk factor\n(n = 84)", "1 risk factor" = "1 risk factor\n(n = 110)",
                            "2 risk factors" = "2 risk factors\n(n= 26)")) +
  theme(legend.position = "right",
        legend.text = element_text(size = 18),
        legend.title = element_text(size = 18),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(size = 18), 
        axis.text.y = element_text(size = 18), 
        axis.title.x = element_text(size = 18), 
        axis.title.y =element_text(size = 18),
        plot.title = element_text(size = 18, hjust = 0.5),
        axis.line = element_line(colour = "black", size = 1, linetype = "solid"))

stacked3


############
# FIGURE 2
############

setwd("/Users/cristian/Library/CloudStorage/Dropbox/Temas\ gastroenterología/Inflammatory\ Bowel\ Diseases/Postoperative\ recurrence\ of\ Crohn\'s\ disease/NIDDK\ Ileal\ CD\ Post\ Op/Clinical\ predictors\ of\ postoperative\ CD\ recurrence/Manuscript/JCC_submission/Revised_versionJCC")

# Figure 2
#tiff("Figure 2.tiff", units="in", width=20, height=8, res=300)

ggarrange(stacked1, stacked3, stacked2, ncol = 3, nrow = 1, common.legend = T,  labels = c("A", "B", "C"))

#dev.off()



##################################################
# ADDITIVE RISK FACTORS (ALL NO SMOKERS PATIENTS)
##################################################

dfep.first.c2.ns <- dfep.first.c

dfep.first.c2.ns <- dfep.first.c2.ns %>% filter(smoking_presurg_bin == "No")

summary(dfep.first.c2.ns$smoking_status)

dfep.first.c2.ns <- dfep.first.c2.ns %>% filter(!is.na(smoking_status))
dfep.first.c2.ns <- dfep.first.c2.ns %>% filter(!is.na(race_bin))
dfep.first.c2.ns <- dfep.first.c2.ns %>% filter(!is.na(sex))
dfep.first.c2.ns <- dfep.first.c2.ns %>% filter(!is.na(tnf_any_current_upd))


summary(dfep.first.c2.ns$sex)
dfep.first.c2.ns$sex_num <- ifelse(dfep.first.c2.ns$sex == "Female", 0, 1)
table(dfep.first.c2.ns$sex_num, useNA = "always")

summary(dfep.first.c2.ns$race_bin)
dfep.first.c2.ns$race_bin_num <- ifelse(dfep.first.c2.ns$race_bin == "White", 0, 1)
table(dfep.first.c2.ns$race_bin_num)


dfep.first.c2.ns$num_risk <- dfep.first.c2.ns$sex_num + dfep.first.c2.ns$race_bin_num

summary(dfep.first.c2.ns$num_risk)

summary(dfep.first.c2.ns$Rutgeerts_bin)

dfep.first.c2.ns <- mutate(dfep.first.c2.ns,
                        num_risk.cat = case_when(
                          num_risk == 0 ~ "No_RF",
                          num_risk == 1 ~ "One_RF",
                          num_risk == 2 ~ "Two_RF",
                          num_risk == 3 ~ "Three_RF",
                          TRUE ~ NA_character_)
)
dfep.first.c2.ns$num_risk.cat <- factor(dfep.first.c2.ns$num_risk.cat, levels = c("No_RF", "One_RF", "Two_RF", "Three_RF"),
                                     labels = c("No risk factor", "1 risk factor",
                                                "2 risk factors", "3 or more risk factors"))
summary(dfep.first.c2.ns$num_risk.cat)


summary(dfep.first.c2.ns$Rutgeerts)

dfep.first.c2.ns$Rutgeerts <- factor(dfep.first.c2.ns$Rutgeerts, levels = c("i4", "i3", "i2", "i1", "i0"))


table1(~ Rutgeerts_bin | num_risk.cat, data= dfep.first.c2.ns,
       topclass="Rtable1-grid Rtable1-shade Rtable1-times", overall = F,
       render.continuous = c(.="median [Q1, Q3]"))

table1(~ Rutgeerts | num_risk.cat, data= dfep.first.c2.ns,
       topclass="Rtable1-grid Rtable1-shade Rtable1-times", overall = F,
       render.continuous = c(.="median [Q1, Q3]"))

summary(dfep.first.c2.ns$num_risk.cat)

df.plot1.ns <- dfep.first.c2.ns %>% 
  group_by(num_risk.cat, Rutgeerts) %>% 
  summarise(count = n()) %>% 
  mutate(percent = count/sum(count))


stacked1.ns <- ggplot(df.plot1.ns, aes(x = factor(num_risk.cat), y = percent*100,  fill= factor(Rutgeerts))) + 
  geom_bar(stat = "identity", width = 0.7, color = "black") +
  geom_text(aes(label=paste0(sprintf("%1.1f", percent*100),"%")),
            position=position_stack(vjust=0.5), colour="black", size = 5) +
  labs(x = "", y = "Percent", fill = "Rutgeerts score") +
  ggtitle("All patients") +
  scale_fill_manual(values = c("#993404", "#FB6A4A", "#FED976", 
                               "#52854C", "#C3D7A4")) +
  scale_x_discrete(labels=c("No risk factor" = "No risk factor\n(n = 134)", "1 risk factor" = "1 risk factor\n(n = 162)",
                            "2 risk factors" = "2 risk factors\n(n= 17)")) +
  theme(legend.position = "right",
        legend.text = element_text(size = 18),
        legend.title = element_text(size = 18),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(size = 18), 
        axis.text.y = element_text(size = 18), 
        axis.title.x = element_text(size = 18), 
        axis.title.y =element_text(size = 18),
        plot.title = element_text(size = 18, hjust = 0.5),
        axis.line = element_line(colour = "black", size = 1, linetype = "solid"))


stacked1.ns


m.glmer.risk1.ns = glmer(Rutgeerts_bin.num ~ num_risk.cat + age_diag + duration + disloc_peri +
                           penetrating_compl +
                        surg_smbow + margin + resection_length + inter_surg_endo +
                        tnf_any_current_upd + ustekinumab_current_upd  + (1|grc.a),
                      data = dfep.first.c2.ns, family = binomial) 

gtsummary::tbl_regression(m.glmer.risk1.ns, exp = T)


#########################################################
# ADDITIVE RISK FACTORS (ONLY NO MEDS NO SMOKER PATIENTS)
########################################################

dfep.first.c2.nomeds.ns <- dfep.first.c2.ns[dfep.first.c2.ns$consortium_id %in% dfep.first.c.no.meds$consortium_id,]

dfep.first.c2.nomeds.ns$num_risk <- dfep.first.c2.nomeds.ns$sex_num + dfep.first.c2.nomeds.ns$race_bin_num 

summary(dfep.first.c2.nomeds.ns$num_risk)

summary(dfep.first.c2.nomeds.ns$Rutgeerts_bin)

summary(dfep.first.c2.nomeds.ns$num_risk.cat)


dfep.first.c2.nomeds.ns <- mutate(dfep.first.c2.nomeds.ns,
                               num_risk.cat = case_when(
                                 num_risk == 0 ~ "No_RF",
                                 num_risk == 1 ~ "One_RF",
                                 num_risk == 2 ~ "Two_RF",
                                 num_risk == 3 ~ "Three_RF",
                                 TRUE ~ NA_character_)
)
dfep.first.c2.nomeds.ns$num_risk.cat <- factor(dfep.first.c2.nomeds.ns$num_risk.cat, levels = c("No_RF", "One_RF", "Two_RF", "Three_RF"),
                                            labels = c("No risk factor", "1 risk factor",
                                                       "2 risk factors", "3 or more risk factors"))
summary(dfep.first.c2.nomeds.ns$num_risk.cat)


m.glmer.risk1.nomeds.ns = glmer(Rutgeerts_bin.num ~ num_risk.cat + age_diag + duration + disloc_peri + penetrating_compl +
                               surg_smbow + margin + resection_length + inter_surg_endo + (1|grc.a),
                             data = dfep.first.c2.nomeds.ns, family = binomial) 

gtsummary::tbl_regression(m.glmer.risk1.nomeds.ns, exp = T)



table1(~ Rutgeerts_bin | num_risk.cat, data= dfep.first.c2.nomeds.ns,
       topclass="Rtable1-grid Rtable1-shade Rtable1-times", overall = F,
       render.continuous = c(.="median [Q1, Q3]"))

table1(~ Rutgeerts | num_risk.cat, data= dfep.first.c2.nomeds.ns,
       topclass="Rtable1-grid Rtable1-shade Rtable1-times", overall = F,
       render.continuous = c(.="median [Q1, Q3]"))


df.plot2.ns <- dfep.first.c2.nomeds.ns %>% 
  group_by(num_risk.cat, Rutgeerts) %>% 
  summarise(count = n()) %>% 
  mutate(percent = count/sum(count))


summary(dfep.first.c2.nomeds.ns$num_risk.cat)

stacked2.ns <- ggplot(df.plot2.ns, aes(x = factor(num_risk.cat), y = percent*100,  fill= factor(Rutgeerts))) + 
  geom_bar(stat = "identity", width = 0.7, color = "black") +
  geom_text(aes(label=paste0(sprintf("%1.1f", percent*100),"%")),
            position=position_stack(vjust=0.5), colour="black", size = 5) +
  labs(x = "", y = "Percent", fill = "Rutgeerts score") +
  ggtitle("Only patients on no postop medication") +
  scale_fill_manual(values = c("#993404", "#FB6A4A", "#FED976", 
                               "#52854C", "#C3D7A4")) +
  scale_x_discrete(labels=c("No risk factor" = "No risk factor\n(n = 52)", "1 risk factor" = "1 risk factor\n(n = 59)",
                            "2 risk factors" = "2 risk factors\n(n= 3)")) +
  theme(legend.position = "right",
        legend.text = element_text(size = 18),
        legend.title = element_text(size = 18),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(size = 18), 
        axis.text.y = element_text(size = 18), 
        axis.title.x = element_text(size = 18), 
        axis.title.y =element_text(size = 18),
        plot.title = element_text(size = 18, hjust = 0.5),
        axis.line = element_line(colour = "black", size = 1, linetype = "solid"))

stacked2.ns



#######################################################
# ADDITIVE RISK FACTORS (ONLY ON MED USERS NO SMOKERS)
#######################################################

dfep.first.c2.meds.ns <- dfep.first.c2.ns[!(dfep.first.c2.ns$consortium_id %in% dfep.first.c.no.meds$consortium_id),]


dfep.first.c2.meds.ns$num_risk <- dfep.first.c2.meds.ns$sex_num + dfep.first.c2.meds.ns$race_bin_num

summary(dfep.first.c2.meds.ns$num_risk)

summary(dfep.first.c2.meds.ns$Rutgeerts_bin)

summary(dfep.first.c2.meds.ns$num_risk.cat)


dfep.first.c2.meds.ns <- mutate(dfep.first.c2.meds.ns,
                             num_risk.cat = case_when(
                               num_risk == 0 ~ "No_RF",
                               num_risk == 1 ~ "One_RF",
                               num_risk == 2 ~ "Two_RF",
                               num_risk == 3 ~ "Three_RF",
                               TRUE ~ NA_character_)
)
dfep.first.c2.meds.ns$num_risk.cat <- factor(dfep.first.c2.meds.ns$num_risk.cat, levels = c("No_RF", "One_RF", "Two_RF", "Three_RF"),
                                          labels = c("No risk factor", "1 risk factor",
                                                     "2 risk factors", "3 or more risk factors"))
summary(dfep.first.c2.meds.ns$num_risk.cat)




table1(~ Rutgeerts_bin | num_risk.cat, data= dfep.first.c2.meds.ns,
       topclass="Rtable1-grid Rtable1-shade Rtable1-times", overall = F,
       render.continuous = c(.="median [Q1, Q3]"))

table1(~ Rutgeerts | num_risk.cat, data= dfep.first.c2.meds.ns,
       topclass="Rtable1-grid Rtable1-shade Rtable1-times", overall = F,
       render.continuous = c(.="median [Q1, Q3]"))


m.glmer.risk1.meds.ns = glmer(Rutgeerts_bin.num ~ num_risk.cat + age_diag + duration + disloc_peri + penetrating_compl +
                             surg_smbow + margin + resection_length + inter_surg_endo ++
                               (1|grc.a),
                           data = dfep.first.c2.meds.ns, family = binomial) 

gtsummary::tbl_regression(m.glmer.risk1.meds.ns, exp = T)



df.plot3.ns <- dfep.first.c2.meds.ns %>% 
  group_by(num_risk.cat, Rutgeerts) %>% 
  summarise(count = n()) %>% 
  mutate(percent = count/sum(count))

summary(dfep.first.c2.meds.ns$num_risk.cat)

stacked3.ns <- ggplot(df.plot3.ns, aes(x = factor(num_risk.cat), y = percent*100,  fill= factor(Rutgeerts))) + 
  geom_bar(stat = "identity", width = 0.7, color = "black") +
  geom_text(aes(label=paste0(sprintf("%1.1f", percent*100),"%")),
            position=position_stack(vjust=0.5), colour="black", size = 5) +
  labs(x = "", y = "Percent", fill = "Rutgeerts score") +
  ggtitle("Only patients on postop medication") +
  scale_fill_manual(values = c("#993404", "#FB6A4A", "#FED976", 
                               "#52854C", "#C3D7A4")) +
  scale_x_discrete(labels=c("No risk factor" = "No risk factor\n(n = 82)", "1 risk factor" = "1 risk factor\n(n = 103)",
                            "2 risk factors" = "2 risk factors\n(n= 14)")) +
  theme(legend.position = "right",
        legend.text = element_text(size = 18),
        legend.title = element_text(size = 18),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(size = 18), 
        axis.text.y = element_text(size = 18), 
        axis.title.x = element_text(size = 18), 
        axis.title.y =element_text(size = 18),
        plot.title = element_text(size = 18, hjust = 0.5),
        axis.line = element_line(colour = "black", size = 1, linetype = "solid"))

stacked3.ns


##########################
# SUPPLEMENTARY FIGURE S3
###########################

ggarrange(stacked1.ns, stacked3.ns, stacked2.ns, ncol = 3, nrow = 1, common.legend = T,  labels = c("A", "B", "C"))


#####################
# AGA RISK CATEGORIES
#####################

###############
# ALL PATIENTS
###############

dfep.first.c3 <- dfep.first.c

dfep.first.c3 <- dfep.first.c3 %>% filter(!is.na(smoking_status))
dfep.first.c3 <- dfep.first.c3 %>% filter(!is.na(resection_length))

summary(dfep.first.c3$age_surg)

sum(dfep.first.c3$age_surg > 50)

sum(dfep.first.c3$age_surg < 30)

summary(dfep.first.c3$smoking_status)

summary(dfep.first.c3$surg_smbow)

summary(dfep.first.c3$resection_length)

nrow(dfep.first.c3[dfep.first.c3$resection_length < 20, ])

summary(dfep.first.c3$duration)

sum(dfep.first.c3$duration > 10)

summary(dfep.first.c3$dis_behavior_bin)

summary(dfep.first.c3$no_meds_since_last)

dfep.first.c3 <- mutate(dfep.first.c3,
                       risk_cat = case_when(
                        surg_smbow == "No" & dis_behavior_bin == "B1_B2" & resection_length < 20 & duration > 10 ~ "Low_risk",
                        age_surg < 30 & surg_smbow == "Yes" & dis_behavior_bin == "B3" ~ "High_risk",
                         TRUE ~ as.character("Moderate_risk")
                       ))
dfep.first.c3$risk_cat <- as.factor(dfep.first.c3$risk_cat)
summary(dfep.first.c3$risk_cat)


table1(~ Rutgeerts_bin + tnf_any_since_last + no_meds_since_last | risk_cat, data= dfep.first.c3,
       topclass="Rtable1-grid Rtable1-shade Rtable1-times", overall = F, extra.col = list("P-value" = pvalue),
       render.continuous = c(.="median [Q1, Q3]"))

summary(dfep.first.c3$risk_cat)

dfep.first.c3$risk_cat <- factor(dfep.first.c3$risk_cat, levels = c("Low_risk", "Moderate_risk", "High_risk"))

dfep.first.c3$Rutgeerts <- factor(dfep.first.c3$Rutgeerts, levels = c("i4", "i3", "i2", "i1", "i0"))

df.plot4 <- dfep.first.c3 %>% 
  group_by(risk_cat, Rutgeerts) %>% 
  summarise(count = n()) %>% 
  mutate(percent = count/sum(count))

summary(dfep.first.c3$risk_cat)

stacked4 <- ggplot(df.plot4, aes(x = factor(risk_cat), y = percent*100,  fill= factor(Rutgeerts))) + 
  geom_bar(stat = "identity", width = 0.7, color = "black") +
  geom_text(aes(label=paste0(sprintf("%1.1f", percent*100),"%")),
            position=position_stack(vjust=0.5), colour="black", size = 5) +
  labs(x = "", y = "Percent", fill = "Rutgeerts score") +
  ggtitle("All patients") +
  scale_fill_manual(values = c("#993404", "#FB6A4A", "#FED976", 
                               "#52854C", "#C3D7A4")) +
  scale_x_discrete(labels=c("Low_risk" = "Low risk\n(n = 12)", "Moderate_risk" = "Moderate risk\n(n = 283)",
                            "High_risk" = "High risk\n(n= 15)")) +
  theme(legend.position = "right",
        legend.text = element_text(size = 18),
        legend.title = element_text(size = 18),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(size = 18), 
        axis.text.y = element_text(size = 18), 
        axis.title.x = element_text(size = 18), 
        axis.title.y =element_text(size = 18),
        plot.title = element_text(size = 18, hjust = 0.5),
        axis.line = element_line(colour = "black", size = 1, linetype = "solid"))


stacked4


dfep.first.c3$risk_cat <- relevel(dfep.first.c3$risk_cat, ref = "Moderate_risk")

m.glmer.risk2 = glmer(Rutgeerts_bin.num ~ risk_cat + sex + race_bin + age_diag + duration + disloc_peri +
                       margin + inter_surg_endo +
                        tnf_any_current_upd + ustekinumab_current_upd  + (1|grc.a),
                      data = dfep.first.c3, family = binomial) 

gtsummary::tbl_regression(m.glmer.risk2, exp = T)


# ADDITIVE FACTORS
summary(dfep.first.c3$age_surg)
dfep.first.c3$age_surg_num <- ifelse(dfep.first.c3$age_surg < 30, 1, 0)
table(dfep.first.c3$age_surg_num, useNA = "always")

summary(dfep.first.c3$smoking_status)
dfep.first.c3$smoking_status_num <- ifelse(dfep.first.c3$smoking_status == "No", 0, 1)
table(dfep.first.c3$smoking_status_num, useNA = "always")

summary(dfep.first.c3$penetrating_compl)
summary(dfep.first.c3$surg_smbow)
dfep.first.c3$first_pen_num <- ifelse(dfep.first.c3$penetrating_compl == "Yes" & dfep.first.c3$surg_smbow == "Yes" & dfep.first.c3$resection_length > 20, 1, 0)
table(dfep.first.c3$first_pen_num)

summary(dfep.first.c3$duration)
dfep.first.c3$duration_num <- ifelse(dfep.first.c3$duration > 10, 0, 1)
table(dfep.first.c3$duration_num)


dfep.first.c3$num_risk_aga <- dfep.first.c3$age_surg_num + dfep.first.c3$smoking_status_num + dfep.first.c3$first_pen_num + dfep.first.c3$duration_num

summary(dfep.first.c3$num_risk_aga)

summary(dfep.first.c3$Rutgeerts_bin)

dfep.first.c3 <- mutate(dfep.first.c3,
                        num_risk_aga_cat = case_when(
                          num_risk_aga == 0 ~ "No_RF",
                          num_risk_aga == 1 ~ "One_RF",
                          num_risk_aga == 2 ~ "Two_RF",
                          num_risk_aga >= 3 ~ "Three_RF",
                          TRUE ~ NA_character_)
)
dfep.first.c3$num_risk_aga_cat <- factor(dfep.first.c3$num_risk_aga_cat, levels = c("No_RF", "One_RF", "Two_RF", "Three_RF"),
                                     labels = c("No risk factor", "1 risk factor",
                                                "2 risk factors", "3 or more risk factors"))
summary(dfep.first.c3$num_risk_aga_cat)


df.plot.aga1 <- dfep.first.c3 %>% 
  group_by(num_risk_aga_cat, Rutgeerts) %>% 
  summarise(count = n()) %>% 
  mutate(percent = count/sum(count))

summary(dfep.first.c3$num_risk_aga_cat)

stacked_aga1 <- ggplot(df.plot.aga1, aes(x = factor(num_risk_aga_cat), y = percent*100,  fill= factor(Rutgeerts))) + 
  geom_bar(stat = "identity", width = 0.7, color = "black") +
  geom_text(aes(label=paste0(sprintf("%1.1f", percent*100),"%")),
            position=position_stack(vjust=0.5), colour="black", size = 5) +
  labs(x = "", y = "Percent", fill = "Rutgeerts score") +
  ggtitle("All patients") +
  scale_fill_manual(values = c("#993404", "#FB6A4A", "#FED976", 
                               "#52854C", "#C3D7A4")) +
  scale_x_discrete(labels=c("No risk factor" = "No risk factor\n(n = 94)", "1 risk factor" = "1 risk factor\n(n = 82)",
                            "2 risk factors" = "2 risk factors\n(n= 124)", "3 or more risk factors" = "3 or more risk factors\n(n = 3)")) +
  theme(legend.position = "right",
        legend.text = element_text(size = 18),
        legend.title = element_text(size = 18),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(size = 18), 
        axis.text.y = element_text(size = 18), 
        axis.title.x = element_text(size = 18), 
        axis.title.y =element_text(size = 18),
        plot.title = element_text(size = 18, hjust = 0.5),
        axis.line = element_line(colour = "black", size = 1, linetype = "solid"))

stacked_aga1



m.glmer.risk.aga1 = glmer(Rutgeerts_bin.num ~ num_risk_aga_cat + disloc_peri +
                            margin + inter_surg_endo +
                             tnf_any_current_upd + ustekinumab_current_upd + (1|grc.a),
                           data = dfep.first.c3, family = binomial) 

gtsummary::tbl_regression(m.glmer.risk.aga1, exp = T)




################################################
# AGA RISK FACTORS (ONLY ON MED USERS)
###############################################

dfep.first.c3.meds <- dfep.first.c3[!(dfep.first.c3$consortium_id %in% dfep.first.c.no.meds$consortium_id),]

summary(dfep.first.c3.meds$age_surg)

sum(dfep.first.c3.meds$age_surg > 50)

summary(dfep.first.c3.meds$smoking_status)

summary(dfep.first.c3.meds$surg_smbow)

summary(dfep.first.c3.meds$resection_length)

nrow(dfep.first.c3.meds[dfep.first.c3.meds$resection_length < 20, ])

summary(dfep.first.c3.meds$duration)

sum(dfep.first.c3.meds$duration > 10)

summary(dfep.first.c3.meds$dis_behavior_bin)

summary(dfep.first.c3.meds$no_meds_since_last)

dfep.first.c3.meds <- mutate(dfep.first.c3.meds,
                               risk_cat = case_when(
                                 surg_smbow == "No" & dis_behavior_bin == "B1_B2" & resection_length < 20 & duration < 10 ~ "Low_risk",
                                 age_surg < 30 & surg_smbow == "Yes" & dis_behavior_bin == "B3" ~ "High_risk",
                                 TRUE ~ as.character("Moderate_risk")
                               ))
dfep.first.c3.meds$risk_cat <- as.factor(dfep.first.c3.meds$risk_cat)
summary(dfep.first.c3.meds$risk_cat)

dfep.first.c3.meds$risk_cat <- factor(dfep.first.c3.meds$risk_cat, levels = c("Low_risk", "Moderate_risk", "High_risk"))


table1(~ Rutgeerts_bin + tnf_any_since_last + no_meds_since_last | risk_cat, data= dfep.first.c3.meds,
       topclass="Rtable1-grid Rtable1-shade Rtable1-times", overall = F, extra.col = list("P-value" = pvalue),
       render.continuous = c(.="median [Q1, Q3]"))

df.plot5 <- dfep.first.c3.meds %>% 
  group_by(risk_cat, Rutgeerts) %>% 
  summarise(count = n()) %>% 
  mutate(percent = count/sum(count))

summary(dfep.first.c3.meds$risk_cat)

stacked5 <- ggplot(df.plot5, aes(x = factor(risk_cat), y = percent*100,  fill= factor(Rutgeerts))) + 
  geom_bar(stat = "identity", width = 0.7, color = "black") +
  geom_text(aes(label=paste0(sprintf("%1.1f", percent*100),"%")),
            position=position_stack(vjust=0.5), colour="black", size = 5) +
  labs(x = "", y = "Percent", fill = "Rutgeerts score") +
  ggtitle("Only patients on postop medication") +
  scale_fill_manual(values = c("#993404", "#FB6A4A", "#FED976", 
                               "#52854C", "#C3D7A4")) +
  scale_x_discrete(labels=c("Low_risk" = "Low risk\n(n = 18)", "Moderate_risk" = "Moderate risk\n(n = 185)",
                            "High_risk" = "High risk\n(n= 13)")) +
  theme(legend.position = "right",
        legend.text = element_text(size = 18),
        legend.title = element_text(size = 18),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(size = 18), 
        axis.text.y = element_text(size = 18), 
        axis.title.x = element_text(size = 18), 
        axis.title.y =element_text(size = 18),
        plot.title = element_text(size = 18, hjust = 0.5),
        axis.line = element_line(colour = "black", size = 1, linetype = "solid"))

stacked5

m.glmer.risk2.meds = glmer(Rutgeerts_bin.num ~ risk_cat + sex + race_bin + age_diag + duration + disloc_peri +
                        margin + inter_surg_endo +
                        tnf_any_current_upd + ustekinumab_current_upd  + (1|grc.a),
                      data = dfep.first.c3.meds, family = binomial) 

gtsummary::tbl_regression(m.glmer.risk2.meds, exp = T)


# ADDITIVE FACTORS
summary(dfep.first.c3.meds$age_surg)
dfep.first.c3.meds$age_surg_num <- ifelse(dfep.first.c3.meds$age_surg < 30, 1, 0)
table(dfep.first.c3.meds$age_surg_num, useNA = "always")

summary(dfep.first.c3.meds$smoking_status)
dfep.first.c3.meds$smoking_status_num <- ifelse(dfep.first.c3.meds$smoking_status == "No", 0, 1)
table(dfep.first.c3.meds$smoking_status_num, useNA = "always")

summary(dfep.first.c3.meds$penetrating_compl)
summary(dfep.first.c3.meds$surg_smbow)
dfep.first.c3.meds$first_pen_num <- ifelse(dfep.first.c3.meds$penetrating_compl == "Yes" & dfep.first.c3.meds$surg_smbow == "Yes" & dfep.first.c3.meds$resection_length > 20, 1, 0)
table(dfep.first.c3.meds$first_pen_num)

summary(dfep.first.c3.meds$duration)
dfep.first.c3.meds$duration_num <- ifelse(dfep.first.c3.meds$duration > 10, 0, 1)
table(dfep.first.c3.meds$duration_num)


dfep.first.c3.meds$num_risk_aga2 <- dfep.first.c3.meds$age_surg_num + dfep.first.c3.meds$smoking_status_num + dfep.first.c3.meds$first_pen_num + dfep.first.c3.meds$duration_num

summary(dfep.first.c3.meds$num_risk_aga2)

summary(dfep.first.c3.meds$Rutgeerts_bin)

dfep.first.c3.meds <- mutate(dfep.first.c3.meds,
                        num_risk_aga_cat2 = case_when(
                          num_risk_aga2 == 0 ~ "No_RF",
                          num_risk_aga2 == 1 ~ "One_RF",
                          num_risk_aga2 == 2 ~ "Two_RF",
                          num_risk_aga2 >= 3 ~ "Three_RF",
                          TRUE ~ NA_character_)
)
dfep.first.c3.meds$num_risk_aga_cat2 <- factor(dfep.first.c3.meds$num_risk_aga_cat2, levels = c("No_RF", "One_RF", "Two_RF", "Three_RF"),
                                         labels = c("No risk factor", "1 risk factor",
                                                    "2 risk factors", "3 or more risk factors"))
summary(dfep.first.c3.meds$num_risk_aga_cat2)


df.plot.aga2 <- dfep.first.c3.meds %>% 
  group_by(num_risk_aga_cat2, Rutgeerts) %>% 
  summarise(count = n()) %>% 
  mutate(percent = count/sum(count))

summary(dfep.first.c3.meds$num_risk_aga_cat2)

stacked_aga2 <- ggplot(df.plot.aga2, aes(x = factor(num_risk_aga_cat2), y = percent*100,  fill= factor(Rutgeerts))) + 
  geom_bar(stat = "identity", width = 0.7, color = "black") +
  geom_text(aes(label=paste0(sprintf("%1.1f", percent*100),"%")),
            position=position_stack(vjust=0.5), colour="black", size = 5) +
  labs(x = "", y = "Percent", fill = "Rutgeerts score") +
  ggtitle("Only patients on postop medication") +
  scale_fill_manual(values = c("#993404", "#FB6A4A", "#FED976", 
                               "#52854C", "#C3D7A4")) +
  scale_x_discrete(labels=c("No risk factor" = "No risk factor\n(n = 53)", "1 risk factor" = "1 risk factor\n(n = 53)",
                            "2 risk factors" = "2 risk factors\n(n = 87)", "3 or more risk factors" = "3 or more risk factors\n(n = 3)")) +
  theme(legend.position = "right",
        legend.text = element_text(size = 18),
        legend.title = element_text(size = 18),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(size = 18), 
        axis.text.y = element_text(size = 18), 
        axis.title.x = element_text(size = 18), 
        axis.title.y =element_text(size = 18),
        plot.title = element_text(size = 18, hjust = 0.5),
        axis.line = element_line(colour = "black", size = 1, linetype = "solid"))

stacked_aga2



m.glmer.risk.aga2 = glmer(Rutgeerts_bin.num ~ num_risk_aga_cat2 + disloc_peri +
                            margin + inter_surg_endo +
                            tnf_any_current_upd + ustekinumab_current_upd + (1|grc.a),
                          data = dfep.first.c3.meds, family = binomial) 

gtsummary::tbl_regression(m.glmer.risk.aga2, exp = T)


################################################
# AGA RISK FACTORS (ONLY ON NO MEDS)
###############################################

dfep.first.c3.nomeds <- dfep.first.c3[dfep.first.c3$consortium_id %in% dfep.first.c.no.meds$consortium_id,]

summary(dfep.first.c3.nomeds$age_surg)

sum(dfep.first.c3.nomeds$age_surg > 50)

summary(dfep.first.c3.nomeds$smoking_status)

summary(dfep.first.c3.nomeds$surg_smbow)

summary(dfep.first.c3.nomeds$resection_length)

nrow(dfep.first.c3.nomeds[dfep.first.c3.nomeds$resection_length < 20, ])

summary(dfep.first.c3.nomeds$duration)

sum(dfep.first.c3.nomeds$duration > 10)

summary(dfep.first.c3.nomeds$dis_behavior_bin)

summary(dfep.first.c3.nomeds$no_meds_since_last)

dfep.first.c3.nomeds <- mutate(dfep.first.c3.nomeds,
                        risk_cat = case_when(
                          surg_smbow == "No" & dis_behavior_bin == "B1_B2" & resection_length < 20 & duration < 10 ~ "Low_risk",
                          surg_smbow == "Yes" & dis_behavior_bin == "B3" ~ "High_risk",
                          TRUE ~ as.character("Moderate_risk")
                        ))
dfep.first.c3.nomeds$risk_cat <- as.factor(dfep.first.c3.nomeds$risk_cat)
summary(dfep.first.c3.nomeds$risk_cat)

dfep.first.c3.nomeds$risk_cat <- factor(dfep.first.c3.nomeds$risk_cat, levels = c("Low_risk", "Moderate_risk", "High_risk"))


table1(~ Rutgeerts_bin + tnf_any_since_last + no_meds_since_last | risk_cat, data= dfep.first.c3.nomeds,
       topclass="Rtable1-grid Rtable1-shade Rtable1-times", overall = F, extra.col = list("P-value" = pvalue),
       render.continuous = c(.="median [Q1, Q3]"))

df.plot6 <- dfep.first.c3.nomeds %>% 
  group_by(risk_cat, Rutgeerts) %>% 
  summarise(count = n()) %>% 
  mutate(percent = count/sum(count))

summary(dfep.first.c3.nomeds$risk_cat)

stacked6 <- ggplot(df.plot6, aes(x = factor(risk_cat), y = percent*100,  fill= factor(Rutgeerts))) + 
  geom_bar(stat = "identity", width = 0.7, color = "black") +
  geom_text(aes(label=paste0(sprintf("%1.1f", percent*100),"%")),
            position=position_stack(vjust=0.5), colour="black", size = 5) +
  labs(x = "", y = "Percent", fill = "Rutgeerts score") +
  ggtitle("Only patients on no postop medication") +
  scale_fill_manual(values = c("#993404", "#FB6A4A", "#FED976", 
                               "#52854C", "#C3D7A4")) +
  scale_x_discrete(labels=c("Low_risk" = "Low risk\n(n = 8)", "Moderate_risk" = "Moderate risk\n(n = 92)",
                            "High_risk" = "High risk\n(n= 9)")) +
  theme(legend.position = "right",
        legend.text = element_text(size = 18),
        legend.title = element_text(size = 18),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(size = 18), 
        axis.text.y = element_text(size = 18), 
        axis.title.x = element_text(size = 18), 
        axis.title.y =element_text(size = 18),
        plot.title = element_text(size = 18, hjust = 0.5),
        axis.line = element_line(colour = "black", size = 1, linetype = "solid"))

stacked6


m.glmer.risk2.nomeds = glmer(Rutgeerts_bin.num ~ risk_cat + sex + race_bin + age_diag + duration + disloc_peri +
                             margin + inter_surg_endo + (1|grc.a),
                           data = dfep.first.c3.nomeds, family = binomial) 

gtsummary::tbl_regression(m.glmer.risk2.nomeds, exp = T)


###########################
# SUPPLEMENTARY FIGURE S4
############################

ggarrange(stacked4, stacked5, stacked6, ncol = 3, nrow = 1, common.legend = T,  labels = c("A", "B", "C"))



# ADDITIVE FACTORS
summary(dfep.first.c3.nomeds$age_surg)
dfep.first.c3.nomeds$age_surg_num <- ifelse(dfep.first.c3.nomeds$age_surg < 30, 1, 0)
table(dfep.first.c3.nomeds$age_surg_num, useNA = "always")

summary(dfep.first.c3.nomeds$smoking_status)
dfep.first.c3.nomeds$smoking_status_num <- ifelse(dfep.first.c3.nomeds$smoking_status == "No", 0, 1)
table(dfep.first.c3.nomeds$smoking_status_num, useNA = "always")

summary(dfep.first.c3.nomeds$penetrating_compl)
summary(dfep.first.c3.nomeds$surg_smbow)
dfep.first.c3.nomeds$first_pen_num <- ifelse(dfep.first.c3.nomeds$penetrating_compl == "Yes" & dfep.first.c3.nomeds$surg_smbow == "Yes" & dfep.first.c3.nomeds$resection_length > 20, 1, 0)
table(dfep.first.c3.nomeds$first_pen_num)

summary(dfep.first.c3.nomeds$duration)
dfep.first.c3.nomeds$duration_num <- ifelse(dfep.first.c3.nomeds$duration > 10, 0, 1)
table(dfep.first.c3.nomeds$duration_num)


dfep.first.c3.nomeds$num_risk_aga3 <- dfep.first.c3.nomeds$age_surg_num + dfep.first.c3.nomeds$smoking_status_num + dfep.first.c3.nomeds$first_pen_num + dfep.first.c3.nomeds$duration_num

summary(dfep.first.c3.nomeds$num_risk_aga3)

summary(dfep.first.c3.nomeds$Rutgeerts_bin)

dfep.first.c3.nomeds <- mutate(dfep.first.c3.nomeds,
                             num_risk_aga_cat3 = case_when(
                               num_risk_aga3 == 0 ~ "No_RF",
                               num_risk_aga3 == 1 ~ "One_RF",
                               num_risk_aga3 == 2 ~ "Two_RF",
                               num_risk_aga3 >= 3 ~ "Three_RF",
                               TRUE ~ NA_character_)
)
dfep.first.c3.nomeds$num_risk_aga_cat3 <- factor(dfep.first.c3.nomeds$num_risk_aga_cat3, levels = c("No_RF", "One_RF", "Two_RF", "Three_RF"),
                                               labels = c("No risk factor", "1 risk factor",
                                                          "2 risk factors", "3 or more risk factors"))
summary(dfep.first.c3.nomeds$num_risk_aga_cat3)


df.plot.aga3 <- dfep.first.c3.nomeds %>% 
  group_by(num_risk_aga_cat3, Rutgeerts) %>% 
  summarise(count = n()) %>% 
  mutate(percent = count/sum(count))

summary(dfep.first.c3.nomeds$num_risk_aga_cat3)

stacked_aga3 <- ggplot(df.plot.aga3, aes(x = factor(num_risk_aga_cat3), y = percent*100,  fill= factor(Rutgeerts))) + 
  geom_bar(stat = "identity", width = 0.7, color = "black") +
  geom_text(aes(label=paste0(sprintf("%1.1f", percent*100),"%")),
            position=position_stack(vjust=0.5), colour="black", size = 5) +
  labs(x = "", y = "Percent", fill = "Rutgeerts score") +
  ggtitle("Only patients on no postop medication") +
  scale_fill_manual(values = c("#993404", "#FB6A4A", "#FED976", 
                               "#52854C", "#C3D7A4")) +
  scale_x_discrete(labels=c("No risk factor" = "No risk factor\n(n = 41)", "1 risk factor" = "1 risk factor\n(n = 29)",
                            "2 risk factors" = "2 risk factors\n(n= 37)", "3 or more risk factors" = "3 or more risk factors\n(n = 3)")) +
  theme(legend.position = "right",
        legend.text = element_text(size = 18),
        legend.title = element_text(size = 18),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(size = 18), 
        axis.text.y = element_text(size = 18), 
        axis.title.x = element_text(size = 18), 
        axis.title.y =element_text(size = 18),
        plot.title = element_text(size = 18, hjust = 0.5),
        axis.line = element_line(colour = "black", size = 1, linetype = "solid"))

stacked_aga3



m.glmer.risk.aga3 = glmer(Rutgeerts_bin.num ~ num_risk_aga_cat3 + disloc_peri +
                            margin + inter_surg_endo + (1|grc.a),
                          data = dfep.first.c3.nomeds, family = binomial) 

gtsummary::tbl_regression(m.glmer.risk.aga3, exp = T)


###########################
# SUPPLEMENTARY FIGURE S4
############################

ggarrange(stacked_aga1, stacked_aga2, stacked_aga3, ncol = 3, nrow = 1, common.legend = T,  labels = c("A", "B", "C"))



# > sessionInfo()
# R version 4.1.1 (2021-08-10)
# Platform: x86_64-apple-darwin17.0 (64-bit)
# Running under: macOS Monterey 12.6
# 
# Matrix products: default
# LAPACK: /Library/Frameworks/R.framework/Versions/4.1/Resources/lib/libRlapack.dylib
# 
# locale:
#   [1] en_CA.UTF-8/en_CA.UTF-8/en_CA.UTF-8/C/en_CA.UTF-8/en_CA.UTF-8
# 
# attached base packages:
#   [1] stats     graphics  grDevices utils     datasets  methods   base     
# 
# other attached packages:
#   [1] visdat_0.5.3    naniar_0.6.1    lubridate_1.8.0 table1_1.4.2    forcats_0.5.2   stringr_1.4.1  
# [7] dplyr_1.0.10    purrr_0.3.4     readr_2.1.2     tidyr_1.2.1     tibble_3.1.8    ggplot2_3.4.0  
# [13] tidyverse_1.3.2
# 
# loaded via a namespace (and not attached):
#   [1] tidyselect_1.1.2    xfun_0.33           haven_2.5.3         gargle_1.2.1       
# [5] colorspace_2.0-3    vctrs_0.5.1         generics_0.1.3      htmltools_0.5.3    
# [9] utf8_1.2.2          rlang_1.0.6         pillar_1.8.1        glue_1.6.2         
# [13] withr_2.5.0         DBI_1.1.3           dbplyr_2.2.1        modelr_0.1.9       
# [17] readxl_1.4.1        lifecycle_1.0.3     plyr_1.8.7          munsell_0.5.0      
# [21] gtable_0.3.1        cellranger_1.1.0    rvest_1.0.3         labeling_0.4.2     
# [25] knitr_1.40          fastmap_1.1.0       tzdb_0.3.0          fansi_1.0.3        
# [29] broom_1.0.1         Rcpp_1.0.9          scales_1.2.1        backports_1.4.1    
# [33] googlesheets4_1.0.1 jsonlite_1.8.0      farver_2.1.1        fs_1.5.2           
# [37] digest_0.6.29       hms_1.1.2           stringi_1.7.8       grid_4.1.1         
# [41] cli_3.4.1           tools_4.1.1         magrittr_2.0.3      Formula_1.2-4      
# [45] crayon_1.5.1        pkgconfig_2.0.3     ellipsis_0.3.2      xml2_1.3.3         
# [49] reprex_2.0.2        googledrive_2.0.0   assertthat_0.2.1    httr_1.4.4         
# [53] rstudioapi_0.14     R6_2.5.1            compiler_4.1.1   



