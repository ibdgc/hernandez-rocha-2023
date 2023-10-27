library(tidyverse)
library(table1)

rm(list=ls())
set.seed(1)

##################
# UPLOADING FILES
##################

setwd("/Users/cristian/Desktop/Rcharger/NIDDK")

# The following data manually modified in the csv files (Pending to fix in RedCap)

# These 2 cases without sex and yob data had a consortium id prior to entry into the CD Ileal Study so the REDCap
# registration Form was not used and therefore these variables are blank
# C864819-992974: Female. YOB: 1991
# C824372-954071: Female. YOB: 1970

# C805435-952954: has a wrong yob (2015). This was manually corrected (1958)
# C888743-939265: White

# The following Rutgeerts score/colonoscopies were manually changed based on the report 
# C988720-955135: Colonoscopy labeled as second in this patient is the first one (not changed in redcap yet).
# C874841-921908: second colonoscopy is not i3, but i0. Already changed in RedCap but not captured in current csv file
# C839456-968754: third is the second and the second is the third (not changed in redcap yet)


# Participants file (presurgical data)
dfp <- read.csv("participants_2022-06-07_modified.csv",header=T, na.strings=c(""), stringsAsFactors = T)

# Endoscopies file (postsurgical data)
dfe <- read.csv("endoscopies_2022-06-07_modified.csv", header=T, na.strings=c(""), stringsAsFactors = T)


#############################
# PREPARING RUTGEERTS SCORE 
############################

# Original Rutgeerts
# There i a problem with the labeling of this variable
# The separation of i2 is incorrect
summary(dfe$rutgeerts_v1)

table1(~ rutgeerts_v1, data=dfe, overall = "Total",
       topclass="Rtable1-grid Rtable1-shade Rtable1-times",
       render.continuous = c(.="Median [Min, Max]"))

dfe$Rutgeerts <- dfe$rutgeerts_v1

# Simplifying labels
dfe$Rutgeerts <- factor(dfe$Rutgeerts,
                            levels = c("i0: No lesions in the distal ileum (Normal)",
                                       "i1: 5 or fewer aphthous ulcers in the distal ileum",
                                       "i2: > 5 aphthous ulcers, normal intervening mucosa OR skip areas between larger lesions OR lesions confined to ileocolonic anastomosis", 
                                       "i2a: lesions confined to the ileocolonic anastomosis (including anastomotic stenosis)",
                                       "i3: Diffuse aphthous ileitis with diffusely inflamed mucosa", 
                                       "i4: Large ulcers with diffuse mucosal inflammation or nodules or stenosis in the neoterminal ileum",
                                       "Unknown"),
                            labels = c("i0", "i1", "i2", "i2", "i3", "i4", "Unknown"))
summary(dfe$Rutgeerts)


# Modified Rutgeerts
summary(dfe$rutgeerts)

table1(~ rutgeerts, data=dfe, overall = "Total",
       topclass="Rtable1-grid Rtable1-shade Rtable1-times",
       render.continuous = c(.="Median [Min, Max]"))

dfe$Modified_Rutgeerts <- dfe$rutgeerts

dfe$Modified_Rutgeerts <- factor(dfe$Modified_Rutgeerts,
                         levels = c("i0: No lesions in the distal ileum (Normal)",
                                    "i1: 5 or fewer aphthous ulcers in the distal ileum",
                                    "i2a: lesions confined to the ileocolonic anastomosis (including anastomotic stenosis)", 
                                    "i2b: more than 5 aphthous ulcers or larger lesions, with normal mucosa in-between, in the neoterminal ileum (with or without anastomotic lesions)", 
                                    "i3: Diffuse aphthous ileitis with diffusely inflamed mucosa", 
                                    "i4: Large ulcers with diffuse mucosal inflammation or nodules or stenosis in the neoterminal ileum",
                                    "Unknown"),
                         labels = c("i0", "i1", "i2a", "i2b", "i3", "i4", "Unknown"))

summary(dfe$Modified_Rutgeerts)


table1(~ Modified_Rutgeerts | redcap_event_name, data=dfe, overall = "Total",
       topclass="Rtable1-grid Rtable1-shade Rtable1-times",
       render.continuous = c(.="Median [Min, Max]"))


# Creating dichotomous Rutgeerts

dfe$Rutgeerts_bin <- plyr::revalue(dfe$Rutgeerts, 
                            c("i0" = "i0_i1", "i1" = "i0_i1", "i2" = "i2_i4", "i3" = "i2_i4", "i4" = "i2_i4", "Unknown" = "Unknown"))
summary(dfe$Rutgeerts_bin)

dfe$Modified_Rutgeerts_bin <- plyr::revalue(dfe$Modified_Rutgeerts, 
                             c("i0" = "i0_i2a", "i1" = "i0_i2a", "i2a" = "i0_i2a", "i2b" = "i2b_i4", "i3" = "i2b_i4", "i4" = "i2b_i4", "Unknown" = "Unknown"))
summary(dfe$Modified_Rutgeerts_bin)



# Optional dichotomization

# Stringent definition of remission (i0 vs i1-i4)
dfe$Rutgeerts_bin22 <- plyr::revalue(dfe$Rutgeerts, 
                                   c("i0" = "i0", "i1" = "i1_i4", "i2" = "i1_i4", "i3" = "i1_i4", "i4" = "i1_i4", "Unknown" = "Unknown"))
summary(dfe$Rutgeerts_bin22)

# Severe recurrence (i1-i2 vs i3-i4)
dfe$Rutgeerts_bin3 <- plyr::revalue(dfe$Rutgeerts, 
                                    c("i0" = "i0_i2", "i1" = "i0_i2", "i2" = "i0_i2", "i3" = "i3_i4", "i4" = "i3_i4", "Unknown" = "Unknown"))
summary(dfe$Rutgeerts_bin3)


####################################
# FORMATTING VARIABLES OF INTEREST
####################################

######
# SEX
######

summary(dfp$sex)

#######################
# AGE AT INDEX SURGERY
#######################

summary(dfp$yob)
summary(dfp$index_date)

# Extracting year of index surgery
summary(dfp$index_date)
dfp$yosurg <- format(as.Date(dfp$index_date, format="%Y-%m-%d"),"%Y")
dfp$yosurg <- as.numeric(dfp$yosurg)
summary(dfp$yosurg)

# Creating Age at index surgery 
dfp$age_surg <- as.numeric(dfp$yosurg - dfp$yob)
summary(dfp$age_surg)


######################
# AGE AT CD DIAGNOSIS
######################

summary(dfp$diag_yr) 
summary(dfp$yob)

## Age at IBD diagnosis
dfp$age_diag <- (dfp$diag_yr - dfp$yob)
summary(dfp$age_diag)


# Creating Montreal classification (A)

length(which(dfp$age_diag <= 16))
length(which(dfp$age_diag > 16 & dfp$age_diag <= 40))
length(which(dfp$age_diag > 40))

dfp <- mutate(dfp,
              age_montreal = case_when(
                      age_diag <= 16 ~ "A1",
                      age_diag > 16 & age_diag <= 40 ~ "A2",
                      age_diag > 40 ~ "A3",
                      TRUE ~ NA_character_)
)
dfp$age_montreal <- as.factor(dfp$age_montreal)
summary(dfp$age_montreal)


###################
# DISEASE DURATION
###################

summary(dfp$yosurg)
summary(dfp$diag_yr)

dfp$duration <- (dfp$yosurg - dfp$diag_yr) 

summary(dfp$duration)


######
# RACE
######

summary(dfp$race)

dfp <- mutate(dfp,
              race_bin = case_when(
                      race == "Asian" | race == "Black/African American" |
                              race == "Other (specify below)" | race == "Native Hawaiian/Pacific Islander" ~ "Non_white",
                      TRUE ~ as.character(race)
              ))
dfp$race_bin <- as.factor(dfp$race_bin)
summary(dfp$race_bin)


#########
# JEWISH
#########

summary(dfp$jewish)

###########
# HISPANIC
###########

summary(dfp$hispanic)


#########################
# COUNTRY OF RECRUITMENT
#########################

summary(dfp$grc)

dfp <- mutate(dfp,
              country = case_when(
                      grc == "Cedars-Sinai Medical Center" | grc == "Emory University" | grc == "Icahn School of Medicine at Mt. Sinai" | grc == "Johns Hopkins University" | grc == "University of Pittsburgh" ~ "USA",
                      grc == "Mt. Sinai Hospital and University of Toronto" | grc == "University of Montreal" ~ "CAN",
                      TRUE ~ NA_character_)
)
dfp$country<- as.factor(dfp$country)
summary(dfp$country)


##################
# GRC ABBREVIATED
#################

dfp$grc.a <- dfp$grc

dfp$grc.a <- gsub("Cedars-Sinai Medical Center", "CSMC", dfp$grc.a)
dfp$grc.a <- gsub("Emory University", "EM", dfp$grc.a)
dfp$grc.a <- gsub("Icahn School of Medicine at Mt. Sinai", "ISMMS", dfp$grc.a)
dfp$grc.a <- gsub("Johns Hopkins University", "JHU", dfp$grc.a)
dfp$grc.a <- gsub("University of Pittsburgh", "Pitt", dfp$grc.a)
dfp$grc.a <- gsub("Mt. Sinai Hospital and University of Toronto", "MSH", dfp$grc.a)
dfp$grc.a <- gsub("University of Montreal", "UdeM", dfp$grc.a)

dfp$grc.a <- as.factor(dfp$grc.a)
summary(dfp$grc.a)


###################
# DISEASE LOCATION
###################

# Creating ileal disease (only taking distal and terminal ileal disease)

summary(dfp$disloc_proxil)
summary(dfp$disloc_distil)
summary(dfp$disloc_termil)

summary(dfp$disloc_ileal_legacy)


dfp <- mutate(dfp,
              ileal = case_when(
                      disloc_distil == "Yes" | disloc_termil == "Yes" ~ "Yes",
                      disloc_distil == "No" & disloc_termil == "No" ~ "No",
                      disloc_distil == "Unknown" | disloc_termil == "Unknown" ~ "Unknown",
                      TRUE ~ NA_character_)
)
dfp$ileal <- as.factor(dfp$ileal)
summary(dfp$ileal)


# Creating colonic disease

summary(dfp$disloc_cecum)
summary(dfp$disloc_colon)
summary(dfp$disloc_rectum)

dfp <- mutate(dfp,
              colo_rec = case_when(
                disloc_cecum == "Yes" | disloc_colon == "Yes" | disloc_rectum == "Yes" ~ "Yes",
                disloc_cecum == "No" & disloc_colon == "No" & disloc_rectum == "No" ~ "No",
                disloc_cecum == "Unknown" | disloc_colon == "Unknown" | disloc_rectum == "Unknown" ~ "Unknown",
                TRUE ~ NA_character_)
)
dfp$colo_rec <- as.factor(dfp$colo_rec)
summary(dfp$colo_rec)


# Creating Montreal location

summary(dfp$ileal)
summary(dfp$colo_rec)

dfp <- mutate(dfp,
              montreal_loc = case_when(
                ileal == "Yes" & colo_rec == "Yes" ~ "L3",
                ileal == "Yes" & colo_rec == "No" ~ "L1",
                ileal == "No" ~ "No_ileal",
                ileal == "Unknown" ~ "No_ileal",
                ileal == "Unknown" | colo_rec == "Unknown" ~ "Unknown",
                TRUE ~ NA_character_)
)
dfp$montreal_loc <- as.factor(dfp$montreal_loc)
summary(dfp$montreal_loc)


dfp <- mutate(dfp,
              montreal_loc2 = case_when(
                      montreal_loc == "L1" | montreal_loc == "No_ileal" ~ "L1",
                      TRUE ~ as.character(montreal_loc)
              ))
dfp$montreal_loc2 <- as.factor(dfp$montreal_loc2)
summary(dfp$montreal_loc2)



##################
# PERIANAL DISEASE
#################
summary(dfp$disloc_peri)


###################
## DISEASE BEHAVIOR
###################

# Merging the  very few B1 and B2
summary(dfp$dis_behavior)

dfp <- mutate(dfp,
              dis_behavior_bin = case_when(
                      dis_behavior == "B1" | dis_behavior == "B2" ~ "B1_B2",
                      TRUE ~ as.character(dis_behavior)
              ))
dfp$dis_behavior_bin <- as.factor(dfp$dis_behavior_bin)
summary(dfp$dis_behavior_bin)


#####################
# PREVIOUS RESECTION
#####################

# Surgery for complication or treatment of CD:
summary(dfp$prior_surg)

# Small bowel resection (Branching logic variable)
summary(dfp$surg_smbow)

dfp <- mutate(dfp,
              surg_smbow2 = case_when(
                      prior_surg == "No" & is.na(surg_smbow) ~ "No",
                      TRUE ~ as.character(surg_smbow))
)
dfp$surg_smbow2 <- as.factor(dfp$surg_smbow2)
summary(dfp$surg_smbow2)


# Number of surgeries
dfp$surg_abdom_num_legacy <- as.factor(dfp$surg_abdom_num_legacy)
summary(dfp$surg_abdom_num_legacy)

dfp <- mutate(dfp,
              num_surg = case_when(
                      surg_smbow2 == "No" ~ "0",
                      TRUE ~ as.character(surg_abdom_num_legacy))
)
dfp$num_surg <- as.factor(dfp$num_surg)
summary(dfp$num_surg)


#####################
# SURGICAL FINDINGS
####################

# Stricture
summary(dfp$surgical_findings1)

# Fistula
summary(dfp$surgical_findings2)

# Abscess
summary(dfp$surgical_findings3)

# None of the above
summary(dfp$surgical_findings4)

# Unknown
summary(dfp$surgical_findings5)

dfp <- mutate(dfp,
              surg_findings = case_when(
                      surgical_findings2 == "Checked" | surgical_findings3 == "Checked" ~ "Penetrating",
                      surgical_findings1 == "Checked" & surgical_findings2 == "Checked" | surgical_findings3 == "Checked" ~ "Penetrating",
                      surgical_findings1 == "Checked" & surgical_findings2 == "Unchecked" & surgical_findings3 == "Unchecked" ~ "Only_Stricturing",
                      surgical_findings1 == "Unchecked" & surgical_findings2 == "Unchecked" & surgical_findings3 == "Unchecked" & surgical_findings4 == "Checked" ~ "Others",
                      TRUE ~ NA_character_)
)
dfp$surg_findings <- as.factor(dfp$surg_findings)
summary(dfp$surg_findings)



dfp <- mutate(dfp,
              surg_findings22 = case_when(
                      surgical_findings1 == "Checked" & surgical_findings2 == "Unchecked" & surgical_findings3 == "Unchecked" ~ "Only_Stricturing",
                      surgical_findings1 == "Unchecked" & surgical_findings2 == "Checked" | surgical_findings3 == "Checked" ~ "Only_Penetrating",
                      surgical_findings1 == "Checked" & surgical_findings2 == "Checked" ~ "Stricturing_Penetrating",
                      surgical_findings1 == "Checked" & surgical_findings3 == "Checked" ~ "Stricturing_Penetrating",
                      surgical_findings1 == "Unchecked" & surgical_findings2 == "Unchecked" & surgical_findings3 == "Unchecked" & surgical_findings4 == "Checked" ~ "Others",
                      TRUE ~ NA_character_)
)
dfp$surg_findings22 <- as.factor(dfp$surg_findings22)
summary(dfp$surg_findings22)


#############################
# OTHER CONCOMITANT SURGERIES
############################

# Separate additional small bowel resection

summary(dfp$other_surgery1)

dfp <- mutate(dfp,
              other_sbow_res = case_when(
                      other_surgery1 == "Checked"  ~ "Yes",
                      other_surgery1 == "Unchecked" ~ "No",
                     TRUE ~ NA_character_)
)
dfp$other_sbow_res <- as.factor(dfp$other_sbow_res)
summary(dfp$other_sbow_res)


# Strictureplasty

summary(dfp$other_surgery2)

dfp <- mutate(dfp,
              other_stric_plasty = case_when(
                      other_surgery2 == "Checked"  ~ "Yes",
                      other_surgery2 == "Unchecked" ~ "No",
                      TRUE ~ NA_character_)
)
dfp$other_stric_plasty <- as.factor(dfp$other_stric_plasty)
summary(dfp$other_stric_plasty)



# Separate colonic resection

summary(dfp$other_surgery3)

dfp <- mutate(dfp,
              other_lbow_res = case_when(
                      other_surgery3 == "Checked"  ~ "Yes",
                      other_surgery3 == "Unchecked" ~ "No",
                      TRUE ~ NA_character_)
)
dfp$other_lbow_res <- as.factor(dfp$other_lbow_res)
summary(dfp$other_lbow_res)



###################
# ANASTOMOSIS TYPE
###################

summary(dfp$anastomosis_type)

dfp$anastomosis_type <- as.factor(gsub("-", "_", dfp$anastomosis_type))

summary(dfp$anastomosis_type)

#####################
# ANASTOMOSIS METHOD
#####################

summary(dfp$sewn_stapled)

dfp <- mutate(dfp,
              sewn_stapled2 = case_when(
                      sewn_stapled == "Hand sewn" | sewn_stapled == "Hand-sewn"  ~ "Hand_sewn",
                      TRUE ~ as.character(sewn_stapled))
)
dfp$sewn_stapled2 <- as.factor(dfp$sewn_stapled2)
summary(dfp$sewn_stapled2)



######################
# LENGTH OF RESECTION
#####################

summary(dfp$resection_length)


######################
# PATHOLOGY
#####################

summary(dfp$granulomas)

summary(dfp$margin)

##########
# SMOKING
##########

# Smoking Status at Diagnosis
summary(dfp$smoking_diag)

# Smoking Status at Time of Index Surgery
summary(dfp$smoking_presurg)

dfp <- mutate(dfp,
                   smoking_presurg_bin = case_when(
                           smoking_presurg == "Ex-smoker" | smoking_presurg == "No" ~ "No",
                           TRUE ~ as.character(smoking_presurg)
                   ))
dfp$smoking_presurg_bin <- as.factor(dfp$smoking_presurg_bin)
summary(dfp$smoking_presurg_bin)



#############################
#############################
# MEDICATION BEFORE SURGERY
#############################
#############################


##################
# IMMUNOMODULATOR
##################

# Immunomodulatory drugs used since diagnosis?
summary(dfp$immuno)

# Immunomodulatory drugs used within one year prior to index surgery?
summary(dfp$immuno_1yr)


dfp <- mutate(dfp,
              immuno_1yr2 = case_when(
                      immuno == "No" & is.na(immuno_1yr) ~ "No",
                      immuno == "Unknown" & is.na(immuno_1yr) ~ "Unknown",
                      TRUE ~ as.character(immuno_1yr)))
dfp$immuno_1yr2 <- as.factor(dfp$immuno_1yr2)
summary(dfp$immuno_1yr2)


# Immunomodulatory drugs used within 3 months prior to index surgery?
summary(dfp$immuno_3mos)

dfp <- mutate(dfp,
              immuno_3mos2 = case_when(
                      immuno_1yr2 == "No" & is.na(immuno_3mos) ~ "No",
                      immuno_1yr2 == "Unknown" & is.na(immuno_3mos) ~ "Unknown",
                      TRUE ~ as.character(immuno_3mos)))
dfp$immuno_3mos2 <- as.factor(dfp$immuno_3mos2)
summary(dfp$immuno_3mos2)


# Immunomodulatory drugs used up to time of surgery?
summary(dfp$immuno_current)

dfp <- mutate(dfp,
              immuno_current2 = case_when(
                      immuno_3mos2 == "No" & is.na(immuno_current) ~ "No",
                      immuno_3mos2 == "Unknown" & is.na(immuno_current) ~ "Unknown",
                      TRUE ~ as.character(immuno_current)))
dfp$immuno_current2 <- as.factor(dfp$immuno_current2)
summary(dfp$immuno_current2)


##############
# METHOTREXATE
##############

# MTX used since diagnosis?
summary(dfp$mtx)

# MTX used with one year prior to index surgery?
summary(dfp$mtx_1yr)

dfp <- mutate(dfp,
              mtx_1yr2 = case_when(
                      mtx == "No" & is.na(mtx_1yr) ~ "No",
                      mtx == "Unknown" & is.na(mtx_1yr) ~ "Unknown",
                      TRUE ~ as.character(mtx_1yr)))
dfp$mtx_1yr2 <- as.factor(dfp$mtx_1yr2)
summary(dfp$mtx_1yr2)


# MTX used within 3 months prior to index surgery?
summary(dfp$mtx_3mos)

dfp <- mutate(dfp,
              mtx_3mos2 = case_when(
                      mtx_1yr2 == "No" & is.na(mtx_3mos) ~ "No",
                      mtx_1yr2 == "Unknown" & is.na(mtx_3mos) ~ "Unknown",
                      TRUE ~ as.character(mtx_3mos)))
dfp$mtx_3mos2 <- as.factor(dfp$mtx_3mos2)
summary(dfp$mtx_3mos2)


# MTX used up to time of surgery?
summary(dfp$mtx_current)

dfp <- mutate(dfp,
              mtx_current2 = case_when(
                      mtx_3mos2 == "No" & is.na(mtx_current) ~ "No",
                      mtx_3mos2 == "Unknown" & is.na(mtx_current) ~ "Unknown",
                      TRUE ~ as.character(mtx_current)))
dfp$mtx_current2 <- as.factor(dfp$mtx_current2)
summary(dfp$mtx_current2)



###################
# BIOLOGIC THERAPY
###################

summary(dfp$othermeds_text)

# There are 2 cases with Entivio added as a free text (other_meds_text). However these cases
# don't have any endoscopy, so won't be included in this project
# However, there is one case from MSH (C834827-974292) who has ustekinumab as free text, but
# since diagnosis No. This will be corrected.


###########
# ANTI-TNF
###########

# INFLIXIMAB

# Infliximab used since diagnosis?
summary(dfp$infliximab)


# Infliximab used within 1 year prior to index surgery?
summary(dfp$infliximab_1yr)

dfp <- mutate(dfp,
              infliximab_1yr2 = case_when(
                      infliximab == "No" & is.na(infliximab_1yr) ~ "No",
                      infliximab == "Unknown" & is.na(infliximab_1yr) ~ "Unknown",
                      TRUE ~ as.character(infliximab_1yr)))
dfp$infliximab_1yr2 <- as.factor(dfp$infliximab_1yr2)
summary(dfp$infliximab_1yr2)


# Infliximab used within 3 months prior index surgery?
summary(dfp$infliximab_3mos)

dfp <- mutate(dfp,
              infliximab_3mos2 = case_when(
                      infliximab_1yr2 == "No" & is.na(infliximab_3mos) ~ "No",
                      infliximab_1yr2 == "Unknown" & is.na(infliximab_3mos) ~ "Unknown",
                      TRUE ~ as.character(infliximab_3mos)))
dfp$infliximab_3mos2 <- as.factor(dfp$infliximab_3mos2)
summary(dfp$infliximab_3mos2)


# Date started:
summary(dfp$infliximab_start)

summary(dfp$infliximab_start_month)

summary(dfp$infliximab_start_year)

dfp$infliximab_start_new <- as.factor(paste0(dfp$infliximab_start_month, "/", dfp$infliximab_start_year))

summary(dfp$infliximab_start_new)

dfp <-  dfp %>% mutate(infliximab_start_new = replace(infliximab_start_new, infliximab_start_new == "NA/NA", NA))

summary(dfp$infliximab_start)

dfp <- dfp %>% 
        mutate(infliximab_start = coalesce(infliximab_start, infliximab_start_new))

summary(dfp$infliximab_start)


dfp <- mutate(dfp,
              infliximab_start2 = case_when(
                      infliximab_3mos2 == "No" & is.na(infliximab_start) ~ "Not_applicable",
                      infliximab_3mos2 == "Unknown" & is.na(infliximab_start) ~ "Unknown",
                      TRUE ~ as.character(infliximab_start)))
dfp$infliximab_start2 <- as.factor(dfp$infliximab_start2)
summary(dfp$infliximab_start2)

# Date of last dose prior to surgery:
summary(dfp$infliximab_stop)

summary(dfp$infliximab_stop_month)

summary(dfp$infliximab_stop_year)

dfp$infliximab_stop_new <- as.factor(paste0(dfp$infliximab_stop_month, "/", dfp$infliximab_stop_year))

summary(dfp$infliximab_stop_new)

dfp <-  dfp %>% mutate(infliximab_stop_new = replace(infliximab_stop_new, infliximab_stop_new == "NA/NA", NA))

summary(dfp$infliximab_stop)

dfp <- dfp %>% 
        mutate(infliximab_stop = coalesce(infliximab_stop, infliximab_stop_new))

summary(dfp$infliximab_stop)


dfp <- mutate(dfp,
              infliximab_stop2 = case_when(
                      infliximab == "No" & is.na(infliximab_stop) ~ "Not_applicable",
                      infliximab_3mos2 == "Yes" & is.na(infliximab_stop) ~ "Not_applicable",
                      infliximab_3mos2 == "Unknown" & is.na(infliximab_stop) ~ "Unknown",
                      TRUE ~ as.character(infliximab_stop)))
dfp$infliximab_stop2 <- as.factor(dfp$infliximab_stop2)
summary(dfp$infliximab_stop2)

# Most recent dose and schedule:
summary(dfp$infliximab_dose)

dfp <- mutate(dfp,
              infliximab_dose2 = case_when(
                      infliximab_3mos2 == "No" & is.na(infliximab_dose) ~ "Not_applicable",
                      infliximab_3mos2 == "Unknown" & is.na(infliximab_dose) ~ "Unknown",
                      TRUE ~ as.character(infliximab_dose)))
dfp$infliximab_dose2 <- as.factor(dfp$infliximab_dose2)
summary(dfp$infliximab_dose2)



# ADALIMUMAB

# Adalimumab used since diagnosis?
summary(dfp$adalimumab)

# Adalimumab used within 1 year prior to index surgery?
summary(dfp$adalimumab_1yr)

dfp <- mutate(dfp,
              adalimumab_1yr2 = case_when(
                      adalimumab == "No" & is.na(adalimumab_1yr) ~ "No",
                      adalimumab == "Unknown" & is.na(adalimumab_1yr) ~ "Unknown",
                      TRUE ~ as.character(adalimumab_1yr)))
dfp$adalimumab_1yr2 <- as.factor(dfp$adalimumab_1yr2)
summary(dfp$adalimumab_1yr2)


# Adalimumab used within 3 months prior index surgery?
summary(dfp$adalimumab_3mos)

dfp <- mutate(dfp,
              adalimumab_3mos2 = case_when(
                      adalimumab_1yr2 == "No" & is.na(adalimumab_3mos) ~ "No",
                      adalimumab_1yr2 == "Unknown" & is.na(adalimumab_3mos) ~ "Unknown",
                      TRUE ~ as.character(adalimumab_3mos)))
dfp$adalimumab_3mos2 <- as.factor(dfp$adalimumab_3mos2)
summary(dfp$adalimumab_3mos2)


# Date started:
summary(dfp$adalimumab_start)

summary(dfp$adalimumab_start_month)

summary(dfp$adalimumab_start_year)

dfp$adalimumab_start_new <- as.factor(paste0(dfp$adalimumab_start_month, "/", dfp$adalimumab_start_year))

summary(dfp$adalimumab_start_new)

dfp <-  dfp %>% mutate(adalimumab_start_new = replace(adalimumab_start_new, adalimumab_start_new == "NA/NA", NA))

summary(dfp$adalimumab_start)

dfp <- dfp %>% 
        mutate(adalimumab_start = coalesce(adalimumab_start, adalimumab_start_new))

summary(dfp$adalimumab_start)


dfp <- mutate(dfp,
              adalimumab_start2 = case_when(
                      adalimumab_3mos2 == "No" & is.na(adalimumab_start) ~ "Not_applicable",
                      adalimumab_3mos2 == "Unknown" & is.na(adalimumab_start) ~ "Unknown",
                      TRUE ~ as.character(adalimumab_start)))
dfp$adalimumab_start2 <- as.factor(dfp$adalimumab_start2)
summary(dfp$adalimumab_start2)


# Date of last dose prior to surgery:
summary(dfp$adalimumab_stop)

summary(dfp$adalimumab_stop_month)

summary(dfp$adalimumab_stop_year)

dfp$adalimumab_stop_new <- as.factor(paste0(dfp$adalimumab_stop_month, "/", dfp$adalimumab_stop_year))

summary(dfp$adalimumab_stop_new)

dfp <-  dfp %>% mutate(adalimumab_stop_new = replace(adalimumab_stop_new, adalimumab_stop_new == "NA/NA", NA))

summary(dfp$adalimumab_stop)

dfp <- dfp %>% 
        mutate(adalimumab_stop = coalesce(adalimumab_stop, adalimumab_stop_new))

summary(dfp$adalimumab_stop)

dfp <- mutate(dfp,
              adalimumab_stop2 = case_when(
                      adalimumab == "No" & is.na(adalimumab_stop) ~ "Not_applicable",
                      adalimumab_3mos2 == "Yes" & is.na(adalimumab_stop) ~ "Not_applicable",
                      adalimumab_3mos2 == "Unknown" & is.na(adalimumab_stop) ~ "Unknown",
                      TRUE ~ as.character(adalimumab_stop)))
dfp$adalimumab_stop2 <- as.factor(dfp$adalimumab_stop2)
summary(dfp$adalimumab_stop2)


# Most recent dose and schedule:
summary(dfp$adalimumab_dose)

dfp <- mutate(dfp,
              adalimumab_dose2 = case_when(
                      adalimumab_3mos2 == "No" & is.na(adalimumab_dose) ~ "Not_applicable",
                      adalimumab_3mos2 == "Unknown" & is.na(adalimumab_dose) ~ "Unknown",
                      TRUE ~ as.character(adalimumab_dose)))
dfp$adalimumab_dose2 <- as.factor(dfp$adalimumab_dose2)
summary(dfp$adalimumab_dose2)



# CERTOLIZUMAB

# Certolizumab used since diagnosis?
summary(dfp$certolizumab)


# Certolizumab used within 1 year prior to index surgery?
summary(dfp$certolizumab_1yr)

dfp <- mutate(dfp,
              certolizumab_1yr2 = case_when(
                      certolizumab == "No" & is.na(certolizumab_1yr) ~ "No",
                      certolizumab == "Unknown" & is.na(certolizumab_1yr) ~ "Unknown",
                      TRUE ~ as.character(certolizumab_1yr)))
dfp$certolizumab_1yr2 <- as.factor(dfp$certolizumab_1yr2)
summary(dfp$certolizumab_1yr2)


# Certolizumab used within 3 months prior index surgery?
summary(dfp$certolizumab_3mos)

dfp <- mutate(dfp,
              certolizumab_3mos2 = case_when(
                      certolizumab_1yr2 == "No" & is.na(certolizumab_3mos) ~ "No",
                      certolizumab_1yr2 == "Unknown" & is.na(certolizumab_3mos) ~ "Unknown",
                      TRUE ~ as.character(certolizumab_3mos)))
dfp$certolizumab_3mos2 <- as.factor(dfp$certolizumab_3mos2)
summary(dfp$certolizumab_3mos2)


# Date started:
summary(dfp$certolizumab_start)

summary(dfp$certolizumab_start_month)

summary(dfp$certolizumab_start_year)

dfp$certolizumab_start_new <- as.factor(paste0(dfp$certolizumab_start_month, "/", dfp$certolizumab_start_year))

summary(dfp$certolizumab_start_new)

dfp <-  dfp %>% mutate(certolizumab_start_new = replace(certolizumab_start_new, certolizumab_start_new == "NA/NA", NA))

summary(dfp$certolizumab_start)

dfp <- dfp %>% 
        mutate(certolizumab_start = coalesce(certolizumab_start, certolizumab_start_new))

summary(dfp$certolizumab_start)



dfp <- mutate(dfp,
              certolizumab_start2 = case_when(
                      certolizumab_3mos2 == "No" & is.na(certolizumab_start) ~ "Not_applicable",
                      certolizumab_3mos2 == "Unknown" & is.na(certolizumab_start) ~ "Unknown",
                      TRUE ~ as.character(certolizumab_start)))
dfp$certolizumab_start2 <- as.factor(dfp$certolizumab_start2)
summary(dfp$certolizumab_start2)

# Date of last dose prior to surgery:
summary(dfp$certolizumab_stop)

summary(dfp$certolizumab_stop_month)

summary(dfp$certolizumab_stop_year)

dfp$certolizumab_stop_new <- as.factor(paste0(dfp$certolizumab_stop_month, "/", dfp$certolizumab_stop_year))

summary(dfp$certolizumab_stop_new)

dfp <-  dfp %>% mutate(certolizumab_stop_new = replace(certolizumab_stop_new, certolizumab_stop_new == "NA/NA", NA))

summary(dfp$certolizumab_stop)

dfp <- dfp %>% 
        mutate(certolizumab_stop = coalesce(certolizumab_stop, certolizumab_stop_new))

summary(dfp$certolizumab_stop)


dfp <- mutate(dfp,
              certolizumab_stop2 = case_when(
                      certolizumab == "No" & is.na(certolizumab_stop) ~ "Not_applicable",
                      certolizumab_3mos2 == "Yes" & is.na(certolizumab_stop) ~ "Not_applicable",
                      certolizumab_3mos2 == "Unknown" & is.na(certolizumab_stop) ~ "Unknown",
                      TRUE ~ as.character(certolizumab_stop)))
dfp$certolizumab_stop2 <- as.factor(dfp$certolizumab_stop2)
summary(dfp$certolizumab_stop2)


# Most recent dose and schedule:
summary(dfp$certolizumab_dose)

dfp <- mutate(dfp,
              certolizumab_dose2 = case_when(
                      certolizumab_3mos2 == "No" & is.na(certolizumab_dose) ~ "Not_applicable",
                      certolizumab_3mos2 == "Unknown" & is.na(certolizumab_dose) ~ "Unknown",
                      TRUE ~ as.character(certolizumab_dose)))
dfp$certolizumab_dose2 <- as.factor(dfp$certolizumab_dose2)
summary(dfp$certolizumab_dose2)


##############################
#  CREATING GOLIMUMAB VARIABLE
##############################

# There are 4 patients who used golimumab before surgery
# A051501-130797 (MSH) but they also used IFX and ADA, but no within 1 yr or 3 mos before surgery
# Simponi was used up to surgery (will add to the antiTNF group)
# C833226-988692 (MSH) but they also used IFX. Using vedo 1 year and 3 mos before surgery
# C834081-908642 (MSH) used IFX, ADA and Simponi since diagnosis but any of them 3 mos before surgery
# C836617-810932 (MSH) used IFX until surgery. Not clear how also used Simponi until surgery
# There is no need to add Sinponi data to antiTNF because IFX is already recorded.


summary(dfp$othermeds)
summary(dfp$othermeds_text)

dfp <- mutate(dfp,
              golimumab = case_when(
                      othermeds == "Yes" & othermeds_text == "Golimumab" ~ "Yes",
                      othermeds == "Yes" & othermeds_text == "Simponi" ~ "Yes",
                      TRUE ~ as.character("No")))
dfp$golimumab <- as.factor(dfp$golimumab)
summary(dfp$golimumab)


summary(dfp$othermeds_3mos)

dfp <- mutate(dfp,
              golimumab_3mos = case_when(
                      othermeds_3mos == "Yes" & othermeds_text == "Golimumab" ~ "Yes",
                      othermeds_3mos == "Yes" & othermeds_text == "Simponi" ~ "Yes",
                      TRUE ~ as.character("No")))
dfp$golimumab_3mos <- as.factor(dfp$golimumab_3mos)
summary(dfp$golimumab_3mos)


#############################
# CREATING ANTI-TNF VARIABLE
#############################

# Anti-TNF since diagnosis

dfp <- mutate(dfp,
              tnf_any = case_when(
                      infliximab == "Yes" | adalimumab == "Yes" | certolizumab == "Yes" | golimumab == "Yes" ~ "Yes",
                      infliximab == "No" & adalimumab == "No" & certolizumab == "No" & golimumab == "No" ~ "No",
                      infliximab == "Unknown" | adalimumab == "Unknown" | certolizumab == "Unknown" | golimumab == "Unknown" ~ "Unknown",
                      TRUE ~ NA_character_))
dfp$tnf_any <- as.factor(dfp$tnf_any)
summary(dfp$tnf_any)


# Anti-TNF within 1yr

dfp <- mutate(dfp,
              tnf_any_1yr = case_when(
                      infliximab_1yr2 == "Yes" | adalimumab_1yr2 == "Yes" | certolizumab_1yr2 == "Yes" ~ "Yes",
                      infliximab_1yr2 == "No" & adalimumab_1yr2 == "No" & certolizumab_1yr2 == "No" ~ "No",
                      infliximab_1yr2 == "Unknown" | adalimumab_1yr2 == "Unknown" | certolizumab_1yr2 == "Unknown" ~ "Unknown",
                      TRUE ~ NA_character_))
dfp$tnf_any_1yr <- as.factor(dfp$tnf_any_1yr)
summary(dfp$tnf_any_1yr)


# Anti-TNF within 3 mos before surgery

dfp <- mutate(dfp,
              tnf_any_3mos = case_when(
                      infliximab_3mos2 == "Yes" | adalimumab_3mos2 == "Yes" | certolizumab_3mos2 == "Yes" | golimumab_3mos == "Yes" ~ "Yes",
                      infliximab_3mos2 == "No" & adalimumab_3mos2 == "No" & certolizumab_3mos2 == "No" & golimumab_3mos == "No" ~ "No",
                      infliximab_3mos2 == "Unknown" | adalimumab_3mos2 == "Unknown" | certolizumab_3mos2 == "Unknown" | golimumab_3mos == "Unknown" ~ "Unknown",
                      TRUE ~ NA_character_))
dfp$tnf_any_3mos <- as.factor(dfp$tnf_any_3mos)
summary(dfp$tnf_any_3mos)


######################
# ANTI-TNF DETAILS
######################

# Infliximab

summary(dfp$infliximab_induction)

dfp <- mutate(dfp,
              infliximab_induction2 = case_when(
                      infliximab_1yr2 == "No" & is.na(infliximab_induction) ~ "Not_applicable",
                      infliximab_1yr2 == "Unknown" & is.na(infliximab_induction) ~ "Unknown",
                      TRUE ~ as.character(infliximab_induction)))
dfp$infliximab_induction2 <- as.factor(dfp$infliximab_induction2)
summary(dfp$infliximab_induction2)


summary(dfp$infliximab_maxdose)

dfp <- mutate(dfp,
              infliximab_maxdose2 = case_when(
                      infliximab_1yr2 == "No" & is.na(infliximab_maxdose) ~ "Not_applicable",
                      infliximab_induction2 == "Not_applicable" & is.na(infliximab_maxdose) ~ "Not_applicable",
                      infliximab_induction2 == "Unknown" & is.na(infliximab_maxdose) ~ "Unknwon",
                      TRUE ~ as.character(infliximab_maxdose)))
dfp$infliximab_maxdose2 <- as.factor(dfp$infliximab_maxdose2)
summary(dfp$infliximab_maxdose2)


summary(dfp$infliximab_interval)

dfp <- mutate(dfp,
              infliximab_interval2 = case_when(
                      infliximab_1yr2 == "No" & is.na(infliximab_interval) ~ "Not_applicable",
                      infliximab_induction2 == "Not_applicable" & is.na(infliximab_interval) ~ "Not_applicable",
                      infliximab_induction2 == "Unknown" & is.na(infliximab_interval) ~ "Unknwon",
                      TRUE ~ as.character(infliximab_interval)))
dfp$infliximab_interval2 <- as.factor(dfp$infliximab_interval2)
summary(dfp$infliximab_interval2)


summary(dfp$infliximab_combo)

dfp <- mutate(dfp,
              infliximab_combo2= case_when(
                      infliximab_1yr2 == "No" & is.na(infliximab_combo) ~ "Not_applicable",
                      infliximab_1yr2 == "Unknown" & is.na(infliximab_combo) ~ "Unknown",
                      TRUE ~ as.character(infliximab_combo)))
dfp$infliximab_combo2 <- as.factor(dfp$infliximab_combo2)
summary(dfp$infliximab_combo2)


summary(dfp$infliximab_combo_drug1)

dfp <- mutate(dfp,
              infliximab_combo_drug12 = case_when(
                      infliximab_1yr2 == "No" ~ "Not_applicable",
                      infliximab_combo2 == "No" ~ "Not_applicable",
                      infliximab_1yr2 == "Unknown" ~ "Unknown",
                      TRUE ~ as.character(infliximab_combo_drug1)))
dfp$infliximab_combo_drug12 <- as.factor(dfp$infliximab_combo_drug12)
summary(dfp$infliximab_combo_drug12)


summary(dfp$infliximab_combo_drug2)

dfp <- mutate(dfp,
              infliximab_combo_drug22 = case_when(
                      infliximab_1yr2 == "No" ~ "Not_applicable",
                      infliximab_combo2 == "No" ~ "Not_applicable",
                      infliximab_1yr2 == "Unknown" ~ "Unknown",
                      TRUE ~ as.character(infliximab_combo_drug2)))
dfp$infliximab_combo_drug22 <- as.factor(dfp$infliximab_combo_drug22)
summary(dfp$infliximab_combo_drug22)


summary(dfp$infliximab_combo_drug3)

dfp <- mutate(dfp,
              infliximab_combo_drug32 = case_when(
                      infliximab_1yr2 == "No" ~ "Not_applicable",
                      infliximab_combo2 == "No" ~ "Not_applicable",
                      infliximab_1yr2 == "No" ~ "Unknown",
                      TRUE ~ as.character(infliximab_combo_drug3)))
dfp$infliximab_combo_drug32 <- as.factor(dfp$infliximab_combo_drug32)
summary(dfp$infliximab_combo_drug32)


summary(dfp$tdm_infliximab_prior)

dfp <- mutate(dfp,
              tdm_infliximab_prior2 = case_when(
                      infliximab_1yr2 == "No" & is.na(tdm_infliximab_prior) ~ "Not_applicable",
                      infliximab_1yr2 == "Unknown" & is.na(tdm_infliximab_prior) ~ "Unknown",
                      TRUE ~ as.character(tdm_infliximab_prior)))
dfp$tdm_infliximab_prior2 <- as.factor(dfp$tdm_infliximab_prior2)
summary(dfp$tdm_infliximab_prior2)

summary(dfp$infliximab_assay)

dfp <- mutate(dfp,
              infliximab_assay2 = case_when(
                      infliximab_1yr2 == "No" & is.na(infliximab_assay) ~ "Not_applicable",
                      tdm_infliximab_prior2 == "No" & is.na(infliximab_assay) ~ "Not_applicable",
                      tdm_infliximab_prior2 == "Unknown" & is.na(infliximab_assay) ~ "Not_applicable",
                      TRUE ~ as.character(infliximab_assay)))
dfp$infliximab_assay2 <- as.factor(dfp$infliximab_assay2)
summary(dfp$infliximab_assay2)


summary(dfp$infliximab_trough_level)

dfp <- mutate(dfp,
              infliximab_trough_level2 = case_when(
                      infliximab_1yr2 == "No" & is.na(infliximab_trough_level) ~ "Not_applicable",
                      tdm_infliximab_prior2 == "No" & is.na(infliximab_trough_level) ~ "Not_applicable",
                      tdm_infliximab_prior2 == "Unknown" & is.na(infliximab_trough_level) ~ "Not_applicable",
                      TRUE ~ as.character(infliximab_trough_level)))
dfp$infliximab_trough_level2 <- as.factor(dfp$infliximab_trough_level2)
summary(dfp$infliximab_trough_level2)


summary(dfp$infliximab_drug_level)

dfp <- mutate(dfp,
              infliximab_drug_level2 = case_when(
                      infliximab_1yr2 == "No" & is.na(infliximab_drug_level) ~ "Not_applicable",
                      tdm_infliximab_prior2 == "No" & is.na(infliximab_drug_level) ~ "Not_applicable",
                      tdm_infliximab_prior2 == "Unknown" & is.na(infliximab_drug_level) ~ "Not_applicable",
                      TRUE ~ as.character(infliximab_drug_level)))
dfp$infliximab_drug_level2 <- as.factor(dfp$infliximab_drug_level2)
summary(dfp$infliximab_drug_level2)


summary(dfp$infliximab_antibody)

dfp <- mutate(dfp,
              infliximab_antibody2 = case_when(
                      infliximab_1yr2 == "No" & is.na(infliximab_antibody) ~ "Not_applicable",
                      tdm_infliximab_prior2 == "No" & is.na(infliximab_antibody) ~ "Not_applicable",
                      tdm_infliximab_prior2 == "Unknown" & is.na(infliximab_antibody) ~ "Not_applicable",
                      TRUE ~ as.character(infliximab_antibody)))
dfp$infliximab_antibody2 <- as.factor(dfp$infliximab_antibody2)
summary(dfp$infliximab_antibody2)



#Adalimumab

summary(dfp$adalimumab_induction)

dfp <- mutate(dfp,
              adalimumab_induction2 = case_when(
                      adalimumab_1yr2 == "No" & is.na(adalimumab_induction) ~ "Not_applicable",
                      adalimumab_1yr2 == "Unknown" & is.na(adalimumab_induction) ~ "Unknown",
                      TRUE ~ as.character(adalimumab_induction)))
dfp$adalimumab_induction2 <- as.factor(dfp$adalimumab_induction2)
summary(dfp$adalimumab_induction2)


summary(dfp$adalimumab_dose_1yr)

dfp <- mutate(dfp,
              adalimumab_dose_1yr2 = case_when(
                      adalimumab_1yr2 == "No" & is.na(adalimumab_dose_1yr) ~ "Not_applicable",
                      adalimumab_1yr2 == "Unknown" & is.na(adalimumab_dose_1yr) ~ "Unknown",
                      adalimumab_induction2 == "Not_applicable" & is.na(adalimumab_dose_1yr) ~ "Not_applicable",
                      TRUE ~ as.character(adalimumab_dose_1yr)))
dfp$adalimumab_dose_1yr2 <- as.factor(dfp$adalimumab_dose_1yr2)
summary(dfp$adalimumab_dose_1yr2)


summary(dfp$adalimumab_freq_1yr)

dfp <- mutate(dfp,
              adalimumab_freq_1yr2 = case_when(
                      adalimumab_1yr2 == "No" & is.na(adalimumab_freq_1yr) ~ "Not_applicable",
                      adalimumab_1yr2 == "Unknown" & is.na(adalimumab_freq_1yr) ~ "Unknown",
                      adalimumab_1yr2 == "Not_applicable" & is.na(adalimumab_freq_1yr) ~ "Not_applicable",
                      TRUE ~ as.character(adalimumab_freq_1yr)))
dfp$adalimumab_freq_1yr2 <- as.factor(dfp$adalimumab_freq_1yr2)
summary(dfp$adalimumab_freq_1yr2)


summary(dfp$adalimumab_combo)

dfp <- mutate(dfp,
              adalimumab_combo2= case_when(
                      adalimumab_1yr2 == "No" & is.na(adalimumab_combo) ~ "Not_applicable",
                      adalimumab_1yr2 == "Unknown" & is.na(adalimumab_combo) ~ "Unknown",
                      TRUE ~ as.character(adalimumab_combo)))
dfp$adalimumab_combo2 <- as.factor(dfp$adalimumab_combo2)
summary(dfp$adalimumab_combo2)


summary(dfp$adalimumab_combo_drug1)

dfp <- mutate(dfp,
              adalimumab_combo_drug12 = case_when(
                      adalimumab_1yr2 == "No" ~ "Not_applicable",
                      adalimumab_1yr2 == "Unknown" ~ "Unkown",
                      adalimumab_combo2 == "No" ~ "Not_applicable",
                      TRUE ~ as.character(infliximab_combo_drug1)))
dfp$adalimumab_combo_drug12 <- as.factor(dfp$adalimumab_combo_drug12)
summary(dfp$adalimumab_combo_drug12)


summary(dfp$adalimumab_combo_drug2)

dfp <- mutate(dfp,
              adalimumab_combo_drug22 = case_when(
                      adalimumab_1yr2 == "No" ~ "Not_applicable",
                      adalimumab_1yr2 == "Unknown" ~ "Unknown",
                      adalimumab_combo2 == "No" ~ "Not_applicable",
                      TRUE ~ as.character(adalimumab_combo_drug2)))
dfp$adalimumab_combo_drug22 <- as.factor(dfp$adalimumab_combo_drug22)
summary(dfp$adalimumab_combo_drug22)


summary(dfp$adalimumab_combo_drug3)

dfp <- mutate(dfp,
              adalimumab_combo_drug32 = case_when(
                      adalimumab_1yr2 == "No" ~ "Not_applicable",
                      adalimumab_1yr2 == "Unknown" ~ "Unknown",
                      adalimumab_combo2 == "No" ~ "Not_applicable",
                      TRUE ~ as.character(adalimumab_combo_drug3)))
dfp$adalimumab_combo_drug32 <- as.factor(dfp$adalimumab_combo_drug32)
summary(dfp$adalimumab_combo_drug32)


summary(dfp$tdm_adalimumab_prior)

dfp <- mutate(dfp,
              tdm_adalimumab_prior2 = case_when(
                      adalimumab_1yr2 == "No" & is.na(tdm_adalimumab_prior) ~ "Not_applicable",
                      adalimumab_1yr2 == "Unknown" & is.na(tdm_adalimumab_prior) ~ "Unknown",
                      TRUE ~ as.character(tdm_adalimumab_prior)))
dfp$tdm_adalimumab_prior2 <- as.factor(dfp$tdm_adalimumab_prior2)
summary(dfp$tdm_adalimumab_prior2)

summary(dfp$adalimumab_assay)

dfp <- mutate(dfp,
              adalimumab_assay2 = case_when(
                      adalimumab_1yr2 == "No" & is.na(adalimumab_assay) ~ "Not_applicable",
                      tdm_adalimumab_prior2 == "No" & is.na(adalimumab_assay) ~ "Not_applicable",
                      tdm_adalimumab_prior2 == "Unknown" & is.na(adalimumab_assay) ~ "Not_applicable",
                      TRUE ~ as.character(adalimumab_assay)))
dfp$adalimumab_assay2 <- as.factor(dfp$adalimumab_assay2)
summary(dfp$adalimumab_assay2)


summary(dfp$adalimumab_trough_level)

dfp <- mutate(dfp,
              adalimumab_trough_level2 = case_when(
                      adalimumab_1yr2 == "No" & is.na(adalimumab_trough_level) ~ "Not_applicable",
                      tdm_adalimumab_prior2 == "No" & is.na(adalimumab_trough_level) ~ "Not_applicable",
                      tdm_adalimumab_prior2 == "Unknown" & is.na(adalimumab_trough_level) ~ "Not_applicable",
                      TRUE ~ as.character(adalimumab_trough_level)))
dfp$adalimumab_trough_level2 <- as.factor(dfp$adalimumab_trough_level2)
summary(dfp$adalimumab_trough_level2)


summary(dfp$adalimumab_drug_level)

dfp <- mutate(dfp,
              adalimumab_drug_level2 = case_when(
                      adalimumab_1yr2 == "No" & is.na(adalimumab_drug_level) ~ "Not_applicable",
                      tdm_adalimumab_prior2 == "No" & is.na(adalimumab_drug_level) ~ "Not_applicable",
                      tdm_adalimumab_prior2 == "Unknown" & is.na(adalimumab_drug_level) ~ "Not_applicable",
                      TRUE ~ as.character(adalimumab_drug_level)))
dfp$adalimumab_drug_level2 <- as.factor(dfp$adalimumab_drug_level2)
summary(dfp$adalimumab_drug_level2)


summary(dfp$adalimumab_antibody)

dfp <- mutate(dfp,
              adalimumab_antibody2 = case_when(
                      adalimumab_1yr2 == "No" & is.na(adalimumab_antibody) ~ "Not_applicable",
                      tdm_adalimumab_prior2 == "No" & is.na(adalimumab_antibody) ~ "Not_applicable",
                      tdm_adalimumab_prior2 == "Unknown" & is.na(adalimumab_antibody) ~ "Not_applicable",
                      TRUE ~ as.character(adalimumab_antibody)))
dfp$adalimumab_antibody2 <- as.factor(dfp$adalimumab_antibody2)
summary(dfp$adalimumab_antibody2)



# Certolizumab

summary(dfp$certolizumab_induction)

dfp <- mutate(dfp,
              certolizumab_induction2 = case_when(
                      certolizumab_1yr2 == "No" & is.na(certolizumab_induction) ~ "Not_applicable",
                      certolizumab_1yr2 == "Unknown" & is.na(certolizumab_induction) ~ "Unknown",
                      TRUE ~ as.character(certolizumab_induction)))
dfp$certolizumab_induction2 <- as.factor(dfp$certolizumab_induction2)
summary(dfp$certolizumab_induction2)


summary(dfp$certolizumab_dose_1yr)

dfp <- mutate(dfp,
              certolizumab_dose_1yr2 = case_when(
                      certolizumab_1yr2 == "No" & is.na(certolizumab_dose_1yr) ~ "Not_applicable",
                      certolizumab_1yr2 == "Unknown" & is.na(certolizumab_dose_1yr) ~ "Unknown",
                      certolizumab_induction2 == "Not_applicable" & is.na(certolizumab_dose_1yr) ~ "Not_applicable",
                      certolizumab_induction2 == "Unknown" & is.na(certolizumab_dose_1yr) ~ "Unknown",
                      TRUE ~ as.character(certolizumab_dose_1yr)))
dfp$certolizumab_dose_1yr2 <- as.factor(dfp$certolizumab_dose_1yr2)
summary(dfp$certolizumab_dose_1yr2)


summary(dfp$certolizumab_freq_1yr)

dfp <- mutate(dfp,
              certolizumab_freq_1yr2 = case_when(
                      certolizumab_1yr2 == "No" & is.na(certolizumab_freq_1yr) ~ "Not_applicable",
                      certolizumab_induction2 == "No" & is.na(certolizumab_freq_1yr) ~ "Not_applicable",
                      certolizumab_induction2 == "Unknown" & is.na(certolizumab_freq_1yr) ~ "Unknown",
                      TRUE ~ as.character(certolizumab_freq_1yr)))
dfp$certolizumab_freq_1yr2 <- as.factor(dfp$certolizumab_freq_1yr2)
summary(dfp$certolizumab_freq_1yr2)


summary(dfp$certolizumab_combo)

dfp <- mutate(dfp,
              certolizumab_combo2= case_when(
                      certolizumab_1yr2 == "No" & is.na(certolizumab_combo) ~ "Not_applicable",
                      certolizumab_1yr2 == "Unknown" & is.na(certolizumab_combo) ~ "Unknown",
                      TRUE ~ as.character(certolizumab_combo)))
dfp$certolizumab_combo2 <- as.factor(dfp$certolizumab_combo2)
summary(dfp$certolizumab_combo2)


summary(dfp$certolizumab_combo_drug1)

dfp <- mutate(dfp,
              certolizumab_combo_drug12 = case_when(
                      certolizumab_1yr2 == "No" ~ "Not_applicable",
                      certolizumab_1yr2 == "Unknown" ~ "Unknown",
                      certolizumab_combo2 == "No" ~ "Not_applicable",
                      TRUE ~ as.character(certolizumab_combo_drug1)))
dfp$certolizumab_combo_drug12 <- as.factor(dfp$certolizumab_combo_drug12)
summary(dfp$certolizumab_combo_drug12)


summary(dfp$certolizumab_combo_drug2)

dfp <- mutate(dfp,
              certolizumab_combo_drug22 = case_when(
                      certolizumab_1yr2 == "No" ~ "Not_applicable",
                      certolizumab_1yr2 == "Unknown" ~ "Unknown",
                      certolizumab_combo2 == "No" ~ "Not_applicable",
                      TRUE ~ as.character(certolizumab_combo_drug2)))
dfp$certolizumab_combo_drug22 <- as.factor(dfp$certolizumab_combo_drug22)
summary(dfp$certolizumab_combo_drug22)


summary(dfp$certolizumab_combo_drug3)

dfp <- mutate(dfp,
              certolizumab_combo_drug32 = case_when(
                      certolizumab_1yr2 == "No"~ "Not_applicable",
                      certolizumab_1yr2 == "Unknown"~ "Unknown",
                      certolizumab_combo2 == "No" ~ "Not_applicable",
                      TRUE ~ as.character(certolizumab_combo_drug3)))
dfp$certolizumab_combo_drug32 <- as.factor(dfp$certolizumab_combo_drug32)
summary(dfp$certolizumab_combo_drug32)


summary(dfp$tdm_certolizumab_prior)

dfp <- mutate(dfp,
              tdm_certolizumab_prior2 = case_when(
                      certolizumab_1yr2 == "No" & is.na(tdm_certolizumab_prior) ~ "Not_applicable",
                      certolizumab_1yr2 == "Unknown" & is.na(tdm_certolizumab_prior) ~ "Unknown",
                      TRUE ~ as.character(tdm_certolizumab_prior)))
dfp$tdm_certolizumab_prior2 <- as.factor(dfp$tdm_certolizumab_prior2)
summary(dfp$tdm_certolizumab_prior2)


summary(dfp$certolizumab_assay)

dfp <- mutate(dfp,
              certolizumab_assay2 = case_when(
                      certolizumab_1yr2 == "No" & is.na(certolizumab_assay) ~ "Not_applicable",
                      tdm_certolizumab_prior2 == "No" & is.na(certolizumab_assay) ~ "Not_applicable",
                      tdm_certolizumab_prior2 == "Unknown" & is.na(certolizumab_assay) ~ "Not_applicable",
                      TRUE ~ as.character(certolizumab_assay)))
dfp$certolizumab_assay2 <- as.factor(dfp$certolizumab_assay2)
summary(dfp$certolizumab_assay2)


summary(dfp$certolizumab_trough_level)

dfp <- mutate(dfp,
              certolizumab_trough_level2 = case_when(
                      certolizumab_1yr2 == "No" & is.na(certolizumab_trough_level) ~ "Not_applicable",
                      tdm_certolizumab_prior2 == "No" & is.na(certolizumab_trough_level) ~ "Not_applicable",
                      tdm_certolizumab_prior2 == "Unknown" & is.na(certolizumab_trough_level) ~ "Not_applicable",
                      TRUE ~ as.character(certolizumab_trough_level)))
dfp$certolizumab_trough_level2 <- as.factor(dfp$certolizumab_trough_level2)
summary(dfp$certolizumab_trough_level2)


summary(dfp$certolizumab_drug_level)

dfp <- mutate(dfp,
              certolizumab_drug_level2 = case_when(
                      certolizumab_1yr2 == "No" & is.na(certolizumab_drug_level) ~ "Not_applicable",
                      tdm_certolizumab_prior2 == "No" & is.na(certolizumab_drug_level) ~ "Not_applicable",
                      tdm_certolizumab_prior2 == "Unknown" & is.na(certolizumab_drug_level) ~ "Not_applicable",
                      TRUE ~ as.character(certolizumab_drug_level)))
dfp$certolizumab_drug_level2 <- as.factor(dfp$certolizumab_drug_level2)
summary(dfp$certolizumab_drug_level2)


summary(dfp$certolizumab_antibody)

dfp <- mutate(dfp,
              certolizumab_antibody2 = case_when(
                      certolizumab_1yr2 == "No" & is.na(certolizumab_antibody) ~ "Not_applicable",
                      tdm_certolizumab_prior2 == "No" & is.na(certolizumab_antibody) ~ "Not_applicable",
                      tdm_certolizumab_prior2 == "Unknown" & is.na(certolizumab_antibody) ~ "Not_applicable",
                      TRUE ~ as.character(certolizumab_antibody)))
dfp$certolizumab_antibody2 <- as.factor(dfp$certolizumab_antibody2)
summary(dfp$certolizumab_antibody2)



#############################
# VEDOLIZUMAB BEFORE SURGERY
############################

# Vedolizumab used since diagnosis?
summary(dfp$vedolizumab)


# Vedolizumab used within 1 year prior to surgery?
summary(dfp$vedolizumab_1yr)

dfp <- mutate(dfp,
              vedolizumab_1yr2 = case_when(
                      vedolizumab == "No" & is.na(vedolizumab_1yr) ~ "No",
                      vedolizumab == "Unknown" & is.na(vedolizumab_1yr) ~ "Unknown",
                      TRUE ~ as.character(vedolizumab_1yr)))
dfp$vedolizumab_1yr2 <- as.factor(dfp$vedolizumab_1yr2)
summary(dfp$vedolizumab_1yr2)


summary(dfp$vedolizumab_induction)

dfp <- mutate(dfp,
              vedolizumab_induction2 = case_when(
                      vedolizumab_1yr2 == "No" & is.na(vedolizumab_induction) ~ "Not_applicable",
                      vedolizumab_1yr2 == "Unknown" & is.na(vedolizumab_induction) ~ "Unknown",
                      TRUE ~ as.character(vedolizumab_induction)))
dfp$vedolizumab_induction2 <- as.factor(dfp$vedolizumab_induction2)
summary(dfp$vedolizumab_induction2)



summary(dfp$vedolizumab_freq_1yr)

dfp <- mutate(dfp,
              vedolizumab_freq_1yr2 = case_when(
                      vedolizumab_1yr2 == "No" & is.na(vedolizumab_freq_1yr) ~ "Not_applicable",
                      vedolizumab_1yr2 == "Unknown" & is.na(vedolizumab_freq_1yr) ~ "Unknown",
                      vedolizumab_induction2 == "Not_applicable" & is.na(vedolizumab_freq_1yr) ~ "Not_applicable",
                      vedolizumab_induction2 == "Unknown" & is.na(vedolizumab_freq_1yr) ~ "Unknown",
                      TRUE ~ as.character(vedolizumab_freq_1yr)))
dfp$vedolizumab_freq_1yr2 <- as.factor(dfp$vedolizumab_freq_1yr2)
summary(dfp$vedolizumab_freq_1yr2)


summary(dfp$vedolizumab_combo)

dfp <- mutate(dfp,
              vedolizumab_combo2 = case_when(
                      vedolizumab_1yr2 == "No" & is.na(vedolizumab_combo) ~ "Not_applicable",
                      vedolizumab_1yr2 == "Unknown" & is.na(vedolizumab_combo) ~ "Unknown",
                      TRUE ~ as.character(vedolizumab_combo)))
dfp$vedolizumab_combo2 <- as.factor(dfp$vedolizumab_combo2)
summary(dfp$vedolizumab_combo2)


summary(dfp$vedolizumab_combo_drug1)

dfp <- mutate(dfp,
              vedolizumab_combo_drug12 = case_when(
                      vedolizumab_1yr2 == "No" & is.na(vedolizumab_combo_drug1) ~ "Not_applicable",
                      vedolizumab_1yr2 == "Unknown" & is.na(vedolizumab_combo_drug1) ~ "Unknown",
                      vedolizumab_combo2 == "No" | vedolizumab_combo2 == "Not_applicable" ~ "Not_applicable",
                      TRUE ~ as.character(vedolizumab_combo_drug1)))
dfp$vedolizumab_combo_drug12 <- as.factor(dfp$vedolizumab_combo_drug12)
summary(dfp$vedolizumab_combo_drug12)


summary(dfp$vedolizumab_combo_drug2)

dfp <- mutate(dfp,
              vedolizumab_combo_drug22 = case_when(
                      vedolizumab_1yr2 == "No" & is.na(vedolizumab_combo_drug2) ~ "Not_applicable",
                      vedolizumab_1yr2 == "Unknown" & is.na(vedolizumab_combo_drug2) ~ "Unknown",
                      vedolizumab_combo2 == "No" | vedolizumab_combo2 == "Not_applicable" ~ "Not_applicable",
                      TRUE ~ as.character(vedolizumab_combo_drug2)))
dfp$vedolizumab_combo_drug22 <- as.factor(dfp$vedolizumab_combo_drug22)
summary(dfp$vedolizumab_combo_drug22)



summary(dfp$vedolizumab_combo_drug3)

dfp <- mutate(dfp,
              vedolizumab_combo_drug32 = case_when(
                      vedolizumab_1yr2 == "No" & is.na(vedolizumab_combo_drug3) ~ "Not_applicable",
                      vedolizumab_1yr2 == "Unknown" & is.na(vedolizumab_combo_drug3) ~ "Unknown",
                      vedolizumab_combo2 == "No" | vedolizumab_combo2 == "Not_applicable" ~ "Not_applicable",
                      TRUE ~ as.character(vedolizumab_combo_drug3)))
dfp$vedolizumab_combo_drug32 <- as.factor(dfp$vedolizumab_combo_drug32)
summary(dfp$vedolizumab_combo_drug32)


summary(dfp$tdm_vedolizumab_prior)

dfp <- mutate(dfp,
              tdm_vedolizumab_prior2 = case_when(
                      vedolizumab_1yr2 == "No" & is.na(tdm_vedolizumab_prior) ~ "Not_applicable",
                      vedolizumab_1yr2 == "Unknown" & is.na(tdm_vedolizumab_prior) ~ "Unknown",
                      TRUE ~ as.character(tdm_vedolizumab_prior)))
dfp$tdm_vedolizumab_prior2 <- as.factor(dfp$tdm_vedolizumab_prior2)
summary(dfp$tdm_vedolizumab_prior2)


summary(dfp$vedolizumab_assay)

dfp <- mutate(dfp,
              vedolizumab_assay2 = case_when(
                      vedolizumab_1yr2 == "No" & is.na(vedolizumab_assay) ~ "Not_applicable",
                      tdm_vedolizumab_prior2 == "No" & is.na(vedolizumab_assay) ~ "Not_applicable",
                      tdm_vedolizumab_prior2 == "Unknown" & is.na(vedolizumab_assay) ~ "Not_applicable",
                      TRUE ~ as.character(vedolizumab_assay)))
dfp$vedolizumab_assay2 <- as.factor(dfp$vedolizumab_assay2)
summary(dfp$vedolizumab_assay2)


summary(dfp$vedolizumab_trough_level)

dfp <- mutate(dfp,
              vedolizumab_trough_level2 = case_when(
                      vedolizumab_1yr2 == "No" & is.na(vedolizumab_trough_level) ~ "Not_applicable",
                      tdm_vedolizumab_prior2 == "No" & is.na(vedolizumab_trough_level) ~ "Not_applicable",
                      tdm_vedolizumab_prior2 == "Unknown" & is.na(vedolizumab_trough_level) ~ "Not_applicable",
                      TRUE ~ as.character(vedolizumab_trough_level)))
dfp$vedolizumab_trough_level2 <- as.factor(dfp$vedolizumab_trough_level2)
summary(dfp$vedolizumab_trough_level2)


summary(dfp$vedolizumab_drug_level)

dfp <- mutate(dfp,
              vedolizumab_drug_level2 = case_when(
                      vedolizumab_1yr2 == "No" & is.na(vedolizumab_drug_level) ~ "Not_applicable",
                      tdm_vedolizumab_prior2 == "No" & is.na(vedolizumab_drug_level) ~ "Not_applicable",
                      tdm_vedolizumab_prior2 == "Unknown" & is.na(vedolizumab_drug_level) ~ "Not_applicable",
                      TRUE ~ as.character(vedolizumab_drug_level)))
dfp$vedolizumab_drug_level2 <- as.factor(dfp$vedolizumab_drug_level2)
summary(dfp$vedolizumab_drug_level2)


summary(dfp$vedolizumab_antibody)

dfp <- mutate(dfp,
              vedolizumab_antibody2 = case_when(
                      vedolizumab_1yr2 == "No" & is.na(vedolizumab_antibody) ~ "Not_applicable",
                      tdm_vedolizumab_prior2 == "No" & is.na(vedolizumab_antibody) ~ "Not_applicable",
                      tdm_vedolizumab_prior2 == "Unknown" & is.na(vedolizumab_antibody) ~ "Not_applicable",
                      TRUE ~ as.character(vedolizumab_antibody)))
dfp$vedolizumab_antibody2 <- as.factor(dfp$vedolizumab_antibody2)
summary(dfp$vedolizumab_antibody2)


# Vedolizumab used within 3 months prior to index surgery?
summary(dfp$vedolizumab_3mos)

dfp <- mutate(dfp,
              vedolizumab_3mos2 = case_when(
                      vedolizumab_1yr2 == "No" & is.na(vedolizumab_3mos) ~ "No",
                      vedolizumab_1yr2 == "Unknown" & is.na(vedolizumab_3mos) ~ "Unknown",
                      TRUE ~ as.character(vedolizumab_3mos)))
dfp$vedolizumab_3mos2 <- as.factor(dfp$vedolizumab_3mos2)
summary(dfp$vedolizumab_3mos2)



summary(dfp$vedolizumab_start)

summary(dfp$vedolizumab_start_month)

summary(dfp$vedolizumab_start_year)

dfp$vedolizumab_start_new <- as.factor(paste0(dfp$vedolizumab_start_month, "/", dfp$vedolizumab_start_year))

summary(dfp$vedolizumab_start_new)

dfp <-  dfp %>% mutate(vedolizumab_start_new = replace(vedolizumab_start_new, vedolizumab_start_new == "NA/NA", NA))

summary(dfp$vedolizumab_start)

dfp <- dfp %>% 
        mutate(vedolizumab_start = coalesce(vedolizumab_start, vedolizumab_start_new))

summary(dfp$vedolizumab_start)


dfp <- mutate(dfp,
              vedolizumab_start2 = case_when(
                      vedolizumab_3mos2 == "No" & is.na(vedolizumab_start) ~ "Not_applicable",
                      vedolizumab_3mos2 == "Unknown" & is.na(vedolizumab_start) ~ "Unknown",
                      TRUE ~ as.character(vedolizumab_start)))
dfp$vedolizumab_start2 <- as.factor(dfp$vedolizumab_start2)
summary(dfp$vedolizumab_start2)


summary(dfp$vedolizumab_stop)

summary(dfp$vedolizumab_stop_month)

summary(dfp$vedolizumab_stop_year)

dfp$vedolizumab_stop_new <- as.factor(paste0(dfp$vedolizumab_stop_month, "/", dfp$vedolizumab_stop_year))

summary(dfp$vedolizumab_stop_new)

dfp <-  dfp %>% mutate(vedolizumab_stop_new = replace(vedolizumab_stop_new, vedolizumab_stop_new == "NA/NA", NA))

summary(dfp$vedolizumab_stop)

dfp <- dfp %>% 
        mutate(vedolizumab_stop = coalesce(vedolizumab_stop, vedolizumab_stop_new))

summary(dfp$vedolizumab_stop)


dfp <- mutate(dfp,
              vedolizumab_stop2 = case_when(
                      vedolizumab == "No" & is.na(vedolizumab_stop) ~ "Not_applicable",
                      vedolizumab_3mos2 == "Yes" & is.na(vedolizumab_stop) ~ "Not_applicable",
                      vedolizumab_3mos2 == "Unknown" & is.na(vedolizumab_stop) ~ "Unknown",
                      TRUE ~ as.character(vedolizumab_stop)))
dfp$vedolizumab_stop2 <- as.factor(dfp$vedolizumab_stop2)
summary(dfp$vedolizumab_stop2)


summary(dfp$vedolizumab_dose)

dfp <- mutate(dfp,
              vedolizumab_dose2 = case_when(
                      vedolizumab_3mos2 == "No" & is.na(vedolizumab_dose) ~ "Not_applicable",
                      vedolizumab_3mos2 == "Unknown" & is.na(vedolizumab_dose) ~ "Unknown",
                      TRUE ~ as.character(vedolizumab_dose)))
dfp$vedolizumab_dose2 <- as.factor(dfp$vedolizumab_dose2)
summary(dfp$vedolizumab_dose2)




#############################
# USTEKINUMAB BEFORE SURGERY
############################

# Ustekinumab used since diagnosis?
summary(dfp$ustekinumab)


# Ustekinumab used within 1 year prior to surgery?
summary(dfp$ustekinumab_1yr)

dfp <- mutate(dfp,
              ustekinumab_1yr2 = case_when(
                      ustekinumab == "No" & is.na(ustekinumab_1yr) ~ "No",
                      ustekinumab == "Unknown" & is.na(ustekinumab_1yr) ~ "Unknown",
                      TRUE ~ as.character(ustekinumab_1yr)))
dfp$ustekinumab_1yr2 <- as.factor(dfp$ustekinumab_1yr2)
summary(dfp$ustekinumab_1yr2)



summary(dfp$ustekinumab_induction)

dfp <- mutate(dfp,
              ustekinumab_induction2 = case_when(
                      ustekinumab_1yr2 == "No" & is.na(ustekinumab_induction) ~ "Not_applicable",
                      ustekinumab_1yr2 == "Unknown" & is.na(ustekinumab_induction) ~ "Unknown",
                      TRUE ~ as.character(ustekinumab_induction)))
dfp$ustekinumab_induction2 <- as.factor(dfp$ustekinumab_induction2)
summary(dfp$ustekinumab_induction2)



summary(dfp$ustekinumab_freq_1yr)

dfp <- mutate(dfp,
              ustekinumab_freq_1yr2 = case_when(
                      ustekinumab_1yr2 == "No" & is.na(ustekinumab_freq_1yr) ~ "Not_applicable",
                      ustekinumab_1yr2 == "Unknown" & is.na(ustekinumab_freq_1yr) ~ "Unknown",
                      ustekinumab_induction2 == "Not_applicable" & is.na(ustekinumab_freq_1yr) ~ "Not_applicable",
                      ustekinumab_induction2 == "Unknown" & is.na(ustekinumab_freq_1yr) ~ "Unknown",
                      TRUE ~ as.character(ustekinumab_freq_1yr)))
dfp$ustekinumab_freq_1yr2 <- as.factor(dfp$ustekinumab_freq_1yr2)
summary(dfp$ustekinumab_freq_1yr2)


summary(dfp$ustekinumab_combo)

dfp <- mutate(dfp,
              ustekinumab_combo2 = case_when(
                      ustekinumab_1yr2 == "No" & is.na(ustekinumab_combo) ~ "Not_applicable",
                      ustekinumab_1yr2 == "Unknown" & is.na(ustekinumab_combo) ~ "Unknown",
                      TRUE ~ as.character(ustekinumab_combo)))
dfp$ustekinumab_combo2 <- as.factor(dfp$ustekinumab_combo2)
summary(dfp$ustekinumab_combo2)


summary(dfp$ustekinumab_combo_drug1)

dfp <- mutate(dfp,
              ustekinumab_combo_drug12 = case_when(
                      ustekinumab_1yr2 == "No" & is.na(ustekinumab_combo_drug1) ~ "Not_applicable",
                      ustekinumab_1yr2 == "Unknown" & is.na(ustekinumab_combo_drug1) ~ "Unknown",
                      ustekinumab_combo2 == "No" | ustekinumab_combo2 == "Not_applicable" ~ "Not_applicable",
                      TRUE ~ as.character(ustekinumab_combo_drug1)))
dfp$ustekinumab_combo_drug12 <- as.factor(dfp$ustekinumab_combo_drug12)
summary(dfp$ustekinumab_combo_drug12)


summary(dfp$ustekinumab_combo_drug2)

dfp <- mutate(dfp,
              ustekinumab_combo_drug22 = case_when(
                      ustekinumab_1yr2 == "No" & is.na(ustekinumab_combo_drug2) ~ "Not_applicable",
                      ustekinumab_1yr2 == "Unknown" & is.na(ustekinumab_combo_drug2) ~ "Unknown",
                      ustekinumab_combo2 == "No" | ustekinumab_combo2 == "Not_applicable" ~ "Not_applicable",
                      TRUE ~ as.character(ustekinumab_combo_drug2)))
dfp$ustekinumab_combo_drug22 <- as.factor(dfp$ustekinumab_combo_drug22)
summary(dfp$ustekinumab_combo_drug22)



summary(dfp$ustekinumab_combo_drug3)

dfp <- mutate(dfp,
              ustekinumab_combo_drug32 = case_when(
                      ustekinumab_1yr2 == "No" & is.na(ustekinumab_combo_drug3) ~ "Not_applicable",
                      ustekinumab_1yr2 == "Unknown" & is.na(ustekinumab_combo_drug3) ~ "Unknown",
                      ustekinumab_combo2 == "No" | ustekinumab_combo2 == "Not_applicable" ~ "Not_applicable",
                      TRUE ~ as.character(ustekinumab_combo_drug3)))
dfp$ustekinumab_combo_drug32 <- as.factor(dfp$ustekinumab_combo_drug32)
summary(dfp$ustekinumab_combo_drug32)


summary(dfp$tdm_ustekinumab_prior)

dfp <- mutate(dfp,
              tdm_ustekinumab_prior2 = case_when(
                      ustekinumab_1yr2 == "No" & is.na(tdm_ustekinumab_prior) ~ "Not_applicable",
                      ustekinumab_1yr2 == "Unknown" & is.na(tdm_ustekinumab_prior) ~ "Unknown",
                      TRUE ~ as.character(tdm_ustekinumab_prior)))
dfp$tdm_ustekinumab_prior2 <- as.factor(dfp$tdm_ustekinumab_prior2)
summary(dfp$tdm_ustekinumab_prior2)


summary(dfp$ustekinumab_assay)

dfp <- mutate(dfp,
              ustekinumab_assay2 = case_when(
                      ustekinumab_1yr2 == "No" & is.na(ustekinumab_assay) ~ "Not_applicable",
                      tdm_ustekinumab_prior2 == "No" & is.na(ustekinumab_assay) ~ "Not_applicable",
                      tdm_ustekinumab_prior2 == "Unknown" & is.na(ustekinumab_assay) ~ "Not_applicable",
                      TRUE ~ as.character(ustekinumab_assay)))
dfp$ustekinumab_assay2 <- as.factor(dfp$ustekinumab_assay2)
summary(dfp$ustekinumab_assay2)


summary(dfp$ustekinumab_trough_level)

dfp <- mutate(dfp,
              ustekinumab_trough_level2 = case_when(
                      ustekinumab_1yr2 == "No" & is.na(ustekinumab_trough_level) ~ "Not_applicable",
                      tdm_ustekinumab_prior2 == "No" & is.na(ustekinumab_trough_level) ~ "Not_applicable",
                      tdm_ustekinumab_prior2 == "Unknown" & is.na(ustekinumab_trough_level) ~ "Not_applicable",
                      TRUE ~ as.character(ustekinumab_trough_level)))
dfp$ustekinumab_trough_level2 <- as.factor(dfp$ustekinumab_trough_level2)
summary(dfp$ustekinumab_trough_level2)


summary(dfp$ustekinumab_drug_level)

dfp <- mutate(dfp,
              ustekinumab_drug_level2 = case_when(
                      ustekinumab_1yr2 == "No" & is.na(ustekinumab_drug_level) ~ "Not_applicable",
                      tdm_ustekinumab_prior2 == "No" & is.na(ustekinumab_drug_level) ~ "Not_applicable",
                      tdm_ustekinumab_prior2 == "Unknown" & is.na(ustekinumab_drug_level) ~ "Not_applicable",
                      TRUE ~ as.character(ustekinumab_drug_level)))
dfp$ustekinumab_drug_level2 <- as.factor(dfp$ustekinumab_drug_level2)
summary(dfp$ustekinumab_drug_level2)


summary(dfp$ustekinumab_antibody)

dfp <- mutate(dfp,
              ustekinumab_antibody2 = case_when(
                      ustekinumab_1yr2 == "No" & is.na(ustekinumab_antibody) ~ "Not_applicable",
                      tdm_ustekinumab_prior2 == "No" & is.na(ustekinumab_antibody) ~ "Not_applicable",
                      tdm_ustekinumab_prior2 == "Unknown" & is.na(ustekinumab_antibody) ~ "Not_applicable",
                      TRUE ~ as.character(ustekinumab_antibody)))
dfp$ustekinumab_antibody2 <- as.factor(dfp$ustekinumab_antibody2)
summary(dfp$ustekinumab_antibody2)


# Ustekinumab used within 3 months prior to index surgery?
summary(dfp$ustekinumab_3mos)

dfp <- mutate(dfp,
              ustekinumab_3mos2 = case_when(
                      ustekinumab_1yr2 == "No" & is.na(ustekinumab_3mos) ~ "No",
                      ustekinumab_1yr2 == "Unknown" & is.na(ustekinumab_3mos) ~ "Unknown",
                      TRUE ~ as.character(ustekinumab_3mos)))
dfp$ustekinumab_3mos2 <- as.factor(dfp$ustekinumab_3mos2)
summary(dfp$ustekinumab_3mos2)


summary(dfp$ustekinumab_start)

summary(dfp$ustekinumab_start_month)

summary(dfp$ustekinumab_start_year)

dfp$ustekinumab_start_new <- as.factor(paste0(dfp$ustekinumab_start_month, "/", dfp$ustekinumab_start_year))

summary(dfp$ustekinumab_start_new)

dfp <-  dfp %>% mutate(ustekinumab_start_new = replace(ustekinumab_start_new, ustekinumab_start_new == "NA/NA", NA))

summary(dfp$ustekinumab_start)

dfp <- dfp %>% 
        mutate(ustekinumab_start = coalesce(ustekinumab_start, ustekinumab_start_new))

summary(dfp$ustekinumab_start)


dfp <- mutate(dfp,
              ustekinumab_start2 = case_when(
                      ustekinumab_3mos2 == "No" & is.na(ustekinumab_start) ~ "Not_applicable",
                      ustekinumab_3mos2 == "Unknown" & is.na(ustekinumab_start) ~ "Unknown",
                      TRUE ~ as.character(ustekinumab_start)))
dfp$ustekinumab_start2 <- as.factor(dfp$ustekinumab_start2)
summary(dfp$ustekinumab_start2)


summary(dfp$ustekinumab_stop)

summary(dfp$ustekinumab_stop_month)

summary(dfp$ustekinumab_stop_year)

dfp$ustekinumab_stop_new <- as.factor(paste0(dfp$ustekinumab_stop_month, "/", dfp$ustekinumab_stop_year))

summary(dfp$ustekinumab_stop_new)

dfp <-  dfp %>% mutate(ustekinumab_stop_new = replace(ustekinumab_stop_new, ustekinumab_stop_new == "NA/NA", NA))

summary(dfp$ustekinumab_stop)

dfp <- dfp %>% 
        mutate(ustekinumab_stop = coalesce(ustekinumab_stop, ustekinumab_stop_new))

summary(dfp$ustekinumab_stop)


dfp <- mutate(dfp,
              ustekinumab_stop2 = case_when(
                      ustekinumab == "No" & is.na(ustekinumab_stop) ~ "Not_applicable",
                      ustekinumab_3mos2 == "Yes" & is.na(ustekinumab_stop) ~ "Not_applicable",
                      ustekinumab_3mos2 == "Unknown" & is.na(ustekinumab_stop) ~ "Unknown",
                      TRUE ~ as.character(ustekinumab_stop)))
dfp$ustekinumab_stop2 <- as.factor(dfp$ustekinumab_stop2)
summary(dfp$ustekinumab_stop2)


summary(dfp$ustekinumab_dose)

dfp <- mutate(dfp,
              ustekinumab_dose2 = case_when(
                      ustekinumab_3mos2 == "No" & is.na(ustekinumab_dose) ~ "Not_applicable",
                      ustekinumab_3mos2 == "Unknown" & is.na(ustekinumab_dose) ~ "Unknown",
                      TRUE ~ as.character(ustekinumab_dose)))
dfp$ustekinumab_dose2 <- as.factor(dfp$ustekinumab_dose2)
summary(dfp$ustekinumab_dose2)



##############################
##############################
# MEDICATION AFTER RESECTION #
##############################
##############################


################
# ORAL STEROIDS
################

# Oral corticosteroids used since last assessment?
summary(dfe$oralster_since_last)

# Oral corticosteroids used currently?
summary(dfe$oralster_current_upd)

dfe <- mutate(dfe,
              oralster_current_upd2 = case_when(
                      oralster_since_last == "No" & is.na(oralster_current_upd) ~ "No",
                      oralster_since_last == "Unknown" & is.na(oralster_current_upd) ~ "Unknown",
                      TRUE ~ as.character(oralster_current_upd)
              ))
dfe$oralster_current_upd2 <- as.factor(dfe$oralster_current_upd2)
summary(dfe$oralster_current_upd2)


##############
# IV STEROIDS
#############

# IV corticosteroids used since last assessment?
summary(dfe$ivster_since_last)

# IV corticosteroids used currently?
summary(dfe$ivster_current_upd)

dfe <- mutate(dfe,
              ivster_current_upd2 = case_when(
                      ivster_since_last == "No" & is.na(ivster_current_upd) ~ "No",
                      ivster_since_last == "Unknown" & is.na(ivster_current_upd) ~ "Unknown",
                      TRUE ~ as.character(ivster_current_upd)
              ))
dfe$ivster_current_upd2 <- as.factor(dfe$ivster_current_upd2)
summary(dfe$ivster_current_upd2)


##############
# ANY STEROID
#############

dfe <- mutate(dfe,
              anyster_since_last = case_when(
                      ivster_since_last == "Yes" | oralster_since_last == "Yes" ~ "Yes",
                      ivster_since_last == "No" & oralster_since_last == "No" ~ "No",
                      ivster_since_last == "Unknown" | oralster_since_last == "Unknown" ~ "Unknown",
                      TRUE ~ as.character(ivster_since_last)
              ))
dfe$anyster_since_last <- as.factor(dfe$anyster_since_last)
summary(dfe$anyster_since_last)


##############
# ANY STEROID
#############

dfe <- mutate(dfe,
              anyster_current_upd = case_when(
                      ivster_current_upd2 == "Yes" | oralster_current_upd2 == "Yes" ~ "Yes",
                      ivster_current_upd2 == "No" & oralster_current_upd2 == "No" ~ "No",
                      ivster_current_upd2 == "Unknown" | oralster_current_upd2 == "Unknown" ~ "Unknown",
                      TRUE ~ as.character(ivster_current_upd)
              ))
dfe$anyster_current_upd <- as.factor(dfe$anyster_current_upd)
summary(dfe$anyster_current_upd)



##############
# ANTIBIOTICS
##############

# Antibiotics used since last assessment?
summary(dfe$antibio_since_last)

# Antibiotics used currently?
summary(dfe$antibio_current_upd)

dfe <- mutate(dfe,
              antibio_current_upd2 = case_when(
                      antibio_since_last == "No" & is.na(antibio_current_upd) ~ "No",
                      antibio_since_last == "Unknown" & is.na(antibio_current_upd) ~ "Unknown",
                      TRUE ~ as.character(antibio_current_upd)
              ))
dfe$antibio_current_upd2 <- as.factor(dfe$antibio_current_upd2)
summary(dfe$antibio_current_upd2)


#################
# IMMUNOMODULATOR
#################

# Immunomodulatory drugs used since last assessment?
summary(dfe$immuno_since_last)

# Immunomodulatory drugs used currently?
summary(dfe$immuno_current_upd)

dfe <- mutate(dfe,
              immuno_current_upd2 = case_when(
                      immuno_since_last == "No" & is.na(immuno_current_upd) ~ "No",
                      immuno_since_last == "Unknown" & is.na(immuno_current_upd) ~ "Unknown",
                      TRUE ~ as.character(immuno_current_upd)
              ))
dfe$immuno_current_upd2 <- as.factor(dfe$immuno_current_upd2)
summary(dfe$immuno_current_upd2)


###############
# METHOTREXATE
###############

# MTX used since last assessment?
summary(dfe$mtx_since_last)

# MTX used currently?
summary(dfe$mtx_current_upd)

dfe <- mutate(dfe,
              mtx_current_upd2 = case_when(
                      mtx_since_last == "No" & is.na(mtx_current_upd) ~ "No",
                      mtx_since_last == "Unknown" & is.na(mtx_current_upd) ~ "Unknown",
                      TRUE ~ as.character(mtx_current_upd)
              ))
dfe$mtx_current_upd2 <- as.factor(dfe$mtx_current_upd2)
summary(dfe$mtx_current_upd2)


#############################
# MERGING THIOPURINES AND MTX
#############################

summary(dfe$immuno_since_last)

summary(dfe$mtx_since_last)

dfe <- mutate(dfe,
                immuno_mtx_since_last = case_when(
                        immuno_since_last == "Yes" | mtx_since_last == "Yes" ~ "Yes",
                        immuno_since_last == "No" & mtx_since_last == "No" ~ "No",
                        immuno_since_last == "Unknown" | mtx_since_last == "Unknown" ~ "Unknown",
                        TRUE ~ NA_character_))
dfe$immuno_mtx_since_last <- as.factor(dfe$immuno_mtx_since_last)
summary(dfe$immuno_mtx_since_last)


summary(dfe$immuno_current_upd2)

summary(dfe$mtx_current_upd2)

dfe <- mutate(dfe,
                immuno_mtx_current_upd = case_when(
                        immuno_current_upd2 == "Yes" | mtx_current_upd2 == "Yes" ~ "Yes",
                        immuno_current_upd2 == "No" & mtx_current_upd2 == "No" ~ "No",
                        immuno_current_upd2 == "Unknown" | mtx_current_upd2 == "Unknown" ~ "Unknown",
                        TRUE ~ NA_character_))
dfe$immuno_mtx_current_upd <- as.factor(dfe$immuno_mtx_current_upd)
summary(dfe$immuno_mtx_current_upd)


###########
# ANTI-TNF 
###########

# INFLIXIMAB

# Infliximab used since last assessment?
summary(dfe$infliximab_since_last)

# Infliximab used currently?. 
summary(dfe$infliximab_current_upd)


dfe <- mutate(dfe,
              infliximab_current_upd2 = case_when(
                      infliximab_since_last == "No" & is.na(infliximab_current_upd) ~ "No",
                      infliximab_since_last == "Unknown" & is.na(infliximab_current_upd) ~ "Unknown",
                      TRUE ~ as.character(infliximab_current_upd)))
dfe$infliximab_current_upd2 <- as.factor(dfe$infliximab_current_upd2)
summary(dfe$infliximab_current_upd2)


summary(dfe$infliximab_upd_start)

dfe <- mutate(dfe,
              infliximab_upd_start2 = case_when(
                      infliximab_since_last == "No" & is.na(infliximab_upd_start) ~ "Not_applicable",
                      infliximab_since_last == "Unknown" & is.na(infliximab_upd_start) ~ "Unknown",
                      TRUE ~ as.character(infliximab_upd_start)))
dfe$infliximab_upd_start2 <- as.factor(dfe$infliximab_upd_start2)
summary(dfe$infliximab_upd_start2)


summary(dfe$infliximab_upd_stop)

dfe <- mutate(dfe,
              infliximab_upd_stop2 = case_when(
                      infliximab_current_upd2 == "Yes" & is.na(infliximab_upd_stop) ~ "Not_applicable",
                      infliximab_since_last == "No" & is.na(infliximab_upd_stop) ~ "Not_applicable",
                      infliximab_since_last == "Unknown" & is.na(infliximab_upd_start) ~ "Unknown",
                      TRUE ~ as.character(infliximab_upd_stop)))
dfe$infliximab_upd_stop2 <- as.factor(dfe$infliximab_upd_stop2)
summary(dfe$infliximab_upd_stop2)


summary(dfe$infliximab_upd_dose)

dfe <- mutate(dfe,
              infliximab_upd_dose2 = case_when(
                      infliximab_since_last == "No" & is.na(infliximab_upd_dose) ~ "Not_applicable",
                      infliximab_since_last == "Unknown" & is.na(infliximab_upd_dose) ~ "Unknown",
                      TRUE ~ as.character(infliximab_upd_dose)))
dfe$infliximab_upd_dose2 <- as.factor(dfe$infliximab_upd_dose2)
summary(dfe$infliximab_upd_dose2)


# ADALIMUMAB

# Adalimumab used since last assessment?
summary(dfe$adalimumab_since_last)


# Adalimumab used currently?. 
summary(dfe$adalimumab_current_upd)



dfe <- mutate(dfe,
              adalimumab_current_upd2 = case_when(
                      adalimumab_since_last == "No" & is.na(adalimumab_current_upd) ~ "No",
                      adalimumab_since_last == "Unknown" & is.na(adalimumab_current_upd) ~ "Unknown",
                      TRUE ~ as.character(adalimumab_current_upd)))
dfe$adalimumab_current_upd2 <- as.factor(dfe$adalimumab_current_upd2)
summary(dfe$adalimumab_current_upd2)


summary(dfe$adalimumab_upd_start)

dfe <- mutate(dfe,
              adalimumab_upd_start2 = case_when(
                      adalimumab_since_last == "No" & is.na(adalimumab_upd_start) ~ "Not_applicable",
                      adalimumab_since_last == "Unknown" & is.na(adalimumab_upd_start) ~ "Unknown",
                      TRUE ~ as.character(adalimumab_upd_start)))
dfe$adalimumab_upd_start2 <- as.factor(dfe$adalimumab_upd_start2)
summary(dfe$adalimumab_upd_start2)


summary(dfe$adalimumab_upd_stop)

dfe <- mutate(dfe,
              adalimumab_upd_stop2 = case_when(
                      adalimumab_current_upd2 == "Yes" & is.na(adalimumab_upd_stop) ~ "Not_applicable",
                      adalimumab_since_last == "No" & is.na(adalimumab_upd_stop) ~ "Not_applicable",
                      adalimumab_since_last == "Unknown" & is.na(adalimumab_upd_stop) ~ "Unknown",
                      TRUE ~ as.character(adalimumab_upd_stop)))
dfe$adalimumab_upd_stop2 <- as.factor(dfe$adalimumab_upd_stop2)
summary(dfe$adalimumab_upd_stop2)


summary(dfe$adalimumab_upd_dose)

dfe <- mutate(dfe,
              adalimumab_upd_dose2 = case_when(
                      adalimumab_since_last == "No" & is.na(adalimumab_upd_dose) ~ "Not_applicable",
                      adalimumab_since_last == "Unknown" & is.na(adalimumab_upd_dose) ~ "Unknown",
                      TRUE ~ as.character(adalimumab_upd_dose)))
dfe$adalimumab_upd_dose2 <- as.factor(dfe$adalimumab_upd_dose2)
summary(dfe$adalimumab_upd_dose2)




# CERTOLIZUMAB

# Certolizumab used since last assessment?
summary(dfe$certolizumab_since_last)


# Certolizumab used currently?. 
summary(dfe$certolizumab_current_upd)

dfe <- mutate(dfe,
              certolizumab_current_upd2 = case_when(
                      certolizumab_since_last == "No" & is.na(certolizumab_current_upd) ~ "No",
                      certolizumab_since_last == "Unknown" & is.na(certolizumab_current_upd) ~ "Unknown",
                      TRUE ~ as.character(certolizumab_current_upd)))
dfe$certolizumab_current_upd2 <- as.factor(dfe$certolizumab_current_upd2)
summary(dfe$certolizumab_current_upd2)


summary(dfe$certolizumab_upd_start)

dfe <- mutate(dfe,
              certolizumab_upd_start2 = case_when(
                      certolizumab_since_last == "No" & is.na(certolizumab_upd_start) ~ "Not_applicable",
                      certolizumab_since_last == "Unknown" & is.na(certolizumab_upd_start) ~ "Unknown",
                      TRUE ~ as.character(certolizumab_upd_start)))
dfe$certolizumab_upd_start2 <- as.factor(dfe$certolizumab_upd_start2)
summary(dfe$certolizumab_upd_start2)


summary(dfe$certolizumab_upd_stop)

dfe <- mutate(dfe,
              certolizumab_upd_stop2 = case_when(
                      certolizumab_current_upd2 == "Yes" & is.na(certolizumab_upd_stop) ~ "Not_applicable",
                      certolizumab_since_last == "No" & is.na(certolizumab_upd_stop) ~ "Not_applicable",
                      certolizumab_since_last == "Unknown" & is.na(certolizumab_upd_stop) ~ "Unknown",
                      TRUE ~ as.character(certolizumab_upd_stop)))
dfe$certolizumab_upd_stop2 <- as.factor(dfe$certolizumab_upd_stop2)
summary(dfe$certolizumab_upd_stop2)


summary(dfe$certolizumab_upd_dose)

dfe <- mutate(dfe,
              certolizumab_upd_dose2 = case_when(
                      certolizumab_since_last == "No" & is.na(certolizumab_upd_dose) ~ "Not_applicable",
                      certolizumab_since_last == "Unknown" & is.na(certolizumab_upd_dose) ~ "Unknown",
                      TRUE ~ as.character(certolizumab_upd_dose)))
dfe$certolizumab_upd_dose2 <- as.factor(dfe$certolizumab_upd_dose2)
summary(dfe$certolizumab_upd_dose2)



# Creating anti-TNF variable

# Anti-TNF since last

dfe <- mutate(dfe,
              tnf_any_since_last = case_when(
                      infliximab_since_last == "Yes" | adalimumab_since_last == "Yes" | certolizumab_since_last == "Yes" ~ "Yes",
                      infliximab_since_last == "No" & adalimumab_since_last == "No" & certolizumab_since_last == "No" ~ "No",
                      infliximab_since_last == "Unknown" | adalimumab_since_last == "Unknown" | certolizumab_since_last == "Unknown" ~ "Unknown",
                      TRUE ~ NA_character_))
dfe$tnf_any_since_last <- as.factor(dfe$tnf_any_since_last)
summary(dfe$tnf_any_since_last)


# Anti-TNF current upd

dfe <- mutate(dfe,
              tnf_any_current_upd = case_when(
                      infliximab_current_upd2 == "Yes" | adalimumab_current_upd2 == "Yes" | certolizumab_current_upd2 == "Yes" ~ "Yes",
                      infliximab_current_upd2 == "No" & adalimumab_current_upd2 == "No" & certolizumab_current_upd2 == "No" ~ "No",
                      infliximab_current_upd2 == "Unknown" | adalimumab_current_upd2 == "Unknown" | certolizumab_current_upd2 == "Unknown" ~ "Unknown",
                      TRUE ~ NA_character_))
dfe$tnf_any_current_upd <- as.factor(dfe$tnf_any_current_upd)
summary(dfe$tnf_any_current_upd)



summary(dfe$othermeds_text_upd)

# There are some biologics added as free text

# A023701-130726 (MSH): Vedo is correctly recorded, therefore, the free text was deleted
# C825395-825466 (UdeM): anti-integrin or placebo at fourth colonooscopy (won't be added to the current analysis). Also no research biopsies
# C822431-966426 (Pitts): azathioprine was added as free text, but it's correctly added to the correct section as well
# A051501-130797 (MSH): Golimumab since last and current will be added to anti-TNF
# C803513-940444 (MSH): Golimumab since last and current will be added to anti-TNF. Also, ustekinumab since last but not current
# C825159-900615 (JHU): Golimumab since last but not current
# C834827-974292 (MSH): free text was deleted because ustekinumab was recorded correctly in the section
# C839160-874932 (ISMMS): Using upadacitinib at third and fourth colonoscopies. It will be considered when excluding patients on therapy
# C825395-825466 (UdeM): apparently patient was no on JAK1 according to the report of third endoscopy but humira.


##############
# VEDOLIZUMAB
#############

# Vedolizumab used since last assessment?
summary(dfe$vedolizumab_since_last)

# Vedolizumab used currently?. Show the field ONLY if: [vedolizumab_since_last] = '1'
summary(dfe$vedolizumab_current_upd)

dfe <- mutate(dfe,
              vedolizumab_current_upd2 = case_when(
                      vedolizumab_since_last == "No" & is.na(vedolizumab_current_upd) ~ "No",
                      vedolizumab_since_last == "Unknown" & is.na(vedolizumab_current_upd) ~ "Unknown",
                      TRUE ~ as.character(vedolizumab_current_upd)
              ))
dfe$vedolizumab_current_upd2 <- as.factor(dfe$vedolizumab_current_upd2)
summary(dfe$vedolizumab_current_upd2)


summary(dfe$vedolizumab_upd_start)

dfe <- mutate(dfe,
              vedolizumab_upd_start2 = case_when(
                      vedolizumab_since_last == "No" & is.na(vedolizumab_upd_start) ~ "Not_applicable",
                      vedolizumab_since_last == "Unknown" & is.na(vedolizumab_upd_start) ~ "Unknown",
                      TRUE ~ as.character(vedolizumab_upd_start)))
dfe$vedolizumab_upd_start2 <- as.factor(dfe$vedolizumab_upd_start2)
summary(dfe$vedolizumab_upd_start2)


summary(dfe$vedolizumab_upd_stop)

dfe <- mutate(dfe,
              vedolizumab_upd_stop2 = case_when(
                      vedolizumab_current_upd2 == "Yes" & is.na(vedolizumab_upd_stop) ~ "Not_applicable",
                      vedolizumab_since_last == "No" & is.na(vedolizumab_upd_stop) ~ "Not_applicable",
                      vedolizumab_since_last == "Unknown" & is.na(vedolizumab_upd_stop) ~ "Unknown",
                      TRUE ~ as.character(vedolizumab_upd_stop)))
dfe$vedolizumab_upd_stop2 <- as.factor(dfe$vedolizumab_upd_stop2)
summary(dfe$vedolizumab_upd_stop2)


summary(dfe$vedolizumab_upd_dose)

dfe <- mutate(dfe,
              vedolizumab_upd_dose2 = case_when(
                      vedolizumab_since_last == "No" & is.na(vedolizumab_upd_dose) ~ "Not_applicable",
                      vedolizumab_since_last == "Unknown" & is.na(vedolizumab_upd_dose) ~ "Unknown",
                      TRUE ~ as.character(vedolizumab_upd_dose)))
dfe$vedolizumab_upd_dose2 <- as.factor(dfe$vedolizumab_upd_dose2)
summary(dfe$vedolizumab_upd_dose2)


#############
#USTEKINUMAB
#############

# Ustekinumab used since last assessment?
summary(dfe$ustekinumab_since_last)

# Ustekinumab used currently?. Show the field ONLY if: [ustekinumab_since_last] = '1'
summary(dfe$ustekinumab_current_upd)

dfe <- mutate(dfe,
              ustekinumab_current_upd2 = case_when(
                      ustekinumab_since_last == "No" & is.na(ustekinumab_current_upd) ~ "No",
                      ustekinumab_since_last == "Unknown" & is.na(ustekinumab_current_upd) ~ "Unknown",
                      TRUE ~ as.character(ustekinumab_current_upd)
              ))
dfe$ustekinumab_current_upd2 <- as.factor(dfe$ustekinumab_current_upd2)
summary(dfe$ustekinumab_current_upd2)


summary(dfe$ustekinumab_upd_start)

dfe <- mutate(dfe,
              ustekinumab_upd_start2 = case_when(
                      ustekinumab_since_last == "No" & is.na(ustekinumab_upd_start) ~ "Not_applicable",
                      ustekinumab_since_last == "Unknown" & is.na(ustekinumab_upd_start) ~ "Unknown",
                      TRUE ~ as.character(ustekinumab_upd_start)))
dfe$ustekinumab_upd_start2 <- as.factor(dfe$ustekinumab_upd_start2)
summary(dfe$ustekinumab_upd_start2)


summary(dfe$ustekinumab_upd_stop)

dfe <- mutate(dfe,
              ustekinumab_upd_stop2 = case_when(
                      ustekinumab_current_upd2 == "Yes" & is.na(ustekinumab_upd_stop) ~ "Not_applicable",
                      ustekinumab_since_last == "No" & is.na(ustekinumab_upd_stop) ~ "Not_applicable",
                      ustekinumab_since_last == "Unknown" & is.na(ustekinumab_upd_stop) ~ "Unknown",
                      TRUE ~ as.character(ustekinumab_upd_stop)))
dfe$ustekinumab_upd_stop2 <- as.factor(dfe$ustekinumab_upd_stop2)
summary(dfe$ustekinumab_upd_stop2)


summary(dfe$ustekinumab_upd_dose)

dfe <- mutate(dfe,
              ustekinumab_upd_dose2 = case_when(
                      ustekinumab_since_last == "No" & is.na(ustekinumab_upd_dose) ~ "Not_applicable",
                      ustekinumab_since_last == "Unknown" & is.na(ustekinumab_upd_dose) ~ "Unknown",
                      TRUE ~ as.character(ustekinumab_upd_dose)))
dfe$ustekinumab_upd_dose2 <- as.factor(dfe$ustekinumab_upd_dose2)
summary(dfe$ustekinumab_upd_dose2)



#####################################
# CREATING COMBO THERAPY FOR ANTI-TNF
#####################################


summary(dfe$tnf_any_since_last)

summary(dfe$immuno_since_last)

summary(dfe$mtx_since_last)

# Combo_sincel_last
dfe <- mutate(dfe,
              combo_since_last = case_when(
                      tnf_any_since_last == "Yes" & immuno_since_last == "Yes"  ~ "Combo",
                      tnf_any_since_last == "Yes" & mtx_since_last == "Yes"  ~ "Combo",
                      tnf_any_since_last == "Yes" & immuno_since_last == "No"  ~ "Mono_TNF",
                      tnf_any_since_last == "Yes" & mtx_since_last == "No"  ~ "Mono_TNF",
                      tnf_any_since_last == "No" & immuno_since_last == "Yes"  ~ "Mono_immuno",
                      tnf_any_since_last == "No" & mtx_since_last == "Yes"  ~ "Mono_mtx",
                      tnf_any_since_last == "No" & immuno_since_last == "No" & mtx_since_last == "No" & vedolizumab_since_last == "Yes" ~ "Mono_Vedo",
                      tnf_any_since_last == "No" & immuno_since_last == "No" & mtx_since_last == "No" & ustekinumab_since_last == "Yes" ~ "Mono_Uste",
                      tnf_any_since_last == "No" & immuno_since_last == "No" & mtx_since_last == "No" & vedolizumab_since_last == "No" & ustekinumab_since_last == "No" ~ "No_therapy",
                      tnf_any_since_last == "Unknown" | immuno_since_last == "Unknown"  ~ "Unknown",
                      tnf_any_since_last == "Unknown" | mtx_since_last == "Unknown"  ~ "Unknown",
                      TRUE ~ NA_character_))
dfe$combo_since_last <- as.factor(dfe$combo_since_last)
summary(dfe$combo_since_last)


dfe_combo_since_last <- subset(dfe, select = c("combo_since_last", "tnf_any_since_last", "immuno_since_last",
                                               "mtx_since_last", "vedolizumab_since_last", "ustekinumab_since_last"))


# Combo_current_upd

dfe <- mutate(dfe,
              combo_current_upd = case_when(
                      tnf_any_current_upd == "Yes" & immuno_current_upd2 == "Yes"  ~ "Combo",
                      tnf_any_current_upd == "Yes" & mtx_current_upd2 == "Yes"  ~ "Combo",
                      tnf_any_current_upd == "Yes" & immuno_current_upd2 == "No"  ~ "Mono_TNF",
                      tnf_any_current_upd == "Yes" & mtx_current_upd2 == "No"  ~ "Mono_TNF",
                      tnf_any_current_upd == "No" & immuno_current_upd2 == "Yes"  ~ "Mono_immuno",
                      tnf_any_current_upd == "No" & mtx_current_upd2 == "Yes"  ~ "Mono_mtx",
                      tnf_any_current_upd == "No" & immuno_current_upd2 == "No" & mtx_current_upd2 == "No" & vedolizumab_current_upd2 == "Yes" ~ "Mono_Vedo",
                      tnf_any_current_upd == "No" & immuno_current_upd2 == "No" & mtx_current_upd2 == "No" & ustekinumab_current_upd2 == "Yes" ~ "Mono_Uste",
                      tnf_any_current_upd == "No" & immuno_current_upd2 == "No" & mtx_current_upd2 == "No" & vedolizumab_current_upd2 == "No" & ustekinumab_current_upd2 == "No" ~ "No_therapy",
                      tnf_any_current_upd == "Unknown" | immuno_current_upd2 == "Unknown"  ~ "Unknown",
                      tnf_any_current_upd == "Unknown" | mtx_current_upd2 == "Unknown"  ~ "Unknown",
                      TRUE ~ NA_character_))
dfe$combo_current_upd <- as.factor(dfe$combo_current_upd)
summary(dfe$combo_current_upd)


dfe_combo_current_upd <- subset(dfe, select = c("combo_current_upd", "tnf_any_current_upd", "immuno_current_upd2",
                                                "mtx_current_upd2", "vedolizumab_current_upd2", "ustekinumab_current_upd2"))


summary(dfe$combo_current_upd)

######################
# NO MEDS AT ENDOSCOPY
######################

summary(dfe$combo_since_last)

dfe <- mutate(dfe,
                   no_meds_since_last = case_when(
                           combo_since_last == "No_therapy" ~ "Yes",
                           TRUE ~ as.character("No")))

dfe$no_meds_since_last <- as.factor(dfe$no_meds_since_last)
summary(dfe$no_meds_since_last)


dfe <- mutate(dfe,
                   no_meds_current_upd = case_when(
                           combo_current_upd == "No_therapy" ~ "Yes",
                           TRUE ~ as.character("No")))

dfe$no_meds_current_upd <- as.factor(dfe$no_meds_current_upd)
summary(dfe$no_meds_current_upd)



######################
# SMOKER AFTER SURGERY
######################

# These are not branching logic
## There are 2 variables addressing smoking status: smoking_status & smoking_status_legacy

# Instrument: Treatment Update and Disease Activity

# smoking_status
# Section Header: Current Smoking Status
# Has patient smoked since last assessment date ([prev_assess_date]):
# prev_assess_date: "Date of ileal resection or previous endoscopy, whichever is most recent"
summary(dfe$smoking_status)


# Section Header: Current Smoking Status (from original REDCap project)
# Smoking status
# From old codebook: Section Header: IMPORTANT: This form is intended to capture changes from previous information.
# The "date of previous assessment" corresponds to the last on-study assessment (either baseline or last follow-up).
summary(dfe$smoking_status_legacy)

dfe <- mutate(dfe,
              smoking_status2 = case_when(
                      is.na(smoking_status) & smoking_status_legacy == "Current smoker" ~ "Yes",
                      is.na(smoking_status) & smoking_status_legacy == "Non-smoker" ~ "No",
                      smoking_status == "Unknown" & smoking_status_legacy == "Current smoker" ~ "Yes",
                      smoking_status == "Unknown" & smoking_status_legacy == "Non-smoker" ~ "No",
                      TRUE ~ as.character(smoking_status)
              ))
dfe$smoking_status2 <- as.factor(dfe$smoking_status2)
summary(dfe$smoking_status2)


#############################
# SAVING FORMATTED METADATA
############################

setwd("/Users/cristian/Desktop/Rcharger/NIDDK")

#saveRDS(dfp, "NIDDK_participants_metadata_formatted.rds")

#saveRDS(dfe, "NIDDK_endoscopies_metadata_formatted.rds")




# > sessionInfo()
# R version 4.1.1 (2021-08-10)
# Platform: x86_64-apple-darwin17.0 (64-bit)
# Running under: macOS Monterey 12.6
# 
# Matrix products: default
# LAPACK: /Library/Frameworks/R.framework/Versions/4.1/Resources/lib/libRlapack.dylib
# 
# locale:
#         [1] en_CA.UTF-8/en_CA.UTF-8/en_CA.UTF-8/C/en_CA.UTF-8/en_CA.UTF-8
# 
# attached base packages:
#         [1] stats     graphics  grDevices utils     datasets  methods   base     
# 
# other attached packages:
#         [1] visdat_0.5.3    naniar_0.6.1    lubridate_1.8.0 table1_1.4.2    forcats_0.5.2   stringr_1.4.1  
# [7] dplyr_1.0.10    purrr_0.3.4     readr_2.1.2     tidyr_1.2.1     tibble_3.1.8    ggplot2_3.4.0  
# [13] tidyverse_1.3.2
# 
# loaded via a namespace (and not attached):
#         [1] tidyselect_1.1.2    xfun_0.33           haven_2.5.3         gargle_1.2.1       
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

