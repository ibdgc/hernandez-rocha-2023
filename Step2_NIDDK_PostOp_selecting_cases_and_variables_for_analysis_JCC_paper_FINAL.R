library(tidyverse)
library(table1)
library(lubridate)
library(naniar)
library(visdat)

rm(list=ls())
set.seed(1)

##################
# UPLOADING FILES
##################


setwd("/Users/cristian/Desktop/Rcharger/NIDDK")

dfp <- as.data.frame(readRDS("NIDDK_participants_metadata_formatted.rds"))

dfe <- as.data.frame(readRDS("NIDDK_endoscopies_metadata_formatted.rds"))

############################
# REMOVING INELIGIBLE CASES
############################

# Removing patients from Emory

# 8 cases from Emory (only 3 have postoperative endoscopy with incomplete data) 

summary(dfp$grc)
summary(dfe$grc)


dfp <- dfp %>% filter(grc != "Emory University")

# Excluding 2 cases with no first endoscopy but whom still can be performed (surgery less than 18 months ago)
# C924315-907486 (2021-06-07) & C912522-864383 (2021-03-29)

dfp <- dfp %>% filter(consortium_id != "C924315-907486" & consortium_id != "C912522-864383")

dfp$index_date <- as.Date(dfp$index_date, format="%Y-%m-%d")
summary(dfp$index_date)


dfp$recruit_date <- as.Date(dfp$recruit_date, format="%Y-%m-%d")
summary(dfp$recruit_date)

######################################################################
# REMOVING PATIENTS THAT DONT MEET THE INCLUSION CRITERIA AT SURGERY
######################################################################

# This is the staring number in the paper (553 screened participants)
nrow(dfp)
# [1] 553

# Patients must have undergone a terminal ileal, ileocecal, or ileocolonic resection (12 cases).

# C812455-919775 (MSH): No TI resection (Index surgery was a resection of mid-jejunum. Patient has never had (before or after index surgery) ileal resection. Last colonoscopy 2018 showed no ileal disease)
# C823283-837985 (Pitt): No TI resection (no evidence for Crohn's disease on 3/17/2017 [index surgery] exploratory laparoscopy, So no bowel resection was done.
# C814526-884440 (JHU): No TI resection (Jejunal resection)
# C927420-833238 (ISMMS): No TI resection (Patient had a proximal ileal + small bowel resection)
# C808768-893090 (ISMMS): No TI resection (Patient initially thought to have had ileal resection surgery but operative and pathology reports reveal that patient only had sigmoid colon resection surgery)
# C824026-935544 (ISMMS): No TI resection (Patient only had small bowel resection. Terminal Ileum and the Ileocolic region were uninvolved and therefore not resected)
# C814974-997802 (ISMMS): No TI resection (Extended hemicolectomy; Patient has not had a ileocolic resection around date of recruitment in 2015)
# C818235-902584 (Pitt): No TI resection (Subject did not undergo a surgical resection, only a hernia repair. No evidence for Crohn's disease on 8/1/2014 diagnostic laparoscopy, so no bowel resection was done)
# C826063-854543 (ISMMS): No TI resection (The surgery was canceled. No longer eligible for the study)
# C830612-832458 (ISMMS): No TI resection (more proximal resection, 4 feet of normal distal ileum are described)
# C831299-957739 (ISMMS): No TI resection (more proximal resection, normal TI is described)
# C827937-932707 (ISMMS): No TI resection (Midileum resection. 120 cm from the ICV)


No_TI_resection <- c("C812455-919775",  "C823283-837985", "C814526-884440", "C927420-833238", "C808768-893090",
                  "C824026-935544", "C814974-997802", "C818235-902584", "C826063-854543", "C830612-832458",
                  "C831299-957739", "C827937-932707")
                  
intersect(dfp$consortium_id, No_TI_resection)

dfp2 <- dfp[!(dfp$consortium_id %in% No_TI_resection), ]


# Patients with ileal resection without ICV resection (7 cases)
# C803992-991865 (JHU): Not an ileocolonic resection. Ileal resection 10 cm proximal to ICV
# C830162-879917 (JHU): ileal resection, no ileocolic resection
# C806160-870807 (ISMMS): Normal 20 cm of TI and ICV. Not resected
# C807973-963012 (JHU): No ICV resection
# C888166-843764 (JHU): No ICV resection
# C818941-838649 (JHU): Distal ileum resection without ICV removal
# C821250-832004 (ISMMS): Distal ileum resection without ICV removal

No_ileocolic_resection <- c("C803992-991865", "C830162-879917", "C806160-870807",
                            "C807973-963012", "C888166-843764", "C818941-838649", "C821250-832004")


intersect(dfp2$consortium_id, No_ileocolic_resection)

dfp2 <- dfp2[!(dfp2$consortium_id %in% No_ileocolic_resection), ]


# Patients must have a definitive diagnosis of Crohn’s disease involving the terminal ileum,
# confirmed based on surgical pathology (10 cases)

# C805004-828110 (UdeM): Ileocolic resection but no ileal involvement (only colonic involvement)
# C814795-993385 (UdeM): Ileocolic resection but no ileal involvement (only the cecum and ascending colon showed strictures)
# C824533-832693 (UdeM): Ileocolic resection but no ileal involvement (only right colon involvement is reported)
# C819718-908489 (MSH): Ileocolic resection but no ileal involvement (only colonic involvement is reported)
# C833272-987132 (ISMMS): Ileocolic resection but no ileal involvement (only colonic involvement is reported)
# C805923-999645 (CSMC): Ileocolic resection but no ileal involvement (only colonic involvement is reported)
# C813115-913290 (ISMMS): Ileocolic resection but no ileal involvement (only colonic involvement is reported)
# C801341-896726 (CSMC): Ileocolic resection but no ileal involvement (no evidence of CD but prior history)
# C832281-876528 (ISMMS): Ileocolic resection but no ileal involvement (neuroendocrine tumor involving the cecum, minimal ileal disease)
# C817565-969643 (ISMMS): Ileocolic resection but no ileal involvement (inflammation secondary to pericholic abscess)

No_ileal_involvement <- c("C805004-828110",  "C814795-993385", "C824533-832693",  "C819718-908489", "C833272-987132", 
              "C805923-999645", "C813115-913290", "C801341-896726", "C832281-876528", "C817565-969643")

intersect(dfp2$consortium_id, No_ileal_involvement)

dfp2 <- dfp2[!(dfp2$consortium_id %in% No_ileal_involvement), ]


# Patient has a sub-total or near sub-total colonic resection (i.e., beyond ileal-descending anastomosis)
# 2 cases

# C835449-957942 & C809363-939551 (this case also had cancer at anastomosis) had ileo-sigmoid anastomosis
# C827803-930850 (this was found later)

Subtotal_colectomy <- c("C970259-852365", "C809363-939551")

intersect(dfp2$consortium_id, Subtotal_colectomy)

dfp2 <- dfp2[!(dfp2$consortium_id %in% Subtotal_colectomy), ]


# Patient undergoes resection with diverting ileostomy (15 cases)

# C800428-803197 (UdeM) Not included
# C838325-845755 (UdeM)
# C826916-967391 (CSMC): no first
# C817498-868118 (ISMMS): no first
# C860067-949716 (ISMMS) : no first
# C810350-912197 (JHU) : no first
# C821475-991229 (MSH) : no first
# C827736-912359 (CSMC) Not included
# C804454-873658 (ISMMS): Not included
# C808269-989349 (ISMMS): Nlt included
# C819764-909997 (ISMMS) Not included
# C809665-877471 (ISMMS) : Not included
# C839699-832803 (ISMMS):
# C932256-847557 (JHU): Index surgery was an reversal ileostomy
# C801096-824949 (ISMMS): No TI resection (only ileostomy donuts and ascending colon resected). An anastomosis was created

Ileostomy <- c("C800428-803197", "C838325-845755",  "C826916-967391", "C817498-868118", 
               "C860067-949716", "C810350-912197",  "C821475-991229", "C827736-912359",
               "C804454-873658", "C808269-989349", "C819764-909997", "C809665-877471",
               "C839699-832803", "C932256-847557", "C801096-824949")

intersect(dfp2$consortium_id, Ileostomy)

dfp2 <- dfp2[!(dfp2$consortium_id %in% Ileostomy), ]



# Patients with ileocecal or ileocolonic resection with primary anastomosis due to ileal disease
nrow(dfp2)
# [1] 507

#######################################################
# ELUCIDATING THE REASONS FOR NO FOLLOW UP COLONOSCOPY
#######################################################

# Patients must be followed after surgery by a gastroenterologist at a site where the required biospecimens
# and clinical information can be collected (e.g., Genetic Research Center or collaborating site)
# Removing patients with no follow up colonoscopy (n = 106)


# Selecting patient without postoperative colonoscopy
dfe.first <- dfe %>% filter(redcap_event_name == "First Endoscopy") 

nrow(distinct(dfe.first, consortium_id))

dfp.nofirst <- dfp2[!(dfp2$consortium_id %in% dfe.first$consortium_id),]

dfp.nofirst2 <- dfp.nofirst

dfp.nofirst <- subset(dfp.nofirst, select = c(1,2,59,306:316))

nrow(dfp.nofirst)
# [1] 106

# Patient no longer wishes to particicpate (8 cases)

consent_withdrew <- dfp.nofirst %>% filter(withdraw_reason == "Patient no longer wishes to particicpate") 

nrow(consent_withdrew)
# [1] 8

dfp2 <- dfp2[!(dfp2$consortium_id %in% consent_withdrew$consortium_id),]


# Endoscopist has not agreed to follow protocol (14 cases)

endoscopist_withdrew <- dfp.nofirst %>% filter(withdraw_ineligible == "Endoscopist has not agreed to follow protocol") 

nrow(endoscopist_withdrew)
# [1] 14

dfp2 <- dfp2[!(dfp2$consortium_id %in% endoscopist_withdrew$consortium_id),]


# Patient will not return to GRC (explain below) & Patient moved

no_follow <- dfp.nofirst %>% filter(withdraw_reason == "Patient will not return to GRC (explain below)" |
                                            withdraw_reason == "Patient moved" |
                                            withdraw_other == "lost to follow up" | withdraw_reason == "No samples obtained at first endoscopy" |
                                            withdraw_reason == "First endoscopy performed off-site" |
                                            withdraw_ineligible == "18 months have passed since patient's index surgery and no biopsies have been collected at endoscopy" |
                                            withdraw_ineligible == "Migrated from local project. 18 months have passed since patient's index surgery and no biopsies have been collected at endoscopy" |
                                            withdraw_ineligible == "despite repeated attempts by 3 different nurses pre and post surgery, we could not obtain any blood specimen from this subject. History of numerous previous surgeries") 

nrow(no_follow)
# [1] 57

dfp2 <- dfp2[!(dfp2$consortium_id %in% no_follow$consortium_id),]



# No reason (Other reasons)
summary(dfp.nofirst$withdraw_ineligible)

no_reason <- dfp.nofirst %>% filter(is.na(withdraw_reason))

nrow(no_reason)
# [1] 27

summary(no_reason$grc)

no_reason2 <- dfp[(dfp$consortium_id %in% no_reason$consortium_id),]

no_reason2 <- mutate(no_reason2,
                    surgical_findings_uncheck = case_when(
                            surgical_findings1 == "Checked" |  surgical_findings2 == "Checked" |
                                    surgical_findings3 == "Checked" ~ "Checked",
                            TRUE ~ NA_character_
                    ))
no_reason2$surgical_findings_uncheck <- as.factor(no_reason2$surgical_findings_uncheck)
summary(no_reason2$surgical_findings_uncheck)


dfp2 <- dfp2[!(dfp2$consortium_id %in% no_reason2$consortium_id),]

# Patients with ileocecal or ileocolonic resection with primary anastomosis due to ileal disease
# and at least one postoperative colonoscopy

nrow(dfp2)
# [1] 401


#######################################################
# LOOKING FOR PATIENTS WHO STILL HAVE WITHDRAWAL NOTES
#######################################################

nrow(dfp2)

table1(~  withdraw_reason, data= dfp2,
       topclass="Rtable1-grid Rtable1-shade Rtable1-times", overall = T)

table1(~  withdraw_not_returning, data= dfp2,
       topclass="Rtable1-grid Rtable1-shade Rtable1-times", overall = T)

table1(~  withdraw_ineligible, data= dfp2,
       topclass="Rtable1-grid Rtable1-shade Rtable1-times", overall = T)

table1(~  withdraw_other, data= dfp2,
       topclass="Rtable1-grid Rtable1-shade Rtable1-times", overall = T)


still_withdraw <- dfp2 %>% filter(!is.na(withdraw_reason))

still_withdraw <- subset(still_withdraw, select = c(1,2,59,306:316))




############################################
# MERGING PARTICIPANTS AND ENDOSCOPIES DATA
############################################

# Deleting the following endoscopies that were not performed, but could not be deleted from RedCap:
# C817861-969543 (Second_Endoscopy): no endoscopy but stool sample data
# C836325-813884 (Second_Endoscopy): performed off-site, but no data about this colonoscopy
# C819807-851526 (Second_Endoscopy): Endoscopy not performed
# C835328-831565 (Third_Endoscopy): Endoscopy not performed
# C802379-911779 (Second_Endoscopy): Endoscopy not performed
# C835617-979161 (Fourth_Endoscopy): Endoscopy not performed
# C829234-910143 (Third_Endoscopy): Endoscopy not performed
# C838512-947457 (Third_Endoscopy): Endoscopy not performed

# Deleting the following events that don't meet inclusion criteria
# C805476-850345 (Second_Endoscopy): Endoscopy not performed (this is a capsule endoscopy that could be i4)
# C817565-969643 (Second_Endoscopy): This was a therapeutic endoscopy performed to close fistula 2 months after the first one
# C973487-831632: first endoscopy already had non-passable stricture (second performed 1 month later was dilatation).
# The third endo of this case was also removed

# These were included by mistake; therefore, they are not mentioned in the paper.

dfe2 <- dfe %>% filter(!(redcap_event_name == "Second Endoscopy" & consortium_id == "C817861-969543")) 
dfe2 <- dfe2 %>% filter(!(redcap_event_name == "Second Endoscopy" & consortium_id == "C836325-813884")) 
dfe2 <- dfe2 %>% filter(!(redcap_event_name == "Second Endoscopy" & consortium_id == "C819807-851526")) 
dfe2 <- dfe2 %>% filter(!(redcap_event_name == "Third (or Greater) Endoscopies" & consortium_id == "C835328-831565")) 
dfe2 <- dfe2 %>% filter(!(redcap_event_name == "Second Endoscopy" & consortium_id == "C802379-911779")) 
dfe2 <- dfe2 %>% filter(!(redcap_event_name == "Third (or Greater) Endoscopies" & redcap_repeat_instance == "2" & consortium_id == "C835617-979161")) 
dfe2 <- dfe2 %>% filter(!(redcap_event_name == "Third (or Greater) Endoscopies" & consortium_id == "C829234-910143")) 
dfe2 <- dfe2 %>% filter(!(redcap_event_name == "Third (or Greater) Endoscopies" & consortium_id == "C838512-947457")) 

dfe2 <- dfe2 %>% filter(!(redcap_event_name == "Second Endoscopy" & consortium_id == "C805476-850345")) 
dfe2 <- dfe2 %>% filter(!(redcap_event_name == "Second Endoscopy" & consortium_id == "C817565-969643")) 
dfe2 <- dfe2 %>% filter(!(redcap_event_name == "Second Endoscopy" & consortium_id == "C973487-831632")) 
dfe2 <- dfe2 %>% filter(!(redcap_event_name == "Third (or Greater) Endoscopies" & consortium_id == "C973487-831632")) 


##################################
# PREPARING COLONOSCOPY NUMBERS
##################################

summary(dfe2$redcap_event_name)
dfe2$redcap_event_name <- gsub("First Endoscopy", "First_Endoscopy", dfe2$redcap_event_name)
dfe2$redcap_event_name <- gsub("Second Endoscopy", "Second_Endoscopy", dfe2$redcap_event_name)
dfe2$redcap_event_name <- as.factor(dfe2$redcap_event_name)
summary(dfe2$redcap_event_name)


summary(dfe2$redcap_event_name)
summary(dfe2$redcap_repeat_instance)

dfe2 <- mutate(dfe2,
                   redcap_event_name22 = case_when(
                           redcap_event_name == "Third (or Greater) Endoscopies" & redcap_repeat_instance == "1" ~ "Third_Endoscopy",
                           redcap_event_name == "Third (or Greater) Endoscopies" & redcap_repeat_instance == "2" ~ "Fourth_Endoscopy",
                           redcap_event_name == "Third (or Greater) Endoscopies" & redcap_repeat_instance == "3" ~ "Fifth_Endoscopy",
                           redcap_event_name == "Third (or Greater) Endoscopies" & redcap_repeat_instance == "4" ~ "Sixth_Endoscopy",
                           redcap_event_name == "Third (or Greater) Endoscopies" & redcap_repeat_instance == "5" ~ "Seventh_Endoscopy",
                           redcap_event_name == "Third (or Greater) Endoscopies" & redcap_repeat_instance == "6" ~ "Eighth_Endoscopy",
                           redcap_event_name == "Third (or Greater) Endoscopies" & redcap_repeat_instance == "7" ~ "Ninth_Endoscopy",
                           TRUE ~ as.character(redcap_event_name))
)
dfe2$redcap_event_name22 <- factor(dfe2$redcap_event_name22,
                                      levels = c("First_Endoscopy", "Second_Endoscopy", "Third_Endoscopy",
                                                 "Fourth_Endoscopy", "Fifth_Endoscopy", "Sixth_Endoscopy",
                                                 "Seventh_Endoscopy", "Eighth_Endoscopy", "Ninth_Endoscopy"))
summary(dfe2$redcap_event_name22)


nrow(dfe2)


############################################
# MERGING PARTICIPANTS AND ENDOSCOPIES DATA
############################################

# Matching participants (already filtered) and endoscopies file

dfe2 <- dfe2[(dfe2$consortium_id %in% dfp2$consortium_id),]

setdiff(dfp2$consortium_id, dfe2$consortium_id)


# Merging dataframes

dfp2 <- subset(dfp2, select = c(-grc))

dfep <- dfe2 %>% left_join(dfp2, by = c("consortium_id")) 

nrow(distinct(dfep, consortium_id))

setdiff(dfep$consortium_id, dfp$consortium_id)


#####################################################
# CREATING INTERVALS BETWEEN SURGERY AND ENDOSCOPIES
#####################################################

dfep$index_date <- as.Date(dfep$index_date, format="%Y-%m-%d")

summary(dfep$index_date) 

summary(dfep$endoscopy_date) 

dfep$inter_surg_endo <- interval(dfep$index_date, dfep$endoscopy_date) %/% months(1) 

summary(dfep$inter_surg_endo)


# Looking for cases with first endoscopy performed more than 18 months after surgery (23 cases)

dfep <- mutate(dfep,
               more18mos = case_when(
                       redcap_event_name == "First_Endoscopy" & inter_surg_endo > 18 ~ "Yes",
                       TRUE ~ as.character("No")
               ))
dfep$more18mos <- as.factor(dfep$more18mos)
summary(dfep$more18mos)

df.more18mos <- dfep %>% filter(more18mos == "Yes")

nrow(distinct(df.more18mos, consortium_id))
# [1] 23

dfep2 <- dfep[!(dfep$consortium_id %in% df.more18mos$consortium_id),]


# Eligible patients (n = 378 with 711 colonoscopies)
nrow(distinct(dfep2, consortium_id))
# [1] 378
nrow(dfep2)
# [1] 710


##########################################
# EXCLUDING ENDOSCOPIES WITHOUT RUTGEERTS
##########################################

nrow(dfep2)

summary(dfep2$Rutgeerts_bin)

no_rutgeerts <- dfep2 %>% filter(is.na(Rutgeerts_bin) | Rutgeerts_bin == "Unknown")

no_rutgeerts <- subset(no_rutgeerts, select = c(1:16))

nrow(no_rutgeerts)
# [1] 25

nrow(distinct(no_rutgeerts, consortium_id))

dfep3 <- dfep2 %>% filter(!(is.na(Rutgeerts_bin) | Rutgeerts_bin == "Unknown"))


########################################
# REMOVING CASES WITH SKIPPED FOLLOW UP
#######################################

# Colonoscopies with prior unknown data (11 colonoscopies)

dfep3 <- droplevels(dfep3)

dfep3 <- dfep3 %>% group_by(consortium_id) %>% mutate(number_endo = n())

summary(dfep3$number_endo)

nrow(distinct(dfep3, consortium_id))


#dfe3 <- dfe3 %>% group_by(consortium_id, Rutgeerts_bin) %>% mutate(count2 = n())

dfep3 <- dfep3 %>% group_by(consortium_id) %>% mutate(sustained_rem = sum(Rutgeerts_bin == "i0_i1"))


length(which(dfep3$number_endo == 1 & dfep3$redcap_event_name22 == "Second_Endoscopy"))

length(which(dfep3$number_endo == 2 & dfep3$redcap_event_name22 == "Third_Endoscopy"))

length(which(dfep3$number_endo == 3 & dfep3$redcap_event_name22 == "Fourth_Endoscopy"))

length(which(dfep3$number_endo == 4 & dfep3$redcap_event_name22 == "Fifth_Endoscopy"))

length(which(dfep3$number_endo == 5 & dfep3$redcap_event_name22 == "Sixth_Endoscopy"))

length(which(dfep3$number_endo == 6 & dfep3$redcap_event_name22 == "Seventh_Endoscopy"))

length(which(dfep3$number_endo == 7 & dfep3$redcap_event_name22 == "Eighth_Endoscopy"))

length(which(dfep3$number_endo == 8 & dfep3$redcap_event_name22 == "Ninth_Endoscopy"))



# There are 4 cases with unknown Rutgeerts at first endoscopy who have a second colonoscopy with Rutgeerts
# C815449-977146 (JHU): Anastomosis not reached due to difficulty and significant colonic inflammation (second deleted)
# C813436-943619 (MSH): Anastomotic stricture with ileum not seen at first (second deleted)
# C806717-823475 (MSH):  Anastomotic stricture with ileum not seen at first (second deleted)
# C825159-900615 (JHU): Anastomotic stricture with ileum not seen at first (second deleted)

dfep3 <- dfep3 %>% filter(consortium_id != "C815449-977146")
dfep3 <- dfep3 %>% filter(!(consortium_id == "C813436-943619" & redcap_event_name22 == "Second_Endoscopy"))
dfep3 <- dfep3 %>% filter(consortium_id != "C806717-823475")
dfep3 <- dfep3 %>% filter(consortium_id != "C825159-900615")

# There are 4 cases with unknown Rutgeerts at second endoscopy who have a third colonoscopy with Rutgeerts
# C802294-845465 (Pitt): TI with multiple moderately deep ulcers. Number not reported (third deleted)
# C804737-920194 (JHU): Unclear description (third deleted)
# C804822-877640 (CSMC): Anastomosis not reached. (third deleted)
# C813436-943619 (MSH): Anastomotic strictute with ileum not seen at first (third deleted)

dfep3 <- dfep3 %>% filter(!(number_endo == 2 & redcap_event_name22 == "Third_Endoscopy"))

# There is 1 case with unknown Rutgeerts at fourth endoscopy who have 3 subsequent colonoscopies with Rutgeerts
# C809463-851736 (MSH): Anastomosis not reached (fourth and subsequent endos deleted)

dfep3 <- dfep3 %>% filter(!(consortium_id == "C809463-851736" & redcap_event_name22 == "Fifth_Endoscopy"))

dfep3 <- dfep3 %>% filter(!(consortium_id == "C809463-851736" & redcap_event_name22 == "Sixth_Endoscopy"))

dfep3 <- dfep3 %>% filter(!(consortium_id == "C809463-851736" & redcap_event_name22 == "Seventh_Endoscopy"))


##########################################################################
# THESE ARE THE ANALYZED PARTICIPANTS (n = 365) & COLONOSCOPIES (n = 674)
##########################################################################

nrow(distinct(dfep3, consortium_id))
# [1] 365

nrow(dfep3)
# [1] 674

summary(dfep3$redcap_event_name)
summary(dfep3$redcap_event_name22)



#################################
# SELECTING VARIABLES TO ANALYZE
################################


dfep.selected <- subset(dfep3, select = c("consortium_id", "grc", "grc.a", "sex", "index_date", "age_surg",
                                          "race", "race_bin", "hispanic", "jewish",
                                         "smoking_presurg", "smoking_presurg_bin",
                                         "age_diag", "age_montreal", "duration", "country",
                                         "montreal_loc2", "dis_behavior", "dis_behavior_bin", "disloc_peri",
                                         "surg_smbow2", "num_surg",
                                         "surg_findings", "surg_findings22",
                                         "other_sbow_res", "other_stric_plasty", "other_lbow_res",  
                                         "anastomosis_type", "sewn_stapled2", "resection_length",
                                         "appendix", "margin", "granulomas",
                                         "immuno", "immuno_1yr2", "immuno_3mos2",
                                         "mtx", "mtx_1yr2", "mtx_3mos2",
                                         
                                         "infliximab", "infliximab_1yr2", 
                                         "infliximab_induction2", "infliximab_maxdose2", "infliximab_interval2",
                                         "infliximab_combo2", 
                                         "tdm_infliximab_prior2", "infliximab_assay2", 
                                         "infliximab_trough_level2", "infliximab_drug_level2", 
                                         "infliximab_antibody2",
                                         
                                         "infliximab_3mos2",
                                         "infliximab_start2", "infliximab_stop2", "infliximab_dose2",
                                         
                                         "adalimumab", "adalimumab_1yr2",
                                         "adalimumab_induction2", "adalimumab_dose_1yr2","adalimumab_freq_1yr2", 
                                         "adalimumab_combo2", 
                                         "tdm_adalimumab_prior2", "adalimumab_assay2", 
                                         "adalimumab_trough_level2", "adalimumab_drug_level2", "adalimumab_antibody2",
                                         
                                         "adalimumab_3mos2",
                                         "adalimumab_start2", "adalimumab_stop2", "adalimumab_dose2",
                                         
                                         "certolizumab", "certolizumab_1yr2",
                                         "certolizumab_induction2", "certolizumab_dose_1yr2", "certolizumab_freq_1yr2", 
                                         "certolizumab_combo2", 
                                         "tdm_certolizumab_prior2", "certolizumab_assay2", 
                                         "certolizumab_trough_level2", "certolizumab_drug_level2", "certolizumab_antibody2",
                                         
                                         "certolizumab_3mos2",
                                         "certolizumab_start2", "certolizumab_stop2", "certolizumab_dose2",
                                         
                                         "tnf_any", "tnf_any_1yr", "tnf_any_3mos",
                                         
                                         "vedolizumab", "vedolizumab_1yr2",
                                         "vedolizumab_induction2", "vedolizumab_freq_1yr2", "vedolizumab_combo2", 
                                         "tdm_vedolizumab_prior2", "vedolizumab_assay2", 
                                         "vedolizumab_drug_level2", "vedolizumab_antibody2", 
                                         
                                         "vedolizumab_3mos2",
                                         "vedolizumab_start2", "vedolizumab_stop2", "vedolizumab_dose2",
                                         
                                         "ustekinumab", "ustekinumab_1yr2",
                                         "ustekinumab_induction2", "ustekinumab_freq_1yr2", "ustekinumab_combo2",
                                         "tdm_ustekinumab_prior2", "ustekinumab_assay2",
                                         "ustekinumab_trough_level2", "ustekinumab_drug_level2", "ustekinumab_antibody2", 
                                         
                                         "ustekinumab_3mos2",
                                         "ustekinumab_start2", "ustekinumab_stop2", "ustekinumab_dose2",
                                         
                                         "inter_surg_endo", "redcap_event_name", "redcap_repeat_instance",
                                         "redcap_event_name22", 
                                         "endoscopy_date", "number_endo", "sustained_rem",
                                         "Rutgeerts", "Modified_Rutgeerts", 
                                         "Rutgeerts_bin", "Modified_Rutgeerts_bin",
                                         "Rutgeerts_bin22", "Rutgeerts_bin3",
                                         "anyster_since_last", "anyster_current_upd",
                                         "antibio_since_last", "antibio_current_upd2",       
                                         "immuno_since_last", "immuno_current_upd2",
                                         "mtx_since_last", "mtx_current_upd2",
                                         "infliximab_since_last", "infliximab_current_upd2",
                                         "adalimumab_since_last", "adalimumab_current_upd2",
                                         "certolizumab_since_last", "certolizumab_current_upd2",
                                         "tnf_any_since_last", "tnf_any_current_upd",
                                         "combo_since_last", "combo_current_upd",
                                         "no_meds_since_last", "no_meds_current_upd",
                                         "vedolizumab_since_last", "vedolizumab_current_upd2",
                                         "ustekinumab_since_last", "ustekinumab_current_upd2",
                                         "smoking_status2"))

# Removing suffixes added during the processing of the variables
colnames(dfep.selected) <- str_replace(colnames(dfep.selected), coll("2"),"")
colnames(dfep.selected) <- str_replace(colnames(dfep.selected), coll(".x"),"")


vis_miss(dfep.selected)

vis_expect(dfep.selected,  ~.x %in% "Unknown", show_perc = T)


nrow(dfep.selected)
nrow(distinct(dfep.selected, consortium_id))


setwd("/Users/cristian/Desktop/Rcharger/NIDDK")

#saveRDS(dfep.selected, "NIDDK_merged_data.rds")



###########################################
# ANALYZING DIFFERENCES BERTWEEN PATIENTS 
# WITH AND WITHOUT FOLLOW-UP
##########################################


dfp.nofirst2$followup <- "No"

dfp.nofirst2 <- subset(dfp.nofirst2, select = c(-grc))

dfp2$followup <- "Yes"

setdiff(dfp2$consortium_id, dfep.selected$consortium_id)

dfp3 <- subset(dfp2, dfp2$consortium_id %in% dfep.selected$consortium_id)

dfp.all <- rbind(dfp3, dfp.nofirst2)


dfp.all <- dfp.all %>%
  mutate(across(where(is.factor), ~na_if(., "Unknown")))

dfp.all <- droplevels(dfp.all)


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


summary(dfp.all$surg_findings)
dfp.all <- mutate(dfp.all,
                  penetrating_compl = case_when(
                    surg_findings == "Penetrating" ~ "Yes",
                    surg_findings == "Only_Stricturing" | surg_findings == "Others" ~ "No",
                    TRUE ~ NA_character_)
)
dfp.all$penetrating_compl <- as.factor(dfp.all$penetrating_compl)
summary(dfp.all$penetrating_compl)


table1(~  age_surg +  sex + race_bin + 
         age_diag + surg_smbow + resection_length + penetrating_compl +
         dis_behavior_bin + margin + duration + disloc_peri + anastomosis_type + 
         tnf_any + ustekinumab + vedolizumab + immuno + mtx | followup, data= dfp.all,
       topclass="Rtable1-grid Rtable1-shade Rtable1-times", overall = F, extra.col = list("P-value" = pvalue),
       render.continuous = c(.="median [Q1, Q3]"))



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


