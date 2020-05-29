# Code for calculating ADC using FY2019 R&B charges ----------------------

#Install and load necessary packages --------------------
#install.packages("readraw_dfl")
#install.packages("writeraw_dfl")
#install.packages("ggplot2")
#install.packages("lubridate")
#install.packages("dplyr")
#install.packages("reshape2")
#install.packages("svDialogs")
#install.packages("stringr")
#install.packages("formattable")
# install.packages("ggpubr")
# install.packages("rprojroot")

# Load libraries ------------------------
library(readxl)
library(writexl)
library(ggplot2)
library(lubridate)
library(dplyr)
library(reshape2)
library(svDialogs)
library(stringr)
library(formattable)
library(scales)
library(reshape2)
library(knitr)
library(rmarkdown)
library(kableExtra)


rm(list = ls())

if (list.files("J://") == "Presidents") {
  user_directory <- "J:/Presidents/HSPI-PM/Operations Analytics and Optimization/Projects/System Operations/COVID Scenario Testing"
} else {
  user_directory <- "J:/deans/Presidents/HSPI-PM/Operations Analytics and Optimization/Projects/System Operations/COVID Scenario Testing"
}

# Import data tables from DataMart ---------------
# Import FY2019 IP discharge data
ip_disch_df_raw <- read_excel(paste0(user_directory, "/IP Discharge Data/IP Disch FY2019 2020-05-13.xlsx"), na = c("", "NA"),
                              col_types = c("text", "text", "text", "text", "date", "date", "date",
                                            "text", "text", "text", "text", "text", "text", "text",
                                            "numeric", "numeric"))

# Import FY2019 IP census room & board charge data
rb_charge_df_raw <- read_excel(paste0(user_directory, "/IP Discharge Data/IP R&B Charges FY2019 2020-05-19.xlsx"), na = c("", "NA"),
                               col_types = c("text", "text", "text", "text", "date", "numeric"))

# Import analysis reference
reference_file <- paste0(user_directory, "/IP Discharge Data/Analysis Reference 2020-05-27.xlsx")
ref_sheets <- excel_sheets(reference_file)

ref_data <- lapply(ref_sheets, function(x) read_excel(reference_file, sheet = x))

ip_site_ref <- ref_data[[1]]
dispo_ref <- ref_data[[2]]
service_line_ref <- ref_data[[3]]
ip_disch_units_ref <- ref_data[[4]]

rb_charge_ipd_ref <- ref_data[[5]]
rb_charge_site_ref <- ref_data[[6]]


# Format inpatient days dataframe ----------------------------------
ip_disch_df <- ip_disch_df_raw

# Create columns with census, admit, and discharge dates
ip_disch_df <- ip_disch_df %>%
  mutate(AdmitDate = date(`Admit Dt Src`),
         DischDate = date(`Dsch Dt Src`),
         AdmitMonth = month(AdmitDate),
         DischMonth = month(DischDate))


# Determine admit source
ip_disch_df <- ip_disch_df %>%
  mutate(AdmitType = ifelse(is.na(`Admit Type Desc Msx`) | str_detect(`Admit Type Desc Msx`, "(INFORMATION NOT AVAILABLE)"), "NonElective",
                            ifelse(str_detect(`Admit Type Desc Msx`, "ELECTIVE"), "Elective",
                                   ifelse(str_detect(`Admit Type Desc Msx`, "NEWBORN"), "Newborn",
                                          ifelse(str_detect(`Admit Type Desc Msx`, "(EMERGENCY)|(URGENT)"), "NonElective", "Other")))))

# Crosswalk raw data with reference data -------------------------
# Crosswalk sites
ip_disch_df <- left_join(ip_disch_df, ip_site_ref, by = c("Facility Msx" = "Facility Msx"))

# Crosswalk service lines to include/exclude
ip_disch_df <- left_join(ip_disch_df, service_line_ref[ , c("Service Desc Msx", "ServiceInclude")], by = c("Service Desc Msx" = "Service Desc Msx"))

# If no discharge unit present, set to "Unknown"
ip_disch_df[is.na(ip_disch_df$`Unit Desc Msx`), "Unit Desc Msx"] <- "Unknown"

# Crosswalk unit to determine if it is an ICU, peds unit, nursery/NICU,
ip_disch_df <- left_join(ip_disch_df, ip_disch_units_ref, by = c("Site" = "Site", "Unit Desc Msx" = "Unit Desc Msx"))



# Inclusion criteria
# Remove any peds encounters
# Remove NICU and newborn discharges
# Include any encounter with an ICU LOS > 0 OR any patient discharged from an adult med/surg unit
ip_disch_subset <- ip_disch_df %>%
  filter(PedsUnit == "No" & NurseryNICU == "No" & AdmitType != "Newborn" & (`Icu Days Msx` > 0 | AdultMedSurg == "Yes"))

# Determine total discharges and ICU ADC for each site based on admit type ----------------------------
# Summarize data for each site based on admit type
ip_disch_admit_type_site_level <- ip_disch_subset %>%
  group_by(Site, AdmitType) %>%
  summarize(VolDisch = n(),
            ICUDays = sum(`Icu Days Msx`)) %>%
  ungroup()

# Summarize data for each site across all admit types
ip_disch_all_admits_site <- ip_disch_subset %>%
  group_by(Site) %>%
  summarize(AdmitType = "All",
            VolDisch = n(),
            ICUDays = sum(`Icu Days Msx`)) %>%
  ungroup()

# Bind data together
ip_disch_site_summary <- rbind(ip_disch_admit_type_site_level, ip_disch_all_admits_site)
ip_disch_site_summary <- ip_disch_site_summary[order(ip_disch_site_summary$Site), ]

# Determine total discharges and ICU ADC for system based on admit type -----------------------------
# Summarize data for each site based on admit type
ip_disch_admit_type_system <- ip_disch_subset %>%
  group_by(AdmitType) %>%
  summarize(Site = "MSHS",
            VolDisch = n(),
            ICUDays = sum(`Icu Days Msx`)) %>%
  ungroup()

ip_disch_admit_type_system <- ip_disch_admit_type_system[ , c("Site", "AdmitType", "VolDisch", "ICUDays")]

# Summarize data for each site across all admit types
ip_disch_all_admits_system <- ip_disch_subset %>%
  summarize(Site = "MSHS",
            AdmitType = "All",
            VolDisch = n(),
            ICUDays = sum(`Icu Days Msx`)) %>%
  ungroup()

# Bind data together
ip_disch_system_summary <- rbind(ip_disch_admit_type_system, ip_disch_all_admits_system)

# Calculate ICU ADC for each site and system
ip_disch_site_summary <- ip_disch_site_summary %>%
  mutate(ICUADC = ICUDays / 365)

ip_disch_system_summary <- ip_disch_system_summary %>%
  mutate(ICUADC = ICUDays / 365)

# Bind site and system level data
ip_disch_summary <- rbind(ip_disch_site_summary, ip_disch_system_summary)

# Melt data frame to calculate percentage of elective and non-elective discharges ----------------------
ip_disch_admit_type_percent <- dcast(ip_disch_summary[ , c("Site", "AdmitType", "VolDisch")],
                                     Site ~ AdmitType, value.var = "VolDisch")

ip_disch_admit_type_percent <- ip_disch_admit_type_percent %>%
  mutate(Elective = Elective / All * 100, NonElective = NonElective / All * 100, All = All / All * 100)

ip_disch_admit_type_percent <- melt(ip_disch_admit_type_percent, id.vars = "Site", variable.name = "AdmitType", value.name = "DischPerc")

ip_disch_admit_type_percent$AdmitType <- as.character(ip_disch_admit_type_percent$AdmitType)

# Bind with summary stats table
ip_disch_summary <- left_join(ip_disch_summary, ip_disch_admit_type_percent, by = c("Site" = "Site", "AdmitType" = "AdmitType"))

ip_disch_summary <- ip_disch_summary[ , c("Site", "AdmitType",
                                          "VolDisch", "DischPerc",
                                          "ICUDays", "ICUADC")]

# Calculate daily census using room and board charges ----------------
rb_charge_df <- rb_charge_df_raw

# Crosswalk sites and unit reference data
rb_charge_df <- left_join(rb_charge_df, rb_charge_site_ref, by = c("Hospital" = "Hospital"))

rb_charge_df <- left_join(rb_charge_df, rb_charge_ipd_ref, by = c("Site" = "Site", "IPD" = "IPD"))

# Create columns for census date and census year
rb_charge_df <- rb_charge_df %>%
  mutate(CensusDate = date(`Service Date`), 
         CensusYear = year(CensusDate))

# Crosswalk admission type from IP discharge data
ip_enc_admit_type <- ip_disch_df %>%
  select(`Encounter No`, `Admit Type Desc Msx`, AdmitType)


rb_charge_df <- left_join(rb_charge_df, ip_enc_admit_type, by = c("Encounter Number" = "Encounter No"))

# Assume encounters with missing admit types are non-elective
rb_charge_df[is.na(rb_charge_df$AdmitType), "AdmitType"] <- "Unknown"

adult_med_surg_df <- rb_charge_df %>%
  filter(AdultMedSurg == "Yes" & CensusYear == 2019)

adult_med_surg_adc <- adult_med_surg_df %>%
  group_by(Site, AdmitType) %>%
  summarize(TotalDays = n(), ICUDays = sum(ICU == "Yes"),
            ADC = TotalDays / 365, ICUADC = ICUDays / 365)

adult_med_surg_enc_df <- rb_charge_df %>%
  filter(AdultMedSurg == "Yes") %>%
  group_by(`Encounter Number`, Msmrn, Site, AdmitType) %>%
  summarize(StartDate = min(CensusDate), EndDate = max(CensusDate),
            StartYr = year(StartDate), EndYr = year(EndDate),
            LOS = n(), ICULOS = sum(ICU == "Yes"))

adult_med_surg_enc_summary <- adult_med_surg_enc_df %>%
  filter(EndYr == 2019) %>%
  group_by(Site, AdmitType) %>%
  summarize(Encounters = n(), TotalDays = sum(LOS), ICUDays = sum(ICULOS),
            ADC = TotalDays / 365, ICUADC = ICUDays / 365)



adult_med_surg_enc_df_2 <- rb_charge_df %>%
  filter(AdultMedSurg == "Yes" & CensusYear == 2019) %>%
  group_by(`Encounter Number`, Msmrn, Site, AdmitType) %>%
  summarize(StartDate = min(CensusDate), EndDate = max(CensusDate),
            StartYr = year(StartDate), EndYr = year(EndDate),
            LOS = n(), ICULOS = sum(ICU == "Yes"))

adult_med_surg_enc_summary_2 <- adult_med_surg_enc_df_2 %>%
  # filter(EndYr == 2019) %>%
  group_by(Site, AdmitType) %>%
  summarize(Encounters = n(), TotalDays = sum(LOS), ICUDays = sum(ICULOS),
            ADC = TotalDays / 365, ICUADC = ICUDays / 365)

adult_med_surg_enc_summary_3 <- adult_med_surg_enc_df_2 %>%
  # filter(EndYr == 2019) %>%
  group_by(Site) %>%
  summarize(Encounters = n(), TotalDays = sum(LOS), ICUDays = sum(ICULOS),
            ADC = TotalDays / 365, ICUADC = ICUDays / 365)



# # Convert room and board to 1 line per patient
# rb_enc_level <- rb_charge_df %>%
#   group_by(`Encounter Number`, Msmrn) %>%
#   summarize(StartDate = min(CensusDate), EndDate = max(CensusDate),
#             StartYr = min(CensusYear), EndYr = max(CensusYear),
#             LOS = n(), ICULOS = sum(ICU == "Yes"), AMSLOS = sum(AdultMedSurg == "Yes"))
# 
# # Subset IP disch data for crosswalk
# ip_disch_crosswalk_subset <- ip_disch_df %>%
#   select(`Encounter No`, Site, 
#          `Admit Type Desc Msx`, AdmitType,
#          `Service Desc Msx`, `Unit Desc Msx`, 
#          AdmitDate, DischDate, 
#          `Icu Days Msx`, `Los No Src`,
#          ICU, AdultMedSurg, PedsUnit, NurseryNICU, Notes)
# 
# # Crosswalk R&B charges and IP data
# crosswalk_df <- left_join(rb_enc_level, ip_disch_crosswalk_subset, by = c("Encounter Number" = "Encounter No"))
# 
# crosswalk_df <- crosswalk_df %>%
#   mutate(AdmitYr = year(AdmitDate),
#          SameLOS = LOS == `Los No Src`,
#          SameICULOS = ICULOS == `Icu Days Msx`)
# 
# crosswalk_df <- crosswalk_df %>%
#   filter(StartYr == 2019 & EndYr == 2019 & AdmitYr == 2019)
# 
# crosswalk_summary_2 <- crosswalk_df %>%
#   filter(AdultMedSurg == "Yes") %>%
#   group_by(Site) %>%
#   summarize(SameLOS = sum(SameLOS), SameICULOS = sum(SameICULOS), Total = n(),
#             PercentSameLOS = SameLOS / Total, PercentSameICULOS = SameICULOS / Total,
#             ICUDaysRB = sum(ICULOS), ICUDaysIP = sum(`Icu Days Msx`),
#             ICUADCRB = ICUDaysRB / 365, ICUADCIP = ICUDaysIP / 365)
# 
# msh_ip_enc_w_icu <- ip_disch_subset %>%
#   filter(Site == "MSH" & `Icu Days Msx` > 0)
# 
# # Convert room and board to 1 line per patient
# rb_enc_level_2 <- rb_charge_df %>%
#   filter(AdultMedSurg == "Yes") %>%
#   group_by(`Encounter Number`, Msmrn, Site) %>%
#   summarize(StartDate = min(CensusDate), EndDate = max(CensusDate),
#             StartYr = min(CensusYear), EndYr = max(CensusYear),
#             LOS = n(), ICULOS = sum(ICU == "Yes"), AMSLOS = sum(AdultMedSurg == "Yes"))
# 
# msh_rb_enc_w_icu <- rb_enc_level_2 %>%
#   filter(Site == "MSH" & ICULOS > 0 & StartYr == 2019 & EndYr == 2019)
# 
# output_list <- list("IP" = msh_ip_enc_w_icu, "RB" = msh_rb_enc_w_icu)
# 
# write_xlsx(output_list, "Compare MSH ICU ADC.xlsx")


