
# Code for crosswalking FY2019 IP discharges and R&B charges ----------------------

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

site_ref <- ref_data[[1]]
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
ip_disch_df <- left_join(ip_disch_df, site_ref, by = c("Facility Msx" = "Facility Msx"))

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


rb_charge_df <- rb_charge_df_raw

sites_units <- unique(rb_charge_df[ , c("Hospital", "IPD")])

rb_charge_enc_disch_unit <- rb_charge_df %>%
  group_by(`Encounter Number`, Msmrn) %>%
  summarize(LastRBDate = date(max(`Service Date`)),
            RBDischDate = LastRBDate + 1, 
            DischUnit = IPD[which.max(`Service Date`)])


ip_disch_enc_summary <- ip_disch_df %>%
  select(`Encounter No`, Msmrn, Site, DischDate, `Unit Desc Msx`)

crosswalk_ip_disch_rb_charge <- left_join(ip_disch_enc_summary, rb_charge_enc_disch_unit, 
                                          by = c("Encounter No" = "Encounter Number", "Msmrn" = "Msmrn"))

crosswalk_ip_disch_rb_charge <- crosswalk_ip_disch_rb_charge %>%
  mutate(Test = DischDate == RBDischDate,
         Dupl = duplicated(`Encounter No`))

unit_ipd_mappings_count <- crosswalk_ip_disch_rb_charge %>%
  filter(!is.na(LastRBDate)) %>%
  group_by(Site, `Unit Desc Msx`, DischUnit) %>%
  summarize(Count = n())

# write_xlsx(unit_ipd_mappings_count, "Disch Unit IPD Mappings 2020-05-28.xlsx")

crosswalk_disch_date_summary <- crosswalk_ip_disch_rb_charge %>%
  group_by(Site) %>%
  summarize(SameDischDate = sum(Test, na.rm = TRUE), DiffDischDate = sum(!Test, na.rm = TRUE), NoMatch = sum(is.na(Test)))

# Determine encounters not in eacch data source
ip_disch_enc <- data.frame("Encounter" = unique(ip_disch_df$`Encounter No`))

rb_charge_enc <- data.frame("Encounter" = unique(rb_charge_df$`Encounter Number`))

ip_disch_not_in_rb_charge <- ip_disch_df %>%
  mutate(EncounterInRB = `Encounter No` %in% rb_charge_enc$Encounter)

ip_disch_not_in_rb_charge_summary <- ip_disch_not_in_rb_charge %>%
  group_by(Site, `Unit Desc Msx`) %>%
  summarize(InRB = sum(EncounterInRB), NotInRB = sum(!EncounterInRB),
            PercentInRB = InRB / (InRB + NotInRB) * 100, PercentNotInRB = NotInRB / (InRB + NotInRB) * 100)

ip_disch_not_in_rb_charge_summary_2 <- ip_disch_not_in_rb_charge %>%
  group_by(Site) %>%
  summarize(InRB = sum(EncounterInRB), NotInRB = sum(!EncounterInRB),
            PercentInRB = InRB / (InRB + NotInRB) * 100, PercentNotInRB = NotInRB / (InRB + NotInRB) * 100)


rb_charge_encounter_summary <- rb_charge_df %>%
  group_by(`Encounter Number`, Msmrn, Hospital) %>%
  summarize(FirstRBDate = date(min(`Service Date`)), LastRBDate = date(max(`Service Date`)), LastRBYear = year(LastRBDate))


rb_charge_encounter_summary <- rb_charge_encounter_summary %>%
  filter(LastRBYear == 2019)

rb_charge_encounter_summary$EncInIPDisch <- rb_charge_encounter_summary$`Encounter Number` %in% ip_disch_enc$Encounter

rb_charge_enc_not_in_ip_disch_summary <- rb_charge_encounter_summary %>%
  group_by(Hospital) %>%
  summarize(InIPDisch = sum(EncInIPDisch), NotInIPDisch = sum(!EncInIPDisch),
            PercentInIPDisch = InIPDisch / (InIPDisch + NotInIPDisch) * 100, 
            PercentNotInIPDisch = NotInIPDisch / (InIPDisch + NotInIPDisch) * 100)
