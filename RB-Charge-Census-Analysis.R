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
ip_disch_2019_df_raw <- read_excel(paste0(user_directory, "/IP Discharge Data/IP Disch FY2019 2020-05-13.xlsx"), na = c("", "NA"),
                              col_types = c("text", "text", "text", "text", "date", "date", "date",
                                            "text", "text", "text", "text", "text", "text", "text",
                                            "numeric", "numeric"))

# Import Jan-Feb 2020 IP discharge data
ip_disch_2020_df_raw <- read_excel(paste0(user_directory, "/IP Discharge Data/IP Disch Jan-Feb20 2020-06-02.xlsx"), na = c("", "NA"),
                                   col_types = c("text", "text", "text", "text", "date", "date", "date",
                                                 "text", "text", "text", "text", "text", "text", "text",
                                                 "numeric", "numeric"))

# Bind 2019 and 2020 IP discharge data
ip_disch_df_raw <- rbind(ip_disch_2019_df_raw, ip_disch_2020_df_raw)
# ip_disch_df_raw <- ip_disch_2019_df_raw

# Import FY2019 IP room & board charge data
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
         DischMonth = month(DischDate),
         DischYear = year(DischDate))

ip_disch_elapsed_days <- as.numeric(max(ip_disch_df$DischDate) - min(ip_disch_df$DischDate) + 1)


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
  mutate(ICUADC = ICUDays / ip_disch_elapsed_days)

ip_disch_system_summary <- ip_disch_system_summary %>%
  mutate(ICUADC = ICUDays / ip_disch_elapsed_days)

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

# Create columns for census date, month, and year. Include days per month and whether or not the
# census date is in a peak flu month (Nov-Mar)
rb_charge_df <- rb_charge_df %>%
  mutate(CensusDate = date(`Service Date`), 
         CensusMonth = month(CensusDate),
         CensusYear = year(CensusDate),
         DaysInMonth = days_in_month(CensusDate),
         FluMonth = ifelse(CensusMonth <= 3 | CensusMonth >= 11, 1, 0))

# Crosswalk admission type from IP discharge data
ip_enc_admit_type <- ip_disch_df %>%
  select(`Encounter No`, `Admit Type Desc Msx`, AdmitType)


rb_charge_df <- left_join(rb_charge_df, ip_enc_admit_type, by = c("Encounter Number" = "Encounter No"))

# Check percent of encounters in R&B charges also present in IP discharge
rb_charge_one_line <- rb_charge_df %>%
  group_by(`Encounter Number`, Msmrn, Hospital, AdmitType) %>%
  summarize(StartDate = min(CensusDate), EndDate = max(CensusDate)) %>%
  mutate(Matched = !is.na(AdmitType))

rb_charge_one_line_summary <- rb_charge_one_line %>%
  group_by(Hospital) %>%
  summarize(MatchedEnc = sum(Matched), NotMatchedEnc = sum(!Matched),
            MatchedPercent = MatchedEnc / (MatchedEnc + NotMatchedEnc) * 100,
            NotMatchedPercent = NotMatchedEnc / (MatchedEnc + NotMatchedEnc) * 100)

rb_charge_one_line_summary_2 <- rb_charge_one_line %>%
  group_by(AdmitType) %>%
  summarize(Count = n()) %>%
  mutate(Percent = Count / sum(Count))

# 98% of encounters across system are matched between R&B and IP discharge data sets  

# Assume encounters with missing admit types are non-elective
rb_charge_df[is.na(rb_charge_df$AdmitType), "AdmitType"] <- "NonElective"

rb_elapsed_days <- as.numeric(max(rb_charge_df$CensusDate) - min(rb_charge_df$CensusDate) + 1)

# Subset R&B charge data to only include adult med surg units and exclude newborn admissions
adult_med_surg_df <- rb_charge_df %>%
  filter(AdultMedSurg == "Yes" & AdmitType != "Newborn")

# Determine ADC for each site based on admit type
adult_med_surg_adc <- adult_med_surg_df %>%
  group_by(Site, AdmitType) %>%
  summarize(TotalDays = n(), ICUDays = sum(ICU == "Yes"),
            ADC = TotalDays / rb_elapsed_days, ICUADC = ICUDays / rb_elapsed_days)

# Determine monthly ADC for each site
adult_med_surg_monthly_adc <- adult_med_surg_df %>%
  group_by(Site, CensusYear, CensusMonth, DaysInMonth, FluMonth) %>%
  summarize(TotalDays = n(), ICUDays = sum(ICU == "Yes")) %>%
  mutate(CensusDate = as.Date(paste0(CensusMonth, "/1/", CensusYear), format = "%m/%d/%Y"),
    ADC = TotalDays / DaysInMonth, ICUADC = ICUDays / DaysInMonth)

adult_med_surg_flu_adc <- adult_med_surg_monthly_adc %>%
  filter(CensusYear == 2019) %>%
  group_by(Site, FluMonth) %>%
  summarize(TotalDays = sum(TotalDays), ICUDays = sum(ICUDays), Days = sum(DaysInMonth)) %>%
  mutate(ADC = TotalDays / Days, ICUADC = ICUDays / Days)

adult_med_surg_flu_melt <- melt(adult_med_surg_flu_adc, id.vars = c("Site", "FluMonth"),
                                measure.vars = c("ADC", "ICUADC"))

adc_labels <- c(ADC = "Adult Med Surg ADC", ICUADC = "ICU ADC")

ggplot(data = adult_med_surg_flu_melt, aes(x = Site, y = value, fill = factor(FluMonth))) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_grid(variable ~ ., scales = "free", labeller = labeller(variable = adc_labels)) +
  labs(title = "2019 Adult Med Surg ADC Flu and Non-Flu Months", x = "Site", y = "ADC") +
  theme_bw() +
  theme(plot.title = element_text(size = 14, hjust = 0.5),
        legend.position = "bottom",
        legend.box = "horizontal",
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 10),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        panel.grid.major = element_line(color = "lightgrey"),
        panel.grid.minor = element_line(color = "lightgrey")) +
  geom_text(aes(label = round(value, digits = 1)), position = position_dodge(width = 1), vjust = -0.25, size = 3) +
  scale_fill_manual(name = "Flu Month", values = c("#221f72", "#00AEEF"), labels = c("No", "Yes")) +
  scale_y_continuous(expand = c(0, 0, 0.1, 0))

non_flu_mon_min_adc <- adult_med_surg_monthly_adc %>%
  filter(CensusYear == 2019 & FluMonth == 0) %>%
  group_by(Site) %>%
  summarize(LowestADCMonth = CensusDate[which.min(ADC)],
            LowestADC = min(ADC),
            LowestICUADC = ICUADC[which.min(ADC)])

flu_mon_max_adc <- adult_med_surg_monthly_adc %>%
  filter(CensusYear == 2019 & FluMonth == 1) %>%
  group_by(Site) %>%
  summarize(HighestADCMonth = CensusDate[which.max(ADC)],
            HighestADC = max(ADC),
            HighestICUADC = ICUADC[which.max(ADC)])

flu_adc_gradient <- left_join(non_flu_mon_min_adc, flu_mon_max_adc, by = c("Site" = "Site"))

flu_adc_gradient <- flu_adc_gradient %>%
  mutate(ADCDiff = HighestADC - LowestADC,
         ICUADCDiff = HighestICUADC - LowestICUADC)

flu_adc_gradient_melt <- melt(flu_adc_gradient, id.vars = "Site",
                              measure.vars = c("LowestADC", "LowestICUADC",
                                               "HighestADC", "HighestICUADC"))

flu_adc_gradient_melt <- flu_adc_gradient_melt %>%
  mutate(CensusType = ifelse(variable == "LowestADC" | variable == "HighestADC", "ADC", "ICUADC"),
         ScenarioType = ifelse(variable == "LowestADC" | variable == "LowestICUADC", "Lowest Non-Flu", "Highest Flu"))

ggplot(data = flu_adc_gradient_melt, aes(x = Site, y = value, fill = factor(ScenarioType))) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_grid(CensusType ~ ., scales = "free", labeller = labeller(CensusType = adc_labels)) +
  labs(title = "2019 ADC Based on Lowest Non-Flu and Highest Flu Months", x = "Site", y = "ADC") +
  theme_bw() +
  theme(plot.title = element_text(size = 14, hjust = 0.5),
        legend.position = "bottom",
        legend.box = "horizontal",
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 10),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        panel.grid.major = element_line(color = "lightgrey"),
        panel.grid.minor = element_line(color = "lightgrey")) +
  geom_text(aes(label = round(value, digits = 1)), position = position_dodge(width = 1), vjust = -0.25, size = 3) +
  scale_fill_manual(name = NULL, values = c("#221f72", "#00AEEF")) +
  scale_y_continuous(expand = c(0, 0, 0.1, 0))
