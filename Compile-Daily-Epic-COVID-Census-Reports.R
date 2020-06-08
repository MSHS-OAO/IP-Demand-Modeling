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
  user_directory <- "J:/Presidents/HSPI-PM/Operations Analytics and Optimization/Projects/System Operations/Covid IP Staffing Model/Data/Epic Census Data"
} else {
  user_directory <- "J:/deans/Presidents/HSPI-PM/Operations Analytics and Optimization/Projects/System Operations/Covid IP Staffing Model/Data/Epic Census Data"
}

covid_file_names <- list.files(path = user_directory, pattern = "^(COVID Census Prior Day )[0-9]{4}\\-[0-9]{2}\\-[0-9]{2}.xlsx")

covid_census_file_list <- lapply(covid_file_names, function(x) read_excel(path = paste0(user_directory, "/", x), col_names = TRUE, na = c("", "NA")))

covid_census_compiled <- NULL

for (k in 1:length(covid_census_file_list)) {
  new_df <- covid_census_file_list[[k]]
  if (ncol(new_df) == 8) {
    new_df <- new_df %>%
      mutate(CENSUS_DATE = as.POSIXct(paste0(month(`CENSUS DATE`), "/", day(`CENSUS DATE`), "/", 
                                             year(`CENSUS DATE`), " 23:59:00"), format = "%m/%d/%Y %H:%M:%S", tz = "UTC"),
             GROUP = ifelse(ICU == "EMERGENCY", "ED", ICU),
             HOSP_ADMSN_TIME = force_tz(as.POSIXct(NA), tz = "UTC"),
             ROOM_ID = NA,
             ROOM_NAME = NA,
             BED_ID = NA,
             BED_LABEL = NA,
             `SERVICE GROUPER` = NA,
             BED_USE = NA,
             ACCOMMODATION_CODE = NA)
    
  } else {
    new_df <- new_df %>%
      mutate(CENSUS_DATE = as.POSIXct(paste(as.Date(`REPORT RUN`), " 23:59:00"), tz = "UTC"),
             HOSP_ADMSN_TIME = force_tz(HOSP_ADMSN_TIME, tz = "UTC"))
    
  }
  
  new_df <- new_df[ , c("MRN", "PAT_NAME", "PAT_ENC_CSN_ID",
                        "DEPARTMENT_NAME", "INFECTION_STATUS",  
                        "HOSP_ADMSN_TIME", "CENSUS_DATE", "GROUP", "LOC_NAME",
                        "ROOM_ID", "ROOM_NAME", "BED_ID", "BED_LABEL",
                        "SERVICE GROUPER", "BED_USE", "ACCOMMODATION_CODE")]
  
  covid_census_compiled <- rbind(covid_census_compiled, new_df)
}

# Remove duplicate entries
covid_census_compiled <- covid_census_compiled[!duplicated(covid_census_compiled), ]

# Sort COVID census by MRN, census date, and setting
# Fix any encounters with missing GROUP -- maybe get rid of this?
covid_census_compiled$GROUP[is.na(covid_census_compiled$GROUP)] <- ifelse(covid_census_compiled$DEPARTMENT_NAME[is.na(covid_census_compiled$GROUP)] == "MSQ ED DISCHARGE", "ED",
                                                       "NON CRITICAL CARE")

# Convert INFECTION_STATUS and GROUP to factors for easier sorting
covid_census_compiled$INFECTION_STATUS <- factor(covid_census_compiled$INFECTION_STATUS,
                                                 levels = c("COVID-19", "SUSC COVID", "PUI - COVID", "PUM - RESP"),
                                                 ordered = TRUE)
covid_census_compiled$GROUP <- factor(covid_census_compiled$GROUP,
                                      levels = c("CRITICAL CARE", "NON CRITICAL CARE", "ED", NA),
                                      ordered = TRUE)

covid_census_compiled <- covid_census_compiled %>%
  arrange(MRN, CENSUS_DATE, INFECTION_STATUS, GROUP)

# Determine if MRN and date is duplicated
covid_census_compiled <- covid_census_compiled %>%
  mutate(DuplMRNDate = duplicated(paste(MRN, CENSUS_DATE)))

# Remove any duplicate entries for MRN and date
covid_census_compiled <- covid_census_compiled %>%
  filter(DuplMRNDate == FALSE)

covid_census_compiled$DuplMRNDate <- NULL

# Export patient level census data to excel
write_xlsx(covid_census_compiled, paste0("COVID MRN Daily Census Export ", Sys.Date(), ".xlsx"))

# Summarize data by site and date
export_daily_mrn_covid_census <- covid_census_compiled %>%
  group_by(LOC_NAME, CENSUS_DATE, INFECTION_STATUS) %>%
  summarize(Census = n())

