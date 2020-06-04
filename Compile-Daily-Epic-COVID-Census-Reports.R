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
      mutate(`REPORT RUN` = date(`CENSUS DATE`),
             GROUP = ifelse(ICU == "EMERGENCY", "ED", ICU),
             HOSP_ADMSN_TIME = NA,
             ROOM_ID = NA,
             ROOM_NAME = NA,
             BED_ID = NA,
             BED_LABEL = NA,
             `SERVICE GROUPER` = NA,
             BED_USE = NA,
             ACCOMMODATION_CODE = NA)

    new_df <- new_df[ , c("MRN", "PAT_NAME", "PAT_ENC_CSN_ID",
                        "DEPARTMENT_NAME", "INFECTION_STATUS",  
                        "HOSP_ADMSN_TIME", "REPORT RUN", "GROUP", "LOC_NAME",
                        "ROOM_ID", "ROOM_NAME", "BED_ID", "BED_LABEL",
                        "SERVICE GROUPER", "BED_USE", "ACCOMMODATION_CODE")]
  } else {
    new_df <- new_df
  }
  covid_census_compiled <- rbind(covid_census_compiled, new_df)
}

# Next steps - remove duplicates, export to excel
