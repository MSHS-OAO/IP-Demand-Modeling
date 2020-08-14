## Code for summarizing 2019 census
# This code should only need to be run one time to import and summarize 2019 census data. 
# The output of this code can then be imported in .Rmd file for comparing and visualizing 2019 and 2020 trends

# Install and load packages (dplyr must be loaded before dbplyr)
# install.packages("DBI")
# install.packages("odbc")
# install.packages("dplyr")
# install.packages("dbplyr")
# install.packages("lubridate")
# install.packages("readxl")
# install.packages("writexl")
# install.packages("ggplot2")
# install.packages("lubridate")
# install.packages("reshape2")
# install.packages("svDialogs")
# install.packages("stringr")
# install.packages("scales")

library(DBI)
library(odbc)
library(dplyr)
library(dbplyr)
library(lubridate)
library(readxl)
library(writexl)
library(ggplot2)
library(lubridate)
library(reshape2)
library(svDialogs)
library(stringr)
library(scales)

# Establish connection to Oracle database
con <- dbConnect(odbc(), 
                 Driver = "Oracle in OraClient12Home1", 
                 DBQ = "idf02-scan/IDMREPD",
                 Schema = "COO_USER",
                 UID = "COO_USER",
                 PWD = "CO02O02rPaVc*MqP", 
                 Port = 1521)

# Import IP census for Epic sites for FY2019
start <- proc.time()

oracle_ip_census_2019_df <- tbl(con, "EMR_ALL_PAT_DAYS_MSX") %>%
  filter(year(CENSUS_DATE) == 2019) %>%
  collect()

end <- proc.time() - start


# Format and subset IP census data
ip_census_2019 <- oracle_ip_census_2019_df %>%
  filter(year(CENSUS_DATE) == 2019 &
           PATIENT_DAY == 1)

ip_census_2019 <- unique(ip_census_2019)

ip_census_2019$CensusDate <- as.Date(ip_census_2019$CENSUS_DATE)

# Summarize census data
ip_census_2019_summary <- ip_census_2019 %>%
  group_by(IP_SITE, CensusDate, 
           CENSUS_DEPT, ACCOMMODATION_DESC, ICU_DAY,
           VENT_DAY, VENT_NON_ICU,
           ADMISSION_TYPE, ADMIT_SOURCE,
           SERVICE_GROUPER, DISPOSITION) %>%
  summarize(Census = n())

# Export census data to be imported in future analysis
if (list.files("J://") == "Presidents") {
  user_directory <- "J:/Presidents/HSPI-PM/Operations Analytics and Optimization/Projects/System Operations/COVID Scenario Testing"
} else {
  user_directory <- "J:/deans/Presidents/HSPI-PM/Operations Analytics and Optimization/Projects/System Operations/COVID Scenario Testing"
}

write_xlsx(ip_census_2019_summary, path = paste0(user_directory, 
                                                 "/Epic Census Trending/Epic FY2019 IP Census Summary ", 
                                                 Sys.Date(), ".xlsx"))
