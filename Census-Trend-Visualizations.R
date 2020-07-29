
## Install and load packages
# install.packages("DBI")
# install.packages("odbc")
# install.packages("dbplyr")

library(DBI)
library(odbc)
library(dbplyr)
library(dplyr)
library(lubridate)
library(readxl)
library(writexl)
library(ggplot2)
library(reshape2)
library(svDialogs)
library(stringr)
library(formattable)
library(scales)

# Establish connection to Oracle database
con <- dbConnect(odbc(), 
                 Driver = "Oracle in OraClient12Home1", 
                 DBQ = "idf02-scan/IDMREPD",
                 Schema = "COO_USER",
                 UID = "COO_USER",
                 PWD = "CO02O02rPaVc*MqP", 
                 Port = 1521)

# df1 <- dbGetQuery(con, "SELECT * FROM COVID_MERGED_ORDER_DTL_FINAL")

# df3 <- tbl(con, "EMR_ALL_PAT_DAYS_MSX") %>%
#   mutate(YEAR = year(CENSUS_DATE)) %>%
#   filter(YEAR >= 2020) %>%
#   collect()

# Import IP census for Epic sites
start1 <- proc.time()

oracle_ip_census_df <- tbl(con, "EMR_ALL_PAT_DAYS_MSX") %>%
  filter(year(CENSUS_DATE) == 2020) %>%
  collect()

end1 <- proc.time() - start1         

# Import COVID census for Epic sites
start2 <- proc.time()

oracle_covid_census_df <- tbl(con, "COVID_IP_PATIENT_DAYS_DETAIL") %>%
  filter(year(CENSUS_DATE) == 2020 & 
           !(IP_SITE %in% c("MSSN", "MSBI", "UNDETERMINED"))) %>%
  collect()

end2 <- proc.time() - start2

covid_census_subset <- oracle_covid_census_df %>%
  select(CSN, MRN, EXTERNAL_ENC_ID, CENSUS_DATE, CENSUS_DEPT, PAT_DAY_INF_STATUS) %>%
  arrange(CSN, CENSUS_DATE, CENSUS_DEPT)

ip_census_subset <- oracle_ip_census_df %>%
  arrange(CSN, CENSUS_DATE, CENSUS_DEPT)

ip_census_test_df <- left_join(ip_census_subset, covid_census_subset,
                          by = c("CSN" = "CSN", "CENSUS_DATE" = "CENSUS_DATE", "CENSUS_DEPT"))

ip_census_test_df <- ip_census_test_df %>%
  mutate(MRNTest = ifelse(!is.na(PAT_DAY_INF_STATUS), MRN.x == MRN.y, NA),
         EncTest = ifelse(!is.na(PAT_DAY_INF_STATUS), EXTERNAL_ENC_ID.x == EXTERNAL_ENC_ID.y, NA))
