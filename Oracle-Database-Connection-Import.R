# Code for connecting to and importing data from Oracle

# Install and load packages (dplyr must be loaded before dbplyr)
# install.packages("DBI")
# install.packages("odbc")
# install.packages("dplyr")
# install.packages("dbplyr")

library(DBI)
library(odbc)
library(dplyr)
library(dbplyr)

# Establish connection to Oracle database
con <- dbConnect(odbc(), 
                 Driver = "Oracle in OraClient12Home1", 
                 DBQ = "idf02-scan/IDMREPD",
                 Schema = "COO_USER",
                 UID = "COO_USER",
                 PWD = "CO02O02rPaVc*MqP", 
                 Port = 1521)


# Import IP census for Epic sites
start1 <- proc.time()

oracle_ip_census_df <- tbl(con, "EMR_ALL_PAT_DAYS_MSX") %>%
  filter(year(CENSUS_DATE) >= 2019) %>%
  collect()

end1 <- proc.time() - start1

# Import COVID census for Epic sites
start2 <- proc.time()

oracle_covid_census_df <- tbl(con, "COVID_IP_PATIENT_DAYS_DETAIL") %>%
  filter(year(CENSUS_DATE) == 2020 &
           !(IP_SITE %in% c("MSSN", "MSBI", "UNDETERMINED"))) %>%
  collect()

end2 <- proc.time() - start2
