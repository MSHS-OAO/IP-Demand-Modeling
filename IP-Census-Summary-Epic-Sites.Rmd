---
title: "MSHS Census Summary"
output: html_document

---
  
#### Report created on `r format(Sys.Date(), "%m/%d/%y")`
```{r Install and load packages, include = FALSE, echo = FALSE, warning = FALSE, message = FALSE}
# install.packages("dplyr")
# install.packages("lubridate")
# install.packages("readxl")
# install.packages("writexl")
# install.packages("ggplot2")
# install.packages("lubridate")
# install.packages("reshape2")
# install.packages("svDialogs")
# install.packages("stringr")
# install.packages("formattable")
# install.packages("scales")
# install.packages("knitr")
# install.packages("rmarkdown")
# install.packages("kableExtra")
# install.packages("DBI")
# install.packages("odbc")
# install.packages("dbplyr")

library(dplyr)
library(lubridate)
library(readxl)
library(writexl)
library(ggplot2)
library(lubridate)
library(reshape2)
library(svDialogs)
library(stringr)
library(formattable)
library(scales)
library(knitr)
library(rmarkdown)
library(kableExtra)
library(DBI)
library(odbc)
library(dbplyr)

```

```{r Set global options, include = FALSE}

rm(list = ls())

knitr::opts_chunk$set(echo = FALSE, message = FALSE, warning = FALSE, fig.align = "center")

```


```{r Determine drive mapping for output}

if (list.files("J://") == "Presidents") {
  user_directory <- "J:/Presidents/HSPI-PM/Operations Analytics and Optimization/Projects/System Operations/COVID Scenario Testing"
} else {
  user_directory <- "J:/deans/Presidents/HSPI-PM/Operations Analytics and Optimization/Projects/System Operations/COVID Scenario Testing"
}

```

```{r Connect to Oracle database and import IP census data for Epic sites}

# Establish connection to Oracle database
con <- dbConnect(odbc(), 
                 Driver = "Oracle in OraClient12Home1", 
                 DBQ = "idf02-scan/IDMREPD",
                 Schema = "COO_USER",
                 UID = "COO_USER",
                 PWD = "CO02O02rPaVc*MqP", 
                 Port = 1521)


# Import IP census for Epic sites for 03/20-04/20
start1 <- proc.time()

oracle_ip_census_df <- tbl(con, "EMR_ALL_PAT_DAYS_MSX") %>%
  filter(year(CENSUS_DATE) == 2020 &
           month(CENSUS_DATE) >= 3 &
           month(CENSUS_DATE) <= 4) %>%
  collect()

end1 <- proc.time() - start1

```


```{r Import reference file with unit mappings}

unit_mappings <- read_xlsx(path = paste0(user_directory, "/Epic Census Trending/Epic-Oracle-Census-Unit-Mappings-2020-08-05.xlsx"), sheet = "UnitMappings", na = c("", "NA"))

# Manually update unit mappings for MSW 14A to reflect change from Antepartum to Med Surg during first surge
unit_mappings <- unit_mappings %>%
  mutate(UnitType = ifelse(CENSUS_DEPT == "MAIN 14A", "Adult MS", UnitType),
         UnitRollUp = ifelse(CENSUS_DEPT == "MAIN 14A", "Med Surg", UnitRollUp))

```


```{r Reference data for tables}

sinai_colors <- c("#221f72", "#00AEEF", "#D80B8C", "#4C4D4C", "#8988dd", "#B2B3B2", "#F887CE")

site_order <- c("MSH", "MSQ", "MSB", "MSM", "MSW", "MSHS")

start_date <- as.Date("3/16/20", format = "%m/%d/%y")


```

```{r Format raw census data}
ip_census_df <- oracle_ip_census_df

# Remove sites not on Epic during surge, format census date, and determine if the census date is an ICU day
# ICU Logic: 
# All sites besides MSQ: accommodation code of intensive care or coronary care
# MSQ: Unit 6 ICU or 5 East and 6 East starting on 3/28/20

ip_census_df <- ip_census_df %>%
  filter(IP_SITE != "MSBI") %>%
  mutate(CensusDate = date(CENSUS_DATE), 
         ICUDay = (IP_SITE != "MSQ" & ACCOMMODATION_DESC %in% c("INTENSIVE CARE", "CORONARY CARE")) |
                         CENSUS_DEPT == "MSQ 6 ICU" | 
                           (CENSUS_DEPT %in% c("MSQ 5 EAST", "MSQ 6 EAST") &
                              CensusDate >= as.Date("03/28/2020", format = "%m/%d/%Y")),
         CriticalCareDay = (ICUDay == 1 | VENT_NON_ICU == 1))

# Crosswalk census data with unit mappings
ip_census_df <- left_join(ip_census_df, unit_mappings,
                          by = c("CENSUS_DEPT" = "CENSUS_DEPT"))

```


```{r Subset adult census and summarize data}

# Subset all IP census days to only include adult ICU, adult med-surg, and COVID surge units and exclude discharge days
adult_census <- ip_census_df %>%
  filter(CensusDate >= start_date, 
    UnitType %in% c("Adult ICU", "Adult MS", "COVID Surge") &
           PATIENT_DAY == 1)

# Summarize census by day for each site
site_adult_census_summary <- adult_census %>%
  group_by(CensusSite, CensusDate) %>%
  summarize(Adult_Census = sum(PATIENT_DAY),
            CC_Census = sum(CriticalCareDay, na.rm = TRUE),
            MS_Census = Adult_Census - CC_Census) %>%
  ungroup()

# Summarize census by day for system
system_adult_census_summary <- adult_census %>%
  group_by(CensusDate) %>%
  summarize(CensusSite = "MSHS",
            Adult_Census = sum(PATIENT_DAY),
            CC_Census = sum(CriticalCareDay, na.rm = TRUE),
            MS_Census = Adult_Census - CC_Census) %>%
  ungroup()

system_adult_census_summary <- system_adult_census_summary[ , colnames(site_adult_census_summary)]

# Bind site and system census together
adult_census_summary <- rbind(site_adult_census_summary, system_adult_census_summary)

# Set census sites as factor for sorting
adult_census_summary$CensusSite <- factor(adult_census_summary$CensusSite, 
                                          levels = site_order,
                                          ordered = TRUE)

```

```{r Determine peak census and create kable}

# Create dataframe with peak total census, critical care census, and med-surg census
peak_census_summary <- adult_census_summary %>%
  group_by(CensusSite) %>%
  summarize(PeakAdultCensusDate = CensusDate[which.max(Adult_Census)], 
            PeakAdultCensus = Adult_Census[which.max(Adult_Census)],
            PeakCCCensusDate = CensusDate[which.max(CC_Census)],
            PeakCCCensus = CC_Census[which.max(CC_Census)],
            PeakMSCensusDate = CensusDate[which.max(MS_Census)],
            PeakMSCensus = MS_Census[which.max(MS_Census)]) %>%
  arrange(CensusSite)


# Create kable to visualize results
peak_census_kable <- kable(peak_census_summary, format = "html", escape = FALSE, align = "c", digits = 0,
      col.names =c("Site", "Date", "Census", "Date", "Census", "Date", "Census"),
      caption = paste0("Peak IP Census During COVID Surge (",
                       format(start_date, "%m/%d/%y"), " - ", 
                       format(max(adult_census_summary$CensusDate), "%m/%d/%y"),
                       ")")) %>%
  kable_styling(bootstrap_options = "hover", position = "center", font_size = 11, full_width = FALSE) %>%
  column_spec(column = c(1, 3, 5, 7), border_right = "thin solid lightgray") %>%
  column_spec(column = c(2:3), background = sinai_colors[1], color = "white", include_thead =  TRUE) %>%
  column_spec(column = c(4:5), background = sinai_colors[2], color = "white", include_thead =  TRUE) %>%
  column_spec(column = c(6:7), background = sinai_colors[3], color = "white", include_thead =  TRUE) %>%
  column_spec(column = c(2:7), background = "inherit", color = "inherit") %>%  
  # row_spec(row = 0, background = "#221f72", color = "white") %>%
  row_spec(row = nrow(peak_census_summary), background = "lightgrey", bold = T) %>%
  add_header_above(c(" " = 1, "Total Adult Census" = 2, "Adult Critical Care Census" = 2, "Adult Med Surg Census" = 2), background = c("", sinai_colors[1], sinai_colors[2], sinai_colors[3]), color = "white", line = FALSE)


```


```{r Create a kable with unit mappings for reference}

unit_rollup_order <- c("ICU", "Med Surg", "COVID Surge",
                       "L&D & Postpartum", 
                       "Nursery & NICU", "Peds & Peds ICU",
                       "Psych & Rehab",
                       "ED", "Psych ED", 
                       "Non-IP")

# Create a dataframe with unit mappings
unit_mappings_reference <- unit_mappings %>%
  filter(CensusSite != "MSBI") %>%
  select(CensusSite, UnitRollUp, CENSUS_DEPT)

unit_mappings_reference$CensusSite <- factor(unit_mappings_reference$CensusSite,
                                         levels = site_order,
                                         ordered = TRUE)

unit_mappings_reference$UnitRollUp <- factor(unit_mappings_reference$UnitRollUp,
                                         levels = unit_rollup_order, 
                                         ordered = TRUE)

unit_mappings_reference <- unit_mappings_reference %>%
  arrange(CensusSite, UnitRollUp)


unit_mappings_kable <- kable(unit_mappings_reference, format = "html", escape = FALSE, align = "c", digits = 0,
      col.names =c("Site", "Unit Type", "Census Dept"),
      caption = "Unit Mappings by Site") %>%
  kable_styling(bootstrap_options = "hover", position = "center", font_size = 11, full_width = FALSE) %>%
  column_spec(column = c(1:3), border_right = "thin solid lightgray") %>%
  column_spec(column = c(1:3), background = sinai_colors[1], color = "white", include_thead = TRUE) %>%
  column_spec(column = c(1:3), background = "inherit", color = "inherit") %>%
  collapse_rows(valign = "top")

```

## {.tabset}

### Peak Census Results
```{r Display peak census results}

peak_census_kable

```

### Methodology and Data Sources
<h4><b>Data Sources:</b></h4>
Analysis based on Epic midnight census data. Data obtained from Oracle database table "EMR_EMR_ALL_PAT_DAYS_MSX" pulled on `r Sys.Date()`.
<h4></h4></br>


<h4><b>Analysis Assumptions:</b></h4>
1. COVID Surge Start Date: Analysis assumes a start date of 3/16/20.
2. Critical Care Classifications:
    + For sites other than MSQ, ICU patients are identified as those with Accommodation Code of Intensive Care or Coronary Care
    + For MSQ, any patient in unit MSQ 6 ICU is classified as a critical care patient. Any patient in MSQ 6 IMCU and MSQ 5 East starting on 3/28/20 is also classified as a critical care patient.
    + For all sites, any patient with a vent but not in an ICU is classified as a critical care patient.
<h4></h4></br>


<h4><b>Analysis Methodology:</b></h4>
Analysis follows process and logic described below:

1. Unit type mappings:
    + Analysis only includes units mapped as Adult ICU, Adult Med Surg, and COVID Surge.
    + All other units (Peds, MCH, Psych, Rehab, etc.) are excluded from analysis.
    + See "Unit Mappings" table for reference.
2. Calculate Total Adult Census and Adult Critical Care Census based on unit inclusion criteria and critical care classifications.
3. Calculate Adult Med Surg Census as difference between Total Adult Census and Adult Critical Care Census.
4. Determine date when Total Adult Census, Adult Critical Care Census, and Adult Med Surge Census peaked for each site.
    + Note: Not all peak census occur on the same day for a given site.
<h4></h4></br>


<h4><b>Unit Mappings:</b></h4>
```{r Display unit mappings kable}

unit_mappings_kable

```


