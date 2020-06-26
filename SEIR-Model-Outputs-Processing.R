# Code for importing tabular data from SEIR model
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

# Import census estimates and admissions probability from SEIR model
seir_census_df_raw <- read_excel(paste0(user_directory, "/SEIR Model Outputs/SEIR Model Census Estimates 2020-06-25.xlsx"))

seir_admit_prob_df_raw <- read_excel(paste0(user_directory, "/SEIR Model Outputs/SEIR Model Admissions Probability by Site 2020-06-25.xlsx"))

# Format SEIR census estimates
seir_census_df <- seir_census_df_raw %>%
  select(date, model, h_mshs, h_mshs_reg, h_mshs_icu) %>%
  mutate(date = date(date),
         Delta = h_mshs - lag(h_mshs, 1))

colnames(seir_census_df) <- c("Date", "Model", "TotalCensus", "FloorCensus", "ICUCensus", "Delta")

# Format SEIR admission probabilities
seir_admit_prob_df <- seir_admit_prob_df_raw %>%
  select(IP_SITE, day_of_admission, prob_adm, smoothed_prob_adm) %>%
  mutate(day_of_admission = date(day_of_admission))

colnames(seir_admit_prob_df) <- c("Site", "Date", "ProbAdm", "SmoothedProbAdm")

# Plot key graphs
sinai_colors <- c("#221f72", "#00AEEF", "#D80B8C", "#4C4D4C", "#8988dd", "#B2B3B2", "#F887CE")

# Plot overall census
ggplot(data = seir_census_df, aes(x = Date, y = TotalCensus, color = Model)) +
  geom_line() + 
  labs(title = "MSHS COVID Census Predictions", x = "Date", y = "Total Census") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "right", legend.box = "vertical", legend.title = element_text(size = 10), legend.text = element_text(size = 8),axis.text.x = element_text(angle = 30, hjust = 1)) +
  
  scale_x_date(limits = c(min(seir_census_df$Date), max(seir_census_df$Date)), breaks = "1 month",
               date_labels = "%b-%y") +
  scale_color_manual(name = "Reintroduction \nRate & Date", values = sinai_colors[1:4])

# Plot ICU/vent census
ggplot(data = seir_census_df, aes(x = Date, y = ICUCensus, color = Model)) +
  geom_line() + 
  labs(title = "MSHS COVID Critical Care Census Predictions", x = "Date", y = "Total Census") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "right", legend.box = "vertical", legend.title = element_text(size = 10), legend.text = element_text(size = 8),axis.text.x = element_text(angle = 30, hjust = 1)) +
  
  scale_x_date(limits = c(min(seir_census_df$Date), max(seir_census_df$Date)), breaks = "1 month",
               date_labels = "%b-%y") +
  scale_color_manual(name = "Reintroduction \nRate & Date", values = sinai_colors[1:4])

# Plot admissions probabilities
ggplot(data = seir_admit_prob_df) +
  geom_point(aes(x = Date, y = ProbAdm, color = Site)) +
  geom_line(aes(x = Date, y = SmoothedProbAdm, color = Site)) +
  labs(title = "COVID Admission Probability by Site", x = "Date", y = "Total Census") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "right", legend.box = "vertical", legend.title = element_text(size = 10), legend.text = element_text(size = 8),axis.text.x = element_text(angle = 30, hjust = 1)) +
  
  scale_x_date(limits = c(min(seir_admit_prob_df$Date), max(seir_admit_prob_df$Date)), breaks = "1 month",
               date_labels = "%b-%y") +
  scale_color_manual(name = "Site", values = sinai_colors)

# Determine average probability of admissions using smoothed data
avg_prob_admit <- seir_admit_prob_df %>%
  group_by(Site) %>%
  summarize(AvgProb = mean(SmoothedProbAdm)*100)

# Determine secondary peak for each reintroduction rate and date scenario
seir_peaks <- seir_census_df %>%
  filter(Date >= as.Date("7/1/20", format = "%m/%d/%y"),
         Delta > 0) %>%
  group_by(Model) %>%
  summarize(PeakCensus = max(TotalCensus),
            PeakTotalCensusDate = Date[which.max(TotalCensus)],
            PeakICUCensus = max(ICUCensus),
            PeakICUCensusDate = Date[which.max(ICUCensus)])

summer_peak_df <- seir_census_df %>%
  filter(Model == "0.017 2020-04-23")

summer_peak <- summer_peak_df %>%
  filter(Date >= as.Date("7/1/20", format = "%m/%d/%y"),
         Delta > 0) %>%
  summarize(PeakCensus = max(TotalCensus),
            PeakTotalCensusDate = Date[which.max(TotalCensus)],
            PeakICUCensus = max(ICUCensus),
            PeakICUCensusDate = Date[which.max(ICUCensus)])

# Scale potential secondary peak by probability of admission at each site
summer_peak_rep <- summer_peak[rep(row.names(summer_peak), nrow(avg_prob_admit)), ]

summer_peak_scale <- cbind(avg_prob_admit, summer_peak_rep)

summer_peak_scale <- summer_peak_scale %>%
  mutate(SiteTotalPeak = AvgProb * PeakCensus / 100,
         SiteICUPeak = AvgProb * PeakICUCensus / 100)

summer_peak_scale <- summer_peak_scale %>%
  select(Site, SiteTotalPeak, SiteICUPeak)

# Remove MSSN from site level summary
site_summer_peak <- summer_peak_scale %>%
  filter(Site != "MSSN")

# Calculate new MSHS total after removing MSSN
new_mshs_peak <- site_summer_peak %>%
  summarize(Site = "MSHS",
            SiteTotalPeak = sum(SiteTotalPeak),
            SiteICUPeak = sum(SiteICUPeak))

system_summer_peak <- rbind(site_summer_peak, new_mshs_peak)
