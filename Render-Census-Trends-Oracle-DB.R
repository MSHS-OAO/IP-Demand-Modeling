# Code to import Oracle database and render markdown

# Clear any current variables
rm(list = ls())

# Establish connection to database and import raw data
source(file = "Oracle-Database-Connection-Import.R")

# Render markdown file for visualizing census trends
rmarkdown::render(input = "Census-Trend-Visualizations.Rmd", 
                  output_file = paste0(user_directory, "/Epic Census Rmd Output/IP Census Dashboard ",
                  Sys.Date()))