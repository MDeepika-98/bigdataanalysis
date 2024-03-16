# DATA INGESTION

library(readr)
data <- read_csv("https://data.ny.gov/api/views/5i6e-asw6/rows.csv")
head(data)

#-----------

#DATACLEANING

library(tidyverse)

# Remove any leading or trailing whitespace from all column names
colnames(data) <- str_trim(colnames(data))

# Rename the "MT CO2e AR5 20 yr" and "MT CO2e AR4 100 yr" columns to more manageable names
colnames(data) <- gsub("MT CO2e AR5 20 yr", "co2e_20yr", colnames(data))
colnames(data) <- gsub("MT CO2e AR4 100 yr", "co2e_100yr", colnames(data))


# Print the first few rows of the cleaned data
head(data)


#---------------

#Research Question 1

library(ggplot2)
library(dplyr)
library(plotly)

# Total emissions by year
emissions_by_year <- data %>% 
  group_by(Year) %>% 
  summarise(Total_Emissions = sum(`co2e_20yr`, na.rm = TRUE))

# Line plot of total emissions by year
p <- ggplot(emissions_by_year, aes(x = Year, y = Total_Emissions)) +
  geom_line(color = "#008080", size = 1.5) +
  labs(title = "Total Greenhouse Gas Emissions by Year in New York State",
       x = "Year",
       y = "Total Greenhouse Gas Emissions (co2e_20yr)") +
  theme_bw() +
  expand_limits(x = c(min(emissions_by_year$Year), max(emissions_by_year$Year)))

# Convert ggplot object to plotly object
ggplotly(p, tooltip = "y")


#---------

#Research Question 2
library(dplyr)
library(ggplot2)
library(plotly)

# Emissions by sector and year
emissions_by_sector_year <- data %>% 
  group_by(Sector, Year) %>% 
  summarise(Total_Emissions = sum(`co2e_20yr`, na.rm = TRUE))

# Line plot of emissions by sector and year
p <- ggplot(emissions_by_sector_year, aes(x = Year, y = Total_Emissions, color = Sector)) + 
  geom_line() + 
  labs(title = "Greenhouse Gas Emissions by Economic Sector and Year in New York State", 
       x = "Year", y = "Greenhouse Gas Emissions (co2e_20yr)")

# Convert ggplot object to plotly object
ggplotly(p, tooltip = c("x", "y", "color"))

#--------

# Univariate Analysis

#Histogram of greenhouse gas emissions in New York State:
library(plotly)
library(dplyr)

data %>%
  plot_ly(x = ~`co2e_20yr`, type = "histogram", nbinsx = 30) %>%
  layout(title = "Histogram of Greenhouse Gas Emissions in New York State",
         xaxis = list(title = "Greenhouse Gas Emissions (co2e_20yr)"),
         yaxis = list(title = "Count"))

#Bar plot of greenhouse gas emissions by economic sector:
library(plotly)
library(dplyr)

data %>%
  group_by(Sector) %>%
  summarise(Total_Emissions = sum(`co2e_20yr`, na.rm = TRUE)) %>%
  plot_ly(x = ~Sector, y = ~Total_Emissions, type = "bar", 
          marker = list(color = "#008080"), 
          text = ~paste(Sector, "<br>Total Emissions: ", Total_Emissions, "co2e_20yr", sep = "")) %>%
  layout(title = "Greenhouse Gas Emissions by Economic Sector in New York State", 
         xaxis = list(title = "Economic Sector"), 
         yaxis = list(title = "Greenhouse Gas Emissions (co2e_20yr)"))

# multivariate analysis: 

# Aggregate emissions by year and economic sector
emissions_by_sector_year <- data %>%
  group_by(Sector, Year) %>%
  summarise(Total_Emissions = sum(`co2e_20yr`, na.rm = TRUE))

# Create scatter plot
ggplot(emissions_by_sector_year, aes(x = Year, y = Total_Emissions, color = Sector, size = Total_Emissions)) +
  geom_point() +
  labs(title = "Greenhouse Gas Emissions by Economic Sector and Year in New York State",
       x = "Year",
       y = "Greenhouse Gas Emissions (co2e_20yr)",
       color = "Economic Sector",
       size = "Total Emissions (co2e_20yr)")


