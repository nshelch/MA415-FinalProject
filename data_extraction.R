
library(tidyverse)

# Read data
crime_data <- read.csv("crime.csv")

# Filter data
crime_data.v1 <- crime_data %>% 
  select(-INCIDENT_NUMBER, -OFFENSE_DESCRIPTION, -UCR_PART)

# Seperate OCCURRED_ON_DATE to include the day of the month

crime_data.v2 <- crime_data.v1 %>% 
  separate(OCCURRED_ON_DATE, c("Date","Time"), sep = " ") %>% 
  separate(Date, c("MM","DAY","YY"), sep = "/") %>% 
  select(-YY, -MM, -Time)

saveRDS(crime_data.v2, "filtered_crime.RDS")
