cat("\014") # clears console
rm(list = ls()) # clears global environment
try(dev.off(dev.list()["RStudioGD"]), silent = TRUE) # clears plots
try(p_unload(p_loaded(), character.only = TRUE), silent = TRUE) # clears packages
options(scipen = 100) # disables scientific notation for entire R session

# Load necessary package
library(dplyr)

# read all csv files
austin_data <- read.csv("Austin.csv")
istanbul_data <- read.csv("Istanbul.csv")
melbourne_data <- read.csv("Melbourne.csv")
toronto_data <- read.csv("Toronto.csv")
paris_data <- read.csv("Paris.csv")

# add the city column to each dataset
austin_data$city <- "Austin"
istanbul_data$city <- "Istanbul"
melbourne_data$city <- "Melbourne"
toronto_data$city <- "Toronto"
paris_data$city <- "Paris"

# convert neighbourhood and neighbourhood_cleansed columns to character
austin_data <- austin_data %>%
  mutate(across(c(neighbourhood, neighbourhood_cleansed), as.character))

istanbul_data <- istanbul_data %>%
  mutate(across(c(neighbourhood, neighbourhood_cleansed), as.character))

melbourne_data <- melbourne_data %>%
  mutate(across(c(neighbourhood, neighbourhood_cleansed), as.character))

toronto_data <- toronto_data %>%
  mutate(across(c(neighbourhood, neighbourhood_cleansed), as.character))

paris_data <- paris_data %>%
  mutate(across(c(neighbourhood, neighbourhood_cleansed), as.character))

# Remove $ sign from price column and convert to numeric
austin_data$price <- as.numeric(gsub("\\$", "", austin_data$price))
istanbul_data$price <- as.numeric(gsub("\\$", "", istanbul_data$price))
melbourne_data$price <- as.numeric(gsub("\\$", "", melbourne_data$price))
toronto_data$price <- as.numeric(gsub("\\$", "", toronto_data$price))
paris_data$price <- as.numeric(gsub("\\$", "", paris_data$price))

# combine all datasets
combined_data <- bind_rows(austin_data, melbourne_data, istanbul_data, toronto_data, paris_data)

# Remove the specified columns
columns_to_remove <- c("id", "listing_url", "scrape_id", "last_scraped", "source", "name", "description", "neighborhood_overview", 
                       "picture_url", "host_id", "host_since", "host_about", "host_response_time", "host_thumbnail_url", 
                       "host_picture_url", "host_neighbourhood", "host_has_profile_pic", "neighbourhood", 
                       "neighbourhood_group_cleansed", "property_type", "accommodates", "bathrooms_text", "beds", 
                       "maximum_nights", "minimum_minimum_nights", "maximum_minimum_nights", "minimum_maximum_nights", 
                       "maximum_maximum_nights", "minimum_nights_avg_ntm", "maximum_nights_avg_ntm", "calendar_updated", 
                       "calendar_last_scraped", "number_of_reviews_ltm", "number_of_reviews_l30d", 
                       "review_scores_cleanliness", "review_scores_checkin", "review_scores_communication", 
                       "review_scores_value", "license")

combined_data <- combined_data %>%
  select(-one_of(columns_to_remove))

# Remove rows with NA values
cleaned_data <- na.omit(combined_data)

# save the cleaned dataset as Airbnb_data.csv
write.csv(cleaned_data, "Airbnb_data.csv", row.names = FALSE)

View(cleaned_data)
