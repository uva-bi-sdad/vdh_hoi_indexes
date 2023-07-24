#load all necessary libraries
library(readr)
library(tigris)
library(tidycensus)
library(dplyr)

#load the data from H+T
data_2015 <- read_csv("VDH_Employment/htaindex2015_data_tracts_51.csv")

data_2019 <- read_csv("VDH_Employment/htaindex2019_data_tracts_51.csv")

data_2020 <- read_csv("VDH_Employment/htaindex2020_data_tracts_51.csv")


#Select only required columns, emp_gravity is emplyment access index
data_2015 <- data_2015[, c("tract", "cbsa", "emp_gravity")]

data_2019 <- data_2019[, c("tract", "cbsa", "emp_gravity")]

data_2020 <- data_2020[, c("tract", "cbsa", "emp_gravity")]

#removing all the quotes for our Convenience

remove_double_quotes <- function(x) {
  gsub('"', '', x)
}

data_2015[] <- lapply(data_2015, remove_double_quotes)

data_2019[] <- lapply(data_2019, remove_double_quotes)

data_2020[] <- lapply(data_2020, remove_double_quotes)

data_2015 <- data.frame(data_2015)
data_2019 <- data.frame(data_2019)
data_2020 <- data.frame(data_2020)


#getting a sample ACS dataset just to get the region names

get_acs_subset <- function(year, state) {
  acs_data <- get_acs(geography = "tract",
                      variables = "B01003_001",  # Just a random variable, as we don't need it
                      year = year,
                      survey = "acs5",
                      state = state,
                      tidy = FALSE)
  subset_data <- acs_data[, c("GEOID", "NAME")]
  return(subset_data)
}


va_acs_2015 <- get_acs_subset(year = 2015, state = "VA")
va_acs_2019 <- get_acs_subset(year = 2019, state = "VA")
va_acs_2020 <- get_acs_subset(year = 2020, state = "VA")



va_acs_2015 <- data.frame(va_acs_2015)
va_acs_2019 <- data_frame(va_acs_2019)
va_acs_2020 <- data_frame(va_acs_2020)

#merging the data for our format
#havibg everything in a function for optimization

merge_and_rename <- function(data, acs_data, year) {
  merged_data <- merge(data, acs_data, by.x = "tract", by.y = "GEOID", all.x = TRUE)
  merged_data$cbsa <- NULL
  colnames(merged_data)[colnames(merged_data) == "tract"] <- "geoid"
  colnames(merged_data)[colnames(merged_data) == "year"] <- paste0("year_", year)
  
  merged_data$measure <- "employment access index"
  merged_data$measure_type <- "index"
  merged_data$year <- year
  colnames(merged_data)[colnames(merged_data) == "NAME"] <- "region_name"
  colnames(merged_data)[colnames(merged_data) == "emp_gravity"] <- "value"
  merged_data$region_type <- "tract"
  
  merged_data <- merged_data[, c("geoid", "measure", "measure_type", "region_name", "region_type", "value", "year")]
  
  return(merged_data)
}



merged_data_2015 <- merge_and_rename(data_2015, va_acs_2015, 2015)

merged_data_2019 <- merge_and_rename(data_2019, va_acs_2019, 2019)

merged_data_2020 <- merge_and_rename(data_2020, va_acs_2020, 2020)


#final combined data
combined_data <- rbind(merged_data_2015, merged_data_2019, merged_data_2020)



write.csv(combined_data, file = "~/VDH_Employment/va_cttr_2015_2019_2020_employment_access_index.csv", row.names = FALSE)