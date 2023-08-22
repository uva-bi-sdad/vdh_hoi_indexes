library(readr)
library(tigris)
library(tidycensus)
library(dplyr)



data_2020 <- read_excel("~/git/vdh_hoi_indexes/data/Incarceration_data_2020.xlsx")


data_2020 <- data.frame(data_2020)

data_2020_df <- data_2020 %>%
  select(FIPS, Incarceration_rate_per_100000) %>%
  mutate(
    geoid = FIPS,
    measure = "incarceration_rate_per_100000",
    year = 2020,
    value = Incarceration_rate_per_100000
  ) %>%
  select(geoid, measure, year, value)


write.csv(data_2020_df, "~/incarceration/va_cttr_2020_incarceration_rate.csv", row.names = FALSE)

