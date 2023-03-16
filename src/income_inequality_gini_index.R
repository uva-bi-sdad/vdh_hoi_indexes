# Calculate the GINI coefficient for census tracts in Virginia:
library(tidyverse)
library(tidycensus)
library(sf)
library(data.table)


va_tract_ginis <- get_acs(geography = "tract",
                          variables = "B19083_001",
                          state = "VA",
                          # county = c("ARLINGTON", "FAIRFAX COUNTY"),
                          geometry = T,
                          output = "wide",
                          year = 2021)

# Transform to range [0, 1]
mn <- min(va_tract_ginis$B19083_001E, na.rm = T)
mx <- max(va_tract_ginis$B19083_001E, na.rm = T)
scaled <- (va_tract_ginis$B19083_001E - mn)/(mx-mn)

va_tract_ginis$scaled <- 1-scaled


# zscores <- (va_tract_ginis$B19083_001E - mean(va_tract_ginis$B19083_001E, na.rm = T)) / sd(va_tract_ginis$B19083_001E, na.rm = T)
# mnz <- min(zscores, na.rm = T)
# mxz <- max(zscores, na.rm = T)
# scaledz <- (zscores - mnz)/(mxz-mnz)



# va_tracts <- get_acs(
#   geography = "tract",
#   variables = c("B01001_001", 
#                 "B19013_001"),
#   state = "VA",
#   geometry = TRUE,
#   output = "wide"
# )
# 
# va_tracts$B01003_001E <- NULL
# va_tracts$B19013_001E <- NULL
# va_tracts$NAME <- NULL
# colnames(va_tracts) <- c("geoid", "population", "med_hh_inc", "geometry")
# 
# 
# # Calculate the total population and total income for each census tract
# va_tracts$income <- va_tracts$med_hh_inc * va_tracts$
# 
# # Sort the data by income: Sort the data frame by income in ascending order.
# va_tracts <- va_tracts %>% 
#   arrange(income)
# 
# # Calculate the cumulative income and population for each census tract: Use the cumsum function to calculate the 
# # cumulative income and population for each census tract.
# va_tracts <- va_tracts %>% 
#   mutate(cum_income = cumsum(income),
#          cum_pop = cumsum(populationE))
# 
# # Calculate the GINI coefficient for each census tract.
# va_tracts$gini_denom = va_tracts$cum_income / sum(va_tracts$income, na.rm = T)
# va_tracts$gini = 1 - (2 * sum(va_tracts$cum_pop * va_tracts$gini_denom) / sum(va_tracts$cum_pop, na.rm = T))
# 
# 
# # Transform to range [0, 1]
# mn <- min(va_tracts$gini, na.rm = T)
# mx <- max(va_tracts$gini, na.rm = T)
# scaled <- (va_tracts$gini - mn)/(mx-mn)
# 
# va_tracts$scaled <- scaled
# 
# # Plot the GINI coefficient
# plot(va_unq_tracts[, c("gini")])
