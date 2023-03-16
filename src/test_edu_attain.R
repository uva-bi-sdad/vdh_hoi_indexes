library(tidycensus)
library(data.table)

# Function to get average years of schooling
get_ays <- function(acs_data, tract_geoid) {
  pop_mf <- acs_data[acs_data$GEOID==tract_geoid & acs_data$variable %in% c("B15001_001"), c("estimate")][[1]]
  pop_m <- acs_data[acs_data$GEOID==tract_geoid & acs_data$variable %in% c("B15001_002"), c("estimate")][[1]]
  pop_f <- acs_data[acs_data$GEOID==tract_geoid & acs_data$variable %in% c("B15001_043"), c("estimate")][[1]]
  
  m_lt_9gr <-       acs_data[acs_data$GEOID==tract_geoid & acs_data$variable %in% c("B15001_004", "B15001_012", "B15001_020", "B15001_028", "B15001_036"),]
  m_hs_grad_no <-   acs_data[acs_data$GEOID==tract_geoid & acs_data$variable %in% c("B15001_005", "B15001_013", "B15001_021", "B15001_029", "B15001_037"),]
  m_hs_grad_yes <-  acs_data[acs_data$GEOID==tract_geoid & acs_data$variable %in% c("B15001_006", "B15001_014", "B15001_022", "B15001_030", "B15001_038"),]
  m_col_some <-     acs_data[acs_data$GEOID==tract_geoid & acs_data$variable %in% c("B15001_007", "B15001_015", "B15001_023", "B15001_031", "B15001_039"),]
  m_col_asoc <-     acs_data[acs_data$GEOID==tract_geoid & acs_data$variable %in% c("B15001_008", "B15001_016", "B15001_024", "B15001_032", "B15001_040"),]
  m_col_bach <-     acs_data[acs_data$GEOID==tract_geoid & acs_data$variable %in% c("B15001_009", "B15001_017", "B15001_025", "B15001_033", "B15001_041"),]
  m_col_grad <-     acs_data[acs_data$GEOID==tract_geoid & acs_data$variable %in% c("B15001_010", "B15001_018", "B15001_026", "B15001_034", "B15001_042"),]
  
  f_lt_9gr <-       acs_data[acs_data$GEOID==tract_geoid & acs_data$variable %in% c("B15001_045", "B15001_053", "B15001_061", "B15001_069", "B15001_077"),]
  f_hs_grad_no <-   acs_data[acs_data$GEOID==tract_geoid & acs_data$variable %in% c("B15001_046", "B15001_054", "B15001_062", "B15001_070", "B15001_078"),]
  f_hs_grad_yes <-  acs_data[acs_data$GEOID==tract_geoid & acs_data$variable %in% c("B15001_047", "B15001_055", "B15001_063", "B15001_071", "B15001_079"),]
  f_col_some <-     acs_data[acs_data$GEOID==tract_geoid & acs_data$variable %in% c("B15001_048", "B15001_056", "B15001_064", "B15001_072", "B15001_080"),]
  f_col_asoc <-     acs_data[acs_data$GEOID==tract_geoid & acs_data$variable %in% c("B15001_049", "B15001_057", "B15001_065", "B15001_073", "B15001_081"),]
  f_col_bach <-     acs_data[acs_data$GEOID==tract_geoid & acs_data$variable %in% c("B15001_050", "B15001_058", "B15001_066", "B15001_074", "B15001_082"),]
  f_col_grad <-     acs_data[acs_data$GEOID==tract_geoid & acs_data$variable %in% c("B15001_051", "B15001_059", "B15001_067", "B15001_075", "B15001_083"),]
  
  lt_9gr <- rbind(m_lt_9gr, f_lt_9gr)
  lt_9gr_ays <- (sum(lt_9gr$estimate)/pop_mf) * 7.5
  
  hs_grad_no <- rbind(m_hs_grad_no, f_hs_grad_no)
  hs_grad_no_ays <- (sum(hs_grad_no$estimate)/pop_mf) * 11
  
  hs_grad_yes <- rbind(m_hs_grad_yes, f_hs_grad_yes)
  hs_grad_yes_ays <- (sum(hs_grad_yes$estimate)/pop_mf) * 12
  
  col_some <- rbind(m_col_some, f_col_some)
  col_some_ays <- (sum(col_some$estimate)/pop_mf) * 13
  
  col_asoc <- rbind(m_col_asoc, f_col_asoc)
  col_asoc_ays <- (sum(col_asoc$estimate)/pop_mf) * 14
  
  col_bach <- rbind(m_col_bach, f_col_bach)
  col_bach_ays <- (sum(col_bach$estimate)/pop_mf) * 16
  
  col_grad <- rbind(m_col_grad, f_col_grad)
  col_grad_ays <- (sum(col_grad$estimate)/pop_mf) * 19

  ays <- sum(lt_9gr_ays, hs_grad_no_ays, hs_grad_yes_ays, col_some_ays, col_asoc_ays, col_bach_ays, col_grad_ays)
  ays
}

# Get Table B15001: Sex by Age by Educational Attainment for all Virginia tracts
acsdata <- get_acs(geography = "tract", table = "B15001", state = "VA", year = 2021)


# Calculate AYS for all Virginia tracts
if (exists("dt_all")) rm(dt_all)
unq_tracts <- unique(acsdata$GEOID)
for (t in unq_tracts) {
  print(t)
  ays_t <- get_ays(acsdata, t)
  if (exists("dt_all")) dt_all <- data.table::rbindlist(list(dt_all, data.table::data.table(geoid = t, ays = ays_t)))
  else dt_all <- data.table::data.table(geoid = t, ays = ays_t)
}
# save to file
data.table::fwrite(dt_all, "../test_edu_attain_va.csv")

# Transform to range [0, 1]
mn <- min(dt_all$ays, na.rm = T)
mx <- max(dt_all$ays, na.rm = T)
scaled <- (dt_all$ays - mn)/(mx-mn)
dt_scaled <- data.table::data.table(geoid = dt_all$geoid, ays = dt_all$ays, score = scaled)
# save to file
data.table::fwrite(dt_scaled, "../test_edu_attain_va_scaled.csv")

