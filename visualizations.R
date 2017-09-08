# visualizations

# step 1 - read in combined events
# step 2 - create boxplots based on forecasts and weather
# step 3 - GDP by Damage over years?


boxplot(GHCND.prcp_cat ~ Q24.f2, data = combined_events)



library(ggplot2)
# showing 24h 3-day forecast 
plotcast <- ggplot(storm_events_14, aes(x = GHCND.prcp_cat, y = Q24.f2)) + 
  geom_boxplot() + scale_y_continuous(name = "3-Day, 24h QPF Category") +
  scale_x_discrete(limits = c(1:6), name = "Observed Precipitation Categories") + 
  ggtitle("2014, Forecasted Precipitation vs. Observed") + geom_segment(aes(x=0,y=0, xend=7, yend=7))

plotcast # Warning message: Removed 148 rows containing non-finite values (stat_boxplot). 
# need to figure out how to count category 0 forecasts and observed. since it still might be some precip.

storm_events <- combined_events %>% select(EVENTS.czname, EVENTS.state, EVENTS.year, 
                                           EVENTS.damage_value, EVENTS.damage_magnitude, GHCND.prcp_cat,
                                           Q24.f2, Q24.f6)
storm_events <- merge(storm_events, gdp_msa_counties, 
                      by.x = c("EVENTS.czname","EVENTS.state","EVENTS.year"), 
                      by.y = c("cnty","state.full", "YEAR"))


# install.packages("tigris")
library(tigris)
cbsa <- core_based_statistical_areas()
storm_map <- geo_join(cbsa, storm_events, "CBSAFP", "CBSA.code", how = "inner")
# install.packages("tmap")
library(tmap)
# this is wrong. need to convert storm damages into real values first
# dammit brian! 
# basically take magnitude and use that as the number of zeros behind the number in the value column.
qtm(storm_map, fill = storm_map$EVENTS.damage_value)
tm_shape(storm_map) + 
  tm_polygons("EVENTS.damage_value", id = "CBSA.code")
