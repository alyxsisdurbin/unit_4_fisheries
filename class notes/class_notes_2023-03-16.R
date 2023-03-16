# Haley Durbin
# 2023-03-16

library(tidyverse)

load('data/RAMLDB v4.495/R Data/DBdata[asmt][v4.495].RData') #load function in R will take the R data and populates them into your environemnt bec it is already in an R file
head(area)

glimpse(stock)
glimpse(timeseries_values_views)
glimpse(taxonomy)


fish = timeseries_values_views %>% 
  left_join(stock) %>% #startiung with the longer table and ading the shorter table
  left_join(taxonomy, by="tsn") # need to identify what to combine by to not screw up data
  

#reducing the table to the columns that we want to work with
fish = timeseries_values_views %>% 
  left_join(stock) %>% 
  left_join(taxonomy) %>%
  select(stockid, stocklong, year, TCbest, tsn, scientificname, 
         commonname, region, FisheryType, taxGroup)

glimpse(fish)
dim(timeseries_values_views)
dim(fish) ##samwe number of columbs, but diff number of rows which is good

glimpse(tsmetrics)
tsmetrics %>% 
  filter(tsshort == "TCbest") # tells us its metric tons of fish, not individual fish

fish = fish %>% 
  filter(stockid !="ACADREDGOMGB")

ggplot() +
  geom_line(aes(x=year, y=TCbest, color=stockid), data=fish) +
  theme(legend.position = "none")

fish %>% filter(TCbest > 20000000) # revealed messed up data, an error 
fish %>% filter(TCbest > 6000000)

# plottting the canadian cod stock drop


fish %>% 
  filter(scientificname=="Gadus morhua") %>%
  distinct(region)

cod_can = fish %>%
  filter(scientificname=="Gadus morhua",
         region == "Canada East Coast",
         !is.na(TCbest))
head(cod_can)

ggplot(data=cod_can) +
  geom_line(aes(x=year, y=TCbest, color=stockid))

cod_can_total = cod_can %>%
  group_by(year) %>%
  summarize(total_catch = sum(TCbest))
head(cod_can_total)

ggplot(data=cod_can_total) +
  geom_line(aes(x=year, y=total_catch))

# did the stock fishery collapse

dat = c(1,3,6,2,3,9,-1)
cummax(dat) # cummax() returns the cumulative maximum value
cumsum(dat) # cumsum() returns the cumulative sum value

cod_collapse = cod_can_total %>%
  mutate(historical_max_catch = cummax(total_catch),
         collapse = total_catch <= 0.1*historical_max_catch)
head(cod_collapse)
tail(cod_collapse)

cod_collapse_year = cod_collapse %>%
  filter(collapse == TRUE) %>%
  summarize(year=min(year)) %>%
  pull(year)

ggplot() +
  geom_line(aes(x=year, y=total_catch, color=collapse), data=cod_collapse) + 
  geom_vline(xintercept = cod_collapse_year)

## global stock collapse

collapse = fish %>% 
  filter(!is.na(TCbest)) %>%
  group_by(stockid) %>%
  mutate(historical_max_catch = cummax(TCbest),
         current_collapse = TCbest < 0.10 * historical_max_catch,
         collapsed_yet = cumsum(current_collapse) > 0) %>%
  ungroup()
            