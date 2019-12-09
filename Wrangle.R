library(tidyverse)
library(readr)
library(rworldmap)


#datasets
park_visits <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-09-17/national_parks.csv")
state_pop <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-09-17/state_pop.csv")
gas_price <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-09-17/gas_price.csv")
locations <- readr::read_csv("/cloud/project/location.csv")
usa1 = map_data("state")

#force LAT and LONG into a numeric

locations$LONG <- as.numeric(locations$LONG)
locations$LAT <- as.numeric(locations$LAT)



#join datasets
park_visits$year <- as.numeric(as.character(park_visits$year))
park_visits <- park_visits %>% left_join(gas_price) 
park_visits <- park_visits %>% left_join(state_pop)
park_visits <- park_visits %>% left_join(locations)

#create year-over-year data
park_visits_yoy <- park_visits %>% 
  select(year, visitors) %>% 
  filter(year != "Total") %>%
  filter(year >1949)%>%
  mutate(year = as.integer(year)) %>%
  arrange(year) %>%
  group_by(year) %>%
  summarise(visitors_total = sum(visitors))

state_pop_yoy <- state_pop %>% 
  select(year, pop) %>% 
  filter(year != "Total") %>%
  filter(year >1949)%>%
  mutate(year = as.integer(year)) %>%
  arrange(year) %>%
  group_by(year) %>%
  summarise(pop_total = sum(pop))

park_visits_yoy <- park_visits_yoy %>% left_join(state_pop_yoy)
park_visits_yoy <- park_visits_yoy %>% left_join(gas_price)

park_visits_yoy <- park_visits_yoy %>% mutate(yoyvisits = visitors_total - lag(visitors_total))
park_visits_yoy <- park_visits_yoy %>% mutate(yoygas = gas_constant - lag(gas_constant))
park_visits_yoy <- park_visits_yoy %>% mutate(yoypop = pop_total - lag(pop_total))
park_visits_yoy <- park_visits_yoy %>% mutate(yoypopper = ((pop_total - lag(pop_total))/lag(pop_total)))
park_visits_yoy <- park_visits_yoy %>% mutate(yoygasper = ((gas_constant - lag(gas_constant))/lag(gas_constant)))
park_visits_yoy <- park_visits_yoy %>% mutate(yoyvisitsper = ((visitors_total - lag(visitors_total))/lag(visitors_total)))
park_visits_yoy <- park_visits_yoy %>% mutate(visitpopper = ((visitors_total/pop_total)))


#tidy park data
map_data <- park_visits %>%
  select(year, visitors, LONG, LAT, unit_type, Name, state, region) %>% 
  filter(unit_type == "National Park")%>%
  filter(year>1949)%>%
  filter(!is.na(LAT))%>%
  filter(!is.na(LONG))%>%
  filter(!is.na(Name))


park_visits_yoy["51", "yoypopper"] <- 0.0084733836

