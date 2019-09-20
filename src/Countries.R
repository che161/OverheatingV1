#installed.package("tidyverse")
library(tidyverse)
read_csv("data/gapminder.csv")
gapminder <- read_csv("data/gapminder.csv")
nrow(gapminder)
ncol(gapminder)
colnames(gapminder)
glimpse(gapminder)
ncol(storms)
nrow(storms)
glimpse(storms)
storms
read_csv("data/Result2_explain.csv")
overheat <- read_csv("data/Result2_explain.csv")
nrow(overheat)
ncol(overheat)
overheat
glimpse(overheat)
select(gapminder,year,country,pop)
select(gapminder,3,1,5)
select(gapminder,3:5)
select(overheat,-X8)
select(gapminder,-(3:5))
justpopulation <- select(gapminder,year,country,pop)
justpopulation 
glimpse(gapminder)
select(gapminder,country,year,pop,gdpPercap)
select(gapminder,1,3,5,6)
select(gapminder,-2,-4)
select(gapminder,starts_with("co"))
select(gapminder,contains("e"))
select(gapminder,ends_with("p"))
select(gapminder,population = pop)
rename(gapminder, population = pop)
filter(gapminder, country == "Australia")
filter(gapminder, year >= 1997)
filter(gapminder, continent == "Europe")
filter(gapminder, lifeExp >= 80, gdpPercap >= 30000)
filter(gapminder, lifeExp >= 80 | gdpPercap >= 30000)
gapminder <- read_csv("data/gapminder.csv")
filter(gapminder, country == "Australia")
gapminder_Aus <- filter(gapminder, country == "Australia")
glimpse(gapminder_Aus)
select(gapminder_Aus,country,year,pop)
select(filter(gapminder, country == "Australia"), country,year,pop)
select(filter(gapminder, country == "Australia"), country,year,pop)
filter(select(gapminder,country,year,pop), country == "Australia")

#pipe
gapminder %>% select(year,pop)
gapminder %>%  filter(country == "Australia", year>= 1997)
small <- gapminder %>%  filter(country == "Australia", year>= 1997)
glimpse(small)
small
newgap <-  gapminder %>%  
  filter(country == "Australia") %>%  
  select(country,year,pop) %>%
  rename(population = pop)
newgap

#mutate
mutate(gapminder, gdp = gdpPercap*pop)
mutate(gapminder, popInMillion = pop/1e6)
mutate(gapminder, Log_pop = log(pop))
str_sub("A long bit of text", start = 1, end =5)
mutate(gapminder,coutry_abbr = str_sub(country, start = 1, end = 3))
mutate(gapminder,coutry_namelength = str_length(country))
mutate(gapminder,gdp = gdpPercap*pop,Log_pop = log(pop),coutry_namelength = str_length(country))
mutate(
  gapminder,
  gdp = gdpPercap*pop,
  Log_pop =  log(pop)
)
mutate(
  gapminder,
  lifeExp_days = lifeExp*365,
  Gdp_bil =  gdpPercap*pop/1e9
)

#Summarising data
summarise(gapminder, mean_life_exp = mean(lifeExp))
summarise(gapminder, mean_life_exp = mean(lifeExp), mean_life_sd = sd(lifeExp), bigggest_gdp = max(gdpPercap))
summarise(gapminder, mean_life_exp = mean(pop), median_life_sd = median(pop))
summarise_if(gapminder, is.numeric, mean)

#group_by
group_by(gapminder, country)
by_country <- group_by(gapminder, country)
by_country
summarise(by_country, mean_life_exp = mean(pop), median_life_sd = median(pop))
by_continent <- group_by(gapminder, continent)
by_continent 
summarise(by_continent, mean_life_exp = mean(pop), median_life_sd = median(pop))
gapminder %>% group_by(continent) %>% 
  summarise(mean(pop), median_life_sd = median(pop))
