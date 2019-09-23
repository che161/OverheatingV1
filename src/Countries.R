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

#arrange
arrange(gapminder, gdpPercap)
arrange(gapminder, desc(gdpPercap))
Country_life_Exp <- summarise(by_country, mean_life_exp = mean(lifeExp))

arrange(Country_life_Exp, mean_life_exp) %>% 
  filter( mean_life_exp == min(mean_life_exp))
arrange(Country_life_Exp, desc(mean_life_exp)) %>% 
  filter( mean_life_exp == max(mean_life_exp))
by_continent <- group_by(gapminder, continent, year)

arrange(Country_life_Exp, mean_life_exp) %>% 
  filter( mean_life_exp == min(mean_life_exp ) | mean_life_exp == max(mean_life_exp))


by_continent 
summarised_gdp <- summarise(by_continent, mean_gdp_per_cap = mean(gdpPercap))
summarised_gdp
arrange(summarised_gdp,desc(year),desc(mean_gdp_per_cap))
arrange(summarised_gdp,desc(mean_gdp_per_cap),desc(year))


summarise(gapminder, num_rows = n())

COUNTS <- summarise(by_country, num_rows = n())

view(COUNTS)

summarise(by_country, num_rows = n()) %>% view()

#Challenge
#installed.package("tidyverse")
library(tidyverse)
read_csv("data/gapminder.csv")
gapminder <- read_csv("data/gapminder.csv")
gapminder %>% filter(year == 1972) %>% 
  arrange(desc(pop))
#  row_number(6)

# read in excel files
library(readxl)
read_excel("data/gapminder.xlsx")
read_excel("data/gapminder.xlsx", range = "A1:E4")
read_excel("data/gapminder.xlsx", sheet = "gapminder")

# writing data out
by_country
write_csv(by_country,"res/By_Country.csv")
gapminder %>% filter(country == "Australia") %>% 
  write_csv("res/By_Country.csv")

#Challenge
library(tidyverse) #installed.package("tidyverse")
read_csv("data/gapminder.csv")
gapminder <- read_csv("data/gapminder.csv") # Read in data
gapV1 <- filter(gapminder,year == 1987 | year == 2007) # Keep only the data for 1987 and 2007
gapV1_group <-  group_by(gapV1,year) #group by year
gapv4 <- arrange(gapV1_group, year, desc(lifeExp)) # sorting the lifeExp for each year
gapv5 <- top_n(gapv4,10,lifeExp) # keep only the top 10 lifeExp for each year
gapv6 <- group_by(gapv5,country) #group by country
gapv7 <- summarise(gapv6,num_rows = n()) # count the number in each country
filter(gapv7,num_rows == 2) # only countries appear twice


#Challenge
library(tidyverse) #installed.package("tidyverse")
read_csv("data/gapminder.csv")
gapminder <- read_csv("data/gapminder.csv") # Read in data
gapminder %>%  filter(year == 1987 | year == 2007) %>% # Keep only the data for 1987 and 2007
    group_by(year) %>% #group by year
    arrange(year, desc(lifeExp)) %>% # sorting the lifeExp for each year
    top_n(10,lifeExp) %>% # keep only the top 10 lifeExp for each year
    group_by(country) %>% #group by country
    summarise(num_rows = n()) %>% # count the number in each country
    filter(num_rows == 2) %>% # only countries appear twice
    select(country) %>% 
    write_csv("res/To10_Countries.csv")

library(tidyverse)
read_table("data/Result2_cutdown.txt")
?read_fwf
read_fwf("data/Result2_cutdown.txt",fwf_cols(Nvalid = 7, UnitNo = 8, StreetNo = 8, StreetType = 16, StreetName = 48, Suburb = 24, WallType = 20, SubDir = 100, ScratchName = 110))
read_fwf("data/Result2_Explain.txt",skip = 1,fwf_cols(Nvalid = 7, UnitNo = 8, StreetNo = 8, 
                                                      StreetType = 16, StreetName = 48, Suburb = 24, WallType = 20,  CZ = 2, SubDir = 98, ScratchName = NA))
overheat_result2 <- read_fwf("data/Result2_Explain.txt",skip = 1, fwf_cols(Nvalid = 7, UnitNo = 8, 
                          StreetNo = 8, StreetType = 16, StreetName = 48, Suburb = 24, WallType = 20, CZ = 2, BL = 1,SubDir = 97, ScratchName = NA))
overheat_result2 %>% group_by(WallType) %>% summarise(num_rows = n()) %>% write_csv("res/NSW_Construction.csv")
