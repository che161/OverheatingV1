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
overheat_result2 <- read_fwf("data/Result2.txt",fwf_cols(Nvalid = 7, UnitNo = 8, 
                                                                           StreetNo = 8, StreetType = 16, StreetName = 48, Suburb = 24, WallType = 20, CZ = 2, BL = 1,SubDir = 97, ScratchName = NA))
overheat_result2 %>% group_by(WallType) %>% summarise(num_rows = n()) %>% write_csv("res/NT_Construction.csv")

#Challenge 20190925
library(tidyverse) #installed.package("tidyverse")
read_csv("data/gapminder.csv")
gapminder <- read_csv("data/gapminder.csv") # Read in data
gapminder
gapminder %>% filter(year == 1957) %>%
  group_by(continent) %>%
  summarise(max_gdpPerCap = max(gdpPercap)) %>% # count the number in each country
  write_csv("res/ContinentMaxgdpPercap.csv")

#Combine rows
gapminder_2012 <- read_csv("data/gapminder_2012.csv") # Read in data
gapminder_2012
?bind_rows()
gapminder_all <- bind_rows(gapminder,gapminder_2012)
gapminder_all
bind_rows(gapminder,gapminder_2012, .id = "country")

rename_2012 <- rename(gapminder_2012, population = pop)
rename_2012
mismatched_names <- bind_rows(gapminder,rename_2012)
mismatched_names
tail(mismatched_names)

#joins lesson
?join
example_vector <- c(1,4,2,7)
example_vector
string_vector <- c('hello', 'this', 'is', 'vector')
string_vector
example_vector[3]
resul <- inner_join(gapminder,rename_2012)
resul
df1 <- tibble(sample = c(1,2,3), measure1 = c(4.2,5.3,6.1))
df1
df2 <- tibble(sample = c(1,3,4), measure2 = c(7.8,6.4,9.0))
df2

inner_join(df1,df2)
full_join(df1,df2)
left_join(df1,df2)

df3 <- tibble(ID = c(1,2,4), measure3 = c(4.7,34,2.6))
full_join(df1,df3, by = c("sample" = "ID"))
full_join(df1,df2, by = c("sample"))
full_join(df1,df3, by = c("sample" = "ID", "measure1" = "measure3"))
left_join(df1,df3, by = c("sample" = "ID", "measure1" = "measure3"))
inner_join(df1,df3, by = c("sample" = "ID", "measure1" = "measure3"))
?nest_join
storms
?bind_rows
 
#tidy up data
cows <- tibble(id = c(1,2,3), weight1 = c(203,227,193), weight2 = c(365,344,329))

#gather and spread

cows_tidy <-  gather(cows,rep, weight, -id)

cows_tidy %>% arrange(id)

spread(cows_tidy, rep, weight)

read_csv("data/ALL-WINDOWS_2.3.3.13.8.2.csv", col_names = FALSE )

spread(cows_tidy, key = rep, value = weight)
table4a
table4a <-  gather(table4a, year, val, -country)
table4a <-  spread(table4a, year, val)
table4a

table2
spread(table2,type,count)
cows
cow_with_breed <- cows %>% mutate(id = c("1_A", "2_A", "3_B"))
cow_with_breed
separate(cow_with_breed, col=id, into = c("ID", "breed"), sep = "_")
separate(cow_with_breed, col=id, into = c("ID", "breed"), "_")
