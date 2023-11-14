library(tidyverse)
library(tidyquant)
library(janitor)
library(lubridate)
library(reshape2)
library(ggthemes)
library(dplyr)
# specific colors I want in my graphs later
# roughly, blue,red,purple,green 
color_set =c("#1010eb","#b84240","#806bb5","#17b024")

# bringing in csv files
core_priceIndexes <- read.csv("fredgraph_threePriceIndexes.csv"
                         , TRUE,
                         ",")

headline_df <- read.csv("headlinePriceIndexes.csv"
                        ,TRUE,",")

# inner-join core and headline dataframes into one total dataframe

all_priceIndexes <- inner_join(core_priceIndexes, headline_df,by = "DATE")

# make a date object out of the col header, "DATE"

all_priceIndexes$date <- mdy(all_priceIndexes$DATE)
# drop DATE column as named from original CSV
all_priceIndexes$DATE <-NULL
# move new_date col to the front(in position 1)
all_priceIndexes <- all_priceIndexes %>% 
  select(date,everything())

# an attempt to find the year over year infl rate of each price index
# without constantly calling mutate on each
# indv. price index column
all_priceIndexes <- all_priceIndexes %>% 
  mutate(across(contains("_Core") |contains("headline"), 
                list(inflation = ~ (./lag(., 12) - 1) * 100 
                     ),
                .names = "{.col}_{.fn}")
         ) %>%
  view()

# filter data so that we only have inflation cols and date

infl_data <- all_priceIndexes %>%
  filter(date >= "1958-01-01") %>% 
  select(date,ends_with("_inflation")) %>% 
  view()

# although our "wide" data was good for inspecting df,
# we need an easier way to plot our indexes w/o simply
# adding additional lines to ggplot

# let's reshape our data to a "long" view
infl_melt <- melt(infl_data, id="date",value.name = "YoY_Inflation", variable.name = "Price_Index")


# creating a line plot but with more recent data,
# say, since 2019

infl_melt %>%
  mutate(isCore =ifelse(str_detect(Price_Index, "Core"), "Yes", "No")) %>% 
  filter(date >= '2019-01-01') %>% 
  ggplot(aes(y = YoY_Inflation, x = date, color = Price_Index, group = Price_Index))+
  geom_line(aes(linetype =isCore),alpha = 1, size = .4)+
  # not sure if there is a better way to colorcode
  scale_color_manual(values =c(
    "CPI_Core_inflation" = "#1010eb",
    "headline_CPI_inflation" = "#1010eb",
    "PCECI_Core_inflation" ="#b84240",
    "headline_PCE_inflation"="#b84240",
    "PPIFES_Core_inflation" ="#17b024",
    "headline_PPIFS_inflation" ="#17b024")
  )+
  labs(title = "U.S. Inflation; Index:Jan 1 2012 = 100",
       subtitle ="How does the rate of inflation vary by Price Index?",
       x = "Month-Year", 
       y = "Percentage Change from Year Ago") +
  scale_x_date(date_labels = "%m-%Y", date_breaks = "1 year")+
  theme_fivethirtyeight()+
  theme(axis.title = element_text())+
  # so that headline is dashed; core is solid
  scale_linetype_manual(values = c("dashed", "solid"))
  
# need to do:
#1) fix scaling- x values are cramped
#2) Groups information is not easy to read


# Part II: explore relationship between inflation and unemp rate
# https://socialsci.libretexts.org/Bookshelves/Economics/Economics_(Boundless)/23%3A_Inflation_and_Unemployment/23.1%3A_The_Relationship_Between_Inflation_and_Unemployment


# get unemployment data
UMP_series <- "UNRATE" %>% 
  tq_get(get = "economic.data", from = "1958-01-01")

# rename variables for readability
UMP_series <- rename(UMP_series,c("series" = "symbol", "UEMP_rate" = "price"))

# drop Series column as named from UMP_series
UMP_series$series <-NULL

# merge infl_data and UMP_series
infl_data <- left_join(infl_data,UMP_series, by = "date")

# melt again- proabaly can just go back and do this to the original melted frame
melt2 <- melt(infl_data,
              id=c("date","UEMP_rate"),
              value.name = "YoY_Inflation",
              variable.name = "Price_Index")

# plot Phillips Curve:
# for practice- only plots curve in terms of inflation from Core CPI
#infl_data %>%
 #filter(date >= '2019-01-01') %>% 
  #ggplot(aes(x = UEMP_rate, y = CPI_Core_inflation))+
  #geom_point(size = 3, alpha = 1)+
  #geom_smooth(se = F, method = "lm")+
  #theme_fivethirtyeight()


# Phillips Curve for each measure of inflation
# for practice, can be ignored now.
#melt2 %>% 
 # filter(date >= '2019-01-01') %>% 
  #ggplot(aes(x = UEMP_rate, y = YoY_Inflation, color = Price_Index, group = Price_Index))+
  #facet_wrap(~Price_Index)+
  #geom_point(size = 3, alpha = 1)+
  #geom_smooth(se = F, method = "lm")+
  #theme_fivethirtyeight()


# Phillips curve for each measure of inflation
# a line of best fit is created per decade in each graph
melt2 %>% 
  mutate(decade = paste0(10*as.numeric(substr(year(date), 1, 3)), "s")) %>% 
  view() %>% 
  drop_na() %>% 
  ggplot(aes(x = UEMP_rate, y = YoY_Inflation))+
  facet_wrap(~Price_Index)+
  geom_point(aes(color = decade), size = 3, alpha = .5)+
  geom_smooth(se = F, method = "lm", aes(color = decade))+
  scale_color_brewer(palette = "Set1")+
  theme_fivethirtyeight()

  


