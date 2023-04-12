library(ggplot2)
library(plotly)
library(gapminder)
library(baseline)
library(plyr)
library(stringr)
library(dplyr)
library(lubridate)
library(data.table)
library(magrittr)


Space_missions <- read.csv("C:/Users/ohagg/OneDrive - University College London/Desktop/Github/Space_missions/Space_Corrected.csv")
head(Space_missions)
str(Space_missions)
View(Space_missions)
Space_missions <- Space_missions %>% select(-c(X))
View(Space_missions)


Space_missions <- Space_missions %>% 
  mutate(launch_date = as_date(parse_date_time(Datum, c("mdy HM", "mdy"), tz = "UTC")))

Space_missions <- Space_missions %>%
  rename(row_names=Unnamed..0)

summary(Space_missions)
sapply(Space_missions, function(x) sum(is.na(x)))

Space_missions <- Space_missions %>% 
  mutate(country =  word(Location,-1))

Space_missions %>% count(country, sort =T)

datatable(
  Space_missions %>% select(country, Location) %>%
    filter(country %in% c("Ocean", "Sea", "Facility", "Site")),
  options = list(columnDefs = list(list(className = 'dt-center')),
                 pageLength = 10)
)

library(data.table)

datatable(
  Space_missions %>% 
    select(country, Location) %>%
    filter(country %in% c("Ocean", "Sea", "Facility", "Site")),
  options = list(columnDefs = list(list(className = 'dt-center'))),
  extensions = c('Buttons', 'ColReorder', 'FixedHeader'),
  rownames = FALSE,
  class = 'cell-border stripe',
  width = '100%'
)

loc <- Space_missions%>%
  select(country, Location)%>%
  filter(country %in% c("Ocean", "Sea", "Facility", "Site"))
View(loc)
