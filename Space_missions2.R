library(ggplot2)
library(plotly)
library(gapminder)
library(baseline)
library(plyr)
library(stringr)
library(dplyr)
library(lubridate)
library(magrittr)
library(forcats)
library(data.table)


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

loc <- Space_missions%>%
  select(country, Location)%>%
  filter(country %in% c("Ocean", "Sea", "Facility", "Site"))

View(loc)

Space_missions <-
  Space_missions %>% mutate(
    country = case_when(
      Location == "LP Odyssey, Kiritimati Launch Area, Pacific Ocean" ~ "Pacific Ocean",
      Location == "LP-41, Kauai, Pacific Missile Range Facility" ~ "Range Facility",
      Location == "K-84 Submarine, Barents Sea Launch Area, Barents Sea" |
        # OR
        Location == "K-496 Submarine, Barents Sea Launch Area, Barents Sea" |
        # OR
        Location == "K-407 Submarine, Barents Sea Launch Area, Barents Sea" ~ "Barents Sea",
      Location == "Tai Rui Barge, Yellow Sea" ~ "Yellow Sea",
      Location == "Launch Plateform, Shahrud Missile Test Site" ~ "Shahrud Missile Test Site",
      Location == "Rocket Lab LC-1A, M?Â\u0081hia Peninsula, New Zealand" ~ "New Zealand",
      
      TRUE ~  word(Location, -1)
    )
  )


Space_missions <- Space_missions %>% 
  mutate(
    country = str_replace(country, "StatusRetired", replacement = "USA"),
    country = str_replace(country, "Yellow Sea", replacement = "China"),
    country = str_replace(country, "Russia", replacement = "Russian Federation"),
    country = str_replace(country, "Shahrud Missile Test Site", replacement = "Iran"),
    country = str_replace(country, "Range Facility", replacement = "USA"),
    country = str_replace(country, "Barents Sea", replacement = "Russia"),
    country = str_replace(country, "Canaria", replacement = "USA")
  ) 

Space_missions %>% count(country, sort = T)


Space_missions %>%
  select(country,Status.Mission)%>%
  group_by(country,Status.Mission) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  ggplot(aes(x = fct_reorder(country, -count), y = count, fill = Status.Mission)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Top 20 Companies",
    subtitle = "Column plot, Top 20 Companiess",
    caption = "Kaggle: All Space Missions from 1957",
    x = "Country name",
    y = "Number of launches"
  )


Space_missions %>%
  group_by(country) %>%
  summarise(count = n()) %>%
  group_by(country) %>%
  summarise(
    count_total = sum(count) # Calculate the total count for each country
  ) %>%
  arrange(desc(count_total)) %>% # Arrange countries by total count in descending order
  ggplot(aes(
    x = fct_reorder(country, -count_total), # Reorder countries by total number of launches
    y = count_total
  )) +
  geom_col() +
  labs(
    title = "Top 20 Companies",
    subtitle = "Column plot, Top 20 Companiess",
    caption = "Kaggle: All Space Missions from 1957",
    x = "Country name",
    y = "Number of launches"
  )


Space_missions %>%
  count(country, Company.Name, sort = TRUE) %>%
  arrange(desc(n))

Space_missions %>%
  filter(country == "Russian Federation") %>%
  count(Company.Name, sort = TRUE) %>%
  arrange(desc(n))

Space_missions %>%
  filter(country == "USA") %>%
  count(Company.Name, sort = TRUE) %>%
  arrange(desc(n))

Space_missions %>%
  filter(country == "China") %>%
  count(Company.Name, sort = TRUE) %>%
  arrange(desc(n))

Space_missions %>%
  filter(country == "USA", country == "Russian Federation", country == "China")%>%
  group_by(country, Company.Name) %>%
  summarise(n = n()) %>%
  group_by(country) %>%
  summarise(total = sum(n)) %>%
  top_n(5, total) %>%
  inner_join(Space_missions, by = "country") %>%
  group_by(country, Company.Name) %>%
  summarise(n = n()) %>%
  ggplot(aes(x = reorder(Company.Name, n), y = n)) +
  geom_col() +
  geom_text(aes(label = n), vjust = -0.5, size = 3) +
  theme_bw() +
  facet_wrap(~country, ncol = 2, scales = "free_x") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


Space_missions %>%
  filter(country %in% c("USA", "Russian Federation", "China")) %>%
  group_by(country, Company.Name) %>%
  summarise(n = n()) %>%
  group_by(country) %>%
  summarise(total = sum(n)) %>%
  top_n(5, total) %>%
  inner_join(Space_missions, by = "country") %>%
  group_by(country, Company.Name) %>%
  summarise(n = n()) %>%
  ggplot(aes(x = reorder(Company.Name, n), y = n)) +
  geom_col() +
  geom_text(aes(label = n), vjust = -0.5, size = 3) +
  theme_bw() +
  facet_wrap(~country, ncol = 2, scales = "free_x") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


Space_missions %>%
  filter(country %in% c("USA", "Russian Federation", "China")) %>%
  group_by(country, Company.Name) %>%
  summarise(n = n()) %>%
  group_by(country) %>%
  summarise(total = sum(n)) %>%
  top_n(5, total) %>%
  inner_join(Space_missions, by = "country") %>%
  group_by(country, Company.Name) %>%
  summarise(n = n()) %>%
  ggplot(aes(x = reorder(Company.Name, n), y = n, fill = n > 150)) +
  geom_col() +
  geom_text(aes(label = n), vjust = -0.5, size = 3) +
  facet_wrap(~country, ncol = 2, scales = "free_x") +
  scale_fill_manual(values = c("FALSE" = "white", "TRUE" = "red")) +
  theme(
    panel.background = element_rect(fill = "gray95", colour = NA)
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

