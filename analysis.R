library(tidyverse)
library(ggplot2)
library(usmap)

incarceration_df <- read.csv("incarceration_trends.csv")

incarceration_df[is.na(incarceration_df)] = 0

#Which state incarcerated the most white people (15-64) in 2018

state_most_percentage_white_2018 <- incarceration_df %>% 
  filter(year == 2018) %>% 
  group_by(state) %>% 
  summarise(total_white_pop = sum(white_pop_15to64)) %>%
  filter(total_white_pop == max(total_white_pop)) %>%
  pull(state)

#Which year did the US incarcerate the least percentage of black people (15-64) not 0

year_least_percentage_black_people <- incarceration_df %>% 
  group_by(year) %>% 
  summarise(total_black_pop_percentage = sum(black_pop_15to64)/sum(total_pop_15to64) * 100) %>%
  filter(total_black_pop_percentage != 0) %>% 
  filter(total_black_pop_percentage == min(total_black_pop_percentage)) %>% 
  pull(year)

#Which state had the most change of Latina people (15-64) between 1970 to 2018

state_most_latina_change <- incarceration_df %>%
  filter(year == 2018 | year == 1990) %>%
  group_by(state, year) %>% 
  summarise(.groups = "drop", total_latina = sum(latinx_pop_15to64)) %>%
  group_by(state) %>% 
  summarise(total_latina_change = max(total_latina) - min(total_latina)) %>%
  filter(total_latina_change == max(total_latina_change)) %>% 
  pull(state)

#Which year in California did the least change occur in the population of Native Americans (15-64)

year_state_least_native_change <- incarceration_df %>%
  filter(state == "CA") %>%
  group_by(year) %>% 
  summarise(total_native = sum(native_pop_15to64)) %>% 
  mutate(changes = total_native - lag(total_native, 1)) %>%
  filter(changes == min(na.omit(changes))) %>%
  select(year, changes)

#What is the percent change of percentage of incarcerations of AAPI in 1990 compared to 2018

average_value_appi_over_years <- incarceration_df %>% 
  filter(year == 2018 | year == 1990) %>%
  group_by(year) %>% 
  summarise(percentage = sum(aapi_pop_15to64) / sum(total_pop_15to64)) %>% 
  summarise(percent_change = max(percentage) / min(percentage) * 100) %>% 
  pull(percent_change)
  

#creates a line plot for population of different races over time (1990 to 2018)


plot_time_v_incarceration <- function(){
  plot_aapi_df <- incarceration_df %>% 
    filter(year > 1989) %>% 
    group_by(year) %>% 
    summarise(total_pop = sum(aapi_pop_15to64) / 1000000) %>%
    mutate(race = "AAPI")
  
  plot_black_df <- incarceration_df %>% 
    filter(year > 1989) %>% 
    group_by(year) %>% 
    summarise(total_pop = sum(black_pop_15to64) / 1000000) %>%
    mutate(race = "Black")
  
  plot_native_df <- incarceration_df %>% 
    filter(year > 1989) %>% 
    group_by(year) %>% 
    summarise(total_pop = sum(native_pop_15to64) / 1000000) %>%
    mutate(race = "Native")
  
  plot_latinx_df <- incarceration_df %>% 
    filter(year > 1989) %>% 
    group_by(year) %>% 
    summarise(total_pop = sum(latinx_pop_15to64) / 1000000) %>%
    mutate(race = "Latinx")
  
  plot_white_df <- incarceration_df %>% 
    filter(year > 1989) %>% 
    group_by(year) %>% 
    summarise(total_pop = sum(white_pop_15to64) / 1000000)%>%
    mutate(race = "White")
  
  plot_df <- rbind(plot_aapi_df, rbind(plot_black_df, rbind(plot_native_df, rbind(plot_latinx_df, plot_white_df))))
  
  ggplot(plot_df, aes(x = year, y = total_pop, colour = race)) +
    geom_line(size = 2) +
    labs(y = "Total Pop (millions of people)", x = "Year", title = "Year (1990 - 2018) vs Incarceration (Different Races)") + 
    scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 10))
}

plot_scatter_total_pop_v_total_pop_15_64 <- function(){
  ggplot(incarceration_df, aes(x = total_pop / 1000000, y = total_pop_15to64 / 1000000)) + 
    geom_point() + 
    labs(y = "Total Pop 15 to 64 (millions of people)", x = "Total Pop (millions of people)", 
         title =  "Total Population vs Total Population From 15 to 64")
}

us_map_plot <- function(){
  
  states_incarcerate_2018 <- incarceration_df %>% 
    filter(year == 2018) %>% 
    group_by(state) %>% 
    summarise(states_total_pop = sum(total_pop)/1000000)
  
  plot_usmap(data = states_incarcerate_2018, values = "states_total_pop") + 
    scale_fill_continuous(low = "white", high = "turquoise", name = "Population 2018 (millions of people)") + 
    theme(legend.position = "right") + 
    labs(title = "Total Incarceration by States")
}
