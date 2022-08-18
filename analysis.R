library("maps")
library("tidyverse")
library("ggplot2")

incarceration_data <- read.csv("incarceration_trends.csv")

# Select the columns to analyze
filtered_columns <- select(incarceration_data, year, state, county_name, aapi_jail_pop, black_jail_pop, latinx_jail_pop, native_jail_pop, white_jail_pop, other_race_jail_pop)
remove_NAs <- filter(filtered_columns, !is.na(aapi_jail_pop) | !is.na(black_jail_pop) | !is.na(latinx_jail_pop) | !is.na(white_jail_pop) | !is.na(native_jail_pop) | !is.na(other_race_jail_pop))
remove_zeros <- filter(filtered_columns, aapi_jail_pop != 0 | black_jail_pop != 0 | latinx_jail_pop != 0 | white_jail_pop != 0 | native_jail_pop != 0 | other_race_jail_pop != 0)

# Function that calculates the total jail population for each race in a given year

jail_population_by_year <- function(specified_year) {
  
  year_filtered <- filter(filtered_columns, year == specified_year)

  total_pop_in_year <- year_filtered %>% group_by(year) %>% summarise(total_aapi = sum(aapi_jail_pop, na.rm = TRUE), 
                                                 total_black = sum(black_jail_pop, na.rm = TRUE), 
                                                 total_latinx = sum(latinx_jail_pop, na.rm = TRUE), 
                                                 total_native = sum(native_jail_pop, na.rm = TRUE),
                                                 total_white = sum(white_jail_pop, na.rm = TRUE), 
                                                 total_other= sum(other_race_jail_pop, na.rm = TRUE))
  return(total_pop_in_year)
}


# Total black jail population in 2018: 247,547
total_in_2018 <- jail_population_by_year("2018")
black_total_2018 <- first(select(total_in_2018, total_black))

# Total white jail population in 2018: 346,062
white_total_2018 <- first(select(total_in_2018, total_white))


# Total black jail population in 1990: 86,947
total_in_1985 <- jail_population_by_year("1985")
black_total_1985 <- first(select(total_in_1985, total_black))

# Total white jail population in 1990: 89,248
white_total_1985 <- first(select(total_in_1985, total_white))



# Compare jail population rates for Black and White populations
filtered_rate_columns <- select(incarceration_data, year, state, county_name, black_jail_pop_rate, white_jail_pop_rate)
rates_remove_NAs <- filter(filtered_rate_columns, !is.na(black_jail_pop_rate) | !is.na(white_jail_pop_rate))

total_rates_in_year <- function(specified_year){
  
  year_filtered <- filter(filtered_rate_columns, year == specified_year)
  
  avg_of_rates <- year_filtered %>% summarise(avg_black_rate = mean(black_jail_pop_rate, na.rm=TRUE), avg_white_rate = mean(white_jail_pop_rate, na.rm = TRUE))
  return(avg_of_rates)
  
}

# National rate in 2018 for Black population per 100,000: 2519
rates_in_2018 <- total_rates_in_year("2018")
black_2018_rate <- first(select(rates_in_2018, avg_black_rate))

# National rate in 2018 for White population per 100,000: 498
white_2018_rate <- first(select(rates_in_2018, avg_white_rate))


# National rate in 1990 for Black population per 100,000: 4041
rates_in_1990 <- total_rates_in_year("1990")
black_1990_rate <- first(select(rates_in_1990, avg_black_rate))

# National rate in 1990 for White population per 100,000: 145
white_1990_rate <- first(select(rates_in_1990, avg_white_rate))


#------------------------------------------------------------------------------------------
# Trends chart

total_pop_by_year <- remove_zeros %>% group_by(year) %>% summarise(total_aapi = sum(aapi_jail_pop, na.rm = TRUE), 
                                                                    total_black = sum(black_jail_pop, na.rm = TRUE), 
                                                                    total_latinx = sum(latinx_jail_pop, na.rm = TRUE), 
                                                                    total_native = sum(native_jail_pop, na.rm = TRUE),
                                                                    total_white = sum(white_jail_pop, na.rm = TRUE), 
                                                                    total_other= sum(other_race_jail_pop, na.rm = TRUE))

df <- total_pop_by_year %>%
  select(year, total_aapi, total_black, total_latinx, total_native, total_white, total_other) %>%
  gather(key = "race", value = "population", -year)

population_trends <- ggplot(df, aes(x = year, y = population, group = race, color = race)) + 
  geom_line() +
  xlab("Year") + 
  ylab("Total Jail Population")
  

#------------------------------------------------------------------------------------------
# Variable Comparison Chart

filtered_rates <- select(incarceration_data, year, black_jail_pop_rate, white_jail_pop_rate)

avg_of_rates <- filtered_rates %>% group_by(year) %>% summarise(avg_black_rate = mean(black_jail_pop_rate, na.rm=TRUE), 
                                            avg_white_rate = mean(white_jail_pop_rate, na.rm = TRUE)) %>% filter(year >= "1990")


rates_df <- avg_of_rates %>% 
  gather(key = "Race", value = "rate" , -year)

comparison_chart <- ggplot(data = rates_df) +
  geom_bar(aes(x = year,
               y = rate,
               fill = Race,
               color = Race),
           stat = "identity",
           position = position_dodge()) +
  scale_x_continuous(n.breaks = 20) +
  xlab("Year") + 
  ylab("Average Rate")


#------------------------------------------------------------------------------------------
# Map

rates <- select(incarceration_data, fips, year, county_name, black_jail_pop_rate, white_jail_pop_rate)

counties_rate_diff_2018 <- rates %>%
                           filter(year == "2018", !is.na(white_jail_pop_rate) | !is.na(black_jail_pop_rate)) %>%
                           summarize(fips = fips, rates_diff = black_jail_pop_rate - white_jail_pop_rate)



state_shape <- map_data("county") %>%
  unite(polyname, region, subregion, sep = ",") %>%
  left_join(county.fips, by = "polyname")

state_shape_with_data <- state_shape %>% left_join(counties_rate_diff_2018, by="fips")

blank_theme <- theme_bw() +
  theme(
    axis.line = element_blank(),        # remove axis lines
    axis.text = element_blank(),        # remove axis labels
    axis.ticks = element_blank(),       # remove axis ticks
    axis.title = element_blank(),       # remove axis titles
    plot.background = element_blank(),  # remove gray background
    panel.grid.major = element_blank(), # remove major grid lines
    panel.grid.minor = element_blank(), # remove minor grid lines
    panel.border = element_blank()      # remove border around plot
  )


incarcerations_rates_map <- ggplot(state_shape_with_data) +
  geom_polygon(
    mapping = aes(x = long, y = lat, group = group, fill = rates_diff),
    color = "white",
    size = .1        
  ) +
  coord_map() + 
  scale_fill_continuous(low = "White", high = "Blue") +
  labs(fill = "Incarceration Rate Difference") +
  blank_theme
  
  





