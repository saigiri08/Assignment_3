install.packages("ggplot2")
library("ggplot2")
install.packages("dplyr")
library("dplyr")
install.packages("stringr")
library("stringr")
install.packages("tidyverse")
library("tidyverse")


Storm_Dataset_2008 <- read.csv("C:/Users/91738/Downloads/StormEvents_data_2008.csv")

head(Storm_Dataset_2008)

#Limit the dataframe to the limited columns
limit_storm_data <- Storm_Dataset_2008 %>% select(BEGIN_YEARMONTH, EPISODE_ID, STATE, STATE_FIPS, CZ_NAME, CZ_TYPE, CZ_FIPS, EVENT_TYPE)


#Arrange the data by the state name (STATE)
arrange_Data <- limit_storm_data %>% arrange(STATE)


#Change state and county names to title case (e.g., “New Jersey” instead of “NEW JERSEY”)
change_data <- arrange_Data %>% mutate(STATE = str_to_title(STATE), CZ_NAME = str_to_title(CZ_NAME))

#Limit to the events listed by county FIPS (CZ_TYPE of “C”) and then remove the CZ_TYPE column
remove_column <- change_data %>% filter(CZ_TYPE == "C") %>% select(-CZ_TYPE)

#Pad the state and county FIPS with a “0” at the beginning
pad_dataset <- remove_column %>% mutate(STATE_FIPS = str_pad(STATE_FIPS, width = 2,side = "left", pad = "0"), 
                                     CZ_FIPS = str_pad(CZ_FIPS, width = 3, pad = "0")) %>% 
 unite(FIPS, STATE_FIPS, CZ_FIPS, sep = "")

#Change all the column names to lower case
rename_data_lower <- pad_dataset %>% rename_all(tolower)

#There is data that comes with base R on U.S. states
data("state")

US_state_data <- data.frame(state_name = state.name, area = state.area, region = state.region)

year <- 2008 

#Filtering given year from all data
filter_year_2008 <- pad_dataset %>% filter(substr(BEGIN_YEARMONTH, 1, 4) == year)

#Create a dataframe with the number of events per state in the year of your birth
events_per_state_in_2008 <- filter_year_2008 %>% group_by(STATE) %>% summarise(events = n())

#Merge in the state information dataframe
merge_state <- merge(events_per_state_in_2008, US_state_data, by.x = "STATE", by.y = "state_name", all.x = TRUE)

remove_state <- merge_state [complete.cases(merge_state), ]

#Scatter Plot
scatter_plot <- ggplot(merge_state, aes(x = area, y = events)) +
  geom_point(aes(color = region)) +
  labs(x = "Land area(square miles)", y = "# of storm events in 2008") +
  ggtitle("Scatter Plot of Land Area vs. Number of storm events in 2008")

scatter_plot

