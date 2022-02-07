#### Preamble ####
# Purpose: Manipulating data for analysis & visulization
# Author: Isfandyar Virani
# Data: 04 Feb 2022
# Contact: isfandyar.virani@mail.utoronto.ca
# License: MIT


#### Workspace setup ####
library(opendatatoronto)
library(tidyverse)
library(dplyr)
library(readr)
library(ggplot2)
library(knitr)
library(vtable)

#### Import data from folder ####
raw_data <- read_csv("starter_folder/inputs/data/raw_data.csv")


#### Grabbing columns of interest from raw_data ###
# Have made a decision to only analyze 2020 data for our analysis
toronto_crime_data_2020 <- raw_data %>% select(Neighbourhood,
                                          F2020_Population_Projection,
                                          Assault_2020,
                                          Assault_Rate2020,
                                          AutoTheft_2020,
                                          AutoTheft_Rate2020,
                                          BreakAndEnter_2020,
                                          BreakAndEnter_Rate2020,
                                          Robbery_2020,
                                          Robbery_Rate2020,
                                          TheftOver_2020,
                                          TheftOver_Rate2020,
                                          Homicide_2020,
                                          Homicide_Rate2020,
                                          Shootings_2020,
                                          Shootings_Rate2020)


toronto_crime_data_2019 <- raw_data %>% select(Neighbourhood,
                                               F2020_Population_Projection,
                                               Assault_2019,
                                               Assault_Rate2019,
                                               AutoTheft_2019,
                                               AutoTheft_Rate2019,
                                               BreakAndEnter_2019,
                                               BreakAndEnter_Rate2019,
                                               Robbery_2019,
                                               Robbery_Rate2019,
                                               TheftOver_2019,
                                               TheftOver_Rate2019,
                                               Homicide_2019,
                                               Homicide_Rate2019,
                                               Shootings_2019,
                                               Shootings_Rate2019)


#### Saving the dataset as .csv file####
write_csv(toronto_crime_data_2020, 'starter_folder/inputs/data/toronto_crime_data_2020.csv')
write_csv(toronto_crime_data_2019, 'starter_folder/inputs/data/toronto_crime_data_2019.csv')
### Data Analysis ###

## Questions to answer:
# 1) The neighbourhoods with the most crimes in Toronto (most dangerous?)
# 2) The neighbourhoods with the least crimes in Toronto (most safe?)
# 3) Does population matter? Do higher populated neighborhood have more crime?


# Find the top 10 (out of 140) neighbourhoods with the most number of crimes in Toronto in 2020

toronto_crime_sum_2020 <- toronto_crime_data_2020 %>% group_by(Neighbourhood) %>% summarise(Total_crimes = sum(Assault_2020,
                                                                                     AutoTheft_2020,
                                                                                     BreakAndEnter_2020,
                                                                                     Robbery_2020,
                                                                                     TheftOver_2020,
                                                                                     Homicide_2020,
                                                                                     Shootings_2020)) %>% arrange(desc(Total_crimes))


toronto_crime_sum_2020 %>% head(10)


# Find the top 10 (out of 140) neighbourhoods with the most number of crimes in Toronto in 2019

toronto_crime_sum_2019 <- toronto_crime_data_2019 %>% group_by(Neighbourhood) %>% summarise(Total_crimes = sum(Assault_2019,
                                                                                                               AutoTheft_2019,
                                                                                                               BreakAndEnter_2019,
                                                                                                               Robbery_2019,
                                                                                                               TheftOver_2019,
                                                                                                               Homicide_2019,
                                                                                                               Shootings_2019)) %>% arrange(desc(Total_crimes)) 

toronto_crime_sum_2019 %>% head(10)


# Find the top 10 (out of 140) neighbourhoods with the least number of crimes in Toronto in 2020

toronto_crime_sum_2020 %>% arrange(Total_crimes) %>% head(10)


# Find the top 10 (out of 140) neighbourhoods with the least number of crimes in Toronto in 2019
toronto_crime_sum_2019 %>% arrange(Total_crimes) %>% head(10)


# Total # of crimes in Toronto in 2020


Toronto_total_crime_2020 <- toronto_crime_sum_2020 %>% summarise(Total = sum(Total_crimes))

Toronto_total_crime_2020

# Total # of crimes in Toronto in 2019

Toronto_total_crime_2019 <- toronto_crime_sum_2019 %>% summarise(Total = sum(Total_crimes))

Toronto_total_crime_2019

# Examining Crime rates (crime based on population of the neighbourhood)


Toronto_Total_crime_rate_2020 <- toronto_crime_data_2020 %>% group_by(Neighbourhood,F2020_Population_Projection) %>% summarise(Total_crime_rate = sum(Assault_Rate2020,
                                                                                         AutoTheft_Rate2020,
                                                                                         BreakAndEnter_Rate2020,
                                                                                         Robbery_Rate2020,
                                                                                         TheftOver_Rate2020,
                                                                                         Homicide_Rate2020,
                                                                                         Shootings_Rate2020)) %>% arrange(desc(Total_crime_rate))




Toronto_Total_crime_rate_2019 <- toronto_crime_data_2019 %>% group_by(Neighbourhood,F2020_Population_Projection) %>% summarise(Total_crime_rate = sum(Assault_Rate2019,
                                                                                         AutoTheft_Rate2019,
                                                                                         BreakAndEnter_Rate2019,
                                                                                         Robbery_Rate2019,
                                                                                         TheftOver_Rate2019,
                                                                                         Homicide_Rate2019,
                                                                                         Shootings_Rate2019)) %>% arrange(desc(Total_crime_rate))






# Double checking if toronto_neighbourhood_crime_rate_total calculation is correct
toronto_crime_data_2020 %>% group_by(Neighbourhood) %>% summarise(Total_crimes_rate = sum(Assault_2020,
                                                                                     AutoTheft_2020,
                                                                                     BreakAndEnter_2020,
                                                                                     Robbery_2020,
                                                                                     TheftOver_2020,
                                                                                     Homicide_2020,
                                                                                     Shootings_2020)/(F2020_Population_Projection/100000)) %>%  arrange(desc(Total_crimes_rate)) %>% head(15) 

#==============================================================================================================

#### GRAPHS ####
## Graphing crime rates:


# Top 10 Toronto Neighbourhoods with highest crime rates in 2020
Toronto_Total_crime_rate_2020 %>% head(10) %>% 
  ggplot(aes(x = reorder(Neighbourhood, Total_crime_rate) , y = Total_crime_rate , fill = F2020_Population_Projection)) +
  geom_bar(stat = 'identity', position = position_dodge()) + coord_flip() +
  labs(x = 'Neighbourhood', y = 'Crime rate', fill = 'Population(2020)',title = 'Toronto Crime Rate - 2020', subtitle = 'Top 10 Toronto Neighbourhoods with highest crime rates in 2020') +
  theme(axis.text.x =  element_text(angle = 27))  +
  scale_fill_gradient2(low='orange', mid='snow', high='blue')


# Top 10 Toronto Neighbourhoods with highest crime rates in 2019
Toronto_Total_crime_rate_2019 %>% head(10) %>% 
  ggplot(aes(x = reorder(Neighbourhood, Total_crime_rate) , y = Total_crime_rate , fill = F2020_Population_Projection)) +
  geom_bar(stat = 'identity', position = position_dodge()) + coord_flip() +
  labs(x = 'Neighbourhood', y = 'Crime rate', fill = 'Population(2020)', title = 'Toronto Crime Rate - 2019', subtitle = 'Top 10 Toronto Neighbourhoods with highest crime rates in 2019') +
  theme(axis.text.x =  element_text(angle = 27))  +
  scale_fill_gradient2(low='orange', mid='snow', high='blue')




# Top 10 Toronto Neighbourhoods with lowest crime rates in 2020
Toronto_Total_crime_rate_2020 %>% arrange(Total_crime_rate)%>% head(10)%>% 
  ggplot(aes(x = reorder(Neighbourhood, Total_crime_rate) , y = Total_crime_rate , fill = F2020_Population_Projection)) +
  geom_bar(stat = 'identity', position = position_dodge()) + coord_flip() +
  labs(x = 'Neighbourhood', y = 'Crime rate', fill = 'Population(2020)',title = 'Toronto Crime Rate - 2020', subtitle = 'Top 10 Toronto Neighbourhoods with lowest crime rates in 2020') +
  theme(axis.text.x =  element_text(angle = 27))  +
  scale_fill_gradient2(low='orange', mid='snow', high='blue')


# Top 10 Toronto Neighbourhoods with lowest crime rates in 2019
Toronto_Total_crime_rate_2019 %>% arrange(Total_crime_rate)%>% head(10)%>% 
  ggplot(aes(x = reorder(Neighbourhood, Total_crime_rate) , y = Total_crime_rate , fill = F2020_Population_Projection)) +
  geom_bar(stat = 'identity', position = position_dodge()) + coord_flip() +
  labs(x = 'Neighbourhood', y = 'Crime rate', fill = 'Population(2020)',title = 'Toronto Crime Rate - 2019', subtitle = 'Top 10 Toronto Neighbourhoods with lowest crime rates in 2019') +
  theme(axis.text.x =  element_text(angle = 27))  +
  scale_fill_gradient2(low='orange', mid='snow', high='blue')

#==============================================================================================================

# Top 10 Toronto Neighbourhoods with highest number of reported crimes in 2020

toronto_crime_data_2020 %>% group_by(Neighbourhood, F2020_Population_Projection) %>% summarise(Total_crimes = sum(Assault_2020,
                                                                                     AutoTheft_2020,
                                                                                     BreakAndEnter_2020,
                                                                                     Robbery_2020,
                                                                                     TheftOver_2020,
                                                                                     Homicide_2020,
                                                                                     Shootings_2020)) %>% arrange(desc(Total_crimes)) %>% head(10) %>%
  ggplot(aes(x = reorder(Neighbourhood, Total_crimes) , y = Total_crimes , fill = F2020_Population_Projection)) +
  geom_bar(stat = 'identity', position = position_dodge()) + coord_flip() +
  labs(x = 'Neighbourhood', y = 'Crime rate', fill = 'Population(2020)', title = 'Toronto Total Crime - 2020', subtitle = 'Top 10 Toronto Neighbourhoods with highest number of reported crimes in 2019') +
  theme(axis.text.x =  element_text(angle = 27))  +
  scale_fill_gradient2(low='orange', mid='snow', high='blue')

# Top 10 Toronto Neighbourhoods with highest number of reported crimes in 2019

toronto_crime_data_2019 %>% group_by(Neighbourhood, F2020_Population_Projection) %>% summarise(Total_crimes = sum(Assault_2019,
                                                                                                                  AutoTheft_2019,
                                                                                                                  BreakAndEnter_2019,
                                                                                                                  Robbery_2019,
                                                                                                                  TheftOver_2019,
                                                                                                                  Homicide_2019,
                                                                                                                  Shootings_2019)) %>% arrange(desc(Total_crimes)) %>% head(10) %>%
  ggplot(aes(x = reorder(Neighbourhood, Total_crimes) , y = Total_crimes , fill = F2020_Population_Projection)) +
  geom_bar(stat = 'identity', position = position_dodge()) + coord_flip() +
  labs(x = 'Neighbourhood', y = 'Crime rate', fill = 'Population(2020)', title = 'Toronto Total Crime - 2019', subtitle = 'Top 10 Toronto Neighbourhoods with highest number of reported crimes in 2019') +
  theme(axis.text.x =  element_text(angle = 27))  +
  scale_fill_gradient2(low='orange', mid='snow', high='blue')





# Top 10 Toronto Neighbourhoods with lowest number of reported crimes in 2020

toronto_crime_data_2020 %>% group_by(Neighbourhood, F2020_Population_Projection) %>% summarise(Total_crimes = sum(Assault_2020,
                                                                                                                  AutoTheft_2020,
                                                                                                                  BreakAndEnter_2020,
                                                                                                                  Robbery_2020,
                                                                                                                  TheftOver_2020,
                                                                                                                  Homicide_2020,
                                                                                                                  Shootings_2020)) %>% arrange(Total_crimes) %>% head(10) %>%
  ggplot(aes(x = reorder(Neighbourhood, Total_crimes) , y = Total_crimes , fill = F2020_Population_Projection)) +
  geom_bar(stat = 'identity', position = position_dodge()) + coord_flip() +
  labs(x = 'Neighbourhood', y = 'Crime rate', fill = 'Population(2020)', title = 'Toronto Total Crime - 2020', subtitle = 'Top 10 Toronto Neighbourhoods with highest number of reported crimes in 2019') +
  theme(axis.text.x =  element_text(angle = 27))  +
  scale_fill_gradient2(low='orange', mid='snow', high='blue')

# Top 10 Toronto Neighbourhoods with highest number of reported crimes in 2019

toronto_crime_data_2019 %>% group_by(Neighbourhood, F2020_Population_Projection) %>% summarise(Total_crimes = sum(Assault_2019,
                                                                                                                  AutoTheft_2019,
                                                                                                                  BreakAndEnter_2019,
                                                                                                                  Robbery_2019,
                                                                                                                  TheftOver_2019,
                                                                                                                  Homicide_2019,
                                                                                                                  Shootings_2019)) %>% arrange(Total_crimes) %>% head(10) %>%
  ggplot(aes(x = reorder(Neighbourhood, Total_crimes) , y = Total_crimes , fill = F2020_Population_Projection)) +
  geom_bar(stat = 'identity', position = position_dodge()) + coord_flip() +
  labs(x = 'Neighbourhood', y = 'Crime rate', fill = 'Population(2020)', title = 'Toronto Total Crime - 2019', subtitle = 'Top 10 Toronto Neighbourhoods with highest number of reported crimes in 2019') +
  theme(axis.text.x =  element_text(angle = 27))  +
  scale_fill_gradient2(low='orange', mid='snow', high='blue')



# ==========================================

Toronto_population_2020 <- toronto_crime_data_2020 %>% summarise(total_population_2020 = sum(F2020_Population_Projection))


TorontoCity_crimeRate <- Toronto_total_crime_2020/(Toronto_population_2020/100000)


# =============================================


mean(Toronto_Total_crime_rate_2020$Total_crime_rate)
median(Toronto_Total_crime_rate_2020$Total_crime_rate)


#===============================================

TotalCrime <- tibble(Year = c('2019', '2020'), TotalCrime = c(Toronto_total_crime_2019$Total, Toronto_total_crime_2020$Total))

ggplot(TotalCrime, aes(fill=Year, y=TotalCrime, x=Year)) +
  geom_bar(position='dodge', stat='identity') +
  labs(x = 'Year',
       y = 'Total reported crimes',
       fill = 'Year',
       title = 'Toronto Total Reported Crime (2019-2020)',
       subtitle = 'The sum of all crimes reported')


#===============================================

# Table 

# Get Min, Max, Mean and Std. Dev of Population, Crime count and Crime Rate of 2019 and 2020

Toronto_2019 <- merge(toronto_crime_sum_2019, Toronto_Total_crime_rate_2019, by= 'Neighbourhood')
Toronto_2020 <- merge(toronto_crime_sum_2020, Toronto_Total_crime_rate_2020, by= 'Neighbourhood')

Toronto_19_20 <- merge(Toronto_2019, Toronto_2020, by = c('Neighbourhood', 'F2020_Population_Projection'))
names(Toronto_19_20)[names(Toronto_19_20) == 'Total_crimes.x'] <- 'Crime Count (2019)'
names(Toronto_19_20)[names(Toronto_19_20) == 'Total_crimes.y'] <- 'Crime Count (2020)'
names(Toronto_19_20)[names(Toronto_19_20) == 'Total_crime_rate.x'] <- 'Crime Rate (2019)'
names(Toronto_19_20)[names(Toronto_19_20) == 'Total_crime_rate.y'] <- 'Crime Rate (2020)'
names(Toronto_19_20)[names(Toronto_19_20) == 'F2020_Population_Projection'] <- 'Population Projection (2020)'



st(Toronto_19_20,  title = 'Toronto Neighbourhood Crime Summary Statistics', out = 'kable')


#==============================================

top10_toronto_crime_sum_2019 <- toronto_crime_sum_2019 %>% head(5)
top10_toronto_crime_sum_2020 <- toronto_crime_sum_2020 %>% head(5)

top10_toronto_crime_sum_2019$Total_crimes <- as.numeric(top10_toronto_crime_sum_2019$Total_crimes)
top10_toronto_crime_sum_2020$Total_crimes <- as.numeric(top10_toronto_crime_sum_2020$Total_crimes)

mean(top10_toronto_crime_sum_2019$Total_crimes)
mean(top10_toronto_crime_sum_2020$Total_crimes)


# The total number of times crime in 2020 went down for the top 10 most dangerous neighbourhoods

bot5_toronto_crime_sum_2019 <- toronto_crime_sum_2019 %>% tail(5)
bot5_toronto_crime_sum_2020 <- toronto_crime_sum_2020 %>% tail(5)

bot5_toronto_crime_sum_2019$Total_crimes <- as.numeric(bot5_toronto_crime_sum_2019$Total_crimes)
bot5_toronto_crime_sum_2020$Total_crimes <- as.numeric(bot5_toronto_crime_sum_2020$Total_crimes)

mean(bot5_toronto_crime_sum_2019$Total_crimes)
mean(bot5_toronto_crime_sum_2020$Total_crimes)

#===================================================
