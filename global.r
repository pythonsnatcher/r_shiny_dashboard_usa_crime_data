# USArrests dataset

# load required datasets
library(dplyr)
library(plotly)
library(ggplot2)
library(ggtext)
library(maps)
library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(DT)
library(plotly)
library(dplyr)
library(ggcorrplot)

#create data object

my_data <- USArrests


# Assigning row names to object
states = rownames(my_data)
my_data = my_data %>%
  mutate(State = states)




# choices for selecting input - without State column
c1 = my_data %>% 
  select(-State) %>% 
  names()


# choices for selecting input - without State and Urban Population

c2 = my_data %>% 
  select(-'State', - 'UrbanPop') %>% 
  names()

state_map <- map_data('state')

#convert state to lower case
my_data1 = my_data %>% 
  mutate(State = tolower(State))

merged = right_join(my_data1, state_map, by=c('State'='region'))

st = data.frame(abb = state.abb, stname=tolower(state.name), x=state.center$x, y=state.center$y)
new_join = left_join(merged, st, by=c("State" = "stname"))
  
  
  
  
  
