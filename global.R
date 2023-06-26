library(shiny)
library(shinyauthr)
library(shiny)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(shinythemes)
library(shinyalert)
library(ggridges)
library(scales)
library(RColorBrewer)
library(tidyr)

#dataframe in care sunt stocate date despre user: username, password
user_base <- tibble::tibble(
  user = c("admin", "user"),
  password = sapply(c("admin", "user"), sodium::password_store),
  permissions = c("admin", "standard"),
  name = c("User One", "User Two")
)

movies_data <- read_csv("data/movies.csv")  #utilizată la choise
movies_data_jh <- read_csv("data/movies.csv") #utilizată la liste cu valori unice
genre_choices <- c('Toate genurile', sort(unique(movies_data$genre)))
rating_choices <- c('Ratings', sort(unique(movies_data$rating)))
unique_years <- c(sort(unique(movies_data_jh$year)))
measures <- c('score', 'gross')
col_vals <- c(1,2,3,4,6,13)
genre_options <- c(sort(unique(movies_data_jh$genre)))