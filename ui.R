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

ui <- fluidPage(
  # logout button
  div(class = "pull-right", shinyauthr::logoutUI(id = "logout")),

  # login section
  shinyauthr::loginUI(id = "login"),

  # Sidebar care se va afisa dupa login
  uiOutput("sidebarpanel"),
  
  #Style (elemente css)
  tags$head(
    tags$title("Dashboard"),
    tags$style(HTML("
     .tabbable .nav-tabs, .nav-tabs>li{
         border-bottom: solid 1px #dddddd;
     }
    .col-sm-12 .row{
        display: flex;
        flex-direction: row;
        padding: 30px 52px;
        align-content: flex-start;
        align-items: center;
     }
     .col-sm-12 .row .col-sm-6:nth-child(1){
     width: fit-content;
     }
     
     .row .col-sm-12 .col-sm-8{
     padding-left: 52px;
     }
     .tabbable{
         max-width: 97%;
     }
     .col-sm-6 h2:nth-child(2){
     margin: 0px;
     }
     ")),
    #Icon: https://fontawesome.com/  -- se comporta ca bootstrap
    tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.15.3/css/all.min.css")
    
  ),
  theme = shinytheme("united")
  
)




shinyUI(ui)