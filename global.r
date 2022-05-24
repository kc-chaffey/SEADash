## global
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyWidgets)
library(shinycssloaders)
library(tidyverse)
library(DT)
library(scales)
library(viridis)
library(plotly)
library(ggplot2)
library(shinymanager)
library(ggiraph)
source("SEAgraphmod.R", local=TRUE)
source("SEAgengraphmod.R", local=TRUE)
##LOAD YOUR DATA HERE
SEAdata <- read.csv("seatabdata.csv")
##


SEAprimarylist <- SEAdata %>%
  filter(primary_disagg!="Overall") %>% 
  pull(primary_disagg) %>% 
  unique() %>% 
  sort()

SEAmetriclist <- SEAdata %>%
  pull(metric_description) %>% 
  unique() %>% 
  sort()

