shh <- suppressPackageStartupMessages
shh(library(glue))
shh(library(stringr))

shh(library(ggplot2))
shh(library(readxl))
shh(library(stringr))
shh(library(dplyr))
shh(library(yaml))
shh(library(purrr))
shh(library(lubridate))

shh(library(rjson))
shh(library(cowplot))
shh(library(shiny))

shh(library(ArmourEverTesty))

options(warn = -1)

source('ui.R')
source('server.R')
#source('functions.R')
#source('data.R')


shinyApp(ui, server, options = list(port = 3838))
