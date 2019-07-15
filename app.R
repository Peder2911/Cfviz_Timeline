
shh <- suppressPackageStartupMessages
shh(library(shiny))
shh(library(glue))
shh(library(stringr))
shh(library(rjson))

shh(library(ggplot2))
shh(library(readxl))
shh(library(stringr))
shh(library(dplyr))
shh(library(yaml))

options(warn = -1)

source('ui.R')
source('server.R')
source('functions.R')
source('data.R')


shinyApp(ui, server, options = list(port = 1337))
