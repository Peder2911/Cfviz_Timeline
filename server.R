shh <- suppressPackageStartupMessages

shh(library(glue))
shh(library(stringr))
#
shh(library(ggplot2))
#shh(library(readxl))
shh(library(stringr))
shh(library(dplyr))
shh(library(yaml))
shh(library(purrr))
shh(library(lubridate))

#shh(library(rjson))
#shh(library(cowplot))

shh(library(RPostgreSQL))

options(warn = -1)

# Function imports ===============================

latestversion <- dget('functions/latestversion.R')
fixCfs <- dget('functions/fixCfs.R') 
timeline <- dget('functions/timeline.R')
nameCfs <- dget('functions/nameCfs.R')
conditionalSubset <- dget('functions/conditionalSubset.R')
varsToDates <- dget('functions/varsToDates.R')

colors <- readRDS('data/colors.rds')

# Config =========================================
fpath <- Sys.getenv('TL_CONFIG')
if(fpath == ""){
   fpath <- 'config.Robj'
}
config <- dget(fpath)
con_config <- config$con

dr <- dbDriver('PostgreSQL')
con_config$dr <- dr

server <- function(input, output, session){
   # ================================================
   # Initialization =================================
   # ================================================

   con <- do.call(dbConnect,con_config) 

   alltables <- dbListTables(con)
   CFTABLE <- latestversion(alltables,'cf')
   ulocations <- unlist(dbGetQuery(con, glue('SELECT location FROM {CFTABLE}'))) %>%
      unique()

   updateSelectInput(session,'location',choices = ulocations)

   ngroups <- 1

   dbDisconnect(con)

   # ================================================
   # Refresh info ===================================
   # ================================================
   observeEvent(input$location,{
      if(input$location != ""){
         con <- do.call(dbConnect,con_config) 
         query <- 'SELECT * FROM {CFTABLE}' %>%
            paste0(' WHERE location =\'{input$location}\'') %>%
            glue()
         cfinfo <- dbGetQuery(con, query)
         dbDisconnect(con)

         # Update inputs
         actorNames <<- cfinfo$name %>%
            str_split(' +- +') %>%
            do.call(c, .) %>%
            unique() %>%
            sort()

         ceasefireIds <<- sort(unique(cfinfo$id))

         updateCheckboxGroupInput(session,'include_actors',choices = actorNames)
         updateCheckboxGroupInput(session,'include_ids',choices = ceasefireIds)
      }
   })

   # ================================================
   # Refresh plot ===================================
   # ================================================
   observeEvent(input$plot,{
      con <- do.call(dbConnect,con_config) 
      query <- glue('SELECT * FROM {CFTABLE} WHERE location =\'{input$location}\'')
      cfs <- dbGetQuery(con, query)
      dbDisconnect(con)
      cfs$year <- year(cfs$start)

      if(!is.null(input$include_actors)){
         inIncluded <- sapply(cfs$name, function(names){
            names <- names %>%
               str_split(' +- +') %>%
               do.call(c, .)
            any(names %in% input$include_actors)
         })

         cfs <- cfs[inIncluded,]
      }
      if(!is.null(input$include_ids)){
         cfs <- cfs %>%
            filter(id %in% input$include_ids)
      }

      # Plot ========================================
      timelineplot <- timeline(cfs,
                               startyear = input$startyear,
                               endyear = input$endyear,
                               colors = colors)

      currentPlot <<- timelineplot
      output$plot <- renderPlot(timelineplot)
   })

   # =================================================
   # Handle downloads ================================
   output$download <- downloadHandler(filename = glue('plot.{input$plotformat}'),
      content = function(file){
         ggsave(file,currentPlot, device = input$plotformat,
                height = input$plotheight, width = input$plotwidth,
                units = input$units)
      }
   )
   
   # Clear / all =====================================
   observeEvent(input$clearincl,{
      walk(c('include_actors','include_ids'), function(id){
         updateCheckboxGroupInput(session,id,selected = character())
      })
   })
   observeEvent(input$allincl,{
      updateCheckboxGroupInput(session,'include_actors',
                               selected = actorNames)
      updateCheckboxGroupInput(session,'include_ids',
                               selected = ceasefireIds)
   })
}
