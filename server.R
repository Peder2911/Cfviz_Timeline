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
         query <- 'SELECT name,id FROM {CFTABLE}' %>%
            paste0(' WHERE location =\'{input$location}\'') %>%
            glue()
         cfinfo <- dbGetQuery(con, query)
         dbDisconnect(con)

         # Update inputs
         unique_names <- cfinfo$name %>%
            str_split(' +- +') %>%
            do.call(c, .) %>%
            unique() %>%
            sort()

         unique_ids <- sort(unique(cfinfo$id))

         output$grouping <- renderUI({
            boxes <- list()

            for(i in 1:length(unique_names)){
               boxes[[i]] <- selectInput(glue('actor_{i}_group'),
                                              label = unique_names[i],
                                              choices = c('No group'))
            }
            boxes
         })
         updateCheckboxGroupInput(session,'include_actors',choices = unique_names)
         updateCheckboxGroupInput(session,'include_ids',choices = unique_ids)
      }
   })

   # ================================================
   # Group naming ===================================
   # Create a panel with textboxes where group names 
   # are specified ==================================

   output$groupnames <- renderUI({
      groups <- list()
      for(group in 1:input$ngroups){
         groupname <- glue('group_{group}_name')
         if(groupname %in% names(input)){
            selected <- isolate(input[[groupname]])
         } else {
            selected <- 'Others'
         }
         groups[[group]] <- textInput(glue('group_{group}_name'),
                                      label = glue('Group {group}:'),
                                      selected)

         # Really ugly, but necessary?
         for(name in names(input)){
            if(str_detect(name,'actor_[0-9]{1,3}_group')){
               selected <- isolate(input[[name]])
               if(selected > input$ngroups){
                  selected <- 'No group'
               }
               updateSelectInput(session, name,
                                 choices = c('No group',
                                             as.character(1:input$ngroups)),
                                 selected = selected)
            }
         }
      }
      groups
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
         cfs <- cfs %>%
            filter(str_detect(name, input$include_actors))
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
   output$download <- downloadHandler(filename = 'plot.zip',
      content = function(file){

         dir <- tempdir()
         stuff <- character()

         if(input$separatelegend){

            legend <- get_legend(currentPlot + theme(legend.position = 'right'))
            stuff[2] <- glue('{dir}/legend.{input$plotformat}')
            ggsave(stuff[2], legend, device = input$plotformat,
                height = input$plotheight, width = input$plotwidth,
                units = input$units)
         }
         
         stuff[1] <- glue('{dir}/plot.{input$plotformat}') 
         ggsave(stuff[1],currentPlot, device = input$plotformat,
                height = input$plotheight, width = input$plotwidth,
                units = input$units)
         zip(file, stuff, flags = '-r9Xj')
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
                               selected = unique(cfs$name))
      updateCheckboxGroupInput(session,'include_ids',
                               selected = unique(cfs$id))
   })
}
