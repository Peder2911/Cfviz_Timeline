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
shh(library(forcats))

#shh(library(rjson))
#shh(library(cowplot))

shh(library(RPostgreSQL))

options(warn = -1)

# Function imports ===============================

timeline <- dget('functions/timeline.R')

COLORS <- readRDS('data/colors.rds')

HEADTABLE <<- 'head'
LOCATIONSTABLE <<- 'locations'
CEASEFIRESTABLE <<- 'ceasefires'
ACTORSTABLE <<- 'actors'

NGROUPS <- 10

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

   # Locations ======================================
   con <- do.call(dbConnect,con_config) 
   ulocations <- dbGetQuery(con,glue('SELECT location FROM {LOCATIONSTABLE}')) %>%
      unlist %>%
      unique() %>%
      sort()
   dbDisconnect(con)

   updateSelectInput(session,'location',choices = ulocations)

   # Get country-specific info ====================== 
   info <- reactive({
      if(input$location != ''){
         variables <- c('{CEASEFIRESTABLE}.cf_id as id',
                        '{ACTORSTABLE}.actor_name as actor',
                        '{ACTORSTABLE}.acid as acid')
         variables <- glue_collapse(sapply(variables,glue), sep = ', ')

         q <- "SELECT {variables} FROM {LOCATIONSTABLE} 
               JOIN {HEADTABLE} ON {HEADTABLE}.cc=locations.cc
               JOIN {CEASEFIRESTABLE} ON {HEADTABLE}.locid=ceasefires.locid
               JOIN {ACTORSTABLE} ON {HEADTABLE}.acid=actors.acid
               WHERE {LOCATIONSTABLE}.location = '{input$location}'
               "

         con <- do.call(dbConnect,con_config) 
         res <- dbGetQuery(con,glue(q))
         dbDisconnect(con)
         res
      } else {
         NULL
      }
   })

   ceasefires <- reactive({
      variables <- c('{CEASEFIRESTABLE}.cf_id as id',
                     '{CEASEFIRESTABLE}.cf_effect_yr as year',
                     '{ACTORSTABLE}.actor_name as name')
      variables <- glue_collapse(sapply(variables,glue), sep = ', ')

      q <- "SELECT {variables} FROM {LOCATIONSTABLE} 
            JOIN {HEADTABLE} ON {HEADTABLE}.cc=locations.cc
            JOIN {CEASEFIRESTABLE} ON {HEADTABLE}.locid=ceasefires.locid
            JOIN {ACTORSTABLE} ON {HEADTABLE}.acid=actors.acid
            WHERE {LOCATIONSTABLE}.location = '{input$location}'
            "

      con <- do.call(dbConnect,con_config) 
      cfs <- dbGetQuery(con,glue(q))
      dbDisconnect(con)

      cfs

   })

   actorGrouping <- reactive({
      if(!is.null(input$actorGrouping)){
         json <- input$actorGrouping %>%
            jsonlite::fromJSON()
         if(length(json) > 0){
            dplyr::bind_rows(json)
         } else {
            NULL
         }
      } else {
         message("Oh no!! ActorGrouping is null!!")
         NULL
      }
   })
   
   groupNames <- reactive({
      if(!is.null(input$groupNames)){
         json <- input$groupNames %>%
            jsonlite::fromJSON()
         if(length(json) > 0){
            dplyr::bind_rows(json)
         } else {
            NULL
         }
      } else {
         message("Oh no!! Groupnames is null!!")
         NULL
      }
   })

   observeEvent(input$location,{
         cfinfo <- info()
         if(!is.null(cfinfo)){
            updateCheckboxGroupInput(session,'include_actors',
                                     choices = sort(unique(cfinfo$actor)))
            updateCheckboxGroupInput(session,'include_ids',
                                     choices = sort(unique(cfinfo$id)))
         }
   })

   output$groups <- renderUI({
      cfinfo <- info()
      actornames <- sort(unique(cfinfo$actor))
      names(actornames) <- NULL

      groups <- lapply(1:NGROUPS, function(i){
         selectInput(glue('groupActors_{i}'),
                     glue("Group {i} actors"), 
                     choices = actornames, selected = NULL,
                     multiple = TRUE, selectize = TRUE)
      })

      groups$id <- "actorGroups"
      do.call(tags$div, groups)
   })

   output$groupnames <- renderUI({
      groups <- lapply(1:NGROUPS, function(i){
         textInput(glue("groupName_{i}"),
                   glue("Group {i} name"))
      })
      groups$id <- "groupNames"
      do.call(tags$div, groups)
   })

   # Update plot (change to enterpress) =============
   observeEvent(input$plot,{
      cfs <- ceasefires()

      # Filtering ======================================
      if(!is.null(input$include_actors)){
         cfs <- filter(cfs, name %in% input$include_actors)
      }

      if(!is.null(input$include_ids)){
         cfs <- filter(cfs, id %in% input$include_ids)
      }

      if(input$usegroups){
         grouping <- actorGrouping()
         groupnaming <- groupNames()

         if(!any(sapply(list(grouping,groupnaming), is.null))){
            cfs <- merge(cfs, actorGrouping(), by='name', all.x = TRUE) %>%
               merge(groupNames(), by = 'group', all.x = TRUE)
            cfs$name <- ifelse(is.na(cfs$groupname),
                               cfs$name,
                               cfs$groupname)
            cfs$groupname <- NULL
            cfs$group <- NULL
         } else {

         }
      } else if(input$lumpnames){
         cfs$name <- fct_lump(cfs$name,n = input$lumpsize)
      }

      # Re-unitization =================================
      cfs <- cfs %>%
         group_by_at(names(cfs)[names(cfs)!='name']) %>%
         summarize(name = glue_collapse(sort(unique(name)),sep = ' - ')) %>%
         ungroup()

      timelineplot <- timeline(cfs,
                               startyear = input$startyear,
                               endyear = input$endyear,
                               colors = COLORS)

      currentPlot <<- timelineplot
      output$plot <- renderPlot(timelineplot)
   })


   # Necessary?


   # Handle downloads ================================
   output$downloadpng <- downloadHandler(filename = glue('plot.png'),
      content = function(file){
         ggsave(file,currentPlot, device = 'png',height = 6, width = 18.9, unit = 'in')
      }
   )
   output$downloadeps <- downloadHandler(filename = glue('plot.eps'),
      content = function(file){
         ggsave(file,currentPlot, device = 'eps', height = 6, width = 18.9, unit = 'in')
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

