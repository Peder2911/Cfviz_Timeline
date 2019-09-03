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

latestversion <- dget('functions/latestversion.R')
fixCfs <- dget('functions/fixCfs.R') 
timeline <- dget('functions/timeline.R')
nameCfs <- dget('functions/nameCfs.R')
conditionalSubset <- dget('functions/conditionalSubset.R')
varsToDates <- dget('functions/varsToDates.R')
sanitizeNames <- dget('functions/sanitizeNames.R')

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
      cfinfo$actor <- sanitizeNames(cfinfo$actor) 

      groups <- lapply(sort(unique(cfinfo$actor)), function(actor){
         message(actor)
         tags$div(id = glue("{actor}"), class = "actor_grouping_box",
            selectInput(glue("{actor}_group"),
                        glue('{actor} group'),
                        1:NGROUPS)
         )
      })
      groups$id <- "actor_grouping"
      do.call(tags$div, groups)
   })

   output$groupnames <- renderUI({
      groups <- lapply(1:NGROUPS, function(group){
         tags$div(id = glue("group_{group}_name_box"), class = "group_name_box",
            textInput(glue("group_{group}_name"),
                      glue('Group {group} name'))
         )
      })
      groups$id <- "group_names"
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
      if(input$lumpnames){
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
         ggsave(file,currentPlot, device = 'png')
      }
   )
   output$downloadeps <- downloadHandler(filename = glue('plot.eps'),
      content = function(file){
         ggsave(file,currentPlot, device = 'eps')
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

