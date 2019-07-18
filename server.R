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
shh(library(purrr))

shh(library(ArmourEverTesty))
shh(library(lubridate))

shh(library(cowplot))

options(warn = -1)

setwd('/srv/shiny-server/cfplot')
saveRDS(c(1,2,3),'/srv/shiny-server/cfplot/eep.rds')


server <- function(input, output, session){
   #output$MAXGROUPS <- 10
   # =================================================

   nocache <- '--nocache' %in% commandArgs(trailingOnly = TRUE)

   nocache <- TRUE

   fixUcdp <- dget('functions/fixUcdp.R')
   ucdp <- read.csv('data/ucdp-dyadic-191.csv') %>% fixUcdp()

   # Ceasefire Data ==================================
   #arglen <- length(commandArgs(trailingOnly = TRUE))
   #dataname <- commandArgs(trailingOnly = TRUE)[arglen]
   # Remove ext. if it was added
   #dataname <- str_replace(dataname,'\\.xlsx','')

   dataname <- 'cf_4_3'
   hascache <- any(str_detect(list.files('cache'),'\\.rds'))
   hascache <- FALSE

   if(hascache & !nocache){
      filename <- paste0(dataname,'.rds')
      if(!filename %in% list.files('cache')) {
         stop(glue('data/{filename} not found!'))
      }

      cfs <- readRDS(glue('cache/{filename}'))

   } else {
      filename <- paste0(dataname,'.xlsx')
      if(!filename %in% list.files('data')){
         stop(glue('data/{filename} not found!'))
      }

      nameDictionary <- read_yaml('data/names.yaml') 

      fixCfs <- dget('functions/fixCfs.R') 
      cfs <- suppressWarnings(read_xlsx(paste0('data/',filename))) %>%
         fixCfs(nameDictionary)

      cfs <- cfs[!is.na(cfs$year),]

      writeLines(glue('Caching {dataname}'))
      saveRDS(cfs,glue('cache/{dataname}.rds'))
   }

   # Deprecated ======================================
   #locations <- c('a','b','c')
   #dates <- seq(as.Date('1999-01-01'),as.Date('2000-01-01'), by = 'years')
   #ectors <- c('a','b','c') 
   #cfids <- 1:10

   # Server Logic ====================================
   ulocations <- sort(unique(cfs$location))
   updateSelectInput(session,'location',choices = ulocations)

   # Update info when Location changes ===============
   observeEvent(input$location,{
      cfs_sub <- cfs %>%
         filter(location == input$location)

      actors <- tibble(Actors = sort(unique(cfs_sub$name)))
      output$allactors <- renderTable(actors, striped = FALSE)

      updateNumericInput(session,'startyear',
                         value = min(cfs_sub$year))
      updateNumericInput(session,'endyear',
                         value = max(cfs_sub$year))
      updateCheckboxGroupInput(session,'include_actors',
                        choices = unique(cfs_sub$name))
      updateCheckboxGroupInput(session,'grouped_actors',
                        choices = unique(cfs_sub$name))
      updateCheckboxGroupInput(session,'include_ids',
                        choices = unique(sort(as.numeric(cfs_sub$id))))

      output$plot <- NULL

      # Necessary?
      cfs_sub <<- cfs_sub
   })

   # Make a plot with current settings ===============
   observeEvent(input$plot,{
      timeline <- dget('functions/timeline.R')
      nameCfs <- dget('functions/nameCfs.R')
      conditionalSubset <- dget('functions/conditionalSubset.R')

      tldata <- nameCfs(cfs_sub, ucdp, input, groups)

      output$debugdat <- renderTable(arrange(tldata[c('id','year','name')], id))
      allids <- min(tldata$id):max(tldata$id)
      missingids <- allids[!allids %in% tldata$id] %>%
         glue_collapse(sep = ', ')
      output$missingids <- renderText(glue('Missing these IDs: {missingids}'))

      tldata <- conditionalSubset(tldata,input)

      timelineplot <- timeline(tldata,input)
      currentPlot <<- timelineplot

      #output$fromplot <- renderTable(timelineplot$data)
      output$plot <- renderPlot(timelineplot)
   })

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



   observeEvent(input$clearincl,{
      walk(c('include_actors','include_ids'), function(id){
         updateCheckboxGroupInput(session,id,selected = character())
      })
   })
   observeEvent(input$cleargrouped,{
      updateCheckboxGroupInput(session,'grouped_actors',selected = character())
   })

   observeEvent(input$allgrouped,{
      updateCheckboxGroupInput(session,'grouped_actors',
                               selected = unique(cfs_sub$name))
   })
   observeEvent(input$allincl,{
      updateCheckboxGroupInput(session,'include_actors',
                               selected = unique(cfs_sub$name))
   })
}

