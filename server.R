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

# ================================

latestversion <- dget('functions/latestversion.R')
fixCfs <- dget('functions/fixCfs.R') 
timeline <- dget('functions/timeline.R')
nameCfs <- dget('functions/nameCfs.R')
conditionalSubset <- dget('functions/conditionalSubset.R')
varsToDates <- dget('functions/varsToDates.R')

# ================================
fpath <- Sys.getenv('TL_CONFIG')
if(fpath == ""){
   fpath <- 'config.Robj'
}
config <- dget(fpath)
con_config <- config$con

# ================================
dr <- dbDriver('PostgreSQL')
con_config$dr <- dr

# ================================

server <- function(input, output, session){

   # =================================================
   # Server Logic ====================================
   # =================================================
   con <- do.call(dbConnect,con_config) 

   alltables <- dbListTables(con)
   CFTABLE <- latestversion(alltables,'cf')
   ulocations <- dbGetQuery(con, glue('SELECT Location FROM {CFTABLE}')) %>%
      unique()
   ulocations <- sort(ulocations[[1]])

   updateSelectInput(session,'location',choices = ulocations)

   ngroups <- 1

   dbDisconnect(con)

   # =================================================
   # Refresh info ====================================
   observeEvent(input$location,{
      if(input$location != ""){

         con <- do.call(dbConnect,con_config) 

         query <- glue('SELECT * FROM {CFTABLE}')
         predicate <- glue(' WHERE Location =\'{input$location}\'')
         cfquery <- paste0(query,predicate)
         namedict <- yaml.load_file('data/names.yaml')

         cfs <- dbGetQuery(con, cfquery) 
         cfs_sub <<- fixCfs(cfs,namedict)

         dbDisconnect(con)

         # Update inputs
         unique_names <- unique(cfs_sub$name)
         unique_ids <- unique(cfs_sub$id)

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
      }
   })

   # =================================================
   # Group naming ====================================
   # Create a panel with textboxes where group names
   # are specified.
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

   # =================================================
   # Plot ============================================
   observeEvent(input$plot,{

      # Figure out naming ===============================
      tldata <- nameCfs(cfs_sub, ucdp,
                        input = input,
                        groups = groups,
                        usegroups = input$usegroup)
      # for debug
      tee <- tldata

      # Only display included ===========================
      tldata <- conditionalSubset(tldata,input)

      # Plot ============================================
      timelineplot <- timeline(tldata,input)
      currentPlot <<- timelineplot
      output$plot <- renderPlot(timelineplot)

      # Debug stuff =====================================
      output$debugdat <- renderTable(arrange(tee[c('id','year','name')], id))
      allids <- min(tee$id):max(tee$id)
      missingids <- allids[!allids %in% tee$id] %>%
         glue_collapse(sep = ', ')
      output$missingids <- renderText(glue('Missing these IDs: {missingids}'))
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
                               selected = unique(cfs_sub$name))
   })
}
