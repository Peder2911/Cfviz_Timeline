server <- function(input, output, session){

   latestVersion <- dget('functions/latestVersion.R')
   fixCfs <- dget('functions/fixCfs.R') 

   # Not implemented
   # fixUcdp <- dget('functions/fixUcdp.R')
   # ucdp <- read.csv(latestVersion('data','ucdp')$path) %>% fixUcdp()

   # =================================================
   # Data setup ======================================
   # =================================================
   # Will try to use the latest version based on 
   # numeric versioning ("cf_4_3" is v.43) and so on

   hascache <- any(str_detect(list.files('cache'),'\\.rds'))
   nocache <- '--nocache' %in% commandArgs(trailingOnly = TRUE)

   if(hascache){
      cachefile <- latestVersion('cache','cf')
      datafile <- latestVersion('data','cf')
      newerdata <- datafile$version > cachefile$version
   } else {
      newerdata <- TRUE
   }


   if(hascache & !nocache & !newerdata){
      cachepath <- latestVersion('cache','cf')$path
      cfs <- readRDS(cachepath)

   } else {
      datafile <- latestVersion('data','cf')
      nameDictionary <- read_yaml('data/names.yaml') 

      writeLines(glue('Using {datafile$filename}'))
      cfs <- suppressWarnings(read_xlsx(datafile$path)) %>%
         fixCfs(nameDictionary)

      cfs <- cfs[!is.na(cfs$year),]
      writeLines(glue('Caching {datafile$filename}'))
      saveRDS(cfs,glue('cache/{datafile$filename}.rds'))
   }

   # =================================================
   # Server Logic ====================================
   # =================================================

   ulocations <- sort(unique(cfs$location))
   updateSelectInput(session,'location',choices = ulocations)
   ngroups <- 1

   # =================================================
   # Refresh info ====================================
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

      # Grouping ui =====================================
      output$grouping <- renderUI({
         unique_names <- unique(cfs_sub$name)
         boxes <- list()

         for(i in 1:length(unique_names)){
            boxes[[i]] <- selectInput(glue('actor_{i}_group'),
                                      label = unique_names[i],
                                      choices = c('No group'))
         }
         boxes
      })

      # Necessary?
      cfs_sub <<- cfs_sub
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
      timeline <- dget('functions/timeline.R')
      nameCfs <- dget('functions/nameCfs.R')
      conditionalSubset <- dget('functions/conditionalSubset.R')

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
