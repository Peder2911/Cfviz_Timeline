
server <- function(input, output, session){
   #output$MAXGROUPS <- 10
   
   cfs_raw <- suppressWarnings(read_xlsx('data/cfs_4_3.xlsx'))
   nameDictionary <- read_yaml('data/names.yaml') 
   ucdp <- read.csv('data/ucdp-dyadic-191.csv') %>% fixUcdp()
   cfs <- cfs_raw

   updateSelectInput(session,'location',choices = sort(unique(cfs_raw$Location)))

   observeEvent(input$location,{
   # Refresh the data
      withProgress({
         cfs <- cfs_raw
   
         names(cfs) <- str_to_lower(names(cfs))
   
         cfs$start_date <- suppressWarnings(varsToDates(list(year = cfs$cf_dec_yr,
                                            month = cfs$cf_dec_month,
                                            day = cfs$cf_dec_day))) 

         cfs$end_date <- suppressWarnings(varsToDates(list(year = cfs$end_yr,
                                            month = cfs$end_month,
                                            day = cfs$end_day)))

         cfs$actor_name <- str_replace_all(cfs$actor_name,'\\s',' ')

         cfs$type <- dictlookup(cfs$ceasefire_type, nameDictionary$types)
         cfs$purpose <- dictlookup(cfs$purpose_1, nameDictionary$purposes)
   
         cfs <- cfs %>%
            select(
               location,
               start = start_date,
               end = end_date,
               id = cf_id,
               actor_name,
               ucdp_dyad,
               purpose,
               type,
               name = actor_name) %>%
            filter(location == input$location) %>%
            unique()
         
         actors <- tibble(Actors = cfs$name %>%
            unique() %>%
            sort())

         output$allactors <- renderTable(actors, striped = FALSE)

         updateDateRangeInput(session,'daterange', 
                              start = min(cfs$start), end = max(cfs$start))
         updateCheckboxGroupInput(session,'exclude_actors',
                           choices = unique(cfs$name))
         updateCheckboxGroupInput(session,'exclude_ids',
                           choices = unique(sort(as.numeric(cfs$id))))
         cfs <<- cfs
      })       
   })

   observeEvent(input$plot,{
      cfs$year <- year(cfs$start)

      if(input$naming == 'Custom'){
         cfs <- mutate(cfs,
                       name = dictReplace(name ,groups(),input$defaultgroup))
      } 
      if (input$naming == 'UCDP'){
         cfs$ucdp_dyad <- as.numeric(cfs$ucdp_dyad)
         cfs <- merge(cfs, ucdp, by.x = 'ucdp_dyad', by.y = 'id', all.x = TRUE)
         cfs$name <- cfs$actor_name
         excluded <- character()
      } else {
         cfs$ucdp_dyad <- -1
         cfs <- cfs %>%
            group_by(start, end, year, purpose, type, location) %>%
               summarize(name = glue_collapse(sort(unique(name)), sep = ' - '),
                         ucdp_dyad = min(ucdp_dyad),
                         id = min(id))
      }

      tldata <- cfs %>%
         filter(start >= input$daterange[1],
                start <= input$daterange[2],
                #!input$exclude | !name %in% input$excluded,
                !input$exclude | !id %in% input$exclude_ids)

      tl <- ggplot(tldata) + 
        geom_histogram(aes(x = year, fill = name), 
                       binwidth = 1)+
        theme_classic()+
        theme(axis.line.y = element_blank(),
              axis.title.y = element_text(size = 10),
              axis.title.x = element_blank(),
              axis.text.x = element_text(angle = 45,
                                         hjust = 1),
              panel.grid.major.x = element_line(color = 'gray'),
              panel.grid.minor.x = element_line(color = 'light gray'),
              legend.position = 'bottom',
              legend.title = element_blank(),
              legend.key.size = unit(0.2,units = 'cm'),
              legend.spacing.x = unit(0.1,units = 'cm'),
              legend.margin = margin(t = 0.1,r = 0.1,b = 0.1,l = 0.1, unit = 'cm'),
              plot.margin = margin(t = 0.3,l = 0.1,r = 0.3,unit = 'cm')) +
        scale_y_continuous(expand = c(0,0),
                           breaks = seq(0,10,2),
                           limits = c(0,10)) +
        scale_x_continuous(expand = c(0,0),
                           breaks = seq(1989, 2018, 1),
                           limits = c(year(input$daterange[1]), 
                                      year(input$daterange[2]))) +
        labs(y = '')
     output$plot <- renderPlot(tl)
   })

   locations <- c('a','b','c')
   dates <- seq(as.Date('1999-01-01'),as.Date('2000-01-01'), by = 'years')
   actors <- c('a','b','c') 
   cfids <- 1:10


   observeEvent(input$pushgroup,{
      updateSelectInput(session,'nfields',selected = as.numeric(input$nfields) + 1)
   })

   observeEvent(input$popgroup,{
      updateSelectInput(session,'nfields',selected = as.numeric(input$nfields) - 1)
   })

   output$location <- renderUI({selectInput('location','Location',locations)})

   output$daterange <- renderUI({
      dateRangeInput('daterange', 'Date range', 
                     start = min(dates, na.rm = T),end = max(dates, na.rm = T))
   })

   output$exclude <- renderUI({
      inputPanel(
         checkboxGroupInput('exclude_actors','Actors',actors),
         checkboxGroupInput('exclude_ids','IDs',cfids)
      )
   })

   output$fields <- renderUI({

      fields <- list()

      for(f in 1:input$nfields){

         fid <- glue('field_{f}')

         x <- ""
         y <- ""
         if(glue('{fid}_x') %in% names(input)){
            isolate({
               x <- input[[glue('{fid}_x')]] 
               y <- input[[glue('{fid}_y')]] 
            })
         }

         fields[[f]] <- tags$div(id = glue(fid),
            inputPanel(
               #HTML(glue('Group {f}')),
               textInput(glue('{fid}_x'),'Name:',x),
               textInput(glue('{fid}_y'),'Expression(s):',y))
         )
      }
      fields
   })


   groups <- reactive({
      groups <- list()
      if(input$nfields > 0){
         for(f in 1:input$nfields){
            name <- input[[glue('field_{f}_x')]]
            expr <- input[[glue('field_{f}_y')]]
            groups[[name]] <- expr
         }
         groups
      } else {
         groups
      }  
   })
}
