
ui <- fluidPage(
   mainPanel(
      fluidRow(
         plotOutput('plot')
      ),
      tags$hr(),
      fluidRow(
         column(3,
            downloadButton('download','Download plot')
         ),
         column(3,
         selectInput('plotformat',NULL,c('png','svf'))
         ),
         column(3,
         checkboxInput('separatelegend','Separate legend')
         )
      ),
      fluidRow(
         column(3,
         numericInput('plotheight','Height',value = 5,min = 0, max = 15)
         ),
         column(3,
         numericInput('plotwidth','Width',value = 10,min = 0, max = 25)
         ),
         column(3,
         selectInput('plotunits','Units',c('cm','in'))
         )
      ),
      conditionalPanel('input.debug == true',
         textOutput('missingids'),
         tableOutput('debugdat')
      )
   ),


   sidebarPanel(
      # =================================================
      fluidRow(
         #uiOutput('location'),
         selectInput('location','Location',NULL),
         actionButton('plot','Plot')
      ),
      tags$hr(),

      # =================================================
      fluidRow(
         #actionButton('refresh','Refresh'),
         numericInput('startyear','Start year',1989, min = 1989),
         numericInput('endyear','End year',2019, max = 1989)
         #dateRangeInput('daterange','Date range'),
         #uiOutput('daterange')
      ),
      tags$hr(),
      fluidRow(
         selectInput('naming','Naming scheme',c('Actor names','UCDP'))
         #tableOutput('allactors')
      ),

      # Others grouping =================================
      tags$hr(),
      fluidRow(
         checkboxInput('usegroup','Group actors'),
         conditionalPanel('input.usegroup == true',
            actionButton('cleargrouped','Clear'),
            actionButton('allgrouped','Select all'),
            textInput('groupname','Group name:','Others'),
            checkboxGroupInput('grouped_actors', 'Grouped actors:', NULL)
         )
      ),

      # Include some ====================================
      tags$hr(),
      fluidRow(
         checkboxInput('showincl','Show included'),
         conditionalPanel('input.showincl == true',
            actionButton('clearincl','Clear'),
            actionButton('allincl','Select all'),
            checkboxGroupInput('include_actors','Actors',NULL),
            checkboxGroupInput('include_ids','IDs',NULL)
         )
      ),
      checkboxInput('debug','Debug',FALSE)
   )
)