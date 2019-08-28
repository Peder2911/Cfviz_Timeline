
ui <- fluidPage(
   mainPanel(
      fluidRow(
         plotOutput('plot')
      ),
      tags$hr(),
      fluidRow(
         column(3,
            downloadButton('downloadpng','Download png'),
            downloadButton('downloadeps','Download eps')
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
      )
   ),


   sidebarPanel(
      # Location ========================================
      fluidRow(
         selectInput('location','Location',NULL),
         actionButton('plot','Plot')
      ),
      tags$hr(),

      # Time ============================================
      fluidRow(
         numericInput('startyear','Start year',1989, min = 1989, max = 2019),
         numericInput('endyear','End year',2019, min = 1989, max = 2019)
      ),

      # Lumping ========================================
      tags$hr(),

      fluidRow(
         column(6,
            checkboxInput('lumpnames','Group names'),
            sliderInput('lumpsize','Keep n:',min=0,max = 10, value = 0)
            ),
         column(6)
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
      )
   )
)
