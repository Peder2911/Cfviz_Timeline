
ui <- fluidPage(
   includeScript('js/grouping.js'),
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
         column(6,
            numericInput('startyear','Start year',1989, min = 1989, max = 2019)
         ),
         column(6,
            numericInput('endyear','End year',2019, min = 1989, max = 2019)
         )
      ),

      # Lumping ========================================
      tags$hr(),
      tags$h3("Names"),
      checkboxInput('lumpnames','Group names'),
      tabsetPanel(
         tabPanel("Group",
            tags$br(),
            tags$p("Group actors using a custom grouping setup"),
            actionButton('update_groups','Update!'),
            fluidRow(
               column(6,
                  uiOutput("groups")
               ),
               column(6,
                  uiOutput("groupnames")
               )
            )
         ),
         tabPanel("Lump",
            tags$br(),
            tags$p("Include n actors while grouping others as \"other\""),
            sliderInput('lumpsize','Keep n:',min=0,max = 10, value = 0)
         ),
         tabPanel("Subset",
            tags$br(),
            tags$p("Only show certain ceasefires, or actors"),
            fluidRow(
               column(6,
                  checkboxGroupInput('include_actors','Actors',NULL)
               ),
               column(6,
                  checkboxGroupInput('include_ids','IDs',NULL)
               )
            )
         )
      )
   )
)
