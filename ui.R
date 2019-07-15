
ui <- fluidPage(
   mainPanel(
      plotOutput('plot')
   ),
   sidebarPanel(
      fluidRow(
         uiOutput('location'),
         actionButton('plot','Plot')
      ),
      fluidRow(
         #actionButton('refresh','Refresh'),
         uiOutput('daterange')
      ),
      tags$hr(),

      fluidRow(
         selectInput('naming','Naming scheme',c('Actor names','UCDP','Custom')),
         
         tableOutput('allactors')
      ),

      fluidRow(
         #checkboxInput('useucdp','Use UCDP dyads',FALSE),
         #checkboxInput('usegroups','Use groups',FALSE),
         conditionalPanel('input.naming == \'Custom\'',
            selectInput('nfields','Number of groups',1:10),
            actionButton('pushgroup','+'),
            actionButton('popgroup','-'),
            textInput('defaultgroup','Default group:','Others'),
            #textOutput("groups"),
            uiOutput('fields')
         )
      ),
      checkboxInput('exclude','Exclude',FALSE),
      conditionalPanel('input.exclude == true',{ 
         uiOutput('exclude')
      })
   )
)
