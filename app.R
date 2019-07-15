library(shiny)
library(glue)
library(stringr)
library(rjson)

ui <- fluidPage(
   selectInput("nfields","number of fields",1:10),
   #textOutput("groups"),
   uiOutput("fields")
)

server <- function(input, output, session){

   output$fields <- renderUI({

      fields <- list()

      for(f in 1:input$nfields){

         print(glue('Doing field {f}'))
         fid <- glue('field_{f}')

         x <- ""
         y <- ""
         if(glue('{fid}_x') %in% names(input)){
            x <- 'foo'
            y <- 'bar' 
         }

         fields[[f]] <- tags$div(id = glue(fid),
            sidebarPanel(
               HTML(glue('Group {f}')),
               textInput(glue('{fid}_x'),'Name:'),
               textInput(glue('{fid}_y'),'Expression(s):'))
         )
      }
      fields
   })
}

   #output$groups <- reactive({
      #groups <- list()
      #if(input$nfields > 0){
         #for(f in 1:input$nfields){
            #name <- input[[glue('field_{f}_x')]]
            #expr <- input[[glue('field_{f}_y')]]
            #groups[[name]] <- expr
         #}
         #toJSON(groups)
      #} else {
         #""
      #}  
   #})

shinyApp(ui, server, options = list(port = 1337))
               
