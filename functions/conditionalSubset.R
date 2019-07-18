
function(data,input){
   if(!is.null(input$include_actors)){
      containsIncluded <- sapply(data$name,function(name){
         any(sapply(input$include_actors,function(incl){
                       str_detect(name,fixed(incl))}))
      }) 
      data <- data[containsIncluded,]
   }

   if(!is.null(input$include_ids)){
      data <- data[data$id %in% input$include_ids,]
   }

   data %>%
      filter(year >= input$startyear,
             year <= input$endyear)
}
