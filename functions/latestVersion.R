
function(dir,name){
   files <- list.files(dir) 
   relevantfiles <- files[str_detect(files,name)] 

   if(length(relevantfiles) == 0){
      stop(glue('No files matched {name} in {dir}'))
   }

   filenumbers <- relevantfiles %>%
      sapply(str_replace_all,
             pattern = '[^0-9]',
             replacement = '') %>%
      as.numeric()

   names(filenumbers) <- relevantfiles
   sorted <- sort(filenumbers, decreasing = TRUE)

   if(length(sorted) == 0){
      stop(glue('No files contained numbers')) 
   } else {
      latest <- sorted[1]
   }
   list(path = file.path(dir,names(latest)),
        filename = names(latest),
        version = latest)
}
