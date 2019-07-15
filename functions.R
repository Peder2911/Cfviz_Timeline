dictReplace <- function(values,dictionary,default){
   # Returns a character vectors with the
   # name of the entry in a list
   # in which a regex mathed with each entry

   keys <- names(dictionary)
   result <- character()
   i <- 1
   for(entry in dictionary){
      replace <- sapply(values,function(value){
                    any(sapply(entry,function(expression){
                       str_detect(value,expression)
                    }))
                 })
      result[replace] <- keys[i]
      i <- i + 1
   }
   result[is.na(result)] <- default
   result
}

dictlookup <- function(values,dictionary){
  fixnull <- function(values){
    sapply(values, function(v){
      ifelse(v == 'NULL',
             NA,
             v)
    })
  }

  dictionary[as.character(values)] %>%
    as.character() %>%
    fixnull()
}
