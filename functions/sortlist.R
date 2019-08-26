
function(x,...){
   num <- unlist(x) %>%
      str_extract('[0-9]+') %>%
      as.numeric()
   x[order(order(num),...)]
}
