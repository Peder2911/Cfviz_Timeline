
function(cfs){
   str_split(cfs$name, ' +- +') %>%
      do.call(c, .) %>%
      unique() %>%
      sort()
}
