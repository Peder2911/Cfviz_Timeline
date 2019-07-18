
function(cfs,nameDictionary){
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

   names(cfs) <- str_to_lower(names(cfs))

   cfs$start_date <- suppressWarnings(varsToDates(list(year = cfs$cf_dec_yr,
                                      month = cfs$cf_dec_month,
                                      day = cfs$cf_dec_day), fixNaMonth = TRUE)) 
   #cfs <- filter(cfs, !is.na(start_date))

   cfs$end_date <- suppressWarnings(varsToDates(list(year = cfs$end_yr,
                                      month = cfs$end_month,
                                      day = cfs$end_day), fixNaMonth = TRUE))

   cfs$year <- year(cfs$start_date)

   cfs$actor_name <- str_replace_all(cfs$actor_name,'\\s',' ')
   cfs$actor_name <- str_replace_all(cfs$actor_name,' +',' ')

   cfs$type <- dictlookup(cfs$ceasefire_type, nameDictionary$types)
   cfs$purpose <- dictlookup(cfs$purpose_1, nameDictionary$purposes)

   cfs %>%
      select(
         location,
         start = start_date,
         #end = end_date,
         id = cf_id,
         actor_name,
         ucdp_dyad,
         purpose,
         type,
         year,
         name = actor_name)
}

