
library(stringr)
library(dplyr)
library(glue)
library(lubridate)
library(ArmourEverTesty)

#' Fix CF Data
#'
#' Returns a "fixed" version of the ceasefire data
source('functions.R')

fixCfData <- function(cfs,nameDictionary){
   names(cfs) <- str_to_lower(names(cfs))

   #cfs <- filter(cfs, location == config$location)
   #if(nrow(cfs) == 0){
   #   stop(glue('Dataset contained no location "{config$location}"'))
   #}

   typedict <- nameDictionary$types
   purposedict <- nameDictionary$purposes


   #saveRDS(ucdp,'tee/ucdp.rds')

   # Fixing dates ======================================
   # NB! depends on custom pkg. AET

   cfs$start_date <- varsToDates(list(year = cfs$cf_dec_yr,
                                      month = cfs$cf_dec_month,
                                      day = cfs$cf_dec_day))

   cfs$end_date <- varsToDates(list(year = cfs$end_yr,
                                      month = cfs$end_month,
                                      day = cfs$end_day))

   cfs$actor_name <- str_replace_all(cfs$actor_name,'\\s',' ')

   # Codes to factors ==================================

   cfs$type <- dictlookup(cfs$ceasefire_type, typedict)
   cfs$purpose <- dictlookup(cfs$purpose_1, purposedict)


   cfs <- cfs %>%
      select(
         location,
         start = start_date,
         end = end_date,
         id = cf_id,
         actor_name,
         ucdp_dyad,
         purpose,
         type) %>%
      unique()

   # To make visualization easier
   #cfs <- cfs %>%
      #mutate(dyad = str_replace_all(dyad,'Government of ([A-Za-z]+)','\\1 gov.'))
}

   # Subset ============================================
transformData <- function(cfs, ucdp, options){

   if(options$use_ucdp_dyads){
      cfs$ucdp_dyad <- as.numeric(cfs$ucdp_dyad)
      cfs <- merge(cfs,ucdp, by.x = "ucdp_dyad", by.y = "id",
                   all.x = TRUE)
   } else {
      if(options$usegroups){
         cfs <- cfs %>%
            mutate(actor_name = dictReplace(actor_name,options$groups,
                                             options$defaultgroup))
      }

      cfs$ucdp_dyad <- -1
      cfs <- cfs %>%
         group_by(start_date, end_date, purpose, type) %>%
         summarize(name = glue_collapse(sort(unique(actor_name)), sep = ' - '),
                   ucdp_dyad = min(ucdp_dyad),
                   cf_id = min(cf_id))
   }

   filter(cfs, location == options$location)
}

   # Ucdp dyads (for names) ============================
fixUcdp <- function(raw){
   raw %>%
      mutate(actor_name = paste(str_replace_all(side_a,'\\([^\\)]+\\)',''),
                          str_replace_all(side_b,'\\([^\\)]+\\)',''),
                          sep = ' - ')) %>%
      select(actor_name, id = dyad_id) %>%
      mutate(id = as.numeric(id)) %>%
      unique()
}
