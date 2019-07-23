
function(cfs_sub, ucdp, input, usegroups, groups){

   # =================================================
   # Grouping ========================================
   # This one is made a bit  complex by the fact that 
   # the UI is adaptive. 
   
   # Really quite simple: 
   # the appropriate info from "input" using glue
   # and regular expressions 
   if(input$usegroup){
      actornames <- unique(cfs_sub$name)

      actorgroup <- character(length(actornames))
      names(actorgroup) <- actornames

      for(name in names(input)){
         if(str_detect(name, 'actor_[0-9]{1,3}_group$')){
            actornumber <- as.numeric(str_extract(name,'[0-9]+'))

            if(input[[name]] == 'No group'){
               actorgroup[actornumber] <- 'No group' 
            } else {
               # Get the group name from
               # appropriate box
               groupnumber <- as.numeric(input[[name]])
               groupname <- isolate(input[[glue('group_{groupnumber}_name')]])
               print(glue('Adding {groupname} to {name}'))
               actorgroup[actornumber] <- groupname
            }

         }
      }

      cfs_sub$name <- ifelse(actorgroup[cfs_sub$name] != 'No group',
                             actorgroup[cfs_sub$name],
                             cfs_sub$name)
   } 

   # =================================================
   # UCDP naming =====================================
   if (input$naming == 'UCDP'){
      cfs_sub$ucdp_dyad <- as.numeric(cfs_sub$ucdp_dyad)
      cfs_sub <- merge(cfs_sub, ucdp, 
                       by.x = 'ucdp_dyad', by.y = 'id', all.x = TRUE)
      cfs_sub$name <- cfs_sub$actor_name
      cfs_sub[is.na(cfs_sub$name),'name'] <- 'missing'

      cfs_sub <- cfs_sub %>%
         group_by(start,year,purpose,type,location) %>%
            summarize(name = glue_collapse(sort(unique(name)), 
                                           sep = ' & '),
                      ucdp_dyad = glue_collapse(as.character(ucdp_dyad), 
                                                sep = ' & '),
                      id = min(id))
      cfs_sub <- unique(cfs_sub)

   # =================================================
   # Actor naming ====================================
   } else {
      cfs_sub$ucdp_dyad <- -1
      cfs_sub <- cfs_sub %>%
         group_by(start, year, purpose, type, location, id) %>%
            summarize(name = glue_collapse(sort(unique(name)), sep = ' - '),
                      ucdp_dyad = min(ucdp_dyad))
   }

   cfs_sub
}
