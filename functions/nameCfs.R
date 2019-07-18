
function(cfs_sub, ucdp, input, groups){
   if(input$usegroup){
      cfs_sub$name <- ifelse(cfs_sub$name %in% input$grouped_actors,
                          input$groupname,
                          cfs_sub$name)
   } 

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
      #excluded <- character()
   } else {
      cfs_sub$ucdp_dyad <- -1
      cfs_sub <- cfs_sub %>%
         group_by(start, year, purpose, type, location, id) %>%
            summarize(name = glue_collapse(sort(unique(name)), sep = ' - '),
                      ucdp_dyad = min(ucdp_dyad))
                      #id = min(id))
   }
   cfs_sub
}
