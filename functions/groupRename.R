
function(cfs, groupnames, actorgroups){
   saveRDS(actorgroups, '/tmp/actgrps.rds')

   for(i in 1:length(actorgroups)){
      actorgroup <- actorgroups[[i]]
      actorname <- names(actorgroups)[[i]]
      groupIndex <- as.numeric(actorgroup)

      if(!is.na(groupIndex) & !is.null(groupIndex)){
         groupname <- groupnames[[as.numeric(actorgroup)]]
         cfs$name <- str_replace_all(cfs$name,actorname,groupname)
         saveRDS(list(gindex = groupIndex, actname = actorname,
                      grpname = groupname), '/tmp/eek.rds')
      }
   }
   cfs
}
