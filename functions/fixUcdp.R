
function(raw){
   raw %>%
      mutate(actor_name = paste(str_replace_all(side_a,'\\([^\\)]+\\)',''),
                          str_replace_all(side_b,'\\([^\\)]+\\)',''),
                          sep = ' - ')) %>%
      select(actor_name, id = dyad_id) %>%
      mutate(id = as.numeric(id)) %>%
      unique()
}
