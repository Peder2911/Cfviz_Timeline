function(x){
   x <- gsub("[^A-Za-z ]","",x) 
   gsub("\\s+"," ",x)
}
