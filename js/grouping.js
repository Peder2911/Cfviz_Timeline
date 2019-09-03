
getActorGrouping = function(){
   var groupedActors = [];
   var $grps = $("#actorGroups select");
   $grps.each(function(i){
      var groupnumber = i + 1;
      var actors = $grps.eq(i).val();
      $.each(actors, function(i, act){
         groupedActors.push({name: act, group: groupnumber});
      });
   });
   return groupedActors;
}

getGroupNames = function(){
   var groups = [];
   var $textboxes = $("#groupNames input:text")
   $textboxes.each(function(i){
      var grpname = $textboxes.eq(i).val(); 
      groups.push({group: String(i+1), groupname: grpname});
   })
   return groups;
}

$(document).ready(function(){
   $("#groupingStuff").on("change","select, input:text",function(){
      Shiny.onInputChange("actorGrouping",JSON.stringify(getActorGrouping()));
      Shiny.onInputChange("groupNames",JSON.stringify(getGroupNames()));
   })
})
