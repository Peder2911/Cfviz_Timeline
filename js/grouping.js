getActorGrouping = function(){
   var actorGrouping = [];
   var boxes = $("#actor_grouping").find(".actor_grouping_box");
   boxes.each(function(i, el){
      var grp = $(el).find("select").val();
      actorGrouping.push({name: el.id, group: grp}) ;
   })
   return actorGrouping;
}

getGroupNames = function(){
   var groups = [];
   var boxes = $("#group_names").find(".group_name_box")
   boxes.each(function(i,el){
      var grpname = $(el).find("input:text").val();
      groups.push({group: String(i+1), groupname: grpname});
   })
   return groups;
}

$(document).ready(function(){
   $("#update_groups").click(function(){
      Shiny.onInputChange("actorGrouping",JSON.stringify(getActorGrouping()));
      Shiny.onInputChange("groupNames",JSON.stringify(getGroupNames()));
   })

   /* NOT IMPLEMENTED
   $("#ngroups").on("change", function(){
      var ngroups = $("#ngroups").val()
      var range = [];
      for(var i; i < ngroups; i++){
         range.push({i: i})
      }

      var boxes = $("#actor_grouping").find(".groupingbox");
      boxes.each(function(i,el){
         var sel = $(el).find("select");
         console.log(sel.val());
         sel.empty();
         $.each(range, function(key, value){
            $el.append($("<option></option>")
               .attr("value",value).text(key))});

         });
      console.log("it changed!")
   })
   */
})
