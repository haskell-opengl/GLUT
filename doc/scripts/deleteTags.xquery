define function deleteTags($x){
 if (node-kind($x) = "text")    then $x
 else if (node-kind($x) = "comment") then " "    
 else if (node-kind($x) = "element")    
  then 
   let $tag := name($x)
   return 
      if ($tag=QName("delete")) then ()
      else element {$tag} { ($x/@*,for $y in $x/node() return deleteTags($y))}
  else $x
}