define function makeverbatim($x,$l) {
    let $commandchars := if ($x/@commandchars) then $x/@commandchars
                               else "?~^" 
    return
    ("\begin{Verbatim}[commandchars=",$commandchars,",fontfamily=courier
                      ,numbers=left,frame=single"
    ,if ($x/@sequel) 
     then (",firstnumber=last") else ""
    ,if ($x/@class) 
     then (",label="
          ,$x/@class
          ,if ($x/@lang) then (".",$x/@lang) else ".java"
          )
     else "" 
    ,"]
"
    ,further($x,$l),"
\end{Verbatim}

"
    )
}

define function startAufgabenCounter() {"\setcounter{aufgabe}{0}"}

define function bfverbatim($x,$lang) {
    ("~?bf ",further($x,$lang),"^")
}

define function itverbatim($x,$lang) {
    ("~?it ",further($x,$lang),"^")
}

define function redverbatim($x,$lang) {
    ("~?color~red^",further($x,$lang),"^")
}


define function yellowverbatim($x,$lang) {
    ("~?color~yellow^",further($x,$lang),"^")
}

define function blueverbatim($x,$lang) {
    ("~?color~blue^",further($x,$lang),"^")
}

define function greenverbatim($x,$lang) {
    ("~?color~green^",further($x,$lang),"^")
}


define function greyverbatim($scale,$x,$lang) {
    ("~?color[gray]~",$scale,"^",further($x,$lang),"^")
}

define function footnoteverbatim($x,$lang) {
    ("?footnote~",further($x,$lang),"^")
}



transform(deleteTags(input())/*,"")
 
