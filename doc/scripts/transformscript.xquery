define function site(){"http://www.panitz.name/"}

define function startLatex($lang,$docstyle,$packages){
("\documentclass[10pt,a4paper]{",$docstyle,"}
\usepackage[",$lang,"]{babel}

",$packages,"

%\usepackage[square]{natbib}

\usepackage{multind}

\usepackage{dsfont}



\setlength\textwidth{14cm}
%\usepackage{moreverb }
\usepackage{fancyvrb }
\usepackage{color }

\usepackage{longtable }

\usepackage{floatflt }
 
\newif\ifpdf
 \ifx\pdfoutput\undefined
   \pdffalse
\else
   \pdfoutput=1
   \pdftrue
\fi


\ifpdf
  \makeindex{Klassen}
\else
\fi 

\pagestyle{headings}

 
\ifpdf
  \usepackage[pdftex]{graphicx}
  \pdfcompresslevel =9
\else
  \usepackage{epsf}
\fi

\usepackage[]{fontenc}
%\usepackage{german}
\usepackage[isolatin]{inputenc}

\setlength\parindent{0pt}
\setlength{\parskip}{5pt plus 2pt minus 1pt}
\newcounter{unteraufgabe}
\setcounter{unteraufgabe}{1}

\newcounter{aufgabe}
"
,startAufgabenCounter()
,"
\setcounter{aufgabe}{0}

\sloppy

\title{Skript}

\begin{document}

",startAufgabenCounter(),"

"
)
}

define function startKlausurLatex(){
"\documentclass[10pt]{article}
\setlength\textwidth{14cm}
\usepackage[german]{babel}
\usepackage{longtable }
\usepackage[]{fontenc}
\usepackage[isolatin]{inputenc}

\usepackage{fancyhdr}
\pagestyle{fancy}

\usepackage{fancyvrb }

\usepackage{multind }


\setlength\parindent{0pt}
\setlength{\parskip}{5pt plus 2pt minus 1pt}
\newcounter{unteraufgabe}
\setcounter{unteraufgabe}{1}

\newcounter{aufgabe}
\setcounter{aufgabe}{0}

\sloppy


\newif\ifpdf
 \ifx\pdfoutput\undefined
   \pdffalse
\else
   \pdfoutput=1
   \pdftrue
\fi

\ifpdf
  \makeindex{Klassen}
\else
\fi 

\ifpdf
  \usepackage[pdftex]{graphicx}
  \pdfcompresslevel =9
\else
  \usepackage{epsf}
\fi


\begin{document}
\renewcommand{\footrulewidth}{0.4pt}

"
}


define function startLoesungenLatex(){
"\documentclass[10pt,a4paper]{article}
\usepackage[german]{babel}
\usepackage{longtable }
\usepackage[]{fontenc}
\usepackage[isolatin]{inputenc}


\setlength\parindent{0pt}
\setlength{\parskip}{5pt plus 2pt minus 1pt}
\newcounter{unteraufgabe}
\setcounter{unteraufgabe}{1}

\newcounter{aufgabe}
\setcounter{aufgabe}{0}

\sloppy

\begin{document}
%\renewcommand{\footrulewidth}{0.4pt}
\small
"
}


define function endLatex(){

"

%\vfill Aus dem XML-Quelltext nach \LaTeX\ übersetzt mit Quip.
\end{document}"}

define function sprecher($sprecher,$text){
("
\item[{\bf ",$sprecher,":}] ",further($text))
}

define function aufgaben($as,$lang){
  ("\chapter{Gesammelte Aufgaben}
\setcounter{aufgabe}{0}"
  ,transform(<dummy>{$as}</dummy>,$lang))
}

define function transformAll($xs,$lang){
  for $x in $xs return transform($x,$lang)}

define function transform($x,$lang){
  let $tag := name($x)
  return
  if (node-kind($x) = "text") then $x
  else if ($tag=QName("skript")) then
    (startLatex(string($x/@lang),"report","%\usepackage{chappg }")
    ,further($x,string($x/@lang))
    ,endLatex())
  else if ($tag=QName("articleSkript")) then
    (startLatex(string($x/@lang),"article","")
    ,further($x,string($x/@lang))
    ,endLatex())
  else if ($tag=QName("book")) then
    (startLatex(string($x/@lang),"book","")
    ,further($x,string($x/@lang))
    ,endLatex())
  else if ($tag=QName("loesungen")) then
    (startLoesungenLatex(),further($x,$lang),endLatex())
  else if ($tag=QName("klausur")) then
    (startKlausurLatex()
    ,"\cfoot{",$x/@titel," (Seite \thepage)}
\centerline{\Large \bf  ",$x/@titel,"}
"
    ,"\lhead{Name:}
"
    ,"\rhead{Matrikelnummer:\hspace*{3cm}}
\lfoot{",$x/@semester,"}\rfoot{",$x/@datum,"}

"
    ,further($x,$lang),endLatex())
  else if ($tag=QName("artikelTitel")) then (artikelTitel($x,$lang),"
\maketitle
")
  else if ($tag=QName("titelseite")) then ("
\begin{titlepage}
\begin{center}
",further($x,$lang),"
\end{center}
\end{titlepage}
")
  else if ($tag=QName("titel")) then
("{\Huge ",further($x,$lang),"}



")

  else if ($tag=QName("untertitel")) then
("{\Large ",further($x,$lang),"}




")
  else if ($tag=QName("intitution")) then
("~\\[1cm]{\Large ",further($x,$lang),"}

")
  else if ($tag=QName("autor")) then
("\vfill
{\Large ",further($x,$lang),"}

")
  else if ($tag=QName("institution")) then
("{\large \bf ",further($x,$lang),"}

{\bf Version \today}

")
  else if ($tag=QName("disclaimer")) then
("\vfill
\begin{quote} ",further($x,$lang),"\end{quote}

")
  else if ($tag=QName("part")) then
    ("
\part",if ($x/@nonumbering) then "*" else "","{",$x/@titel,"}
",further($x,$lang))
  else if ($tag=QName("kapitel")) then
    ("
\chapter",if ($x/@nonumbering) then "*" else "","{",$x/@titel,"}
",further($x,$lang))
  else if ($tag=QName("section")) then
    ("
\section",if ($x/@nonumbering) then "*" else "","{",$x/@titel,"}
",further($x,$lang))
  else if ($tag=QName("subsection")) then
    ("
\subsection{",$x/@titel,"}
",further($x,$lang))
  else if ($tag=QName("subsubsection")) then
    ("
\subsubsection{",$x/@titel,"}
",further($x,$lang))
  else if ($tag=QName("paragraph")) then
    ("
\paragraph{",$x/@titel,"}
",further($x,$lang))
  else if  ($tag=QName("regel")) then
  ("\begin{minipage}{0.98\textwidth}
{\bf ",$x/nt," ::=}\\[0.2cm]
\hspace*{\fill}\begin{minipage}{0.9\textwidth}\begin{sl}"
,further($x/lhs,$lang),"\end{sl}\end{minipage}
\end{minipage}\\[0.5cm]
")
  else if  ($tag=QName("minipage")) then
  let $align := if ($x/@align) then $x/@align else "t"
  return 
("\begin{minipage}[",$align,"]{",$x/@dimension,"}"
,further($x,$lang),"\end{minipage}")
  else if  ($tag=QName("indent")) then
   "\hspace*{0.4cm}"
  else if ($tag=QName("alts")) then
  (let $alts := $x/alt
   let $c    := count($alts)
   for $i    in 1 to $c
   let $alt  := $alts[$i]
   return
    (if ($i=1) then "~" else ""  
    ,if ($i=$c) then further($alt,$lang)
     else (further($alt,$lang),"\\
$\mid$")))
  else if ($tag=QName("bar")) then "$\mid$"
  else if ($tag=QName("anhang")) then
   ("\appendix

",further($x,$lang))
  else if ($tag=QName("wortliste")) then
   ('\chapter{W"orterliste}
',further($x/erklaerung,$lang),'
\begin{center}
\begin{longtable}{l|l}
{\bf Deutsch}&{\bf Englisch}\\\hline
',for $entry in $x/wliste/entry sortby (upper-case(./deutsch/text()))
  return ($entry/deutsch,"&",$entry/english,"\\
"),"
\end{longtable}
\end{center}


\begin{center}
\begin{longtable}{l|l}
{\bf Englisch}&{\bf Deutsch }\\\hline
",for $entry in $x/wliste/entry sortby (upper-case(./english/text()))
  return ($entry/english,"&",$entry/deutsch,"\\
")
,"
\end{longtable}
\end{center}
")
  else if ($tag=QName("resetAufgabenCounter")) then
   "\setcounter{aufgabe}{0}"
  else if ($tag=QName("aufgabe")) then
    ("
\paragraph{\mbox{Aufgabe~\arabic{aufgabe}}} \addtocounter{aufgabe}{1}\setcounter{unteraufgabe}{1}
",if ($x/@punkte) then ("{\bf (",$x/@punkte," Punkte)}\\ ") else "",further($x,$lang))
  else if ($tag=QName("loesung")) then
    ("


{\bf  LÃ¶sung}\\
",further($x,$lang))
  else if ($tag=QName("itemize")) then
    ("
\begin{itemize}
",further($x,$lang),"
\end{itemize}
")
  else if ($tag=QName("item")) then
    ("
\item ",further($x,$lang))
  else if ($tag=QName("unteraufgaben")) then
    ("
\begin{itemize}
",further($x,$lang),"
\end{itemize}
")
  else if ($tag=QName("teil")) then
    ("
\item[{\bf \alph{unteraufgabe})}] \addtocounter{unteraufgabe}{1}%
",further($x,$lang))
  else if ($tag=QName("dialog")) then
    ("
\begin{description}
",further($x,$lang),"
\end{description}
")
  else if ($tag=QName("fbox")) then
    ("\fbox{",further($x,$lang),"}")
  else if ($tag=QName("hfill")) then "\hfill "
  else if ($tag=QName("vfill")) then "\vfill "
  else if ($tag=QName("amp")) then "&"
  else if ($tag=QName("eq")) then "&=&"
  else if ($tag=QName("reduce")) then "&\rightarrow&"
  else if ($tag=QName("and")) then "\wedge"
  else if ($tag=QName("or")) then "\vee"
  else if ($tag=QName("index")) then
    "" 
  else if ($tag=QName("em")) then
    ("{\em ",further($x,$lang),"}")
  else if ($tag=QName("m")) then
    ("$",further($x,$lang),"$")
  else if ($tag=QName("cal")) then
    ("{$\cal ",further($x,$lang),"$}")
  else if ($tag=QName("sl")) then
    ("{\sl ",further($x,$lang),"}")
  else if ($tag=QName("bf")) then
    ("{\bf ",further($x,$lang),"}")
  else if ($tag=QName("b")) then
    ("{\bf ",further($x,$lang),"}")
  else if ($tag=QName("ttt")) then
    ("{\tt ",further($x,$lang),"}")
  else if ($tag=QName("mb")) then
    ("\mbox{\bf ",further($x,$lang),"}")
  else if ($tag=QName("mathText")) then
    ("\mbox{\em ",further($x,$lang),"}")
  else if ($tag=QName("math")) then
    ("$",further($x,$lang),"$")
  else if ($tag=QName("displaymath")) then
    ("\begin{displaymath}",further($x,$lang),"\end{displaymath}")
  else if ($tag=QName("subscript")) then
    ("_{",further($x,$lang),"}")
  else if ($tag=QName("superscript")) then
    ("^{",further($x,$lang),"}")
  else if ($tag=QName("sum")) then
    ("\sum",further($x,$lang),"")
  else if ($tag=QName("von")) then
    ("_{",further($x,$lang),"}")
  else if ($tag=QName("bis")) then
    ("^{",further($x,$lang),"}")
  else if ($tag=QName("mT")) then
    ("\mbox{\em ",further($x,$lang),"}")
  else if ($tag=QName("m")) then
    ("$",further($x,$lang),"$")
  else if ($tag=QName("lb")) then "\-"
  else if ($tag=QName("mbox")) then
    ("\mbox{",further($x,$lang),"}")
  else if ($tag=QName("red")) then
    ("{\color{red}",further($x,$lang),"}")
  else if ($tag=QName("tt")) then
    (if (contains($x/text()[1],"+")) then ("\verb#",further($x,$lang),"#") 
    else ("\verb+",further($x,$lang),"+"))
  else if ($tag=QName("token")) then
    (if (contains($x/text()[1],"+")) then ("\verb-",further($x,$lang),"-") 
    else ("\verb+",further($x,$lang),"+"))
  else if ($tag=QName("footnote")) then
    ("\footnote{",further($x,$lang),"}")
  else if ($tag=QName("code")) then
    (if ($x/@class) then ("

\index{Klassen}{",$x/@class,"}") else ""
    ,makeverbatim ($x,$lang))
  else if ($tag=QName("bv")) then bfverbatim ($x,$lang)
  else if ($tag=QName("itv")) then itverbatim ($x,$lang)
  else if ($tag=QName("redv")) then redverbatim ($x,$lang)
  else if ($tag=QName("yellowv")) then yellowverbatim ($x,$lang)
  else if ($tag=QName("bluev")) then blueverbatim ($x,$lang)
  else if ($tag=QName("greenv")) then greenverbatim ($x,$lang)
  else if ($tag=QName("greyv")) then greyverbatim($x/@scale,$x,$lang)
  else if ($tag=QName("footnotev")) then footnoteverbatim($x,$lang)
  else if ($tag=QName("w")) then "~"
  else if ($tag=QName("ws")) then "  "
  else if ($tag=QName("nl")) then " 
"
  else if ($tag=QName("cite")) then
    ("\cite{",$x/@label,"}")
  else if ($tag=QName("scode")) then
    ("

\begin{footnotesize}\begin{verbatim}
",further($x,$lang),"
\end{verbatim}
\end{footnotesize}

")

  else if ($tag=QName("wichtig")) then
    ("

\begin{quote}{\bf
",further($x,$lang),"
}\end{quote}

")
  else if ($tag=QName("verb")) then
    ("

\begin{verbatim}
",further($x,$lang),"
\end{verbatim}

")
  else if ($tag=QName("quote")) then
    ("\begin{quote}
",further($x,$lang),"
\end{quote}")
  else if ($tag=QName("kommentar")) then
    ("\begin{quote}
",further($x,$lang),"
\hspace*{\fill}{\em ",$x/@who,"}
\end{quote}")
  else if ($tag=QName("zitat")) then
    ("\begin{quote}
",further($x/derText,$lang),"\\
\hspace*{\fill}{\em ",further($x/wer,$lang),"}
\end{quote}")
  else if ($tag=QName("eqnarray")) then
    ("
\begin{eqnarray*}",further($x,$lang),"
\end{eqnarray*}")
  else if ($tag=QName("beispiel")) then
    ("\begin{quote}{\bf ",if ($lang="english") then
       "Example" else "Beispiel",":}\\",further($x,$lang),"
\end{quote}")
  else if ($tag=QName("example")) then
    ("\begin{quote}{\bf ",if ($lang="english")then
     "Example" else "Beispiel",":}\\",further($x,$lang),"
\end{quote}")
  else if ($tag=QName("center")) then
    ("\begin{center}
",further($x,$lang),"
\end{center}")
  else if ($tag=QName("link")) then
    (further($x,$lang)," {\footnotesize (\verb+",site(),$x/@local,$x/@address,"+)} ")
  else if ($tag=QName("exlink")) then
    (further($x,$lang)," {\footnotesize (\verb+",$x/@address,"+)} ")

  else if ($tag=QName("ref")) then
    ("\ref{",$x/@name,"}")

  else if ($tag=QName("bild")) then
    let $psscale := if ($x/@psscale) then $x/@psscale else 1.0
    let $pdfscale := if ($x/@pdfscale) then $x/@pdfscale else 1.0
    let $centerline :=if (not($x/@nocenter)) then "\centerline" else""
    let $float := not($x/@nofloat)
    return bild($float,$psscale,$pdfscale,$centerline,$x,$lang) 
  else if ($tag=QName("bilder")) then
    let $float := not($x/@nofloat)
    return bilder($float,"",$x,$lang) 
  else if ($tag=QName("figure")) then
     figure("true",$x,$lang) 
  else if ($tag=QName("img")) then
    ("\ifpdf 
        \centerline{\includegraphics{images/",$x/@name,".pdf}}
 \else
        \centerline{\epsfbox{images/",$x/@name,".epsf}}
 \fi")  
  else if ($tag=QName("table")) then
   ("\begin{longtable}{",$x/@layout,"}
    ",further($x,$lang),"
    \end{longtable}
")
  else if ($tag=QName("array")) then
   ("\begin{array}{",$x/@layout,"}
    ",further($x,$lang),"
    \end{array}
")
  else if ($tag=QName("longtable")) then
   ("\begin{longtable}{",$x/@layout,"}
    ",further($x,$lang),"
    \end{longtable}
")
  else if ($tag=QName("zeile")) then ("
",zelle($x,$lang),"\\")
  else if ($tag=QName("zelle")) 
   then (further($x,$lang),if (./last() = ./position()) then "" else "&")
  else if ($tag=QName("hline")) then ("\hline")
  else if ($tag=QName("printindex")) then 
    ("\printindex"
    ,if ($x/@name)  then ("{",$x/@name,"}") else "" 
    ,if ($x/@titel) then ("{",$x/@titel,"}") else "" 
    )
  else if ($tag=QName("br")) then"\\"
  else if ($tag=QName("dots")) then"\dots"
  else if ($tag=QName("white")) then"~"
  else if ($tag=QName("w")) then"~"
  else if ($tag=QName("rpar")) then"\}"
  else if ($tag=QName("pi")) then"\pi"
  else if ($tag=QName("alpha")) then"\alpha"
  else if ($tag=QName("beta")) then"\beta"
  else if ($tag=QName("lpar")) then"\{"
  else if ($tag=QName("otimes")) then"\otimes "
  else if ($tag=QName("oplus")) then"\oplus "
  else if ($tag=QName("quot")) then '"$~$'
  else if ($tag=QName("rightarrow")) then"$\rightarrow$"
  else if ($tag=QName("setN")) then"$\mathds{N}$"
  else if ($tag=QName("setZ")) then"$\mathds{Z}$"
  else if ($tag=QName("setQ")) then"$\mathds{Q}$"
  else if ($tag=QName("setR")) then"$\mathds{R}$"
  else if ($tag=QName("setC")) then"$\mathds{C}$"
  else if ($tag=QName("subset")) then"$\subset$"
  else if ($tag=QName("bibliography")) then" 
\bibliography{bib}
\bibliographystyle{alpha}
"
  else if ($tag=QName("p")) then ("


",further($x,$lang))
  else if ($tag=QName("abstand")) then"~~~~~"
  else if ($tag=QName("eject")) then"
\eject
"
  else if ($tag=QName("LaTeX")) then"\LaTeX"
  else if ($tag=QName("toc")) then "\tableofcontents 
" 
  else if ($tag=QName("listoffigures")) then "\listoffigures 
" 
  else if ($tag=QName("alleaufgaben")) then
   let $del := deleteTags(input())
   return  aufgaben(($del/*/part/kapitel//aufgabe
                    ,$del/*/kapitel//aufgabe
                    ),$lang)
  else if ($tag=QName("include")) 
    then let $doc := document((string($x/@uri))) 
          return ("
\",if (($x)/@asThis) then $x/@asThis else "chapter","{",$doc/*/titelseite/titel,"}
",transformAll($doc//section,$lang))

  else if ($tag=QName("pakete")) then ()
  else if ($tag=QName("delete")) then ()
  else if ($tag=QName("deleted")) then ()
  else further($x,$lang)
}   

define function zelle($x,$lang){
  let $zs := $x/zelle
  let $l := count($zs)
  for $i in 1 to $l
  return if ($i=$l) then further($zs[$i],$lang) else (further($zs[$i],$lang),"&")
}

define function further($x,$lang) {
  for $y in $x/node()
  return transform($y,$lang)
}

define function artikelTitel($x,$lang) {
  for $y in $x/node()
  return transformTitel($y,$lang)
}

define function transformTitel($x,$lang){
  let $tag := name($x)
  return
  if (node-kind($x) = "text") then $x
  else if ($tag=QName("titel")) then ("\title{",further($x,$lang),"}")
  else if ($tag=QName("autor")) then ("\author{",further($x,$lang),"}")
  else further($x,$lang) 
}

define function bild($float,$psscale,$pdfscale,$centerline,$x,$lang) {
    (if ($float) then"

\begin{figure}[!hbt]
" else ""
    ,"\ifpdf 
        ",$centerline,"{\includegraphics[scale=",$pdfscale,"]{images/",$x/@name,".pdf}}
 \else
        ",$centerline,"{\epsfxsize=",$psscale,"\textwidth\epsfbox{images/",$x/@name,".epsf}}
 \fi"
  ,if ($x/@caption) then ("\caption{",string($x/@caption),"\label{",string($x/@name),"}}") else ""
  ,if ($float) then "\end{figure}" else "","

"
  ) 
}

define function bilder($float,$centerline,$x,$lang) {
 (if ($float) then"\begin{figure}[!hbt]
" else ""
 ,"\begin{center}"
 ,(for $bild in $x/sbild
  let $psscale := if ($bild/@psscale) then $bild/@psscale else 1.0
  let $pdfscale := if ($bild/@pdfscale) then $bild/@pdfscale else 1.0
  return
   ("\ifpdf 
        ",$centerline,"{\includegraphics[scale=",$pdfscale,"]{images/",$bild/@name,".pdf}}
 \else
        ",$centerline,"{\epsfxsize=",$psscale,"\textwidth\epsfbox{images/",$bild/@name,".epsf}}
 \fi"))
 ,"\end{center}"
,if ($x/@caption) then ("\caption{",string($x/@caption),"\label{",string($x/@name),"}}") else ""

,if ($float) then "\end{figure}" else "","

"

  ) 
}

define function figure($float,$x,$lang) {
    (if ($float) then "

\begin{figure}[!hbt]
" else ""
    ,"\begin{center}
",further($x,$lang),"
\end{center}"
  ,if ($x/@caption) then ("\caption{",string($x/@caption),"\label{",string($x/@name),"}}") else ""
  ,if ($float) then "\end{figure}" else "","

"
  ) 
} 




transform(deleteTags(input())/*,"")
