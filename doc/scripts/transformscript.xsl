<xsl:stylesheet version="1.0"
                xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

<xsl:template match="/">
     <html><head><title><xsl:value-of select="titelseite/titel"/>
           </title></head>
	<xsl:apply-templates/>
     </html>   
</xsl:template>


<xsl:template match="skript">
     <body>
        <center>       <xsl:apply-templates select="titelseite"/>
        </center>
               <xsl:apply-templates select="kapitel|anhang"/>
     </body>

</xsl:template>


<xsl:template match="titelseite/titel">
<h2><xsl:apply-templates/></h2>
</xsl:template>

<xsl:template match="titelseite/semester|autor|institution">
 <h3><xsl:apply-templates/></h3>
</xsl:template>

<xsl:template match="kapitel">
     <h1><xsl:number level="multiple"
                 count="kapitel"
                 format="1.1 "/><xsl:text> </xsl:text><xsl:apply-templates select="./@titel"/></h1>
      <xsl:apply-templates/>
</xsl:template>


<xsl:template match="section">
     <h2><xsl:number level="multiple"
                 count="kapitel|section"
                 format="1.1 "/><xsl:text> </xsl:text><xsl:apply-templates select="./@titel"/></h2>
      <xsl:apply-templates/>
</xsl:template>

<xsl:template match="subsection">
     <h3><xsl:number level="multiple"
                 count="kapitel|section|subsection"
                 format="1.1 "/><xsl:text> </xsl:text>
       <xsl:apply-templates select="./@titel"/></h3>
      <xsl:apply-templates/>
</xsl:template>

<xsl:template match="subsubsection">
     <h4><xsl:apply-templates select="./@titel"/></h4>
      <xsl:apply-templates/>
</xsl:template>

<xsl:template match="paragraph">
     <h5><xsl:apply-templates select="./@titel"/></h5>
      <xsl:apply-templates/>
</xsl:template>


<xsl:template match="anhang">
     <h1>Anhang</h1>
      <xsl:apply-templates/>
</xsl:template>


<xsl:template match="anhang//kapitel">
     <h1><xsl:number level="multiple"
                 count="anhang/kapitel"
                 format="A.1 "/><xsl:text> </xsl:text><xsl:apply-templates select="./@titel"/></h1>
      <xsl:apply-templates/>
</xsl:template>


<xsl:template match="anhang//section">
     <h2><xsl:number level="multiple"
                 count="anhang/kapitel|section"
                 format="A.1 "/><xsl:text> </xsl:text><xsl:apply-templates select="./@titel"/></h2>
      <xsl:apply-templates/>
</xsl:template>

<xsl:template match="anhang//subsection">
     <h3><xsl:number level="multiple"
                 count="anhang|section|subsection"
                 format="A.1 "/><xsl:text> </xsl:text>
       <xsl:apply-templates select="./@titel"/></h3>
      <xsl:apply-templates/>
</xsl:template>


<xsl:template match="itemize">
     <ul><xsl:apply-templates select="./item"/></ul>
</xsl:template>


<xsl:template match="item">
     <p/><li><xsl:apply-templates/></li>
</xsl:template>

<xsl:template match="fbox">
<table border="1" cellpadding="2" cellspacing="2">
 <tbody>
      <tr>
        <td>
<xsl:apply-templates/></td></tr></tbody></table>
</xsl:template>

<xsl:template match="em">
     <em><xsl:apply-templates/></em>
</xsl:template>


<xsl:template match="b">
     <b><xsl:apply-templates/></b>
</xsl:template>

<xsl:template match="code">
     <pre><xsl:apply-templates/></pre>
</xsl:template>

<xsl:template match="scode">
     <pre><xsl:apply-templates/></pre>
</xsl:template>

<xsl:template match="table">
     <table><xsl:apply-templates/></table>
</xsl:template>

<xsl:template match="longtable">
     <table><xsl:apply-templates/></table>
</xsl:template>

<xsl:template match="zeile">
     <tr><xsl:apply-templates/></tr>
</xsl:template>

<xsl:template match="zelle">
     <td><xsl:apply-templates/></td>
</xsl:template>

<xsl:template match="quote">
     <blockquote><xsl:apply-templates/></blockquote>
</xsl:template>

<xsl:template match="eqnarray">
     <blockquote><center><xsl:apply-templates/></center></blockquote>
</xsl:template>

<xsl:template match="beispiel">
     <blockquote><b>Beispiel:</b><br/><xsl:apply-templates/></blockquote>
</xsl:template>

<xsl:template match="lpar">
     <xsl:text>{</xsl:text>
</xsl:template>

<xsl:template match="quot">
     <xsl:text>&quot;</xsl:text>
</xsl:template>

<xsl:template match="rpar">
     <xsl:text>}</xsl:text>
</xsl:template>

<xsl:template match="bild">
  <img>
    <xsl:attribute name = "src">images/<xsl:value-of select="@name"/><xsl:text>.gif</xsl:text></xsl:attribute>
  </img>
</xsl:template>

<xsl:template match="img">
  <img>
    <xsl:attribute name = "src">images/<xsl:value-of select="@name"/><xsl:text>.gif</xsl:text></xsl:attribute>
  </img>
</xsl:template>


<xsl:template match="tt">
     <tt><xsl:apply-templates/></tt>
</xsl:template>

<xsl:template match="ttt">
     <tt><xsl:apply-templates/></tt>
</xsl:template>

<xsl:template match="token">
     <tt><xsl:apply-templates/></tt>
</xsl:template>

<xsl:template match="center">
     <center><xsl:apply-templates/></center>
</xsl:template>

<xsl:template match="rightarrow">
     <xsl:text disable-output-escaping="yes">--&gt;</xsl:text>
</xsl:template>


<xsl:template match="alt">
<xsl:if test="not(position()=1)">
<br/><xsl:text>    |</xsl:text><xsl:apply-templates/>
</xsl:if>
<xsl:if test="(position()=1)">
<br/><xsl:text>    </xsl:text><xsl:apply-templates/>
</xsl:if>
</xsl:template>


<xsl:template match="br">
     <br/>
</xsl:template>


<xsl:template match="p">
     <p><xsl:apply-templates/></p>
</xsl:template>


<xsl:template match="//aufgabe">
     <p>
     <b>Aufgabe <xsl:number level="any"
                 count="//aufgabe"
                 format="1.1 "/></b> </p>
     
     <xsl:apply-templates/>
</xsl:template>

<xsl:template match="anhang//aufgabe">
     <p>
     <b>Aufgabe <xsl:number level="any"
                 count="anhang//aufgabe"
                 format="1 "/></b> </p>
     
     <xsl:apply-templates/>
</xsl:template>


<xsl:template match="teil">
     <br/>
     <b><xsl:number level="multiple"
                 count="teil"
                 format="a"/>) </b>
     
     <xsl:apply-templates/>
</xsl:template>

<xsl:template match="wortliste">
  <h1>Wortliste</h1>
     <xsl:apply-templates/>
</xsl:template>


<xsl:template match="wliste">
  <table border="true"><tr><td><b>deutsch</b></td><td><b>english</b></td></tr>
<xsl:apply-templates select="entry">
    <xsl:sort select="deutsch"/> 
    </xsl:apply-templates>
  </table>
     
</xsl:template>

<xsl:template match="entry">
     <tr><xsl:apply-templates/></tr>
</xsl:template>


<xsl:template match="deutsch">
     <td><xsl:apply-templates/></td>
</xsl:template>

<xsl:template match="english">
     <td><xsl:apply-templates/></td>
</xsl:template>

<xsl:template match="delete">
</xsl:template>

<xsl:template match="footnote">
</xsl:template>

<xsl:template match="exlink">
 <a><xsl:attribute name="href"><xsl:value-of select="@address"/></xsl:attribute><xsl:apply-templates/></a>
</xsl:template>

<xsl:template match="link">
 <a><xsl:attribute name="href"><xsl:value-of select="@address"/></xsl:attribute><xsl:apply-templates/></a>
</xsl:template>

<xsl:template match="white"><xsl:text> </xsl:text></xsl:template>

<xsl:template match="LaTeX"><xsl:text>LaTeX</xsl:text></xsl:template>

<xsl:template match="regel">
<p/>
<b><xsl:value-of select="nt"/></b>::=
<blockquote><xsl:apply-templates select="lhs"/></blockquote>
</xsl:template>




</xsl:stylesheet>
