hl() <-
structure(list(basefontsize = function () 
{
    outputFormat = opts_knit$get("rmarkdown.pandoc.to")
    if(outputFormat == 'latex') out <- "\\large"
    else  out <- "<font size=\"+1\">"
    out
}, vspace = function (n=2) 
{
    outputFormat = opts_knit$get("rmarkdown.pandoc.to")
    if(outputFormat == 'latex') {
        out <- paste0("\\vspace{", n*2, "mm}")
    }
    else {
         out <- paste(rep("<br>", n), collapse="")
    }
    out
}, fontcolor = function(x="Warning", color="red"){
  outputFormat = opts_knit$get("rmarkdown.pandoc.to")
  if(outputFormat == 'latex') 
        out <- paste0("\\textcolor{",color,"}{",x,"}")
  else 
      out <- paste0("<font color='", color,"'>", x, "</font>")
  out
}, fontsize = function (size=2) 
{
    outputFormat = opts_knit$get("rmarkdown.pandoc.to")
    if(outputFormat == 'latex') {
        if(size==1) out <- "\\large"
        if(size==2) out <- "\\Large"        
        if(size==3) out <- "\\huge"                
        if(size==4) out <- "\\Huge"                
    }
    else {
        if(size==1) out <- "<font size=\"+1\">"
        if(size==2) out <- "<font size=\"+2\">"
        if(size==3) out <- "<font size=\"+3\">"
        if(size==4) out <- "<font size=\"+4\">"        
    }
    out
}, reset = function () 
{
    outputFormat = opts_knit$get("rmarkdown.pandoc.to")
    if(outputFormat == 'latex') out <- "\\normalsize"
    else  out <- "<font size=\"+1\">"
    out
}, hr = function () 
{
    outputFormat = opts_knit$get("rmarkdown.pandoc.to")
    if(outputFormat == 'latex') 
        out <- "\\noindent\\makebox[\\linewidth]{\\rule{\\paperwidth}{0.4pt}}"
    else  out <- "<hr>"
    out
}, `NA` = 0, `NA` = 0), .Names = c("basefontsize", "vspace", 
"fontcolor", "fontsize", "reset", "hr", NA, NA))
