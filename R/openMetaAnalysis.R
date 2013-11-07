openMetaAnalysis <-
function(content,lefthand,righthand, cofactor, topic, theme) {
temp <- content
# http://stat.ethz.ch/R-manual/R-devel/library/base/html/regex.html
#temp <- gsub('\n', '', fixed = TRUE, temp, perl = TRUE)
#temp <- gsub("\\s+$", "", temp, perl = TRUE) #Removing trailing whitespace
#temp <- gsub(",+$", "", temp, perl = TRUE) #Remove trailing comma if accidentally added by user online
temp <- gsub("\t", ' ', fixed = TRUE, temp)
temp <- gsub(',', '","', fixed = TRUE, temp)
temp <- paste('"',temp,'"',sep = '')
temp <- paste('Mymatrix <- matrix(c(',temp,'), ncol=6, byrow=TRUE,dimnames = list(NULL, c("Study","exp_events", "exp_total","control_events","control_total","cofactor")))')
x<-eval(parse(file = "", n = NULL, text = temp))
myframe <- data.frame (x)
KUBlue = "#0022B4"
SkyBlue = "#6DC6E7"
#par(col.axis="black" ,col.lab=KUBlue ,col.main=KUBlue ,col.sub=KUBlue, col=KUBlue,new = TRUE) #bg=SkyBlue)
if (cofactor=="ignore")
	{
	meta_OR <- meta.DSL(myframe[["exp_total"]], myframe[["control_total"]], myframe[["exp_events"]], myframe[["control_events"]], data=myframe)

	plot(meta_OR)
   
	if(theme=="KU"){display_logo(x=1.2,y=0.05)}
	}
else
	{
	}
}
