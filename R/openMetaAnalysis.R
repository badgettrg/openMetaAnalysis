openMetaAnalysis <- function(content,lefthand,righthand, type, topic, theme, package) {
temp <- content
# http://stat.ethz.ch/R-manual/R-devel/library/base/html/regex.html
temp <- gsub('\n', '', fixed = TRUE, temp, perl = TRUE)
#temp <- gsub("\\s+$", "", temp, perl = TRUE) #Removing trailing whitespace
#temp <- gsub(",+$", "", temp, perl = TRUE) #Remove trailing comma if accidentally added by user online
temp <- gsub("\t", ' ', fixed = TRUE, temp)
temp <- gsub(',', '","', fixed = TRUE, temp)
temp <- paste('"',temp,'"',sep = '')
temp <- paste('Mymatrix <- matrix(c(',temp,'), ncol=6, byrow=TRUE,dimnames = list(NULL, c("Study","exp_events", "exp_total","control_events","control_total","cofactor")))')
x<-eval(parse(file = "", n = NULL, text = temp))
myframe <- data.frame (x)
myframe$Study<-gsub("\'", '', fixed = TRUE, myframe$Study)
myframe$Study<-as.character(str_trim(myframe$Study))
myframe$exp_events<-as.numeric(as.character(str_trim(myframe$exp_events)))
myframe$exp_total<-as.numeric(as.character(str_trim(myframe$exp_total)))
myframe$control_events<-as.numeric(as.character(str_trim(myframe$control_events)))
myframe$control_total<-as.numeric(as.character(str_trim(myframe$control_total)))
myframe$cofactor<-as.numeric(as.character(str_trim(myframe$cofactor)))
attach(myframe)
KUBlue = "#0022B4"
SkyBlue = "#6DC6E7"
#par(col.axis="black" ,col.lab=KUBlue ,col.main=KUBlue ,col.sub=KUBlue, col=KUBlue,new = TRUE) #bg=SkyBlue)
if (package=="rmeta")
	{
	meta_OR <- meta.DSL(exp_total, control_total, exp_events, control_events, data=myframe, names=Study)
	tabletext<-cbind(c(NA,meta_OR$names,NA,"Summary","Heterogeneity p-value"),
	c("Intervention",paste(myframe$exp_events,"/",myframe$exp_total),NA,NA,NA),
	c("Control", paste(myframe$control_events,"/",myframe$control_total), NA,NA,NA),
	c("OR",sprintf("%.2f",exp(meta_OR$logs)),NA,sprintf("%.2f",exp(meta_OR$logDSL)),sprintf("%.2f",meta_OR$het[3]))
	)
	m<- c(NA,meta_OR$logs,NA,meta_OR$logDSL)
	l<- m-c(NA,meta_OR$selogs,NA,meta_OR$selogDSL)*2
	u<- m+c(NA,meta_OR$selogs,NA,meta_OR$selogDSL)*2
	forestplot(tabletext,m,l,u,zero=0,is.summary=c(TRUE,rep(FALSE,length(meta_OR$logs)),TRUE,TRUE),xlog=TRUE,clip=c(log(0.5),log(2)),xticks=c(0.5,1,2),
	col=meta.colors(box="royalblue",line="darkblue", summary="royalblue"),xlab=c("       Favors intervention       Favors control"))
	}
else
	{
	#dev.new(width=10, height=7)
	if (type=="ignore")
		{
		meta1 <- metabin(exp_events, exp_total, control_events,control_total, data=myframe, sm="OR", method="I", studlab=paste(Study))
		}
	if (type=="subgroup")
		{
		meta1 <- metabin(exp_events, exp_total, control_events,control_total, data=myframe, sm="OR", method="I", studlab=paste(Study),byvar=cofactor)
		}
	#forest(meta1, leftcols="studlab",rightcols=FALSE, xlim=c(0.1, 10),ff.hetstat="plain",col.diamond="blue", col.diamond.lines="blue",comb.fixed=FALSE,print.tau2=FALSE)
	forest(meta1, xlim=c(0.1, 10),ff.hetstat="plain",col.diamond="blue", col.diamond.lines="blue",comb.fixed=FALSE,print.tau2=FALSE)
	}
#if(theme=="KU"){display_logo(x=1.2,y=0.05)}
}
