openMetaAnalysis <-
function(content,lefthand,righthand, type, topic, theme) {
temp <- content
# http://stat.ethz.ch/R-manual/R-devel/library/base/html/regex.html
#temp <- gsub('\n', '', fixed = TRUE, temp)
#temp <- gsub("\\s+$", "", temp, perl = TRUE) #Removing trailing whitespace
#temp <- gsub(",+$", "", temp, perl = TRUE) #Remove trailing comma if accidentally added by user online
temp <- gsub("\t", ' ', fixed = TRUE, temp)
temp <- gsub(',', '","', fixed = TRUE, temp)
temp <- paste('"',temp,'"',sep = '')
temp <- paste('Mymatrix <- matrix(c(',temp,'), ncol=6, byrow=TRUE,dimnames = list(NULL, c("Study","exp_events", "exp_total","control_events","control_total","cofactor")))',sep = '')
x<-eval(parse(file = "", n = NULL, text = temp))
myframe <- data.frame (x)
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
if (type=="ignore")
	{
	meta_OR <- meta.DSL(myframe$exp_total, myframe$control_total, myframe$exp_events, myframe$control_events, data=myframe, names=myframe$Study)
	plot(meta_OR,col=meta.colors("Black"),xlim=c(-2,2),xlog=TRUE, c(5, 5, 7))

	tabletext<-cbind(c(NA,meta_OR$names,NA,"Summary"),
	c("Intervention",paste(myframe$exp_events,"/",myframe$exp_total),NA,NA),
	c("Control", paste(myframe$control_events,"/",myframe$control_total), NA,NA),
	c("OR",format(exp(meta_OR$logs),digits=2),NA,format(exp(meta_OR$logDSL),digits=2))
	)
	
	m<- c(NA,meta_OR$logs,NA,meta_OR$logDSL)
	l<- m-c(NA,meta_OR$selogs,NA,meta_OR$selogDSL)*2
	u<- m+c(NA,meta_OR$selogs,NA,meta_OR$selogDSL)*2

	#forestplot(tabletext,m,l,u,zero=0,is.summary=c(TRUE,rep(FALSE,length(meta_OR$logs)),TRUE),xlog=TRUE,clip=c(log(0.5),log(2)),xticks=c(0.5,1,2),
	#col=meta.colors(box="royalblue",line="darkblue", summary="royalblue"),xlab=c("       Favors intervention       Favors control"))

	#if(theme=="KU"){display_logo(x=1.2,y=0.05)}
	}
else
	{
	}
}
