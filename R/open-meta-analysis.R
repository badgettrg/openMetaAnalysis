open-meta-analysis <-
function(content,type,theme) {
temp <- gsub('\n', '', fixed = TRUE, content, perl = TRUE)
temp <- gsub("\\s+$", "", temp, perl = TRUE) #Removing trailing whitespace
temp <- gsub(",+$", "", temp, perl = TRUE) #Remove trailing comma if accidentally added by user online
temp <- paste('Mymatrix <- matrix(c(',temp,'), ncol=2, byrow=TRUE,dimnames = list(NULL, c("Reason","count")))')
x<-eval(parse(file = "", n = NULL, text = temp))
KUBlue = "#0022B4"
SkyBlue = "#6DC6E7"
par(col.axis="black" ,col.lab=KUBlue ,col.main=KUBlue ,col.sub=KUBlue, col=KUBlue,new = TRUE) #bg=SkyBlue)
if (type=="p" || type=="P")
	{
	count <- as.numeric(x [,2])
	names(count) <- x [,1]
	pareto.chart(count , ylab = "Frequency", col=SkyBlue, cumperc = seq(0, 100, by = 10), xlab="", border=KUBlue,main="Pareto chart: frequencies of causes of non-conformity")
	mtext("Reasons", side=1, line=3, col=KUBlue , cex=1.5)
	if(theme=="KU"){display_logo(x=1.2,y=0.05)}
	}
else
	{
	#plot(c(0, dev.size("px")[1]), c(0, dev.size("px")[2]),axes=F,type="b",xlab="", ylab = "", new = TRUE) # needed if rasterimage later adds anything
	Myframe <- as.data.frame(x)
	Myframe$count<-as.numeric(as.character(Myframe$count))
	ggplot(Myframe, aes(x = reorder(Reason, -count), y = count)) + 
		geom_bar(fill = SkyBlue,stat="identity") + xlab("Reason")+ ylab("Count") +
		theme(plot.background = element_rect(fill = "#FFFFFF")) + 
		labs(title = "Frequencies of causes of non-conformity")+ theme(plot.title = element_text(size = rel(2))) + theme(plot.title = element_text(colour = KUBlue))  +
		 theme(axis.text = element_text(colour = KUBlue))+
		 theme(axis.title = element_text(colour = KUBlue))
	}
}
