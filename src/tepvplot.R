##data frame contains month, allnprM all page requests and google translation project column
#assigned the same page views as the all page views column
#displays a plot of page views, highlighting the page views for the project period and a 
#smoothended model with 95% confidence intervals
#returns graphics object
plot_gtp_pr<-function(filename="data/tewp total page views.txt") {
library(ggplot2)
library(scales)
tetpv<-read.csv(filename,header=TRUE,sep=" ",na.strings=c("NA"),colClasses=c("Date",rep("numeric",4)))
tpv<-tetpv[tetpv$month > (as.Date("2008-06-01")) & tetpv$month< (as.Date("2012-07-01")),]
g<-ggplot(tpv,aes(month,allnprM  )) +
    geom_line() +
    geom_smooth(se=T)+
    geom_vline(aes(xintercept=as.numeric(as.Date("2009-07-01"))),linetype=4, colour="red")+
    geom_vline(aes(xintercept=as.numeric(as.Date("2011-06-30"))),linetype=4, colour="red")+    
    scale_x_date(labels = date_format("%Y/%m"),date_breaks="6 month")+
    labs(y="Page requests (in Millions)",x="Year/month",title="Telugu Wikipedia page requests")
g
}
