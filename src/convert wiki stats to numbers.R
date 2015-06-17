##Function to  convert wiki stats to numbers
##input is in .csv format with headers cleaned up and emply strings quoted as ""
#for headers check sample input file tewiki-stats-201503.csv
## function to convert wikistats from code to real numbers
## input is a list of char which contains k,kB,MB
## output is a list of int with equivalent numeric values
##depends on helper function (see below) to format values in char format ending in k|KB|MB to numerics
#main function
prepwstats<-function(filename="data/tewiki-stats-201503.csv") {
library(lubridate)
testats<-read.csv(filename,header=TRUE,sep=",",as.is=TRUE,na.strings=c(""))
#get rid of  rows containing blank elements
tc<-testats
sn<-names(tc)
fs<-tc[14,]
#units<-character()
snew<-character(20)
for (i in 1:length(fs)) {
    #find suffix and add to sn with space replaced by underscore
    m<-regexpr("[k|M|%]",as.character(fs[1,i]))
    if (m>0) snew[i]<-paste0(sn[i],"_",substr(as.character(fs[1,i]),m,nchar(as.character(fs[1,i])))) 
    else snew[i]<-sn[i]

}
snew<-gsub("%","pct",snew)
tco<-tc
names(tco)<-snew
#convert date
names(tco)[1]<-"month"
tco$month<-as.Date(myd(paste0(tc$Date," 15")))
#convert the factors to vectors for all columns
tco$wikipedians_tot<-convtonumeric(tc$wikipedians_tot)
tco$wikipedians_new<-convtonumeric(tc$wikipedians_new )
tco$wikipedians_edits5<-convtonumeric(tc$wikipedians_edits5)
tco$wikipedians_edits100<-convtonumeric(tc$wikipedians_edits100)
tco$art_count_official_k<-convtonumeric(tc$art_count_official)
tco$art_count_200ch_k<-convtonumeric(tc$art_count_200ch)                                    
tco$art_newperday<-convtonumeric(tc$art_newperday)
tco$art_mean_edits<-convtonumeric(tc$art_mean_edits)
tco$art_mean_bytes<-convtonumeric(tc$art_mean_bytes)
tco$art_size_halfkb_pct<-as.integer(substr(tc$art_size_halfkb,1,nchar(tc$art_size_halfkb)-1))
tco$art_size_2kb_pct<-as.integer(substr(tc$art_size_2kb,1,nchar(tc$art_size_2kb)-1))
tco$db_edits_k<-convtonumeric(tc$db_edits)
tco$db_size_MB<-convtonumeric(tc$db_size)
tco$db_size_words_M<-convtonumeric(tc$db_size_words)
tc$db_size_words<-convtonumeric(tc$db_size_words)
tco$links_int_k=convtonumeric(tc$links_int)
tco$links_interwiki_k=convtonumeric(tc$links_interwiki)
tco$links_images_k=convtonumeric(tc$links_images)
tco$links_external_k=convtonumeric(tc$links_external)
tco$links_redirects_k=convtonumeric(tc$links_redirects)

tco
}
##helper function
convtonumeric<-function(x) {
    ivec<-numeric(length=length(x))
    x<-as.character(x)
    mf<-grepl("M",x,fixed=TRUE)
    if (sum(mf)>0 ) {
        m1<-grepl("M",x,fixed=TRUE)
        mi<-regexpr("M",x,fixed=TRUE)
        ivec[which(m1)]<-as.numeric(substr(x[which(m1)],1,mi[which(m1)]-2))
        m2<-grepl("k",x,fixed=TRUE) 
        mi<-regexpr("k",x,fixed=TRUE) 
        ivec[which(m2)]<-as.numeric(substr(x[which(m2)],1,mi[which(m2)]-2))/1000
        m<-!(m1|m2)
        ivec[which(m)]<-as.numeric(x[which(m)])/1000000
    } else {
        kf<-grepl("k",x,fixed=TRUE)
        if (sum(kf)>0 ) {
            m2<-grepl("k",x,fixed=TRUE) 
            mi<-regexpr("k",x,fixed=TRUE) 
            ivec[which(m2)]<-as.numeric(substr(x[which(m2)],1,mi[which(m2)]-2))
            m<-!(m2)
            ivec[which(m)]<-as.numeric(x[which(m)])/1000
        }else {
            m<-grepl("\\.",x,fixed=TRUE)
            if (sum(m)>0) ## real exits
                ivec<-as.numeric(x)
            else
                ivec<-as.integer(x)
        }
        
    }
    ivec
}