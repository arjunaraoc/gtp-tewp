##getdata gets pagerequests for a list of article names, in a 
##given language wikiproject for the specified month from   http://stats.grok.se/
##lang "te"
##prj project code for wikipedia "", for wikisource "s" etc
##prj is wikiproject code as used for http://stats.grok.se/json/
##ptfile page_title with underscore in place of spaces in titles in one per line with no quotes as obtained in .tsv files
##ym is yyyymm for which data is required
##pv is data frame consisting of page title, pagerequests if empty 
##start getting for all anames,else look for the last article for which 
## data is present and then get data for remaining articles
##2015-05-13 add error handling
updatepvdata<-function(prj,ptfile,ym,pvfile) {
    library(RCurl)
    library(jsonlite)
    urlprefix<-"http://stats.grok.se/json/"
    urlc<-paste0(urlprefix,prj,"/",ym,"/")
    ##check the data already obtained
    ##read titles data
    if (file.exists(ptfile)) {
        pt<-read.table(ptfile,header=TRUE,as.is=TRUE)
        nrows<-dim(pt)[1]
        if (nrows==0) {
            cat("input file does not contain page_titles")
            stop()
        }
    }
    ##initialise data frame 
    pv<-data.frame(page_requests=as.integer(),page_title=as.character(),stringsAsFactors=FALSE)
    uris<-vector(mode="character",length=0)
    ##make urls experiment try for only 3 rows
    #nrows=100
    nrows<-dim(pt)[1]
    for (i in 1:nrows) {
        uris[i]<-paste0(urlc,pt$page_title[i])
    }
    ## get data and transfrom into R object from json
    ##retval<-getURIs(uris)
    ##serial version
    txt<-list()
    ##txt<-lapply(uris, getURL)
    txt<-getURI(uris,async=FALSE)
    #txt<-getURI(uris) async is failing
    ##   
    ##comment out JSON code to look at received data
    for (i in 1:nrows) {
         ptv<-fromJSON(txt[[i]],flatten=TRUE)
         ptvcount<-sum(unlist(ptv[1]$daily_views))
         tdf<-data.frame(page_requests=ptvcount,page_title=pt$page_title[i],stringsAsFactors=FALSE)
         pv<-rbind(pv,tdf) 
    }
    ##lapply(txt[[1]], ptvfun )
    ## write pvfile at the end
    write.table(pv,file=pvfile,row.names=FALSE,quote=FALSE)
    cat("page views obtained successfully")
}
##function as per http://www.omegahat.org/RCurl/concurrent.html
getURIs =
    function(uris, ..., multiHandle = getCurlMultiHandle(), .perform = TRUE)
    {
        content = list()
        curls = list()
        
        for(i in uris) {
            curl = getCurlHandle()
            content[[i]] = basicTextGatherer()
            opts = curlOptions(URL = i, writefunction = content[[i]]$update, ...)    
            curlSetOpt(.opts = opts, curl = curl)
            multiHandle = push(multiHandle, curl)
        }
        
        if(.perform) {
            complete(multiHandle)
            lapply(content, function(x) x$value())
            ## lapply(content, function(x) {fromJSON(x$value(),flatten=TRUE)})
        } else {
            cat("Errors occured, returning raw content")
            return(list(multiHandle = multiHandle, content = content))
        }
    }
