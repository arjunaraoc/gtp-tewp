#function to compute percentage changes of a data frame, all are assumed to be coercible to numerical
#returns another data frame with one row less and percentage numbers.
pctcompute<-function(x) {
    imax<-dim(x)[1]
    y<-data.matrix(x)
    for (i in 1:imax-1)
             y[i,]=(y[i,]-y[i+1,])*100/(y[i+1,])
    y
}