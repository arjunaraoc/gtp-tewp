# Example preprocessing script.
# convert dates to R date format
# change date to character and append 15 to represent month data
# ws$Date<-sprintf("%s %2s",as.character(ws$Date[1]),"15")
# library(lubridate)
# ws$Date<-myd(ws$Date)