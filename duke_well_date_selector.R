#ICD-9 cleaner
dateselector<-function(DataTable, output="cleandate"){
        DataTable<-DataTable[DataTable$numeric.result!="",] #removes blank ICD-9 codes
        DataTable[,report.date:=(mdy_hms(as.character(DataTable$report.date)))] #convert to date
        
        jan2011 <- as.POSIXlt.date("2011-01-01", "%Y-%m-%d",tz="UTC")
        dec2011 <- as.POSIXlt.date("2011-12-31", "%Y-%m-%d",tz="UTC")
        jan2013 <- as.POSIXlt.date("2013-01-01", "%Y-%m-%d",tz="UTC")
        dec2013 <- as.POSIXlt.date("2013-12-31", "%Y-%m-%d",tz="UTC")
        DataTable<-DataTable[report.date<dec2011] #remove rows that have dates outside the area of interest
        assign(output,DataTable,envir=.GlobalEnv)
}

bdt[,a1c:=as.numeric(numeric.result)]
cdt<-bdt[,"a1cmeans"<-mean(a1c),by=patient.identifier]
setnames(cdt,"V1","a1cmeans")