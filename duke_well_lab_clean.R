#lab result cleaner
labclean<-function(DataTable, output="cleanlab"){
        DataTable<-DataTable[DataTable$numeric.result!="",] #removes blank results
        DataTable[,report.date:=(mdy_hms(as.character(DataTable$report.date)))] #convert to date
        
        jan2011 <- as.POSIXlt.date("2011-01-01", "%Y-%m-%d",tz="UTC")
        dec2011 <- as.POSIXlt.date("2011-12-31", "%Y-%m-%d",tz="UTC")
        jan2013 <- as.POSIXlt.date("2013-01-01", "%Y-%m-%d",tz="UTC")
        dec2013 <- as.POSIXlt.date("2013-12-31", "%Y-%m-%d",tz="UTC")
        DataTable<-DataTable[(report.date<dec2013)&(report.date>jan2013)] #remove rows that have dates outside the area of interest
        DataTable[,a1c:=as.numeric(numeric.result)]
        cdt<-DataTable[,.SD[,meana1c<-mean(a1c)],by=patient.identifier]
        setnames(cdt,"V1","a1cmeans")
        assign(output,cdt,envir=.GlobalEnv)
}