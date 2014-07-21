#selects proper ICD9 codes and creates a disease indicator
diseaseindicator<-function(diagnosistable){
#converts the ICD9 codes to disease indicators
setkey(diagnosistable,icd.diagnosis.code)
#CHF indicator creator
        diagnosistable<-diagnosistable[diagnosistable$diagnosis.date!="",] #removes blank ICD-9 codes
        diagnosistable[,diagnosis.date:=(mdy_hms(as.character(diagnosistable$diagnosis.date)))] #convert to date
        jan2011 <- as.POSIXlt.date("2011-01-01", "%Y-%m-%d",tz="UTC")
        dec2011 <- as.POSIXlt.date("2011-12-31", "%Y-%m-%d",tz="UTC")
        jan2013 <- as.POSIXlt.date("2013-01-01", "%Y-%m-%d",tz="UTC")
        dec2013 <- as.POSIXlt.date("2013-12-31", "%Y-%m-%d",tz="UTC")
        diagnosistable<-diagnosistable[diagnosis.date<dec2011,] 
        #remove rows that have dates outside the area of interest
        diagnosistable[c("398.91","402.11","402.91","404.11","404.13","404.91","404.93","428",
        "428.0","428.1","428.20","428.21","428.22","428.23","428.30","428.31",
        "428.32","428.33","428.40","428.41","428.42","428.43","428.9"),
        chf.indicator:=1]
        diagnosistable[!c("398.91","402.11","402.91","404.11","404.13","404.91","404.93","428",
        "428.0","428.1","428.20","428.21","428.22","428.23","428.30","428.31",
        "428.32","428.33","428.40","428.41","428.42","428.43","428.9"),
        chf.indicator:=0]
        #Liver Disease indicator
        diagnosistable[c("070.32","070.33","070.54","456.0","456.1","456.20","456.21","571.0",
        "571.2","571.3","571.40","571.41","571.42","571.49","571.5","571.6",
        "571.8","571.9","572.2","572.8"),liver.indicator:=1]
        diagnosistable[!c("070.32","070.33","070.54","456.0","456.1","456.20","456.21","571.0",
        "571.2","571.3","571.40","571.41","571.42","571.49","571.5","571.6",
        "571.8","571.9","572.2","572.8"),liver.indicator:=0]
        #Diabetes indicator creator
        diagnosistable[c("250","250.0","250.00","250.01","250.02","250.03","250.1","250.10",
        "250.11","250.12","250.13","250.2","250.20","250.21","250.22","250.23",
        "250.30","250.31","250.32","250.33","250.4","250.40","250.41","250.42",
        "250.43","250.5","250.50","250.51","250.52","250.53","250.6","250.60",
        "250.61","250.62","250.63","250.7","250.70","250.71","250.72","250.73",
        "250.9","250.90","250.91","250.92","250.93"),diabetes.indicator:=1]
diagnosistable[!c("250","250.0","250.00","250.01","250.02","250.03","250.1","250.10",
       "250.11","250.12","250.13","250.2","250.20","250.21","250.22","250.23",
       "250.30","250.31","250.32","250.33","250.4","250.40","250.41","250.42",
       "250.43","250.5","250.50","250.51","250.52","250.53","250.6","250.60",
       "250.61","250.62","250.63","250.7","250.70","250.71","250.72","250.73",
       "250.9","250.90","250.91","250.92","250.93"),diabetes.indicator:=0][order(patient.identifier)]
        setkey(diagnosistable,patient.identifier)

#compresses the ICD-9 codes and patients so that if any of the ICD-9 indicators
#are 1 then that patient's code will have a 1
        dx.id<-diagnosistable[,list(chf.indicator=max(chf.indicator),
               diabetes.indicator=max(diabetes.indicator),
               liver.indicator=max(liver.indicator)),by=patient.identifier]
        assign(x="cleandx",dx.id,envir=.GlobalEnv)
}
#setkeyv(diagnosistable,c("patient.identifier","chf.indicator"))
#diagnosistable[J("1215842",1),]
#can use the above code to determine whether it correctly identified the people
#who had at least 1 in the CHF indicator

#this finally ensures that the person actually has a diabetes diagnosis as of 2011
adt<-intervention[intervention$diabetes.indicator==1,]



