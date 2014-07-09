adt<-fread("/Users/tcd8/Documents/Med School/3rd year/Duke Requirements/Research/Data/DukeWell/Intervention/patientdiagnoses.csv",sep2=",")
#initial formatting
setnames(adt,names(adt),gsub(" ",".",names(adt)))
setnames(adt,names(adt),tolower(names(adt)))
adt[,patient.identifier:=as.character(patient.identifier)]
setkey(adt,patient.identifier)


#convert to date
cdt[,diagnosis.date:=(mdy_hms(as.character(cdt$diagnosis.date)))]
#remove rows that have dates outside the area of interest
jan2011 <- as.POSIXlt.date("2011-01-01", "%Y-%m-%d",tz="UTC")
dec2011 <- as.POSIXlt.date("2011-12-31", "%Y-%m-%d",tz="UTC")
jan2013 <- as.POSIXlt.date("2013-01-01", "%Y-%m-%d",tz="UTC")
dec2013 <- as.POSIXlt.date("2013-12-31", "%Y-%m-%d",tz="UTC")
cdt<-cdt[diagnosis.date<dec2011]

#converts the ICD9 codes to disease indicators
setkey(cdt,icd.diagnosis.code)
#Diabetes
#anything that starts with 2? (diabetes ICD9) 
#grep("^2",x=bdt$icd.diagnosis.code)

#CHF
cdt[c("571.8","496"),chf.indicator:=1]
cdt[!c("571.8","496"),chf.indicator:=0][order(patient.identifier)]
setkey(cdt,patient.identifier)


