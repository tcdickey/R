require(data.table)
require(lubridate)
bdt<-fread("/Users/tcd8/Documents/Med School/3rd year/Duke Requirements/Research/Data/DukeWell/Control/dec2007_patientdiagnoses.csv",sep2=",")
#initial formatting
icd9formatter<- function(adt){
        setnames(adt,names(adt),gsub(" ",".",names(adt)))
        setnames(adt,names(adt),tolower(names(adt)))
        adt[,patient.identifier:=as.character(patient.identifier)]
#        setkeyv(adt,c('patient.identifier','encounter.identifier'))
        setkeyv(adt,names(adt))
}

#merge data frames, using data tables are 5-7 times faster
system.time(cdt<-merge(adt,bdt,by=c("patient.identifier",
"encounter.identifier","icd.diagnosis.code","diagnosis.date"),all=TRUE))
#system.time(cdf<-merge(adf,bdf,by=c("patient.identifier",
#"encounter.identifier","icd.diagnosis.code","diagnosis.date"),all=TRUE))