require(data.table) 
require(lubridate)
bdt<-fread("/Users/tcd8/Documents/Med School/3rd year/Duke Requirements/Research/Data/DukeWell/predw.csv",sep2=",")
#initial formatting
fileformatter<- function(adt){
        setnames(adt,names(adt),gsub(" ",".",names(adt)))
        setnames(adt,names(adt),tolower(names(adt)))
        adt[,patient.identifier:=as.character(patient.identifier)]
#        setkeyv(adt,c('patient.identifier','encounter.identifier'))
        setkey(adt,patient.identifier)
}

#merge data tables (of same type) that are too long, using data tables are 5-7 times faster
setkeyv(adt,names(adt))
system.time(cdt<-merge(adt,bdt,by=c("patient.identifier",
"encounter.identifier","icd.diagnosis.code","diagnosis.date"),all=TRUE))
#system.time(cdf<-merge(adf,bdf,by=c("patient.identifier",
#"encounter.identifier","icd.diagnosis.code","diagnosis.date"),all=TRUE))

#merge the different data tables together
#set all keys to patient.identifier
bringtogether<-function(output="bdt"){
        bdt<-merge(cleandemo,cleandx)
        bdt<-merge(bdt,cleanutil)
        bdt<-merge(bdt,cleanlab,all.x=TRUE) #not enough lab values
        assign(output,bdt,envir=.GlobalEnv)
}
#save cohort
write.csv(dw.dt, "/Users/tcd8/Documents/Med School/3rd year/Duke Requirements/Research/Data/DukeWell/dw_analysis.csv")


