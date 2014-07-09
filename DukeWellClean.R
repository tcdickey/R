a<-read.csv("/Users/tcd8/Documents/Med School/3rd year/Duke Requirements/Research/Data/DukeWell/Intervention/hospitalencounter.csv")


whoisdead <- function(a){
#Are you dead? data.frame = a
        for(i in nrow(a)){
                if(a$Patient.Death.Indicator[i] != "ALIVE"){
                        print(a$Patient.Identifier)
                }
        }
}

# Add on a new data frame .GlobalEnv
bringtogether<- function(a,b){
#        x<-join(a,b,by=c("Patient.Identifier"),type="full")
#        x<-merge(a,b,by=c("Patient.Identifier","Encounter.Identifier"),all=TRUE)
        x<-merge(a,b,by=c("Patient.Identifier","Encounter.Identifier"),all=TRUE)
        assign("combined",x, envir= .GlobalEnv)
#        print('patients in a')
#        print(length(unique(a$Patient.Identifier)))
#        print('encounters in a')
#        print(length(unique(a$Encounter.Identifier)))
#        print('patients in b')
#        print(length(unique(b$Patient.Identifier)))
#        print('encounters in b')
#        print(length(unique(b$Encounter.Identifier)))
        print('number of patients you should have')
        print(length(unique(b$Patient.Identifier))+length(unique(a$Patient.Identifier))-length(unique(match(a$Patient.Identifier,b$Patient.Identifier))))
        print('number of encounters you should have')
        print(length(unique(b$Encounter.Identifier))+length(unique(a$Encounter.Identifier))-length(unique(match(a$Encounter.Identifier,b$Encounter.Identifier))))
        print('patients in combined')
        print(length(unique(combined$Patient.Identifier)))
        print('encounters in combined')
        print(length(unique(combined$Encounter.Identifier)))
}

#adt is the original data table
bdt[,patient.identifier:=as.character(patient.identifier)]
setkey(bdt,patient.identifier)
#lowercase names
setnames(adt,names(adt),tolower(names(adt)))
#convert to date
adt[,ed.arrival.date:=mdy_hms(as.character(ed.arrival.date))]
#dates that are between a range
jan2011 <- as.POSIXlt.date("2011-01-01", "%Y-%m-%d",tz="UTC")
dec2011 <- as.POSIXlt.date("2012-12-31", "%Y-%m-%d",tz="UTC")
jan2013 <- as.POSIXlt.date("2013-01-01", "%Y-%m-%d",tz="UTC")
dec2013 <- as.POSIXlt.date("2013-12-31", "%Y-%m-%d",tz="UTC")
cdt[ed.arrival.date<jan2011,ed.arrival.date:=NA]
#try adt[bdt]
bdt<-adt[(unclass(ED.Arrival.Date)>1),mdy_hms(ED.Arrival.Date)]
#tabulates the total number of factor events in a list populated by a single dataframe
cdt<-adt[,(sum(unclass(ED.Arrival.Date)>1)),by=Patient.Identifier]
