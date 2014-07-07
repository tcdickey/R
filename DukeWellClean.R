a<-read.csv("/Users/tcd8/Documents/Med School/3rd year/Duke Requirements/Research/Data/DukeWell/Intervention/patientdiagnoses.csv")


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


