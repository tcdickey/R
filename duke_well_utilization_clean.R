utilizationcleaner<-function(input.dt=adt,output.dt="cleanutil"){
#convert to date
        input.dt[,ed.arrival.date:=mdy_hms(as.character(ed.arrival.date))]
        input.dt[,admit.date:=mdy_hms(as.character(admit.date))]
# for post-control        input.dt[,arrival.date:=mdy_hms(as.character(arrival.date))]

#dates that are between a range
#note - cannot perform OR binary search operations, only AND binary search 
#operations.  must use vector search for R (or create weird index thing)
        jan2011 <- as.POSIXlt.date("2011-01-01", "%Y-%m-%d",tz="UTC")
        dec2011 <- as.POSIXlt.date("2011-12-31", "%Y-%m-%d",tz="UTC")
        jan2013 <- as.POSIXlt.date("2013-01-01", "%Y-%m-%d",tz="UTC")
        dec2013 <- as.POSIXlt.date("2013-12-31", "%Y-%m-%d",tz="UTC")
# for post-control input.dt[(arrival.date>jan2013&arrival.date<dec2013)|(ed.arrival.date>jan2013&ed.arrival.date<dec2013)|(admit.date>jan2013&admit.date<dec2013)]
        input.dt[((ed.arrival.date<jan2013)|(ed.arrival.date>dec2013)|(is.na(ed.arrival.date))),ed.arrival.date:=0]
        input.dt[((admit.date<jan2013)|(admit.date>dec2013)|(is.na(admit.date))),admit.date:=0]

#tabulates the total number of hospitalizations and ed visits
        cdt<-input.dt[,list((sum(unclass(ed.arrival.date)>1)),(sum(unclass(admit.date)>1))),
                by=patient.identifier]
        setnames(cdt,c("V1","V2"),c("edvisits","hospitalizations"))
        assign(output.dt,cdt,envir=.GlobalEnv)
#check that numbers are correct with this. this patient has 10 ed visits
#cdt["532390",ed.arrival.date,by=ed.arrival.date]
        
}

#extra cleaner for encounter dates this ensures that a person had an encounter both during 2011 and during 2013
input.dt[,arrival.date:=mdy_hms(as.character(arrival.date))]
adt<-interenc[((arrival.date>jan2011)&(arrival.date<dec2011))
              |((ed.arrival.date>jan2011)&(ed.arrival.date<dec2011))
              |((admit.date>jan2011)&(admit.date<dec2011))]
cont11and13<-merge(cont2011,cont2013,
                   by=c("patient.identifier","encounter.identifier",
                        "arrival.date"),all=TRUE)


