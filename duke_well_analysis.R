#final formatting
#remove duplicates from the groups
adt<-control[!unique(intervention$patient.identifier),]
#convert everything to factors
adt$patient.gender<-as.factor(adt$patient.gender)
adt$patient.race<-as.factor(adt$patient.race)
adt$duke.well<-as.factor(adt$duke.well)
#levels(adt$duke.well)<-c("control","intervention")
adt$liver.indicator<-as.factor(adt$liver.indicator)
#levels(adt$liver.indicator)<-c("absent","present")
adt$diabetes.indicator<-as.factor(adt$liver.indicator)
adt$chf.indicator<-as.factor(adt$chf.indicator)
#and so on for the other two indicators
#finally...
predw<-copy(adt)

#fit propensity score model using log-odds or "logit"
dw.lm<-glm(duke.well~hospitalizations+edvisits+liver.indicator
           +chf.indicator+patient.race+age+patient.gender
           ,na.action="na.fail",data=adt,family=binomial)
bdt$predict<-predict(dw.lm,bdt,type="response")
#cut off outliers (1 outlier in DW group with a prediction of -2.79)
adt[edvisits<50]
adt[duke.well=="intervention",max(predict)]
adt<-adt[predict>(-5.3)] #min of dw
adt<-adt[predict<(-3.1)] #max of dw
adt$quintiles<-cut_interval(adt$predict,n=5)

#utilization difference creator
adt$ed.diff<-(adt$post.edvisits-adt$edvisits)
adt$hospital.diff<-(adt$post.hospitalizations-adt$hospitalizations)

#number of folks in each strata
table(adt[quintiles=="q4",duke.well])
#table 1 jazzy stuff
xtabs(~patient.gender+duke.well,data=adt[quintiles=="q5"])
prop.table(xtabs(~patient.gender+duke.well,data=adt[quintiles=="q5"]),2)
chisq.test(xtabs(~patient.gender+duke.well,data=adt[quintiles=="q5"]))
#race
xtabs(~patient.race+duke.well,data=adt[quintiles=="q5"])
prop.table(xtabs(~patient.race+duke.well,data=adt[quintiles=="q5"]),2)
#CHF and liver
xtabs(~liver.indicator+duke.well,data=adt[quintiles=="q5"])
prop.table(xtabs(~liver.indicator+duke.well,data=adt[quintiles=="q5"]),2)
#age
t.test(age~duke.well,data=adt[quintiles=="q5"])
#incidence rates
adt[duke.well=="intervention",sum(hospitalizations)/nrow(.SD)*100,by="quintiles"]
#can use the following code to check the chisq prediction
a<-chisq.test(xtabs(~patient.gender+duke.well,data=adt[quintiles=="q1"]))
a$expected

#RESULTS
#chisq test
        a<-as.array(c(adt[(duke.well=="intervention"&quintiles=="q5"),sum(hospitalizations)],
                adt[(duke.well=="intervention"&quintiles=="q5"),sum(post.hospitalizations)],
                adt[(duke.well=="control"&quintiles=="q5"),sum(hospitalizations)],
                adt[(duke.well=="control"&quintiles=="q5"),sum(post.hospitalizations)]))
        dim(a)<-c(2,2)
        rownames(a)<-list("pre","post")
        colnames(a)<-list("dw","control")
        a
        b<-chisq.test(a)
        b
        b$expected
#wilcoxon rank
wilcox.test(post.edvisits~duke.well,data=(adt[quintiles=="q4",]),conf.int=TRUE)
#ttest on intital data
t.test(post.hospitalizations~duke.well,data=q1)
#number of people in each group
nrow(q5[duke.well=="intervention"])
nrow(q5[duke.well=="control"])
#IRR
air[5,]<-escalc(x1i=(q5[duke.well=="intervention",sum(hospitalizations)]),
            x2i=(q5[duke.well=="control",sum(hospitalizations)]),
            t1i=nrow(q5[duke.well=="intervention"]),
            t2i=nrow(q5[duke.well=="control"]),measure="IRR")
#fixed effects model
exair<-exp(summary(air))
fema<-rma(yi=yi,vi=vi,data=air,method="FE",weights=c(0.11,0.44,0.35,0.08,0.03))
#test of differences
t.test(ed.diff~duke.well,q4)
#pooled differences multiplied by the proportion of folks in each of the 3 quintiles
((q1[duke.well=="intervention",mean(ed.diff)]-q1[duke.well=="control",mean(ed.diff)])*(29/263)+
(q2[duke.well=="intervention",mean(ed.diff)]-q2[duke.well=="control",mean(ed.diff)])*(115/263)+
(q3[duke.well=="intervention",mean(ed.diff)]-q3[duke.well=="control",mean(ed.diff)])*(91/263)+
(q4[duke.well=="intervention",mean(ed.diff)]-q4[duke.well=="control",mean(ed.diff)])*(21/263)+  
(q5[duke.well=="intervention",mean(ed.diff)]-q5[duke.well=="control",mean(ed.diff)])*(7/263)) 



#graphs of propensity scores
ggplot(adt,aes(predict,colour=duke.well))+geom_density()
ggplot(adt,aes(a1cmeans,colour=duke.well),)+geom_density()+xlim(2,14)
#this graph shows there is enough overlap at the tails
ggplot(adt,aes(predict,colour=duke.well))+geom_histogram()+xlim(.03,.058)

#proportion of women in each group
bdt<-adt[,mean(patient.gender),by=c("quintiles","duke.well")]
bdt<-bdt[,V1:=((V1-2)*-1)]
setnames(bdt,"V1","proportion.of.women")
ggplot(bdt,aes(proportion.of.women,colour=duke.well))+geom_histogram()+xlim(.03,.058)
