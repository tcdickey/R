
demographicclean<-function(.input=adt,output="cleandemo"){
#convert things to factors and only includes alive people
        .input$patient.gender<-as.factor(.input$patient.gender)
        .input$patient.race<-as.factor(.input$patient.race)
        .input$age<-(2014-as.numeric(.input$patient.year.of.birth))
        bdt<-.input[patient.death.indicator=="ALIVE",c(1,2,3,7),with=FALSE]
        assign(output,bdt,envir=.GlobalEnv)
}
