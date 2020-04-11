Sel_Opt<-c("COVID-19(JAMA)","FLU(ICHE)","Neither(ICHE)")
Interval<-50
#How many patients to add each cycle through loop leading to total patients.
PatJump<-list()
PatJump[["COVID-19(JAMA)"]]<-1
PatJump[["FLU(ICHE)"]]<-1
PatJump[["Neither(ICHE)"]]<-19
#Total patients by Disease 
UpperLimitPat<-list()
UpperLimitPat[["COVID-19(JAMA)"]]<-Interval*PatJump[["COVID-19(JAMA)"]]
UpperLimitPat[["FLU(ICHE)"]]<-Interval*PatJump[["FLU(ICHE)"]]
UpperLimitPat[["Neither(ICHE)"]]<-Interval*PatJump[["Neither(ICHE)"]]

Symptoms<-list()

# Report of the WHO-China Joint Mission on Coronavirus Disease 2019 (COVID-19)
# (16-24 February 2020)
Symptoms[["COVID-19(WHO)"]]<-data.frame(matrix(0,nrow=2,ncol=0))
rownames(Symptoms[["COVID-19(WHO)"]])<-c("Freq","CtsForSamp")
Symptoms[["COVID-19(WHO)"]]["Freq","Fever"]<-.879
Symptoms[["COVID-19(WHO)"]]["Freq","Cough"]<-.677
# Symptoms[["COVID-19(WHO)"]]["Freq","Sore Throat"]<-.139
# Symptoms[["COVID-19(WHO)"]]["Freq","Headache"]<-.136
# Symptoms[["COVID-19(WHO)"]]["Freq","Myalgia"]<-.148
Symptoms[["COVID-19(WHO)"]]["Freq","Nausea or Vomiting"]<-.05
Symptoms[["COVID-19(WHO)"]]["Freq","Diarrhea"]<-.037

#Clinical Signs and Symptoms Predicting Influenza Infection
#Arnold S. Monto MD et al in Arch Intern Med (November 27, 2000)
Symptoms[["FLU(AIM)"]]<-data.frame(matrix(0,nrow=2,ncol=0))
rownames(Symptoms[["FLU(AIM)"]])<-c("Freq","CtsForSamp")
Symptoms[["FLU(AIM)"]]["Freq","Fever"]<-.68
Symptoms[["FLU(AIM)"]]["Freq","Cough"]<-.93
# Symptoms[["FLU(AIM)"]]["Freq","Sore Throat"]<-.84
# Symptoms[["FLU(AIM)"]]["Freq","Headache"]<-.91
# Symptoms[["FLU(AIM)"]]["Freq","Myalgia"]<-.94
Symptoms[["FLU(AIM)"]]["Freq","Nausea or Vomiting"]<-.01
Symptoms[["FLU(AIM)"]]["Freq","Diarrhea"]<-.01

#Clinical Characteristics of 138 Hospitalized Patients With 2019 Novel Coronavirus-Infected Pneumonia inWuhan, China
#DaweiWang, MD et al in JAMA (Pub:Febuary 7, 2020 Corrected: Febuary 20, 20202)
Symptoms[["COVID-19(JAMA)"]]<-data.frame(matrix(0,nrow=2,ncol=0))
rownames(Symptoms[["COVID-19(JAMA)"]])<-c("Freq","CtsForSamp")
Symptoms[["COVID-19(JAMA)"]]["Freq","Fever"]<-.986
Symptoms[["COVID-19(JAMA)"]]["Freq","Cough"]<-.594
# Symptoms[["COVID-19(JAMA)"]]["Freq","Sore Throat"]<-.174
# Symptoms[["COVID-19(JAMA)"]]["Freq","Headache"]<-.065
# Symptoms[["COVID-19(JAMA)"]]["Freq","Myalgia"]<-.348
Symptoms[["COVID-19(JAMA)"]]["Freq","Nausea or Vomiting"]<-.101 #Note we used Nausea frequency .101 assuming almost all Vomiting (.036) occur in this group 
Symptoms[["COVID-19(JAMA)"]]["Freq","Diarrhea"]<-.101

#Symptoms of Influenza Virus Infection in Hospitalized Patients
#C. van den Dool, MSc et al in INFECTION CONTROL AND HOSPITAL EPIDEMIOLOGY (April 2008)
Symptoms[["FLU(ICHE)"]]<-data.frame(matrix(0,nrow=2,ncol=0))
rownames(Symptoms[["FLU(ICHE)"]])<-c("Freq","CtsForSamp")
Symptoms[["FLU(ICHE)"]]["Freq","Fever"]<-.45
Symptoms[["FLU(ICHE)"]]["Freq","Cough"]<-.75
# Symptoms[["FLU(ICHE)"]]["Freq","Sore Throat"]<-.35
# Symptoms[["FLU(ICHE)"]]["Freq","Headache"]<-.55
# Symptoms[["FLU(ICHE)"]]["Freq","Myalgia"]<-.3
Symptoms[["FLU(ICHE)"]]["Freq","Nausea or Vomiting"]<-.01
Symptoms[["FLU(ICHE)"]]["Freq","Diarrhea"]<-.01

Symptoms[["Neither(ICHE)"]]<-data.frame(matrix(0,nrow=2,ncol=0))
rownames(Symptoms[["Neither(ICHE)"]])<-c("Freq","CtsForSamp")
Symptoms[["Neither(ICHE)"]]["Freq","Fever"]<-.183
Symptoms[["Neither(ICHE)"]]["Freq","Cough"]<-.298
# Symptoms[["Neither(ICHE)"]]["Freq","Sore Throat"]<-.09
# Symptoms[["Neither(ICHE)"]]["Freq","Headache"]<-.235
# Symptoms[["Neither(ICHE)"]]["Freq","Myalgia"]<-.168
Symptoms[["Neither(ICHE)"]]["Freq","Nausea or Vomiting"]<-.01
Symptoms[["Neither(ICHE)"]]["Freq","Diarrhea"]<-.01


#Initialize dataframes 
Parameters_COVID19_Recall<-data.frame(matrix(0,nrow = 0,ncol=Interval))
Parameters_FLU_Recall<-data.frame(matrix(0,nrow = 0,ncol=Interval))

Parameters_COVID19_Selectivity<-data.frame(matrix(0,nrow = 0,ncol=Interval))
Parameters_FLU_Selectivity<-data.frame(matrix(0,nrow = 0,ncol=Interval))

z<-1
while(z<=10){

SingleSymp_COVID19_JAMA<-list()
SingleSymp_FLU_ICHE<-list()
SingleSymp_Neither_ICHE_COVID19<-list()
SingleSymp_Neither_ICHE_FLU<-list()

l<-1
while(l<=length(Sel_Opt)){
  Sel<-Sel_Opt[l]
  
  #Filling character arrays for each symptom with elements representing having the symptom (1) and not (0).
  MarbPerJar<-1e3 #Total elements to pull for each symptom (Both have symptom, 1, and dont have,0.)
  Symptoms[[Sel]]["CtsForSamp",]<-round(Symptoms[[Sel]]["Freq",]*MarbPerJar) #The number of elements for each symbol representing having it (1).
  Jar<-list()
  s<-1
  while(s<=ncol(Symptoms[[Sel]])){
    Curr_Symp<-colnames(Symptoms[[Sel]])[s]
    Jar[[Curr_Symp]]<-rep(0,MarbPerJar)
    Jar[[Curr_Symp]][1:(Symptoms[[Sel]]["CtsForSamp",Curr_Symp])]<-rep(1,Symptoms[[Sel]]["CtsForSamp",Curr_Symp])
    s<-s+1
  }
  
  #Simulate patients with one symptom
  k<-PatJump[[Sel]]
  while(k<=UpperLimitPat[[Sel]]){
    SingleSymp_temp<-data.frame(matrix(0,nrow=0,ncol=ncol(Symptoms[[Sel]])))
    colnames(SingleSymp_temp)<-colnames(Symptoms[[Sel]])
    temp<-data.frame(matrix(0,nrow=1,ncol=ncol(Symptoms[[Sel]])))
    colnames(temp)<-colnames(Symptoms[[Sel]])
    j<-1
    while(j<=k){
      print(paste("Selection ",l," :Patient with 1 Sympt: ",j," for ",k,sep=""))
      i<-1
      while(i<=ncol(Symptoms[[Sel]])){
        Curr_Symp<-colnames(Symptoms[[Sel]])[i]
        
        Pull<-sample.int(length(Jar[[Curr_Symp]]),1) #Selecting whether patient has symptom j or not.
        Marbles<-Jar[[Curr_Symp]][Pull] #If index slects a 1 they have the symptom and 0 else. 
        temp[1,Curr_Symp]<-Marbles #Recording 1 (having symptom) or 0 (not having symptom) for patient i (row) symptom j (column) in data frame.
        
        i<-i+1
      }
      #If patient has only fever or only cough keep them and iterate j by one.
      if(sum(temp)==1){
        SingleSymp_temp[j,]<-temp
        j<-j+1 
      }
    }
    if(Sel=="COVID-19(JAMA)"){
      SingleSymp_COVID19_JAMA[[paste(k," patients",sep="")]]<-SingleSymp_temp
      #Labeling where symptoms simulated from.
      SingleSymp_COVID19_JAMA[[paste(k," patients",sep="")]][,"Label"]<-"COVID-19"
      #Using initial symptom by stochastic progression model to predict disease.
      SingleSymp_COVID19_JAMA[[paste(k," patients",sep="")]][,"Model"]<-"NOT"
      SingleSymp_COVID19_JAMA[[paste(k," patients",sep="")]][which(SingleSymp_COVID19_JAMA[[paste(k," patients",sep="")]][,"Fever"]==1),"Model"]<-"COVID-19"
    }
    if(Sel=="FLU(ICHE)"){
      SingleSymp_FLU_ICHE[[paste(k," patients",sep="")]]<-SingleSymp_temp
      #Labeling where symptoms simulated from.
      SingleSymp_FLU_ICHE[[paste(k," patients",sep="")]][,"Label"]<-"FLU"
      #Using initial symptom by stochastic progression model to predict disease.      
      SingleSymp_FLU_ICHE[[paste(k," patients",sep="")]][,"Model"]<-"NOT"
      SingleSymp_FLU_ICHE[[paste(k," patients",sep="")]][which(SingleSymp_FLU_ICHE[[paste(k," patients",sep="")]][,"Cough"]==1),"Model"]<-"FLU"
    }
    if(Sel=="Neither(ICHE)"){
      #For comparing to COVID-19
      SingleSymp_Neither_ICHE_COVID19[[paste(k," patients",sep="")]]<-SingleSymp_temp
      #Labeling where symptoms simulated from.
      SingleSymp_Neither_ICHE_COVID19[[paste(k," patients",sep="")]][,"Label"]<-"NOT"
      #Using initial symptom by stochastic progression model to predict disease.      
      SingleSymp_Neither_ICHE_COVID19[[paste(k," patients",sep="")]][,"Model"]<-"NOT"
      SingleSymp_Neither_ICHE_COVID19[[paste(k," patients",sep="")]][which(SingleSymp_Neither_ICHE_COVID19[[paste(k," patients",sep="")]][,"Fever"]==1),"Model"]<-"COVID-19"
      
      #For comparing to Influenza
      SingleSymp_Neither_ICHE_FLU[[paste(k," patients",sep="")]]<-SingleSymp_temp
      #Labeling where symptoms simulated from.
      SingleSymp_Neither_ICHE_FLU[[paste(k," patients",sep="")]][,"Label"]<-"NOT"
      #Using initial symptom by stochastic progression model to predict disease.  
      SingleSymp_Neither_ICHE_FLU[[paste(k," patients",sep="")]][,"Model"]<-"NOT"
      SingleSymp_Neither_ICHE_FLU[[paste(k," patients",sep="")]][which(SingleSymp_Neither_ICHE_FLU[[paste(k," patients",sep="")]][,"Cough"]==1),"Model"]<-"FLU"
    }
    k<-k+PatJump[[Sel]]
  }
  l<-l+1
}

DiseaseCount<-seq(1,Interval,1)*1
NeitherCount<-seq(1,Interval,1)*19  

CT_COVID19<-list()
Parameters_COVID19<-data.frame(matrix(0,nrow=2,ncol=0))
rownames(Parameters_COVID19)<-c("Recall","Selectivity")
k<-1
while(k<=Interval){
  print(paste("Contingency Table for ",k," of ",Interval,sep=""))
  
  CT_COVID19[[paste(DiseaseCount[k]," over ",(DiseaseCount[k]+NeitherCount[k]),sep="")]]<-data.frame(matrix(0,nrow=2,ncol=2))
  colnames(CT_COVID19[[paste(DiseaseCount[k]," over ",(DiseaseCount[k]+NeitherCount[k]),sep="")]])<-c("True +","True -")
  rownames(CT_COVID19[[paste(DiseaseCount[k]," over ",(DiseaseCount[k]+NeitherCount[k]),sep="")]])<-c("Predicted +","Predicted -")
  CT_COVID19[[paste(DiseaseCount[k]," over ",(DiseaseCount[k]+NeitherCount[k]),sep="")]]["Predicted +","True +"]<-length(which(SingleSymp_COVID19_JAMA[[paste(DiseaseCount[k]," patients",sep="")]][,"Label"]==SingleSymp_COVID19_JAMA[[paste(DiseaseCount[k]," patients",sep="")]][,"Model"]))
  CT_COVID19[[paste(DiseaseCount[k]," over ",(DiseaseCount[k]+NeitherCount[k]),sep="")]]["Predicted -","True +"]<-length(which(SingleSymp_COVID19_JAMA[[paste(DiseaseCount[k]," patients",sep="")]][,"Label"]!=SingleSymp_COVID19_JAMA[[paste(DiseaseCount[k]," patients",sep="")]][,"Model"]))
  CT_COVID19[[paste(DiseaseCount[k]," over ",(DiseaseCount[k]+NeitherCount[k]),sep="")]]["Predicted +","True -"]<-length(which(SingleSymp_Neither_ICHE_COVID19[[paste(NeitherCount[k]," patients",sep="")]][,"Label"]!=SingleSymp_Neither_ICHE_COVID19[[paste(NeitherCount[k]," patients",sep="")]][,"Model"]))
  CT_COVID19[[paste(DiseaseCount[k]," over ",(DiseaseCount[k]+NeitherCount[k]),sep="")]]["Predicted -","True -"]<-length(which(SingleSymp_Neither_ICHE_COVID19[[paste(NeitherCount[k]," patients",sep="")]][,"Label"]==SingleSymp_Neither_ICHE_COVID19[[paste(NeitherCount[k]," patients",sep="")]][,"Model"]))
  
  Parameters_COVID19["Recall",paste(DiseaseCount[k]," over ",(DiseaseCount[k]+NeitherCount[k]),sep="")]<-CT_COVID19[[paste(DiseaseCount[k]," over ",(DiseaseCount[k]+NeitherCount[k]),sep="")]]["Predicted +","True +"]/(CT_COVID19[[paste(DiseaseCount[k]," over ",(DiseaseCount[k]+NeitherCount[k]),sep="")]]["Predicted +","True +"]+CT_COVID19[[paste(DiseaseCount[k]," over ",(DiseaseCount[k]+NeitherCount[k]),sep="")]]["Predicted -","True +"])
  Parameters_COVID19["Selectivity",paste(DiseaseCount[k]," over ",(DiseaseCount[k]+NeitherCount[k]),sep="")]<-CT_COVID19[[paste(DiseaseCount[k]," over ",(DiseaseCount[k]+NeitherCount[k]),sep="")]]["Predicted -","True -"]/(CT_COVID19[[paste(DiseaseCount[k]," over ",(DiseaseCount[k]+NeitherCount[k]),sep="")]]["Predicted -","True -"]+CT_COVID19[[paste(DiseaseCount[k]," over ",(DiseaseCount[k]+NeitherCount[k]),sep="")]]["Predicted +","True -"])
  
  k<-k+1
}

CT_FLU<-list()
Parameters_FLU<-data.frame(matrix(0,nrow=2,ncol=0))
rownames(Parameters_FLU)<-c("Recall","Selectivity")
k<-1
while(k<=Interval){
  print(paste("Contingency Table for ",k," of ",Interval,sep=""))
  
  CT_FLU[[paste(DiseaseCount[k]," over ",(DiseaseCount[k]+NeitherCount[k]),sep="")]]<-data.frame(matrix(0,nrow=2,ncol=2))
  colnames(CT_FLU[[paste(DiseaseCount[k]," over ",(DiseaseCount[k]+NeitherCount[k]),sep="")]])<-c("True +","True -")
  rownames(CT_FLU[[paste(DiseaseCount[k]," over ",(DiseaseCount[k]+NeitherCount[k]),sep="")]])<-c("Predicted +","Predicted -")
  CT_FLU[[paste(DiseaseCount[k]," over ",(DiseaseCount[k]+NeitherCount[k]),sep="")]]["Predicted +","True +"]<-length(which(SingleSymp_FLU_ICHE[[paste(DiseaseCount[k]," patients",sep="")]][,"Label"]==SingleSymp_FLU_ICHE[[paste(DiseaseCount[k]," patients",sep="")]][,"Model"]))
  CT_FLU[[paste(DiseaseCount[k]," over ",(DiseaseCount[k]+NeitherCount[k]),sep="")]]["Predicted -","True +"]<-length(which(SingleSymp_FLU_ICHE[[paste(DiseaseCount[k]," patients",sep="")]][,"Label"]!=SingleSymp_FLU_ICHE[[paste(DiseaseCount[k]," patients",sep="")]][,"Model"]))
  CT_FLU[[paste(DiseaseCount[k]," over ",(DiseaseCount[k]+NeitherCount[k]),sep="")]]["Predicted +","True -"]<-length(which(SingleSymp_Neither_ICHE_FLU[[paste(NeitherCount[k]," patients",sep="")]][,"Label"]!=SingleSymp_Neither_ICHE_FLU[[paste(NeitherCount[k]," patients",sep="")]][,"Model"]))
  CT_FLU[[paste(DiseaseCount[k]," over ",(DiseaseCount[k]+NeitherCount[k]),sep="")]]["Predicted -","True -"]<-length(which(SingleSymp_Neither_ICHE_FLU[[paste(NeitherCount[k]," patients",sep="")]][,"Label"]==SingleSymp_Neither_ICHE_FLU[[paste(NeitherCount[k]," patients",sep="")]][,"Model"]))
  
  Parameters_FLU["Recall",paste(DiseaseCount[k]," over ",(DiseaseCount[k]+NeitherCount[k]),sep="")]<-CT_FLU[[paste(DiseaseCount[k]," over ",(DiseaseCount[k]+NeitherCount[k]),sep="")]]["Predicted +","True +"]/(CT_FLU[[paste(DiseaseCount[k]," over ",(DiseaseCount[k]+NeitherCount[k]),sep="")]]["Predicted +","True +"]+CT_FLU[[paste(DiseaseCount[k]," over ",(DiseaseCount[k]+NeitherCount[k]),sep="")]]["Predicted -","True +"])
  Parameters_FLU["Selectivity",paste(DiseaseCount[k]," over ",(DiseaseCount[k]+NeitherCount[k]),sep="")]<-CT_FLU[[paste(DiseaseCount[k]," over ",(DiseaseCount[k]+NeitherCount[k]),sep="")]]["Predicted -","True -"]/(CT_FLU[[paste(DiseaseCount[k]," over ",(DiseaseCount[k]+NeitherCount[k]),sep="")]]["Predicted -","True -"]+CT_FLU[[paste(DiseaseCount[k]," over ",(DiseaseCount[k]+NeitherCount[k]),sep="")]]["Predicted +","True -"])
  
  k<-k+1
}

colnames(Parameters_COVID19_Recall)<-colnames(Parameters_COVID19)
Parameters_COVID19_Recall[z,]<-Parameters_COVID19["Recall",]
colnames(Parameters_FLU_Recall)<-colnames(Parameters_FLU)
Parameters_FLU_Recall[z,]<-Parameters_FLU["Recall",]

colnames(Parameters_COVID19_Selectivity)<-colnames(Parameters_COVID19)
Parameters_COVID19_Selectivity[z,]<-Parameters_COVID19["Selectivity",]
colnames(Parameters_FLU_Selectivity)<-colnames(Parameters_FLU)
Parameters_FLU_Selectivity[z,]<-Parameters_FLU["Selectivity",]


z<-z+1
}

