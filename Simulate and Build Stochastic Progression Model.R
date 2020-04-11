library("hasseDiagram") #Library for creating diagram illustrating our model.
Simulate<-TRUE #Turn on the code that simulates patients for this study.

#Names given to all studeis that frequencies came from (Remove which ones not desired for current loop).
Sel_Opt<-c( "COVID-19(WHO)"
           ,"COVID-19(NEJM)"
           ,"COVID-19(NEJM) Severe","COVID-19(NEJM) Nonsevere"
           ,"COVID-19(JAMA)" 
           ,"FLU(AIM)"
           ,"FLU(ICHE)"
           ,"SARS(JAMA)"
           ,"SARS(R)","MERS(R)"
           ,"MERS(JI)"
           )

s<-1
while(s<=length(Sel_Opt)){
Sel<-Sel_Opt[s]

Symptoms<-list()

# Report of the WHO-China Joint Mission on Coronavirus Disease 2019 (COVID-19)
# (16-24 February 2020)
Symptoms[["COVID-19(WHO)"]]<-data.frame(matrix(0,nrow=2,ncol=0))
rownames(Symptoms[["COVID-19(WHO)"]])<-c("Freq","CtsForSamp")
Symptoms[["COVID-19(WHO)"]]["Freq","Fever"]<-.879
Symptoms[["COVID-19(WHO)"]]["Freq","Cough"]<-.677
Symptoms[["COVID-19(WHO)"]]["Freq","Sore Throat"]<-.139
Symptoms[["COVID-19(WHO)"]]["Freq","Headache"]<-.136
Symptoms[["COVID-19(WHO)"]]["Freq","Myalgia"]<-.148
Symptoms[["COVID-19(WHO)"]]["Freq","Nausea or Vomiting"]<-.05
Symptoms[["COVID-19(WHO)"]]["Freq","Diarrhea"]<-.037

#Clinical Characteristics of Coronavirus Disease 2019 in China
#Guan PhD et. al. in The New England Journal of Medicine
Symptoms[["COVID-19(NEJM)"]]<-data.frame(matrix(0,nrow=2,ncol=0))
rownames(Symptoms[["COVID-19(NEJM)"]])<-c("Freq","CtsForSamp")
Symptoms[["COVID-19(NEJM)"]]["Freq","Fever"]<-.887
Symptoms[["COVID-19(NEJM)"]]["Freq","Cough"]<-.678
Symptoms[["COVID-19(NEJM)"]]["Freq","Sore Throat"]<-.139
Symptoms[["COVID-19(NEJM)"]]["Freq","Headache"]<-.136
Symptoms[["COVID-19(NEJM)"]]["Freq","Myalgia"]<-.149
Symptoms[["COVID-19(NEJM)"]]["Freq","Nausea or Vomiting"]<-.05
Symptoms[["COVID-19(NEJM)"]]["Freq","Diarrhea"]<-.038

Symptoms[["COVID-19(NEJM) Severe"]]<-data.frame(matrix(0,nrow=2,ncol=0))
rownames(Symptoms[["COVID-19(NEJM) Severe"]])<-c("Freq","CtsForSamp")
Symptoms[["COVID-19(NEJM) Severe"]]["Freq","Fever"]<-.881
Symptoms[["COVID-19(NEJM) Severe"]]["Freq","Cough"]<-.673
Symptoms[["COVID-19(NEJM) Severe"]]["Freq","Sore Throat"]<-.14
Symptoms[["COVID-19(NEJM) Severe"]]["Freq","Headache"]<-.134
Symptoms[["COVID-19(NEJM) Severe"]]["Freq","Myalgia"]<-.145
Symptoms[["COVID-19(NEJM) Severe"]]["Freq","Nausea or Vomiting"]<-.046
Symptoms[["COVID-19(NEJM) Severe"]]["Freq","Diarrhea"]<-.035

Symptoms[["COVID-19(NEJM) Nonsevere"]]<-data.frame(matrix(0,nrow=2,ncol=0))
rownames(Symptoms[["COVID-19(NEJM) Nonsevere"]])<-c("Freq","CtsForSamp")
Symptoms[["COVID-19(NEJM) Nonsevere"]]["Freq","Fever"]<-.919
Symptoms[["COVID-19(NEJM) Nonsevere"]]["Freq","Cough"]<-.705
Symptoms[["COVID-19(NEJM) Nonsevere"]]["Freq","Sore Throat"]<-.133
Symptoms[["COVID-19(NEJM) Nonsevere"]]["Freq","Headache"]<-.15
Symptoms[["COVID-19(NEJM) Nonsevere"]]["Freq","Myalgia"]<-.173
Symptoms[["COVID-19(NEJM) Nonsevere"]]["Freq","Nausea or Vomiting"]<-.069
Symptoms[["COVID-19(NEJM) Nonsevere"]]["Freq","Diarrhea"]<-.058

#Clinical Characteristics of 138 Hospitalized Patients With 2019 Novel Coronavirus-Infected Pneumonia inWuhan, China
#DaweiWang, MD et al in JAMA (Pub:Febuary 7, 2020 Corrected: Febuary 20, 20202)
Symptoms[["COVID-19(JAMA)"]]<-data.frame(matrix(0,nrow=2,ncol=0))
rownames(Symptoms[["COVID-19(JAMA)"]])<-c("Freq","CtsForSamp")
Symptoms[["COVID-19(JAMA)"]]["Freq","Fever"]<-.986
Symptoms[["COVID-19(JAMA)"]]["Freq","Cough"]<-.594
Symptoms[["COVID-19(JAMA)"]]["Freq","Sore Throat"]<-.174
Symptoms[["COVID-19(JAMA)"]]["Freq","Headache"]<-.065
Symptoms[["COVID-19(JAMA)"]]["Freq","Myalgia"]<-.348
Symptoms[["COVID-19(JAMA)"]]["Freq","Nausea or Vomiting"]<-.101 #Note we used Nausea frequency .101 assuming almost all Vomiting (.036) occur in this group 
Symptoms[["COVID-19(JAMA)"]]["Freq","Diarrhea"]<-.101

#Clinical Signs and Symptoms Predicting Influenza Infection
#Arnold S. Monto MD et al in Arch Intern Med (November 27, 2000)
Symptoms[["FLU(AIM)"]]<-data.frame(matrix(0,nrow=2,ncol=0))
rownames(Symptoms[["FLU(AIM)"]])<-c("Freq","CtsForSamp")
Symptoms[["FLU(AIM)"]]["Freq","Fever"]<-.68
Symptoms[["FLU(AIM)"]]["Freq","Cough"]<-.93
Symptoms[["FLU(AIM)"]]["Freq","Sore Throat"]<-.84
Symptoms[["FLU(AIM)"]]["Freq","Headache"]<-.91
Symptoms[["FLU(AIM)"]]["Freq","Myalgia"]<-.94
Symptoms[["FLU(AIM)"]]["Freq","Nausea or Vomiting"]<-.01
Symptoms[["FLU(AIM)"]]["Freq","Diarrhea"]<-.01

#Symptoms of Influenza Virus Infection in Hospitalized Patients
#C. van den Dool, MSc et al in INFECTION CONTROL AND HOSPITAL EPIDEMIOLOGY (April 2008)
Symptoms[["FLU(ICHE)"]]<-data.frame(matrix(0,nrow=2,ncol=0))
rownames(Symptoms[["FLU(ICHE)"]])<-c("Freq","CtsForSamp")
Symptoms[["FLU(ICHE)"]]["Freq","Fever"]<-.45
Symptoms[["FLU(ICHE)"]]["Freq","Cough"]<-.75
Symptoms[["FLU(ICHE)"]]["Freq","Sore Throat"]<-.35
Symptoms[["FLU(ICHE)"]]["Freq","Headache"]<-.55
Symptoms[["FLU(ICHE)"]]["Freq","Myalgia"]<-.3
Symptoms[["FLU(ICHE)"]]["Freq","Nausea or Vomiting"]<-.01
Symptoms[["FLU(ICHE)"]]["Freq","Diarrhea"]<-.01

#Clinical Features and Short-term Outcomes of 144 Patients With SARS in the Greater Toronto Area
#Christopher M. Booth, MD et al in JAMA (June 4, 2003)
Symptoms[["SARS(JAMA)"]]<-data.frame(matrix(0,nrow=2,ncol=0))
rownames(Symptoms[["SARS(JAMA)"]])<-c("Freq","CtsForSamp")
Symptoms[["SARS(JAMA)"]]["Freq","Fever"]<-.993
Symptoms[["SARS(JAMA)"]]["Freq","Cough"]<-(.694+.049)
Symptoms[["SARS(JAMA)"]]["Freq","Sore Throat"]<-.125
Symptoms[["SARS(JAMA)"]]["Freq","Headache"]<-.354
Symptoms[["SARS(JAMA)"]]["Freq","Myalgia"]<-.493
Symptoms[["SARS(JAMA)"]]["Freq","Nausea or Vomiting"]<-.194
Symptoms[["SARS(JAMA)"]]["Freq","Diarrhea"]<-.236

#RESPIRATORY INFECTIONS IN THE ASIA-PACIFIC REGION
#YUDONG YIN AND RICHARD G. WUNDERINK in Respirology (September 17, 2017)
Symptoms[["SARS(R)"]]<-data.frame(matrix(0,nrow=2,ncol=0))
rownames(Symptoms[["SARS(R)"]])<-c("Freq","CtsForSamp")
Symptoms[["SARS(R)"]]["Freq","Fever"]<-.997
Symptoms[["SARS(R)"]]["Freq","Cough"]<-.583
Symptoms[["SARS(R)"]]["Freq","Sore Throat"]<-.171
Symptoms[["SARS(R)"]]["Freq","Headache"]<-.389
Symptoms[["SARS(R)"]]["Freq","Myalgia"]<-.591
Symptoms[["SARS(R)"]]["Freq","Nausea or Vomiting"]<-.154
Symptoms[["SARS(R)"]]["Freq","Diarrhea"]<-.174

Symptoms[["MERS(R)"]]<-data.frame(matrix(0,nrow=2,ncol=0))
rownames(Symptoms[["MERS(R)"]])<-c("Freq","CtsForSamp")
Symptoms[["MERS(R)"]]["Freq","Fever"]<-.841
Symptoms[["MERS(R)"]]["Freq","Cough"]<-.633
Symptoms[["MERS(R)"]]["Freq","Sore Throat"]<-.135
Symptoms[["MERS(R)"]]["Freq","Headache"]<-.188
Symptoms[["MERS(R)"]]["Freq","Myalgia"]<-.4
Symptoms[["MERS(R)"]]["Freq","Nausea or Vomiting"]<-.151
Symptoms[["MERS(R)"]]["Freq","Diarrhea"]<-.204

#Predictive factors for pneumonia development and progression to respiratory failure in MERS-CoV infected patients
#Jae-Hoon Ko et al in Journal of Infection (August 5, 2016) Note: Only Initial Symptoms
Symptoms[["MERS(JI)"]]<-data.frame(matrix(0,nrow=2,ncol=0))
rownames(Symptoms[["MERS(JI)"]])<-c("Freq","CtsForSamp")
Symptoms[["MERS(JI)"]]["Freq","Fever"]<-.559
Symptoms[["MERS(JI)"]]["Freq","Cough"]<-.333
Symptoms[["MERS(JI)"]]["Freq","Myalgia"]<-.378
Symptoms[["MERS(JI)"]]["Freq","Diarrhea"]<-.067

#If all symptoms were random.
Symptoms[["Random"]]<-data.frame(matrix(0,nrow=2,ncol=0))
rownames(Symptoms[["Random"]])<-c("Freq","CtsForSamp")
Symptoms[["Random"]]["Freq","Fever"]<-.5
Symptoms[["Random"]]["Freq","Cough"]<-.5
Symptoms[["Random"]]["Freq","Sore Throat"]<-.5
Symptoms[["Random"]]["Freq","Headache"]<-.5
Symptoms[["Random"]]["Freq","Myalgia"]<-.5
Symptoms[["Random"]]["Freq","Nausea or Vomiting"]<-.5
Symptoms[["Random"]]["Freq","Diarrhea"]<-.5

#Creating the Hasse Diagram (The graph representing flow of poset).

#Creating nodes and node labels
NS<-ncol(Symptoms[[Sel]])
PermOfSymp<-data.frame(matrix(0,nrow=(2^NS),ncol=NS))
colnames(PermOfSymp)<-colnames(Symptoms[[Sel]])
i<-1
while(i<=NS){
  PermOfSymp[,i]<-rep(c(rep(1,(2^(NS-i))),rep(0,(2^(NS-i)))),(2^(i-1)))
  i<-i+1
}
PermOfSymp<-PermOfSymp[order(rowSums(PermOfSymp)),]
rownames(PermOfSymp)<-seq(1,nrow(PermOfSymp),1)
rownames(PermOfSymp)<-apply(PermOfSymp,1,function(x) gsub("[ , ]","",(toString(x))))

#Creating graph by connecting nodes by possible edges
G_data<-matrix(data=FALSE, nrow = nrow(PermOfSymp), ncol = nrow(PermOfSymp))
i<-1
while(i<=nrow(G_data)){
  G_data[i,]<-(rowSums(abs(PermOfSymp[,colnames(Symptoms[[Sel]])]-PermOfSymp[rep(i,nrow(PermOfSymp)),colnames(Symptoms[[Sel]])]))==1)
  i<-i+1
}
G_data[lower.tri(G_data,diag = TRUE)]<-FALSE
rownames(G_data)<-rownames(PermOfSymp)
colnames(G_data)<-rownames(PermOfSymp)

#Printing Graph
hasse(G_data,labels = rownames(PermOfSymp))

#Code to simulate patients
if(Simulate){
  MarbPerJar<-1e3 #Total elements to pull for each symptom (Both have symptom, 1, and dont have,0.)
  Symptoms[[Sel]]["CtsForSamp",]<-round(Symptoms[[Sel]]["Freq",]*MarbPerJar) #The number of elements for each symbol representing having it (1).
  
  #Filling character arrays for each symptom with elements representing having the symptom (1) and not (0).
  Jar<-list()
  i<-1
  while(i<=ncol(Symptoms[[Sel]])){
    Curr_Symp<-colnames(Symptoms[[Sel]])[i]
    Jar[[Curr_Symp]]<-rep(0,MarbPerJar)
    Jar[[Curr_Symp]][1:(Symptoms[[Sel]]["CtsForSamp",Curr_Symp])]<-rep(1,Symptoms[[Sel]]["CtsForSamp",Curr_Symp])
    i<-i+1
  }
  
  NumSimPat<-5e5 #Number of patients to simulate
  Sim_Pat<-data.frame(matrix(0,nrow=NumSimPat,ncol=ncol(Symptoms[[Sel]]))) #Creating data frame to store simulated patients
  colnames(Sim_Pat)<-c(colnames(Symptoms[[Sel]]))
  
  #Simulating each patient and each symptom (using Bernouli Distribution for each symptom in each patient)
  i<-1
  while(i<=nrow(Sim_Pat)){
    print(paste("Simulating Patient ",i," of ",nrow(Sim_Pat))) #Simulating patient i
    j<-1 
    while(j<=ncol(Symptoms[[Sel]])){
      Curr_Symp<-colnames(Symptoms[[Sel]])[j] #Simulating symptom j of patient i
      Pull<-sample.int(length(Jar[[Curr_Symp]]),1) #Selecting whether patient i has symptom j or not
      Marbles<-Jar[[Curr_Symp]][Pull] #If index slects a 1 they have the symptom and 0 else. 
      Sim_Pat[i,Curr_Symp]<-Marbles #Recording 1 (having symptom) or 0 (not having symptom) for patient i (row) symptom j (column) in data frame.
      j<-j+1
    }
    i<-i+1
  }

}

Sim_Pat[,"RowSums"]<-rowSums(Sim_Pat) #Total Number of Symptoms for each patient
Sim_Pat[,"Grouped"]<-apply(Sim_Pat[,colnames(Symptoms[[Sel]])],1,function(x) gsub("[ , ]","",(toString(x)))) #Matching set of binary variable representing having and not having symptoms with nodes on graph.

#Finding State probabilities from simulated patients
cat("\n")
print("STATE PROBABILITIES")
StateProbMat<-data.frame(matrix(NA,nrow=nrow(G_data),ncol=1))
rownames(StateProbMat)<-rownames(G_data)
colnames(StateProbMat)<-"State Probabilites"

Curr<-rownames(PermOfSymp)[1]
print(paste("State Prob. of ",Curr,sep=""))
Curr_SP<-1
print(Curr_SP)
StateProbMat[Curr,]<-Curr_SP
i<-2
while(i<=nrow(PermOfSymp)){
  Curr<-rownames(PermOfSymp)[i]
  print(paste("State Prob. of ",Curr,sep=""))
  Curr_SP<-(length(which(Sim_Pat$Grouped==Curr))/length(which(Sim_Pat$RowSums==sum(PermOfSymp[Curr,])))) #All patients with particular i symptoms overall all patients with i symptoms. 
  print(Curr_SP)
  StateProbMat[Curr,]<-Curr_SP
  i<-i+1
}

#Finding Transition probabilities from simulated patients
cat("\n")
print("TRANSITION PROBABILITIES")
TransProbMat<-data.frame(matrix(NA,nrow=nrow(G_data),ncol=ncol(G_data)))
rownames(TransProbMat)<-rownames(G_data)
colnames(TransProbMat)<-colnames(G_data)

i<-1
while(i<=nrow(G_data)){
  
  Curr<-colnames(G_data)[which(t(G_data[i,]))] #Finding names of all possible terminal nodes that share an initial node in the graph.
  Den<-length(which(Sim_Pat$Grouped %in% Curr)) #Finding number of all possible terminal nodes that share an initial node in the graph.
  
  GivLab<-colSums(PermOfSymp[which(rownames(PermOfSymp) %in% Curr),]) #Labeling initial node all possible terminal nodes come from (Given)
  GivLab[GivLab!=(length(Curr))]<-0
  GivLab[GivLab==(length(Curr))]<-1
  GivLab<-gsub("[ , ]","",(toString(GivLab)))
  
  j<-1
  while(j<=length(Curr)){
    #For all but final transition to maximum set of graph.
    if(!(gsub("[ , ]","",toString(rep(1,ncol(Symptoms[[Sel]])))) %in% Curr)){
      print(paste("Trans. Prob. of ",Curr[j]," Given ",GivLab,sep=""))
      Curr_TP<-(length(which(Sim_Pat$Grouped==Curr[j]))/Den) #Number of simulated patients at selected terminal node over all nodes that the Given set could have gone to.
      print(Curr_TP)
      TransProbMat[GivLab,Curr[j]]<-Curr_TP
    }else{
      #For final transition to maximum set of graph.
      print(paste("A Trans. Prob.  to ",Curr[j],sep=""))
      Curr_TP<-(length(which(Sim_Pat$Grouped==Curr[j]))/Den) #Number of simulated patients at selected terminal node overmall states the Given set could have gone to.
      print(Curr_TP)
      TransProbMat[G_data[,Curr[j]],Curr[j]]<-Curr_TP
    }
    
    j<-j+1
  }
  
  i<-i+1
}

write.csv(StateProbMat,paste("State Prob. For ",Sel," with 7 Symptoms with 500k Sims.csv",sep="")) #Save State Probabilities to CSV.
write.csv(TransProbMat,paste("Transition Prob. For ",Sel," with 7 Symptoms with 500k Sims.csv",sep="")) #Save Transition Probabilities to CSV.

s<-s+1
}
