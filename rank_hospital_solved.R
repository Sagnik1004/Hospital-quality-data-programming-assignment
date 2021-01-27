#rankhospital function

outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
head(outcome)
ncol(outcome)
names(outcome)
outcome[,11]<- as.numeric(outcome[,11])
hist(outcome[,11])
data<- outcome[,c(2,7,11,17,23)] #contains only the columns for hospital name, state, heart attack deaths, heart failure deaths and penumonia deaths
names(data)[3]<- "Heart.Attack"
names(data)[4]<- "Heart.Failure"
names(data)[5]<- 'Pneumonia'
head(data)


rankhospital<- function(state, outcome, num){
  st<- data[,2]                #st takes all the abreiveated state names 
  if (state %in% st== FALSE){  #if argument state is not in st, code will throw error
    message('Invalid state')
  }
  else{                       #if the argument state is a valid state name, it will execute the rest of the code    
    dataset<- data.frame(Hospital.Name= c(), State= c(), Heart.Attack= c(), Heart.Failure=c(), Pneumonia= c()) #dataset is an empty dataframe
    for (i in seq_along(st)) {     #looping through st i.e. all the state names to find the state given by user       
      if (st[i]==state){          #whenever the inputed state is found down the rows, all the coulumns in data are appended into dataset(which was first intitialised as an empty dataframe) 
        placeholder<- data[i,]     
        dataset= rbind(dataset, placeholder)
      }
    }
    placeholder_dataset<- dataset #copying the contents of dataset(which by now is a dataframe containing all the data for the 3 outcomes and hospital names for the given state name entered by user) into placeholder_dataset
    dataset<- data.frame(Hospital.Name= c(), State= c(), Heart.Attack= c(), Heart.Failure=c(), Pneumonia= c()) #Turning dataset into an empty dataframe again after copying its contents into placeholder_dataset
    
    if (outcome== 'heart attack' || outcome== 'Heart attack'){  #if outcome entered by user is heart attack
      HA<- subset(placeholder_dataset, select = -c(Heart.Failure, Pneumonia)) #dropping the columns for Heart.Failure and Pneumonia
      HA_good<- complete.cases(HA) #removing NA values now
      HA_complete<- HA[HA_good,] #HA_complete is now the dataframe for heart attack without NA values
      HA2<- tapply(HA_complete$Heart.Attack, HA_complete$Hospital.Name, min ) #applying min function to find the best hospital
      HA2_numeric<- as.numeric(HA2)
      HA2_numeric_good<- complete.cases(HA2_numeric)
      HA2_withoutNA<- HA2_numeric[HA2_numeric_good]
      if(num> HA2_numeric){
        print(NA)
      }
      a_HA<- sort(HA2_withoutNA)
      b_HA<- c()
      for(i in seq_along(HA2_numeric)){
        x_HA<- which(HA2_numeric== a_HA[i])
        b_HA= append(b_HA, x_HA)
        
      }
      c_HA<- unique(b_HA)
      d_HA<- HA2[c_HA]
      #print(d_HA)
      if (num=='best'){
        print(d_HA[1])
      }
      else if(num=='worst'){
        print(tail(d_HA, 1))
      }
      else{
        print(d_HA[num])
      }
      
      
    }          #Same logic is used if outcomes entered by user are heart failure or Pneumonia down below
    
    else if(outcome== 'heart failure' || outcome== 'Heart failure'){
      HF<- subset(placeholder_dataset, select = -c(Heart.Attack, Pneumonia))
      HF_good<- complete.cases(HF)
      HF_complete<- HF[HF_good,]
      HF2<- tapply(HF_complete$Heart.Failure, HF_complete$Hospital.Name, min )
      HF2_numeric<- as.numeric(HF2)
      HF2_numeric_good<- complete.cases(HF2_numeric)
      HF2_withoutNA<- HF2_numeric[HF2_numeric_good]
      if(num> HF2_numeric){
        print(NA)
      }
      
      a<- sort(HF2_withoutNA)
      b<- c()
      for(i in seq_along(HF2_numeric)){
        x<- which(HF2_numeric== a[i])
        b= append(b, x)
        
      }
      c<- unique(b)
      d<- HF2[c]
      #print(d)
      if (num=='best'){
        print(d[1])
      }
      else if(num=='worst'){
        print(tail(d, 1))
      }
      else{
        print(d[num])
      }
      
    }
    else if(outcome== 'Pneumonia'){
      PN<- subset(placeholder_dataset, select = -c(Heart.Attack, Heart.Failure))
      PN_good<- complete.cases(PN)
      PN_complete<- PN[PN_good,]
      PN2<- tapply(PN_complete$Pneumonia, PN_complete$Hospital.Name, min )
      PN2_numeric<- as.numeric(PN2)
      PN2_numeric_good<- complete.cases(PN2_numeric)
      PN2_withoutNA<- PN2_numeric[PN2_numeric_good]
      if(num> PN2_numeric){
        print(NA)
      }
      a_PN<- sort(PN2_withoutNA)
      b_PN<- c()
      for(i in seq_along(PN2_numeric)){
        x_PN<- which(PN2_numeric== a_PN[i])
        b_PN= append(b_PN, x_PN)
        
      }
      c_PN<- unique(b_PN)
      d_PN<- PN2[c_PN]
      #print(d_PN)
      if (num=='best'){
        print(d_PN[1])
      }
      else if(num=='worst'){
        print(tail(d_PN, 1))
      }
      else{
        print(d_PN[num])
      }
    }
    else { #if outcome entered by user is neither of heart attack, heart failure or pneumonia
      message('Outcome is invalid')
    }
    
  }
  
}

rankhospital("MD", "heart attack", "worst")
rankhospital("MD", "heart attack", "best")