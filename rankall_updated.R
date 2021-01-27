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

rankall<- function(outcome, num){
  dataset<- data.frame(Hospital.Name= c(), State= c(), Heart.Attack= c(), Heart.Failure=c(), Pneumonia= c())
  if (outcome== 'heart attack' || outcome== 'Heart attack'){
    x<- data[order(data$State, data$Heart.Attack, data$Hospital.Name),]
    dataset1<- data.frame(Hospital.Name= c(), State= c(), Heart.Attack= c())
    data_HA<- subset(x, select = -c(Heart.Failure, Pneumonia))
    #dataset<- subset(dataset, select = -c(Heart.Failure, Pneumonia))
    HA_good<- complete.cases(data_HA)
    HA_complete<- data_HA[HA_good,]
    x1<- split(HA_complete, HA_complete$State)
    
    for (i in seq_along(x1)){
      y<- x1[[i]]
      if (num=='best'){
        dataset1= rbind(dataset1, y[1,])
      }
      else if(num=='worst'){
        dataset1= rbind(dataset1, tail(y,1))
      }
      else{
        dataset1= rbind(dataset1, y[num,])
      }
      
      
    }
    print(dataset1)
  }
  else if (outcome== 'heart failure' || outcome== 'Heart failure'){
    x<- data[order(data$State, data$Heart.Failure, data$Hospital.Name),]
    dataset2<- data.frame(Hospital.Name= c(), State= c(), Heart.Attack= c())
    data_HF<- subset(x, select = -c(Heart.Attack, Pneumonia))
    #dataset<- subset(dataset, select = -c(Heart.Failure, Pneumonia))
    HF_good<- complete.cases(data_HF)
    HF_complete<- data_HF[HF_good,]
    x2<- split(HF_complete, HF_complete$State)
    
    for (i in seq_along(x2)){
      y<- x2[[i]]
      if (num=='best'){
        dataset2= rbind(dataset2, y[1,])
      }
      else if(num=='worst'){
        dataset2= rbind(dataset2, tail(y,1))
      }
      else{
        dataset2= rbind(dataset2, y[num,])
      }
      
      
    }
    print(dataset2)
  }
  else if (outcome== 'Pneumonia' || outcome== 'pneumonia'){
    x<- data[order(data$State, data$Pneumonia, data$Hospital.Name),]
    dataset3<- data.frame(Hospital.Name= c(), State= c(), Heart.Attack= c())
    data_PN<- subset(x, select = -c(Heart.Attack, Heart.Failure))
    #dataset<- subset(dataset, select = -c(Heart.Failure, Pneumonia))
    PN_good<- complete.cases(data_PN)
    PN_complete<- data_PN[PN_good,]
    x3<- split(PN_complete, PN_complete$State)
    
    for (i in seq_along(x3)){
      y<- x3[[i]]
      
      if (num=='best'){
        dataset3= rbind(dataset3, y[1,])
      }
      else if(num=='worst'){
        dataset3= rbind(dataset3, tail(y,1))
      }
      else{
        dataset3= rbind(dataset3, y[num,])
      }
      
    }
    print(dataset3)
  }
  else { #if outcome entered by user is neither of heart attack, heart failure or pneumonia
    message('Outcome is invalid')
  }
}
#rankall("heart attack", 20)
#r <- rankall("heart attack", 4)
#as.character(subset(r, State == "HI")$Hospital.Name)
#r <- rankall("pneumonia", "worst")
#as.character(subset(r, State == "NJ")$Hospital.Name)
r <- rankall("heart failure", 10)
as.character(subset(r, State == "NV")$Hospital.Name)