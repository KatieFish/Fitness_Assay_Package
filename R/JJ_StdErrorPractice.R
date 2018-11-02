get_some_stderror<- function(FC_data){
  
  #create booleans based on user input for 
    #1) do they want to eliminate data below a certain threshold 
    #) if so, what is that threshold? 
   # example 
 # group_replicates<- as.logical(readline(prompt="Group replicates? (TRUE/FALSE): "))
  
  
  FC_data$"Std.Error" <- NA #Creates a new column
  c <- ncol(FC_data)
  for (i in 1:nrow(FC_data)) {
    count<- FC_data[i, seq(2,ncol(FC_data)-1,2)]
    refcount<- FC_data[i, seq(3,ncol(FC_data)-1,2)]
    diff<- count-refcount
    natlog<- log(diff/refcount)
    natlog<-as.numeric(natlog[1,])
    time_points<- (c(0:((ncol(FC_data)-4)/2)))*10
    regress<- lm(natlog ~ time_points)
    slope<- as.numeric(coef(regress)[2])
    stderror<- as.numeric(coef(summary(regress))[2,2])               
    FC_data[i,c] <- stderror
  }
  return(FC_data)
  
  # if the user wants to get rid of std errors below certain threshold 
  # removes rows that do not meet the threshold 
  
  view(FC_data)
}

