setwd("C:/Users/chery/Desktop/403PROJECT")
numsim=10000
counter=0

for(i in 1:numsim){
  
  #load data from file
  gh=read.table(file="pre_ex_v1.csv",header=TRUE,sep=",")
  
  #make sure it is a dataframe
  gh=as.data.frame(gh)
  gh
  #make factors
  gh$greenhouse=factor(gh$greenhouse)
  gh$humidity=factor(gh$humidity)
  gh$pot=factor(gh$pot)
  gh$plant=factor(gh$plant)
  gh$yield=factor(gh$yield)
  
  #to simulate power, we need to simulate yields, analyze data and repeat
  
  #how many greenhouses
  (num.gh=unique(gh$greenhouse))
  (num.gh=length(num.gh))
  
  #generate errors for the greenhouses
  error1=rnorm(num.gh, mean=0, sd=1)
  gh$error1=rep(error1, each=4)
  gh
  
  #generate errors for each plant withing a greenhouse
  num.plants=length(gh$plant)
  gh$error2=rnorm(num.plants, mean=0, sd=0.5)
  gh
  
  #now we have to generate the effects for humidity and also plant variety
  # the values are not important, but the difference (here 1kg) is important
  # Notice that the design has 8 h's followed by 8 l's
  (num.hum=length(gh$pot)/2)
  
  gh$humidity.effect[1:num.hum]=2
  gh$humidity.effect[(num.hum+1):length(gh$pot)]=1
  gh
  
  # Notice that the design has 2 B52's followed by 2 NL's.  This repeats 4 times
  plant.effect=c(2,2,1,1)
  gh$plant.effect=rep(plant.effect,times=2)
  gh
  
  #now put it all together to compute yield
  gh$yield=gh$humidity.effect+gh$plant.effect+gh$error1+gh$error2
  
  #Now that we have data, we should analyze it
  #This part is hard
  result <- aov(yield ~ humidity * plant + Error(greenhouse:humidity), data = gh)
  x=summary(result)
  x=x$`Error: Within`
  # first way x[[1]]$`Pr(>F)`[1]
  
  #second way
  z=unlist(x)
  pval=z[13]
  counter=counter+(pval<0.05)
  
}

counter/numsim*100
