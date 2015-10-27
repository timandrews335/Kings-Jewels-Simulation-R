#This is a script which runs the King's Jewels simulation as described
#at the Empty Pipes blog post @ http://emptypipes.org/2015/10/21/kings-jewels/
#Change the number of iterations as needed
#
#
#
#

require(reshape2)
require(ggplot2)

#User Inputs

  #simulate n times
  iterations <- 10000
  
  #discard counts that we want to go through.  0 to 9
  discards <- c(0:9)



#Function with simulates picking jewels as per the rules
simulateJewelPicking <- function(discardCount){
  #Get 10 random jewels
  jewelValues <- sample(0:9, 10, replace=T)
  jewelIDs <- 0:9
  jewels <- data.frame(jewelIDs, jewelValues)
  colnames(jewels) <- c('ID', 'value')
  
  
  #Look at and discard jewels, up to the discard count
  #Record the highest value seen
  #Record the current row we are on
  highestSeen <- -1
  rowIndex <- discardCount + 1
  if (discardCount > 0) {
    for (i in 1:discardCount) {
      currentValue <- jewels[i,2]
      rowIndex <- i + 1
      #print(paste('current value', currentValue, sep=' '))
      if (currentValue > highestSeen)
      {
        highestSeen <- currentValue
      }
    }
  }
  
  #print(paste('highest seen ' , highestSeen, sep=" "))
  
  #get a data frame of all jewels we have not look at yet
  leftOvers <- jewels[rowIndex:10,]
  leftOvers <- subset(leftOvers, leftOvers$value > highestSeen)
  
  #if there are no other jewels high than the last highest seen
  #the chosen jewel is the last jewel
  winner <- if (nrow(leftOvers) == 0) {
    jewels[10,2]
  } else {
    leftOvers[1,2]
  }
  
  
  #print(paste('chosen number ', as.character(winner), sep = ' '))


  return(winner)
}

#simulates picking jewels multiple times
simulateMultiplePickings <- function(discardCount, iterationCount){
  return(replicate(iterationCount, simulateJewelPicking(discardCount)))
}

#run a simulation against each discard scenario the same number of times
#get back a matrix
matrixSims <- mapply(simulateMultiplePickings, iterationCount=iterations, discardCount=discards)

#turn that matrix into a data frame
#each column represents a discard scenario
dfSims <- as.data.frame(matrixSims)
colnames(dfSims) <- discards

#Melt the data frame
meltSims <- melt(dfSims)
colnames(meltSims) <- c('discard.count', 'chosen.value')
meltSims$discard.count <- paste('discard count ', meltSims$discard.count, ' ') 

head(meltSims)

myPlot <- ggplot(meltSims, aes(x=chosen.value)) +
geom_histogram(binwidth=.5, fill='dodgerblue') +
facet_wrap(~ discard.count)

myPlot

