#I changed the problem found at this link a little bit.
#https://www.r-bloggers.com/100-prisoners-100-lines-of-code/
  
#PROBLEM
#There will be x students, each of them having a roll number. There are y number of boxes( y>=x) with a number in it. Every student is given a certain number(z) of tries to find the box with a value<= his/her roll number. If they get the correct number, then they can skip assignments else they would have the penalty of completing an extra assignnment. Here the success would indicate every time he/she gets the correct box. 

#Method1:
#Try opening boxes at random until he had exhausted his/her tries or found the number.

library(ggplot2)
library(gridExtra)

students=60
no.boxes=70
no.iters=1000
tries=15

run.game <- function(no.students,n.boxes,iters,maxtries) 
{
  so_far = rep(0,iters)
  # Labels for our prisoners
  students = 1:no.students
  
  for(i in 1:iters) {
    # A random permutation:
    boxes = sample(1:n.boxes,size=n.boxes,replace=FALSE)
    # Track how many "winners" we have
    foundIt = 0
    # Main loop over the prisoners
    for(st in students) {
      # create a copy of the boxes object to
      # keep an index of the opened boxes for 
      # each student
      boxes.temp <- boxes
      # Track the student's list of opened boxes
      path = c(st)
      tries = 1
      # Look first in the box that matches your own number
      
      while(tries < maxtries) { 			
        inBox = sample(boxes.temp,1)
        path = c(path, inBox) 			 			
        if(inBox <= st) { 				
          foundIt = foundIt + 1 				
          break; 	
        } else { 				
          # choose randomly among any of the remaining boxes
          # delete that box from the box pool
          boxes.temp <- boxes.temp[boxes.temp!=inBox]	
          # choose among the remaining boxes
          
        }
        
#Observations
#As you increase the no of boxes, you are giving more options for the random function to choose from, which decreases the chance of getting the right box.**
          
#In this game, with no strategy at all( choosing randomly) you cannot increase the count of people succeeding, beyond a certain number because every student moves from randomly from one box to another.**
 

        tries = tries+1 		
        }
      #print(path)
    }
    # How many students have found their number till this iteration?
    so_far[i] = foundIt
  }
  return(so_far)
}

results <- run.game(students,no.boxes,no.iters,tries)
ggplot(data.frame(model=c(rep("game",no.iters)),values=results)) +
  aes(x=values) +
  geom_density(aes(fill=as.factor(model),colour=as.factor(model)),alpha=0.3) + 
  labs(title="Density plot of number of students who get to skip assignments",
       x="Success event",
       y="Density")    


ggplot(data.frame(model=c(rep('game',no.iters)),
                  values=results)) +
  aes(x=values) +
  geom_histogram(position="dodge",stat="bin",binwidth=1,
                 aes(fill=as.factor(model),
                     colour=as.factor(model)),alpha=0.3) +
  geom_freqpoly(binwidth=1) + 
  labs(title="Histogram of the number of students who succeed in every iteration",
       x="Success event",
       y="Frequency")


