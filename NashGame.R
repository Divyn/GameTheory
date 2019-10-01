#Nash game
#you get to pick a number between 2 and 100, it has to be an integer.
#Okay, so I should be trying to name 2/3 of what I think the number is going to be.
#why 2 and 100? minimum 2/3 value I can guess is 1, so the original number has to start from 1.5
library(ggplot2)
no_of_people=50
no_iters=1000
run.others<-function(no.people){
  
  so_far=rep(0,no.people)
  person_chose=rep(0,no.people)
  for(i in 1:no.people)
   {
   person_chose[i]==sample(2:100,size=1,replace=FALSE )
   
    my_number=sample(1:100,size=1,replace=FALSE)
    two_thirds=(2*person_chose[i])/3
    if(two_thirds==my_number)
      so_far[i]=1
   }


return(so_far)
}

#i am given a certain number of tries to guess the number
run.others.with_tries=function(no.people){
  so_far=rep(0,no.people)
  person_chose=rep(0,no.people)
  person_tries=rep(0,no.people)
  for(i in 1:no.people)
   {
  			tries = 1
  			person_chose[i]==sample(2:100,size=1,replace=FALSE)
  			while(tries < 20) {
          
        
           my_number=sample(1:100,size=1,replace=FALSE)
           two_thirds=(2*person_chose[i])/3
           if(two_thirds==my_number){
             so_far[i]=1
             break
           }
        tries=tries+1

  			}
        person_tries[i]=tries


  }
  return(so_far)
}

run.others.dynamic_count<-function(){
  #cur_avg=0
  so_far=rep(0,100)
  for(j in 1:100){
  for(i in 1:j)
   {
   person_chose=sample(2:100,size=1,replace=FALSE )
   
    my_number=sample(1:100,size=1,replace=FALSE)
    two_thirds=(2*person_chose)/3
    if(two_thirds==my_number){
      so_far[j]=so_far[j]+1
      break
   }
 }
  }
  
return(so_far)
}

guessing_avg=run.others(no_of_people)
ggplot(data.frame(model=c(rep('game',no_of_people)),
                  values=guessing_avg)) +
  aes(x=values) +
  geom_histogram(position="dodge",stat="bin",binwidth=1,
                 aes(fill=as.factor(model),
                     colour=as.factor(model)),alpha=0.3) +
  geom_freqpoly(binwidth=1) +
  labs(title="Histogram of the number of times I guessed correctly",
       x="Success event",
       y="Frequency")

guessing_avg_with_tries=run.others.with_tries(no_of_people)
 ggplot(data.frame(model=c(rep('game',no_of_people)),
                         values=guessing_avg_with_tries)) +
         aes(x=values) +
         geom_histogram(position="dodge",stat="bin",binwidth=1,
                        aes(fill=as.factor(model),
                            colour=as.factor(model)),alpha=0.3) +
         geom_freqpoly(binwidth=1) +
         labs(title="Histogram of the number of times I guessed correctly with multiple tries",
              x="Success event",
              y="Frequency")

guessing_avg_no_people=run.others.dynamic_count()
               ggplot(data.frame(model=c(rep('game',no_of_people)),
                                       values=guessing_avg_no_people)) +
                       aes(x=values) +
                       geom_histogram(position="dodge",stat="bin",binwidth=1,
                                      aes(fill=as.factor(model),
                                          colour=as.factor(model)),alpha=0.3) +
                       geom_freqpoly(binwidth=1) +
                       labs(title="Histogram of the guessing correctly for different count of people",
                            x="Success event",
                            y="Frequency")

               
               
##################
