#A game that describes the purely cooperative situation.
#this is a very simple game with no strategy
#You and I walk towards each other on the sidewalk in chennai. We can each decide whether to go to our respective left 
#or respective right. And if we pick the same side then all is good. We avoid a collision. 
#If we don't, then we do collide and that's equally bad for both of us. 

situation <- matrix(c('ok','collide','collide','ok'), ncol=2)
 colnames(situation) <- c('you-left', 'you-right')
 rownames(situation) <- c('I-left', 'I-right')
 situation.table <- as.table(situation)
View(situation)

library(ggplot2)


no.iters=1000

run.game=function(iters){
  #method 1
  so_far = rep(0,iters)
  for(i in 1:iters) {
  #We don't ask each other but we rely on our intuition and sample from ('left' , 'right') for you and me
    for_me=sample(c('left','right'),1)
    for_you=sample(c('left','right'),1)
  #sample 'l' or 'r' for me
    if(for_me==for_you)
      result=1
    else
      result=0
  #if same, add to success count
    
    so_far[i] =result 
    
  }
  #method 2
  #sample for you
  #you indicate to me
  #I choose same
  #show success 100
  return (so_far)
}

walking_on_street=run.game(no.iters)

ggplot(data.frame(model=c(rep('game',no.iters)),
                  values=walking_on_street)) +
  aes(x=values) +
  geom_histogram(position="dodge",stat="bin",binwidth=1,
                 aes(fill=as.factor(model),
                     colour=as.factor(model)),alpha=0.3) +
  geom_freqpoly(binwidth=1) + 
  labs(title="Histogram of the number of times we avoided a collision",
       x="Success event",
       y="Frequency")
