#multi armed bandit problem - Had its orgins in Game theory and is also used in reinforcement learning to demonstrate exploittation for maximum rewards.

dataset = read.csv('Ads_Optimisation.csv')
dataset=do.call(rbind, lapply(dataset, as.numeric))
# Implementing Random Selection

N = 10000
d = 10
ads_selected = list()
j=0
total_reward = 0
for (i in 1:N){
    ad = sample(1:d, 1, replace=F)
    ads_selected[j]=ad
    j=j+1
    reward = dataset[ad,i]
    total_reward = total_reward + reward
}

###upper confidence bound algorithm
#Explore actions which are more uncertain, exploit actions with high average rewards obtained so far
ads_selected_ucb = list()
numbers_of_selections = as.list(rep(0,d))
sums_of_reward = as.list(rep(0,d))
total_reward_ucb = 0

for (n in 1:N){
  ad = 0
max_upper_bound = 0
for (i in 1:d){
  if (numbers_of_selections[[i]] > 0){
  average_reward = sums_of_reward[[i]] / numbers_of_selections[[i]]
  delta_i = sqrt(2 * log(n+1) / numbers_of_selections[[i]])
  upper_bound = average_reward + delta_i
  }
  else
    upper_bound = 1e400
if (upper_bound > max_upper_bound)
{  max_upper_bound = upper_bound
  ad = i}
}
ads_selected_ucb[[n]]=ad
numbers_of_selections[[ad]]=numbers_of_selections[[ad]]+ 1
reward = dataset[ ad,n]
sums_of_reward[[ad]] =sums_of_reward[[ad]]+ reward
total_reward_ucb=total_reward_ucb+ reward

}


    
