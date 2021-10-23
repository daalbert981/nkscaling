## Input Values

N=22
N.start=10
K=7
mult=1000000000
m=mult
T=50
add.N.prob <- 0.1
S=3 #how many different N additions sampled ?

search <- "random"  #set to "random" or "greedy"

list.agents=2
agent.capability <- c(1,0)
active.N <- c(1:N.start)
sample.seeds <- sample(runif((T-1)*list.agents, 1,100))
