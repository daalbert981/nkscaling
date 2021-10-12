## Input Values

N=30
N.start=10
K=10
mult=1000000000
m=mult
T=50
add.N.prob <- 0.1

list.agents=2
active.N <- c(1:N.start)
sample.seeds <- sample(runif((T-1)*list.agents, 1,100))
