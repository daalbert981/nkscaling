## Input Values

N=30
N.start=10
K=10
mult=1e+09
#mult=1000000000
m=mult
T=50
add.N.prob <- 0.1
S=3 #how many different N additions sampled ?

search <- "greedy"  #set to "random" or "greedy"
record.landscape <- 0 # 0 = No, 1 = yes
agents.differ = "yes"


list.agents=2
agent.capability <- c(1,0)
active.N <- c(1:N.start)
sample.seeds <- sample(runif((T-1)*list.agents*2, 1,100))

#Landscape stat blanks
global.peak = 0
local.peaks.number = 0
lp_avg = 0
lp_sd = 0