##########################################################################################################
##### Dynamic Scale NK Landscape #########################################################################
##### R-Code by Daniel Albert (based on contribution-value-engine by Marting Ganco) ######################
version = 0.01
##########################################################################################################
args = commandArgs(trailingOnly = TRUE)   #Cluster Computing related command
iter = as.integer(args[1])                #Cluster Computing related command
##########################################################################################################

##### Read in files
source(file = "input_values.R")
source(file = "storage_variables.R")
source(file = "functions.R")


####### SIMULATION BEGINS ################################################################################

##########   (STEP 1): Landscape initialization
##########
L <- landscape.init(K,N) #initialize full scale (N) Landscape (influence matrix)
D <- matrix(runif(2*N^2), ncol=N*2, nrow=N) #reference values for contribution value generation

##########   (STEP 2): Position seeding for period = 1 (no search in t = 1)
##########


for(agent in 1:list.agents)
{
  agent.pos.list[[agent]][1] <- i.bin.to.integ(rep(sample(c(0,1),N,replace = TRUE)))
  agent.payoff.list[[agent]][1] <- fun.payoff(i.integ.to.bin(agent.pos.list[[agent]][1]),active.N)
  agent.adapt.list[[agent]][1] <- 0
  agent.K.list[[agent]][1] <- (sum(L[active.N,active.N])-length(active.N))/length(active.N)
  agent.N.list[[agent]][1] <- length(active.N)
}

for(agent in 1:list.agents)
{
agent.rel.perf.closest.list[[agent]][1] <- agent.payoff.list[[agent]][1]/max(sapply(agent.payoff.list[-agent], function(z) z[[1]]))
agent.addition.list[[agent]][1] <- 0
}


##########   (STEP 3): Time Loop for agent search (t = 2 to T) 
##########
for(t in 2:T)
{
  for(agent in 1:list.agents)
      {
        agent.pos.list[[agent]][t] <- i.bin.to.integ(i.greedy.active(i.integ.to.bin(agent.pos.list[[agent]][t-1]),active.N))
        agent.payoff.list[[agent]][t] <- fun.payoff(i.integ.to.bin(agent.pos.list[[agent]][t]),active.N)
        agent.adapt.list[[agent]][t] <- ifelse(all(agent.pos.list[[agent]][t-1] == agent.pos.list[[agent]][t]),0,1)
      }
  
  #### innovation stage #####
  for(agent in 1:list.agents)
  {
    set.seed(sample.seeds[length(sample.seeds)])
    sample.seeds <- sample.seeds[-length(sample.seeds)]
    agent.rel.perf.closest.list[[agent]][t] <- agent.payoff.list[[agent]][t]/max(sapply(agent.payoff.list[-agent], function(z) z[[t]]))
    if(length(active.N) < N)
    {
      agent.addition.list[[agent]][t] <- sample(c(0,1), 1, replace = TRUE, prob = c((1 - strategy(agent,'closer')), strategy(agent,'closer')))
      active.N <- c(1:(length(active.N)+agent.addition.list[[agent]][t]))  
    }else{agent.addition.list[[agent]][t] <- 0}
    
  }
  #new.N.prime <- max(active.N) + sum(sapply(agent.addition.list, function(x) x[[t]]))
  #active.N <- c(1:new.N.prime)

  for(agent in 1:list.agents)
  {
    agent.K.list[[agent]][t] <- (sum(L[active.N,active.N])-length(active.N))/length(active.N)
    agent.N.list[[agent]][t] <- length(active.N)
    
  }
}


##########   (STEP 4): Process for export
##########
source(file = "export.R")



