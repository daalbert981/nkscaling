args = commandArgs(trailingOnly = TRUE)   #Cluster Computing related command
iter = as.integer(args[1])                #Cluster Computing related command
version = 0.01


source(file = "input_values.R")
source(file = "storage_variables.R")
source(file = "functions.R")


L <- landscape.init(K,N) #initialize full scale Landscape (influence matrix)
set.seed(1.248933202425107)
D = matrix(runif(2*N^2), ncol=N*2, nrow=N) #reference values for seed generation

#define active elements N.prime
active.N <- c(1:N.start) #track which N elements are active

position.string <- rep(0,N)

for(agent in 1:list.agents)
{
  agent.pos.list[[agent]][1] <- i.bin.to.integ(rep(sample(c(0,1),N,replace = TRUE)))
  agent.payoff.list[[agent]][1] <- fun.payoff(i.integ.to.bin(agent.pos.list[[agent]][1]),active.N)
  agent.adapt.list[[agent]][1] <- 0
}


for(t in 2:T)
{
  for(agent in 1:list.agents)
      {
        agent.pos.list[[agent]][t] <- i.bin.to.integ(i.greedy.active(i.integ.to.bin(agent.pos.list[[agent]][t-1]),active.N))
        agent.payoff.list[[agent]][t] <- fun.payoff(i.integ.to.bin(agent.pos.list[[agent]][t]),active.N)
        agent.adapt.list[[agent]][t] <- ifelse(all(agent.pos.list[[agent]][t-1] == agent.pos.list[[agent]][t]),0,1)
      }

}


raw.data <- as.data.frame(cbind(unlist(agent.pos.list),unlist(agent.adapt.list),unlist(agent.payoff.list)))


#write.csv(results, file = paste("ganco_",version ,"_it_", iter, ".csv",sep=""))
