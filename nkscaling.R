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
active.N <- c(1:N.start)

for(agent in 1:list.agents)
{
  agent.pos.list[[agent]][1] <- i.bin.to.integ(rep(sample(c(0,1),N,replace = TRUE)))
  agent.payoff.list[[agent]][1] <- fun.payoff(i.integ.to.bin(agent.pos.list[[agent]][1]),active.N)
  agent.adapt.list[[agent]][1] <- 0
  agent.K.list[[agent]][1] <- (sum(L)-N)/N
  agent.N.list[[agent]][1] <- length(active.N)
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
        agent.K.list[[agent]][t] <- (sum(L)-N)/N
        agent.N.list[[agent]][t] <- length(active.N)
      }

}

##########   (STEP 4): Process for export
##########
source(file = "export.R")



