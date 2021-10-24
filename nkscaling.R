##########################################################################################################
##### Dynamic Scale NK Landscape #########################################################################
##### R-Code by Daniel Albert (based on contribution-value-engine by Marting Ganco) ######################
version = 0.042
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

t = 1

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
agent.element.dependencies.list[[agent]][1] <- 0
agent.element.influences.list[[agent]][1] <- 0
agent.ham.closest.list[[agent]][1] <- ham.closest(agent,active.N)
}

########## Landscape Stats
if(record.landscape==1)
{
  source("landscape_stats.R")  
}


local.peaks.performance[1] = lp_avg
local.peaks.sd[1] = lp_sd
number.local.peaks[1] = local.peaks.number
gp.performance[1] = global.peak

##########   (STEP 3): Time Loop for agent search (t = 2 to T) 
##########
for(t in 2:T)
{
  for(agent in 1:list.agents)
      {
    if(search == "greedy")
    {
      agent.pos.list[[agent]][t] <- i.bin.to.integ(i.greedy.active(i.integ.to.bin(agent.pos.list[[agent]][t-1]),active.N))  
    }else{
      set.seed(sample.seeds[length(sample.seeds)])
      sample.seeds <- sample.seeds[-length(sample.seeds)]
      agent.pos.list[[agent]][t] <- i.bin.to.integ(i.rand.local(i.integ.to.bin(agent.pos.list[[agent]][t-1]),active.N))
      }

    agent.payoff.list[[agent]][t] <- fun.payoff(i.integ.to.bin(agent.pos.list[[agent]][t]),active.N)
    agent.adapt.list[[agent]][t] <- ifelse(all(agent.pos.list[[agent]][t-1] == agent.pos.list[[agent]][t]),agent.adapt.list[[agent]][t-1],agent.adapt.list[[agent]][t-1]+1)
    
    }
  
  #### innovation stage #####
  for(agent in 1:list.agents)
  {
    print(agent)
    agent.rel.perf.closest.list[[agent]][t] <- agent.payoff.list[[agent]][t]/max(sapply(agent.payoff.list[-agent], function(z) z[[t]]))
    print("clear 01")
    set.seed(sample.seeds[length(sample.seeds)])
    sample.seeds <- sample.seeds[-length(sample.seeds)]
    print("clear 02")
    if(length(active.N) < N & agent.capability[agent]==1)
    {
      print("clear 03")
      agent.addition.list[[agent]][t] <- agent.addition.list[[agent]][t-1] + sample(c(0,1), 1, replace = TRUE, prob = c((1 - add.N.prob), add.N.prob))
      print("clear 04")
      if(agent.addition.list[[agent]][t] != agent.addition.list[[agent]][t-1])
      {
        print("clear 05")
        n.new <- N.sample(agent.pos.list[[agent]][t], S)  
        print("clear 06")
        agent.element.dependencies.list[[agent]][t] <- sum(L[n.new,active.N])
        agent.element.influences.list[[agent]][t] <- sum(L[active.N,n.new])
        print("clear 07")
        active.N <- c(active.N,n.new)
        active.N <- sort(active.N, decreasing = F)
        print("clear 08")
        ########## Landscape Stats
        if(record.landscape==1)
        {
          source("landscape_stats.R")  
        }
      }else{
        agent.element.dependencies.list[[agent]][t] <- 0
        agent.element.influences.list[[agent]][t] <- 0
      }
      print("clear 09")
    }else{ 
      print("clear 09.1")
    agent.addition.list[[agent]][t] <- agent.addition.list[[agent]][t-1]
    agent.element.dependencies.list[[agent]][t] <- 0
    agent.element.influences.list[[agent]][t] <- 0
    }
    print("clear 10")
  }
  
  local.peaks.performance[t] = lp_avg
  local.peaks.sd[t] = lp_sd
  number.local.peaks[t] = local.peaks.number
  gp.performance[t] = global.peak
  print("clear 11")
  #wrap up info about this period
  for(agent in 1:list.agents)
  {
    print("clear 12")
    agent.K.list[[agent]][t] <- (sum(L[active.N,active.N])-length(active.N))/length(active.N)
    agent.N.list[[agent]][t] <- length(active.N)
    agent.ham.closest.list[[agent]][t] <- ham.closest(agent,active.N)
    print("clear 13")
  }
  print("clear 14")
}


##########   (STEP 4): Process for export
##########
source(file = "export.R")



