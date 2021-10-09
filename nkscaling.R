args = commandArgs(trailingOnly = TRUE)   #Cluster Computing related command
iter = as.integer(args[1])                #Cluster Computing related command
version = 0.02


source(file = "input_values.R")
source(file = "storage_variables.R")
source(file = "functions.R")


L <- landscape.init(K,N) #initialize full scale Landscape (influence matrix)
set.seed(1.248933202425107)
D = matrix(runif(2*N^2), ncol=N*2, nrow=N) #reference values for seed generation

#define active elements N.prime
active.N <- c(1:N.prime) #track which N elements are active

position.string <- rep(0,N)


for(t in 1:T)
{
  #agent.a
  #agent.b 
  
}





#write.csv(results, file = paste("ganco_",version ,"_it_", iter, ".csv",sep=""))
