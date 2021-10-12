#storage_variables

# basic variables for storage
local_peaks_payoff =c()
local_peaks_sd = c()
local_peaks_number = c()
global.peak = c()

#### Scalable agent-specifc storage ######
##########################################
agent.pos.list <- list()
agent.adapt.list <- list()
agent.payoff.list <- list()
agent.K.list <- list()
agent.N.list <- list()
agent.rel.perf.closest.list <- c()
agent.addition.list <- c()

for(i in 1:list.agents)
{
  agent.pos.list <- append(agent.pos.list, list(c()))
  agent.adapt.list <- append(agent.adapt.list, list(c()))
  agent.payoff.list <- append(agent.payoff.list, list(c()))
  agent.K.list <- append(agent.K.list, list(c()))
  agent.N.list <- append(agent.N.list, list(c()))
  agent.rel.perf.closest.list <- append(agent.rel.perf.closest.list, list(c()))
  agent.addition.list <- append(agent.addition.list, list(c()))
}
##########################################

