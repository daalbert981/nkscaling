#storage_variables

# basic variables for storage
local.peaks.performance = c()
local.peaks.sd = c()
number.local.peaks = c()
gp.performance = c()

benchmark.f.landscape = c()

#### Scalable agent-specifc storage ######
##########################################
agent.pos.list <- list()
agent.adapt.list <- list()
agent.payoff.list <- list()
agent.K.list <- list()
agent.N.list <- list()
agent.rel.perf.closest.list <- c()
agent.addition.list <- c()
agent.element.dependencies.list <- c()
agent.element.influences.list <- c()
agent.ham.closest.list <- c()

for(i in 1:list.agents)
{
  agent.pos.list <- append(agent.pos.list, list(c()))
  agent.adapt.list <- append(agent.adapt.list, list(c()))
  agent.payoff.list <- append(agent.payoff.list, list(c()))
  agent.K.list <- append(agent.K.list, list(c()))
  agent.N.list <- append(agent.N.list, list(c()))
  agent.rel.perf.closest.list <- append(agent.rel.perf.closest.list, list(c()))
  agent.ham.closest.list <- append(agent.ham.closest.list, list(c()))
  agent.addition.list <- append(agent.addition.list, list(c()))
  agent.element.dependencies.list <- append(agent.element.dependencies.list, list(c()))
  agent.element.influences.list <- append(agent.element.influences.list, list(c()))
}
##########################################

