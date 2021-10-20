## Export processing
apayoffs <- c()
for(l in 1:length(agent.payoff.list)){apayoffs <- cbind(apayoffs, 'paste("ag",l)' = agent.payoff.list[[l]])}
colnames(apayoffs) <- paste('agent',1:ncol(apayoffs),'payoff', sep = ".")

aadapts <- c()
for(l in 1:length(agent.adapt.list)){aadapts <- cbind(aadapts,agent.adapt.list[[l]])}
colnames(aadapts) <- paste('agent',1:ncol(aadapts),'adapt', sep = ".")

aN <-c()
for(l in 1:length(agent.N.list)){aN <- cbind(aN,agent.N.list[[l]])}
colnames(aN) <- paste('agent',1:ncol(aN),'N', sep = ".")

aK <-c()
for(l in 1:length(agent.K.list)){aK <- cbind(aK,agent.K.list[[l]])}
colnames(aK) <- paste('agent',1:ncol(aK),'K', sep = ".")

aadd <- c()
for(l in 1:length(agent.addition.list)){aadd <- cbind(aadd,agent.addition.list[[l]])}
colnames(aadd) <- paste('agent',1:ncol(aadd),'add', sep = ".")

period <- c(1:T)

raw.data <- as.data.frame(cbind(N,N.start,K.start = K,period,aN,aK,aadd,apayoffs/mult,aadapts, 
                                Global.Peak = gp.performance/mult, Local.Peak.Avg = lp_avg/mult, Local.Peak.SD = lp_sd/mult, Number.Local.Peaks = number.local.peaks))

write.csv(raw.data, file = paste("nkscaling_",version ,"_it_", iter, ".csv",sep=""))