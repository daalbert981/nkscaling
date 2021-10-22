library(data.table)

df <- fread("data/nks0022.csv") 
df <- fread("data/nks0022_bench.csv")
df <- as.data.frame(df)

nks <- aggregate(df, list(df$period), function(x) mean(x))
nks <- nks[,-c(1:2)]

#write.csv(nks, file = "mean_output.csv")
timestamp()

write.csv(nks, file = "results0022_bench.csv")

library(ggplot2)

f1 <- ggplot(nks) + 
  geom_line(aes(x = period, y = agent.1.payoff, color = "Landscape Innovator")) +
  geom_line(aes(x = period, y = agent.2.payoff, color = "Landscape Follower")) +
  xlab("Period") + ylab("Performance") + 
  scale_color_manual(name = "Firm" , values = c("Landscape Innovator" = "red", "Landscape Follower" = "blue"))+
  theme_bw()
png("figures/raw_performance.png",units = "px", width = 2400, height = 1680, res = 300 )
plot(f1)
dev.off()


f2 <- ggplot(nks) + 
  geom_line(aes(x = period, y = agent.1.payoff/Global.Peak, color = "Landscape Innovator")) +
  geom_line(aes(x = period, y = agent.2.payoff/Global.Peak, color = "Landscape Follower")) +
  xlab("Period") + ylab("Performance (relative to GP)") + 
  scale_color_manual(name = "Firm" , values = c("Landscape Innovator" = "red", "Landscape Follower" = "blue"))+
  theme_bw()
png("figures/relative_peak_performance.png",units = "px", width = 2400, height = 1680, res = 300 )
plot(f2)
dev.off()


f3 <- ggplot(nks) + 
  geom_line(aes(x = period, y = agent.1.K/agent.1.N)) +
  xlab("Period") + ylab("K/N") + 
  scale_color_manual(values = c("black"))+
  theme_bw()
png("figures/K_over_N.png",units = "px", width = 2400, height = 1680, res = 300 )
plot(f3)
dev.off()

##############
##############
df$add <- 0
a <- sapply(c(1:(1000-51)), function(x) (1)+(50*x))
for(i in setdiff(1:50000,cbind(1,a)))
{
  df$add[i] <- df$agent.1.add[i] - df$agent.1.add[i-1]
}
ggplot(subset(df, add==1)) +
  geom_bar(aes(y = ..count.., x = (agent.1.newDep+agent.1.newInf), fill = agent.1.newDep>agent.1.newInf))

  
    geom_histogram(aes(x = agent.1.newDep/agent.1.N, y = stat(count)), color = "green", bins = 20, alpha = 0.2)+
  geom_histogram(aes(x = agent.1.newInf/agent.1.N, y = stat(count)), color = "red", bins = 20, alpha = 0.2)

ggplot(subset(df, add==1)) +  
geom_histogram(aes(x = agent.1.newInf, y = stat(count)), color = "red")

###############
###############

f.bench <- ggplot() + 
  geom_line(data = nks, aes(x = period, y = agent.1.payoff, color = "Landscape Innovator")) +
  geom_line(data = nks, aes(x = period, y = agent.2.payoff, color = "Landscape Follower")) +
  geom_line(data = nks2, aes(x = period, y = agent.1.payoff,color = "Redraw Benchmark"))+
  xlab("Period") + ylab("Performance") + 
  scale_color_manual(name = "Firm" , values = c("Landscape Innovator" = "red", "Landscape Follower" = "blue", "Redraw Benchmark" = "black"))+
  theme_bw()
png("figures/benchmark.png",units = "px", width = 2400, height = 1680, res = 300 )
plot(f.bench)
dev.off()

f.benchk <- ggplot() + 
  geom_line(data = nks, aes(x = period, y = agent.1.K/agent.1.N, color = "Innovator")) +
  geom_line(data = nks2, aes(x = period, y = agent.1.K/agent.1.N, color = "Redraw Benchmark")) +
  xlab("Period") + ylab("K/N") + 
  scale_color_manual(name = "", values = c("Innovator" = "blue","Redraw Benchmark" = "black"))+
  theme_bw()
png("figures/bench_K_over_N.png",units = "px", width = 2400, height = 1680, res = 300 )
plot(f.benchk)
dev.off()
