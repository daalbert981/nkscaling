library(data.table)

df <- fread("data/nks_01.csv") 
df <- as.data.frame(df)

nks <- aggregate(df, list(df$period), function(x) mean(x))
nks <- nks[,-c(1:2)]

#write.csv(nks, file = "mean_output.csv")
timestamp()
