#Functions

landscape.init <- function(K = "Kauffman Complexity", N = "Number of Elements")
{
  L=diag(x=1, N) #identity matrix
  slots = which(L==0,F, arr.ind = T)  # returns all matrix positions.  the nrow being the number of all slots; the matrix' first column ist the i th element, and the second column is the jth element 
  #define diagonal values (1 influences 1, 2 influences 2 etc. these values can not be chosen then)
  #r.choose = sample(nrow(slots),K, replace=F)  # random slot determined 
  for(filling in 1:N)
  {
    r.choose = sample(setdiff(1:N,filling),K, replace=F)
    #r.ow = slots[r.choose[filling],1] #first row element from random  slot matrix (corresponds to i-th element)
    #c.ol = slots[r.choose[filling],2] #second column element from random slot matrix (corresponds to j-th element)
    L[filling,r.choose] = 1
  }  
  return(as.matrix(L))
}

fun.payoff = function(pos = "Position", active = "Selected elements from L" )
{
  X = L
  #Mask inactive elements
  X[setdiff(1:N,active),] <- 0
  X[,setdiff(1:N,active)] <- 0
  w=vector(length=N)
  for(i in 1:N)
  {
    D_temp=D[,(i*2)+1:2-2]
    w_temp = sum((diag(D_temp[,pos+1])*X[i,]))
    if(w_temp!=0L)
    {
      set.seed(w_temp*mult/sum(X[i,]))
      w[i]=runif(1, min=0, max=1)
    }else{w[i]<-0}
  }
  return(as.integer(m*sum(w)/length(active)))
}

i.bin.to.integ = function(binary = "Binary string of length N"){Reduce(function(x,y) x*2+y, rev(binary))} #converts binary into integer (number) 
i.integ.to.bin = function(integ = "Integer between 0 and (2^N)-1"){as.integer(((intToBits(integ))[1:N]))} #converts integer into N-length binary string

i.greedy.active = function(position = "current position", active = "selected elements")
{
  tmp.string <- matrix(position,nrow = length(active),ncol = length(position),byrow = TRUE)
  for(j in 1:length(active))
  {
    tmp.string[j,active[j]] <- abs(tmp.string[j,active[j]]-1) 
  }
  tmp.string <- rbind(tmp.string,position)
  tmp.payoffs <- apply(tmp.string,1, function(y) fun.payoff(y,active))
  return(as.vector(tmp.string[which.max(tmp.payoffs),]))
}


strategy <- function(focal.agent = "focal firm", s = "strategy (closer,wider,lower)") #-0.25 tau --> the smaller the difference, the greater the prob
{
  
  get.agent.perfs <- (sapply(agent.payoff.list, function(o) o[[t]]))/mult
  f <- get.agent.perfs[focal.agent]
  differences <- get.agent.perfs - f
  differences[focal.agent] <- 100 # just set a high value so that this one won't be picked as minumum (as it is 0 (itself))
  c <- get.agent.perfs[which.min(abs(differences))]
  
  if(s == "closer")
  {
    diff <- abs(f - c)/f
    res <- pnorm(diff,0,0.11,lower.tail = F)  
  }
  if(s == "higher")
  {
    diff <- ifelse( ((f-c)/f)<0,0,(f-c)/f)
    res <- 0.5 - pnorm(diff,0,0.11,lower.tail = F)  
  }
  if(s == "lower")
  {
    diff <- ifelse( ((c-f)/f)<0,0,(c-f)/f)
    res <- 0.5 - pnorm(diff,0,0.11,lower.tail = F) }
  return(res)
}

