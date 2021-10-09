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