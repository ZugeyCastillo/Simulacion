

# VA geométrica----

p <- .7
q <- 1-p

n <- 1000

x <- floor((log(runif(n))/log(q))) + 1


# ----


# VA Poisson----

n <- 1000
lambda <- 4

xf <- c()

for(i in 1:n){
  
u <- runif(1)

i <- 0
p <- exp(-lambda)
FF <- p
señal <- 0

while(señal == 0){
  
  if(u < FF){
    
    x <- i
    señal <- 1
    
  }else{
    
    p <- (lambda/(i+1))*p
    FF <- FF + p
    i <- i+1
    
  }
  
  
}


xf <- c(xf,x)

}




# ----


# VA Binomial----

n <- 1000
p <- .5
nb <- 40
c <- p/(1-p)

xf <- c()

for(i in 1:n){
  
  u <- runif(1)
  
  i <- 0
  pr <- (1-p)**nb
  FF <- pr
  señal <- 0
  
  
  while(señal == 0){
    
    if(u < FF){
      
      x <- i
      señal <- 1
      
    }else{
      
      pr <- c*((nb-i)/(i+1))*pr
      FF <- FF + pr
      i <- i+1
      
    }
    
    
  }
  
  
  xf <- c(xf,x)
  
}




# ----

