

# VA discreta con función de probabilidad P{x=i},pi----


x <- c(1,2,3,4,5,6,7)
px <- c(.2,.3,.1,.1,0,.15,.15)
n <- 10000

va <- c()
for(j in 1:n){
u <- runif(1)
acumulada <- cumsum(px)

for(i in 1:length(acumulada)){

  if( u < acumulada[i]){
    va <- c(va,x[i])
    break()
  }
  
}


}

va

# ----



# VA discreta con mismas probabilidades----


n <- 30

x <- floor(n*runif(1000))+1

x

# ----