
xi <- c(-3,4,34,121,1000,0)
# M�todo de aceptaci�n y rechazo----

n <- 1000
x <- c(1,2,3,4,5,6)
pj <- c(.2,.1,.2,.15,.3,.05)

c <- max(pj/(1/length(x)))
qj <- (1/length(x))

xf <- c()
for(i in 1:n){

se�al <- 0
while(se�al == 0){
Y <- floor(length(x)*runif(1))+1
u <- runif(1)

if(u <  (pj[Y]/(c*qj))  ){
  X <- Y
  se�al <- 1
}
}
xf <- c(xf,X)
}

# ----



