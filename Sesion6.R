



#va normal est�ndar por aceptaci�n y rechazo----

n <- 1000
xf <- c()
for(i in 1:n){
se�al <- 0
while(se�al == 0){
Y1 <- -log(runif(1))
Y2 <- -log(runif(1))

if( Y2 >= ((Y1-1)**2)/2   ){
  x <- Y1
  se�al <- 1
}
}

u <- runif(1)
if( u <= .5){
  x <- -x
}

xf <- c(xf,x)

}


media <- 10
sd <- 2

x <- (xf*sd)+media

#----