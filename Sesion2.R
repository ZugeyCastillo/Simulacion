
# Sesión 2
x <- runif(20000)

mean(4*(x**3))
mean(exp(x))


y <- runif(20000)
mean(exp((4*y)-2)*4)


resultado <- c()
for(k in 1:10){

y <- runif(20000)
x <- (2/(1-y))-1
resultado <- c(resultado,mean((x*((1+(x**2))**-2))*(2/((1-y)**2))))

}
