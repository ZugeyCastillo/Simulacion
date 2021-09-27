
# Generar números pseudoaleatorios

# Un solo numero -----

a <- 7**5
m <- (2**31) - 1    
semilla <- 1000

xn <- (a*semilla) %% m

u <- xn/m

# -----

# Varios números -----

a <- 7**5
m <- (2**31) - 1    
semilla <- 1000
n <- 1000
  
xn <- c(semilla)
for( i in 1:n){
  xn <- c(xn, (a*xn[i]) %% m   )
}  
xn <- xn[-1]  

u <- xn/m


# -----

# Varios números en una función -----

random <- function(x,y){

a <- 7**5
m <- (2**31) - 1    
semilla <- y
n <- x

xn <- c(semilla)
for( i in 1:n){
  xn <- c(xn, (a*xn[i]) %% m   )
}  
xn <- xn[-1]  

u <- xn/m

return(u)

}

u <- runif(1)

# -----



