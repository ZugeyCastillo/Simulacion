library(xts) # Generar series de tiempo
library(PerformanceAnalytics) # Calculo de retornos
library(MASS) # Para obtener la inversa de una matriz
library(timeDate) # Para identificar fines de semana
library(quantmod) # Datos desde yahoo finance
# install.packages("ggplot2")

# Cargar datos
etfs <- c('VOO','VTI','QQQ','VGT',"VNQ","VEA","VWO")  #PORTAFOLIO COMPLETO
etfs <- c('VOO','VTI','QQQ','VWO')  # Mejor media
startdate <- c('2016-05-24')
enddate <- Sys.Date()

for(i in 1:length(etfs)){
  if(i == 1){  
    datos <- getSymbols(etfs[i], src='yahoo', from = startdate, to = enddate,auto.assign = FALSE)[,4]
    }else{
      datos <- cbind(datos, getSymbols(etfs[i], src='yahoo', from = startdate, to = enddate,auto.assign = FALSE)[,4]  )
    }  
}
colnames(datos) <- etfs
nombres <- etfs
plot(datos,main="Historico de 5 a�os de precios de los ETF")

# Etapa 1: Transformación y procesado de datos
datos2 <- na.omit(Return.calculate(datos))                      #1.1: Serie de tiempo de retornos
medias <- apply(datos2,2,mean)                                  #1.2: Vector de medias (u) 
varianza <- cov(datos2)                                         #1.2: Matriz de covarianza
cholesky <- t(chol(varianza))                                   #1.2: Descomposici?n de Cholesky(L) 

# Etapa 2: Proyecciones
years <- 1                                                      #2.1: Definir los a?os a simular
simulaciones <- 100                                            #2.1: Definir el n?mero de escenarios  
d <- datos[nrow(datos),]                                        #2.1: Precios del ?ltimo d?a de la serie (d)

# Fechas para la proyección
fechas <- matrix(index(d),nrow=1,ncol=1)

for(i in 1:(252*years)){
  fecha <- as.Date( fechas[nrow(fechas),] + 1)
  while(isWeekend(fecha) == TRUE){
    fecha <- as.Date(as.numeric(fecha) + 1)
   }
  fechas <- rbind(fechas,fecha)}
fechas <- fechas[-1,]

forecasts <- list()# En la variable forecasts se guardar?n las proyecciones     
for(k in 1:simulaciones){
  forecast <- matrix(as.numeric(d),nrow=1,ncol=ncol(datos2))
  for(i in 1:(252*years)){
    randomnumbers <- as.matrix(c(rnorm(ncol(datos2),0,.5)),ncol=1,nrow= ncol(datos2) )  #2.2: Generaci?n de vector de n?meros aleatorios (x)
    v <- t(cholesky%*%randomnumbers) + medias                   #2.3: u + Lx
    forecast <- rbind(forecast,forecast[nrow(forecast),]*(v+1)) #2.4: Transfrmaci?n de retornos a precios
  }
  forecast <- forecast[-1,]
  forecasts[[k]] <- as.xts(forecast, as.Date(fechas))
}

# lots
plots <- list()
for(v in 1:ncol(datos)){
instrumento1 <- rbind(datos[,v],forecasts[[1]][,v])
for(i in 2:simulaciones){
  instrumento1 <- merge(instrumento1,rbind(datos[,v],forecasts[[i]][,v]))
}
plots[[v]] <- plot(instrumento1,main=nombres[v])
}

# Proyecciones

#Portafolio EW
w <- rep(1/length(etfs),length(etfs))

#Ejemplificar
r <- na.omit(CalculateReturns(forecasts[[1]]))
for(j in 1:ncol(r)){
  r[,j] <- r[,j]*w[j]
}
rportewdiario <- apply(r,1,sum)#1 significa fila, 2 columna

rportewdiario[1:5]*100

capitalinicial <- 1000
cd2 <- capitalinicial*(1+rportewdiario[1])
cd3 <- cd2*(1+rportewdiario[2])
cd4 <- cd3*(1+rportewdiario[3])
cd5 <- cd4*(1+rportewdiario[4])
cd6 <- cd5*(1+rportewdiario[5])
c(cd2,cd3,cd4,cd5,cd6)
ac <- 1
for(k in 1:length(rportewdiario)){
  ac <- c(ac, ac[k]*(1+ as.numeric(rportewdiario[k]) ))
}
ac <- as.xts(ac, order.by =  index(forecasts[[1]]))

capitalinicial <- 1000
for(i in 1:length(forecasts)){
  r <- na.omit(CalculateReturns(forecasts[[i]]))
  for(j in 1:ncol(r)){
    r[,j] <- r[,j]*w[j]
  }
  rportewdiario <- apply(r,1,sum)
  ac <- 1
  for(k in 1:length(rportewdiario)){
    ac <- c(ac, ac[k]*(1+ as.numeric(rportewdiario[k]) ))
  }
  ac <- as.xts(ac, order.by =  index(forecasts[[i]])     )*capitalinicial
  if(i == 1){
    PortafoliosEW <- ac
  }else{
    PortafoliosEW <- cbind(PortafoliosEW,ac)
  }
}
colnames(PortafoliosEW) <- c(1:ncol(PortafoliosEW))

plot( PortafoliosEW, main = paste0( "Portafolios EW, Media = ",  round(mean(PortafoliosEW[nrow(PortafoliosEW),]),2)  )  )

# bMean Variance Optimization

library(quadprog) #Para solver cuadr?tico
library(ggplot2)

# Como se busca un año hacia adelante, se toman los retornos anualizados
ps <- c(nrow(datos):253)
retornosanualizados <- c()
for(i in ps){
  retornosanualizados <- rbind(retornosanualizados,  (as.numeric(datos[i])/as.numeric(datos[i-252])) - 1)
}
colnames(retornosanualizados) <- etfs

# Vector de retornos
vector_de_retornos_esperados <- apply(retornosanualizados,2,mean)

#Obtener varianza de retornos
varianza_de_retornos <- var(retornosanualizados)

Rt <- seq(.01,.17,length.out=100)
for(u in 1:length(Rt)){
#Construyendo b0
suma_weights <- 1
retorno_minimo_esperado <- Rt[u]
wnn0 <- rep(0,ncol(datos))
wnn1 <- rep(1,ncol(datos))*-1

b0 <- c(suma_weights,retorno_minimo_esperado,wnn0,wnn1)

# Construyendo A
suma_weights_A <- matrix(rep(1,ncol(datos)), ncol=1)
retorno_minimo_esperado_A <- as.vector(vector_de_retornos_esperados)
wnn0A <- diag(1, ncol(datos), ncol(datos))
wnn1A <- diag(1, ncol(datos), ncol(datos))*-1

A <- cbind(suma_weights_A, retorno_minimo_esperado_A,wnn0A,wnn1A)#A

# Optimizar
qp <- solve.QP(varianza_de_retornos, vector_de_retornos_esperados, A, b0, meq=2)

if(u == 1){
ws <- qp$solution
ws[which(ws<0)] <- 0
varport <- sqrt(t(qp$solution)%*%varianza_de_retornos%*%qp$solution)
}else{
  ws <- rbind(ws,qp$solution)
  ws[which(ws<0)] <- 0
  varport <- rbind(varport, sqrt(t(qp$solution)%*%varianza_de_retornos%*%qp$solution  ))
}
}

meanvariance <- data.frame(Riesgo = varport,Retorno = Rt,W = ws)
colnames(meanvariance) <- c("Riesgo","Retorno",etfs)
ggplot(meanvariance[,c(1,2)], aes(x = Riesgo, y = Retorno)) +
  geom_point(aes(color = Riesgo), size = 1) 

# Siempre utilizar la frontera eficiente
meanvarianceeficiente <- meanvariance[c(which.min(meanvariance$Riesgo):nrow(meanvariance)),]
ggplot(meanvarianceeficiente[,c(1,2)], aes(x = Riesgo, y = Retorno)) +
  geom_point(aes(color = Riesgo), size = 1)

#Seleccionar retorno y nivel de resgo deseado

w <- as.numeric(meanvarianceeficiente[1,c(3:ncol(meanvarianceeficiente))])
names(w) <- etfs

capitalinicial <- 1000
for(i in 1:length(forecasts)){
  r <- na.omit(CalculateReturns(forecasts[[i]]))
  for(j in 1:ncol(r)){
    r[,j] <- r[,j]*w[j]
  }
  rportewdiario <- apply(r,1,sum)
  ac <- 1
  for(k in 1:length(rportewdiario)){
    ac <- c(ac, ac[k]*(1+ as.numeric(rportewdiario[k]) ))
  }
  ac <- as.xts(ac, order.by =  index(forecasts[[i]])     )*capitalinicial
  if(i == 1){
    PortafoliosMV <- ac
  }else{
    PortafoliosMV <- cbind(PortafoliosMV,ac)
  }
}
colnames(PortafoliosMV) <- c(1:ncol(PortafoliosMV))

plot( PortafoliosMV, main = paste0( "Portafolios MV, Media = ",  round(mean(PortafoliosMV[nrow(PortafoliosMV),]),2)  ),col=palette("Dark 2")  )

#HRP
getIVP <- function(covMat) {
  invDiag <- 1/diag(as.matrix(covMat))
  weights <- invDiag/sum(invDiag)
  return(weights)
}

getClusterVar <- function(covMat, cItems) {
  covMatSlice <- covMat[cItems, cItems]
  weights <- getIVP(covMatSlice)
  cVar <- t(weights) %*% as.matrix(covMatSlice) %*% weights
  return(cVar)
}

getRecBipart <- function(covMat, sortIx) {
  w <- rep(1,ncol(covMat))
  w <- recurFun(w, covMat, sortIx)
  return(w)
}

recurFun <- function(w, covMat, sortIx) {
  subIdx <- 1:trunc(length(sortIx)/2)
  cItems0 <- sortIx[subIdx]
  cItems1 <- sortIx[-subIdx]
  cVar0 <- getClusterVar(covMat, cItems0)
  cVar1 <- getClusterVar(covMat, cItems1)
  alpha <- 1 - cVar0/(cVar0 + cVar1)
  
  w[cItems0] <- w[cItems0] * as.numeric(alpha)
  w[cItems1] <- w[cItems1] * (1-as.numeric(alpha))
  
  if(length(cItems0) > 1) {
    w <- recurFun(w, covMat, cItems0)
  }
  if(length(cItems1) > 1) {
    w <- recurFun(w, covMat, cItems1)
  }
  return(w)
}

covMat <- cov(retornosanualizados)
corMat <- cor(retornosanualizados)

clustOrder <- hclust(dist(corMat), method = 'single')$order
plot(hclust(dist(corMat), method = 'single'))

w <- getRecBipart(covMat, clustOrder)
names(w) <- etfs

capitalinicial <- 1000
for(i in 1:length(forecasts)){
  
  r <- na.omit(CalculateReturns(forecasts[[i]]))
  for(j in 1:ncol(r)){
    r[,j] <- r[,j]*w[j]
  }
  rportewdiario <- apply(r,1,sum)
  
  ac <- 1
  for(k in 1:length(rportewdiario)){
    ac <- c(ac, ac[k]*(1+ as.numeric(rportewdiario[k]) ))
  }
  ac <- as.xts(ac, order.by =  index(forecasts[[i]])     )*capitalinicial
  
  if(i == 1){
    PortafoliosHRP <- ac
  }else{
    PortafoliosHRP <- cbind(PortafoliosHRP,ac)
  }
  
}
colnames(PortafoliosHRP) <- c(1:ncol(PortafoliosHRP))

plot( PortafoliosHRP, main = paste0( "Portafolios HRP, Media = ",  round(mean(PortafoliosHRP[nrow(PortafoliosHRP),]),2)  ), col=palette("Pastel 2") )
