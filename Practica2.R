# Práctica 2

# Ej 1



load("College4.RData")
datos<-College4[,-1]
attach(datos)

head(datos,n=3)
dim(datos)

# Separamos muestra entrenamiento y de test
set.seed(40)
nobs <- nrow(datos)
itrain <- sample(nobs, 0.8 * nobs)
train <- datos[itrain, ]
test <- datos[-itrain, ]

# Variable respuesta Accept
# Variables explicativas el resto (no consideramos Private)

x<-as.matrix(train[,-2])
y<-train$Accept


#a)
#install.packages("glmnet")
library(glmnet)

set.seed(40)
cv.lasso<-cv.glmnet(x,y,alpha=1)
plot(cv.lasso)

# Seleccionando lambda mediante la regla de "un error estándar"
# de Breiman et al. (1984)

cv.lasso$lambda.1se


# b)

coef(cv.lasso,s="lambda.1se")

newx<-as.matrix(test[,-2])
pred_lasso<-predict(cv.lasso,newx=newx,s="lambda.1se")

obs<-test$Accept
plot(pred_lasso, obs, main = "Observado frente a predicciones",
     xlab = "Predicción", ylab = "Observado")
abline(a=0,b=1)
res <- lm(obs ~ pred_lasso)
abline(res, lty = 2,col="red")
legend(x="topleft",legend = "lasso",col="red",lty = 1)


# Medidas de error:

accuracy <- function(pred, obs, na.rm = FALSE,
                     tol = sqrt(.Machine$double.eps)) {
  err <- obs - pred # Errores
  if(na.rm) {
    is.a <- !is.na(err)
    err <- err[is.a]
    obs <- obs[is.a]
  }
  perr <- 100*err/pmax(obs, tol) # Errores porcentuales
  return(c(
    me = mean(err), # Error medio
    rmse = sqrt(mean(err^2)), # Raíz del error cuadrático medio
    mae = mean(abs(err)), # Error absoluto medio
    mpe = mean(perr), # Error porcentual medio
    mape = mean(abs(perr)), # Error porcentual absoluto medio
    r.squared = 1 - sum(err^2)/sum((obs - mean(obs))^2) # Pseudo R-cuadrado
  ))
}

accuracy(pred_lasso,obs)

# c)

cv.lasso$lambda.min

coef(cv.lasso,s="lambda.min")



# Ej 2

# a)

# Ajustar un modelo mediante regresión spline adaptativa multivariante (MARS)
# empleando el método "earth" del paquete caret.

library(caret)

# Validacion cruzada con 5 grupos para seleccionar los valores optimos
# de los hiperparametros considerando degree=1 y nprune=c(5,10,15,20) y 
# fijar nk=30

tuneGrid<-expand.grid(degree=1,nprune=c(5,10,15,20))

set.seed(40)
caret.mars=train(Accept~., data=train,method="earth",nk=30,
                 trControl=trainControl(method="cv", number=5),
                 tuneGrid=tuneGrid)
caret.mars

final.model<-caret.mars$finalModel
caret.mars$bestTune

# b)

# Estudiar el efecto de los predictores incluidos en el modelo final
# y obtener medidas de su importancia

summary(final.model)
library(ggplot2)
ggplot(caret.mars)

# Para representar los efectos de las variable

library(plotmo)
plotmo(final.model)

# Importancia de las variables

varimp<-evimp(final.model)
varimp
plot(varimp)

# c)
# Evaluar las predicciones en la muestra de test

obs<-test$Accept
pred_mars<-predict(caret.mars,newdata=test)
accuracy(pred_mars,obs)

obs<-test$Accept
plot(pred_mars, obs, main = "Observado frente a predicciones",
     xlab = "Predicción", ylab = "Observado")
abline(a=0,b=1)
res <- lm(obs ~ pred_mars)
abline(res, lty = 2,col="green")
legend(x="topleft",legend = "mars",col="green",lty = 1)

# Ej 3

library(mgcv)

# a)

set.seed(40)
gam<-gam(Accept~s(Apps)+s(Enroll)+s(Top10perc)+s(Outstate)+s(Grad.Rate),data=train,select=T)
summary(gam)

# b)

obs<-test$Accept
pred_gam<-predict(gam,newdata=test)
accuracy(pred_gam,obs)

plot(pred_gam, obs, xlab = "Predicción", ylab = "Observado")
abline(a = 0, b = 1)
res_gam <- lm(obs ~ pred_gam)
abline(res_gam, lty = 2,col="blue")
res_lasso <- lm(obs ~ pred_lasso)
abline(res_lasso, lty = 2,col="red")
res_mars <- lm(obs ~ pred_mars)
abline(res_mars, lty = 2,col="green")
legend(x="topleft",legend = c("gam","lasso","mars"),
       col=c("blue","red","green"),lty = c(1,1,1))