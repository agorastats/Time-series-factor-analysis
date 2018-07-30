library(tsfa)
library(nFactors)
library(vars)
library(readr)
library(foreach)
library(doParallel)
library(lubridate)
library(forecast)

load("festivos_2017.RData")
load("tabla_festivos.RData")
superm<-read_delim("Quarter Hourly Electrical Energy_01-01-2017_31-12-2017.csv", 
                   ";", escape_double = FALSE, trim_ws = TRUE)

nf = 5

param = 310
dif = 20

datos=matrix(superm$`General [kWh]`,nrow=param-dif,ncol=96, byrow = T)
validat=matrix(superm$`General [kWh]`[-c(1:((param-dif)*96))],nrow=dif,ncol=96, byrow = T)

##domingos y festivos
valdom=ifelse(weekdays(dmy(superm$Date))=="domingo",1,0)

datdom=matrix(valdom,nrow = param-dif,ncol = 96,byrow = T)[,1]
preddom=matrix(valdom[-c(1:((param-dif)*96))],nrow=dif,ncol=96, byrow = T)[,1]

##festivos y fiestas navidad y sem santa
festiusdat=matrix(news$festivo,nrow = param-dif,ncol = 96,byrow = T)[,1]
festiuspred=matrix(news$festivo[-c(1:((param-dif)*96))],nrow=dif,ncol=96, byrow = T)[,1]

hwfiesdat=matrix(news$holw,nrow=param-dif,ncol=96, byrow = T)[,1]
hwfiespred=matrix(news$holw[-c(1:((param-dif)*96))],nrow=dif,ncol=96, byrow = T)[,1]



chosing <-function(ch=0){
    ## function bucle
  ## Iniciaremos la función dando peso nulo al ETS y valor Maximo al ARIMA.
  coefic_arima = 1
  coefic_ets = 0
  value_pred = Inf
  errors = vector()
  for(k in 1:11){
    j<-1
    datos2 <- datos
    datdom2<-datdom
    fest2 <- festiusdat
    nad2 <- hwfiesdat
    p<-vector()
    factorspred <- matrix(nrow = nf,ncol = nrow(validat))
    resppred <- matrix(nrow = nrow(validat),ncol = 96)
    resppredarima <- matrix(nrow = nrow(validat),ncol = 96)
    for(i in nrow(datos):(param-1)){
      
      factores<-estTSF.ML(datos2,nf,normalize=F,maxit=30000)
      fmodel<-factors(factores)
      serief1ts <- ts(fmodel)

      ##Autoarima with also three covariables auxiliary. And ets with trend of 0.2
      prediccarima <- vector()
      for(t in 1:ncol(serief1ts)){
        if(ch == 0){
          vararima <- auto.arima(serief1ts[,t],max.p = 7,max.q = 0,stationary = TRUE)
          ab<-predict(vararima)
        }else if(ch == 1){
          vararima <- auto.arima(serief1ts[,t],max.p = 7,max.q = 0,stationary = TRUE,xreg = cbind(dat=datdom2,fest=fest2,nad=nad2))
          ab<-predict(vararima,newxreg = cbind(preddom[j],festiuspred[j],hwfiespred[j]))
        }
        
        varets <- as.vector(forecast(ets(serief1ts[,t],model='AAN'),h = 1)[2]$mean)
        #prediccarima <- c(prediccarima,ab$pred)
        prediccarima <- c(prediccarima,c(coefic_arima*ab$pred+coefic_ets*varets))
      }
      
      resppredarima[j,]<-t(factores$loadings%*%prediccarima)
      
      
      datos2 <- rbind(datos2,validat[j,])
      datdom2 <- c(datdom2,preddom[j])
      fest2 <- c(fest2,festiuspred[j])
      nad2 <- c(nad2,hwfiespred[j])
      # print(j)
      # print(nrow(datos2))
      # print(i)
    j = j + 1 
    }
    # print(Metrics::rmse(as.vector(t(validat)),as.vector(t(resppredarima))))
    val=Metrics::rmse(as.vector(t(validat)),as.vector(t(resppredarima)))
    #print(val)
    errors = c(errors,val)
    if(val < value_pred){
      value_pred = val
      best_conf = c(coefic_ets,coefic_arima)
      cat("Best conf is for coef_ets & coef_arima:\n")
      print(best_conf)
      cat("Value for RMSE is then:\n")
      print(value_pred)
    }
    coefic_ets = coefic_ets + 0.1
    coefic_arima = coefic_arima - 0.1
  }
  return(list(best_value=value_pred,best_selec=best_conf,errores=errors))
}

arima_select = chosing()

# arima_select = chosing(ch=1)
