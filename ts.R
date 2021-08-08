Eforecast <- function(y,h_fore=h)
{
  ###################Functions to be used ######################################
  ##########Frequency Function###############################
  findfrequency <- function(x)
  {
    n <- length(x)
    x <- as.ts(x)
    # Remove trend from data
    x <- residuals(tslm(x ~ trend))
    # Compute spectrum by fitting ar model to largest section of x
    n.freq <- 500
    spec <- spec.ar(c(na.contiguous(x)), plot=FALSE, n.freq=n.freq)
    if(max(spec$spec)>10) # Arbitrary threshold chosen by trial and error.
    {
      period <- floor(1/spec$freq[which.max(spec$spec)] + 0.5)
      if(period==Inf) # Find next local maximum
      {
        j <- which(diff(spec$spec)>0)
        if(length(j)>0)
        {
          nextmax <- j[1] + which.max(spec$spec[(j[1]+1):n.freq])
          if(nextmax < length(spec$freq))
            period <- floor(1/spec$freq[nextmax] + 0.5)
          else
            period <- 1L
        }
        else
          period <- 1L
      }
    }
    else
      period <- 1L
    
    return(as.integer(period))
  }
  #######################Combi_holt Function#########################
  Combi_holt=function(x,h)
  {
    x=ts(x,frequency=12)
    x <- as.ts(x)
    m <- frequency(x)
    lenx <- length(x)
    l.start <- x[1]
    b.start <- x[2L] - x[1L]
    zz=function(alpha,beta,lenx,l.start,b.start,phi=1,x=x)
    {
      level <- trend <- season <- xfit <- residuals <- numeric(lenx)
      SSE <- 0
      lastlevel <- level0 <- l.start
      lasttrend <- trend0 <- b.start
      for(i in 1:(lenx))
      {
        # definel l(t-1)
        if(i>1)
          lastlevel <- level[i-1]
        #define b(t-1)
        if(i>1)
          lasttrend <- trend[i-1]
        
        xhat <- lastlevel + phi*lasttrend 
        
        xfit[i] <- xhat
        res=x[i] - xhat
        residuals[i] <- res
        SSE <- SSE + res*res
        #calculate level[i]
        level[i] <- alpha * (x[i]) + (1 - alpha)*(lastlevel + phi*lasttrend)
        trend[i] <- beta*(level[i] - lastlevel) + (1 - beta)* phi* lasttrend
        
      }
      s=list(SSE=SSE,fitted= xfit,residuals = residuals,level = c(level0,level),trend=c(trend0,trend),phi = phi)
      return(s)
    }
    r=list()
    s=1
    hh=vector()
    k=data.frame()
    m=data.frame()
    mm=data.frame()
    trend_o=vector()
    level_o=vector()
    data_param=expand.grid(data.frame(alpha=seq(0.1,0.9,0.1),id2=seq(0.1,0.9,0.1)))
    for(i in 1:nrow(data_param))
    {
      
      p=zz(alpha=data_param[i,1],beta=data_param[i,2],lenx,l.start=l.start,b.start=b.start,phi=1,x=x)
      k=rbind(k,p$fitted)
      hh=c(hh,p$SSE)
      level_o=c(level_o,p$level[length(p$level)])
      trend_o=c(trend_o,p$trend[length(p$trend)])
      m=cbind(data_param[i,1],data_param[i,2])
      mm=rbind(mm,m)
    }
    k=cbind(k,hh,mm,level_o,trend_o)
    v=k[order(hh),]
    alpha_opt=v[1,(length(v)-3)]
    beta_opt= v[1,(length(v)-2)]
    level_last=v[1,(length(v)-1)]
    trend_last=v[1,(length(v))]
    phi=1
    forecast=vector()
    level_1=vector()
    trend_1=vector()
    x_hat=vector()
    x_fit=level_last+(phi*trend_last)
    for(j in 1:h)
    {
      if(j>1)
      {
        level_last=level_1[j-1]
        trend_last=trend_1[j-1]
      }
      x_hat[j]=level_last+(phi*trend_last)
      level_1[j] <- alpha_opt * (x_hat[j]) + (1 - alpha_opt)*(level_last + phi*trend_last)
      trend_1[j] <- beta_opt*(level_1[j] - level_last) + (1 - beta_opt)* phi* trend_last
      forecast=c(forecast,x_hat[j])
    }
    d=list(forecast,alpha_opt,beta_opt)
    return(d)
  }
  #######################Combi_Arima Function#########################
  Combi_Arima=function(x,h)
  {
    data_Combi_Arima=data.frame()
    arima_SSE=vector()
    ar_data=list()
    param=expand.grid(data.frame(p=c(0:4),d=c(0:4),q=c(0:4)))
    param=as.data.frame(param)
    param_sort=param[order(param$d),]
    param_sort_1=param_sort[param_sort$d<2,]
    for(i in 1:nrow(param))
    {
      order_1=as.numeric(param_sort_1[i,])
      a_out=tryCatch({
        forecast(arima(x,order=order_1,method="ML"),h=h)},error=function(e){})
      if(!is.null(a_out))
      {
        arima_row=c(a_out$mean,sum(a_out$residuals)^2,order_1)
        data_Combi_Arima=rbind(data_Combi_Arima,arima_row)
      }
    }
    colnames(data_Combi_Arima)=c(rep("mean",h),"sse","p","d","q")
    sort_data_Combi_Arima=data_Combi_Arima[order(data_Combi_Arima$sse),]
    ar_data$mean=as.numeric(sort_data_Combi_Arima[2,(1:h)])
    ar_data$model=as.numeric(sort_data_Combi_Arima[2,((length(sort_data_Combi_Arima)-2):length(sort_data_Combi_Arima))])
    return(ar_data)
  }
  ###################moving average function ###################################
  movngavg=function(clean_data,h)
  {
    data_res=vector()
    for(i in 1:h)
    {
      data_1=clean_data[(length(clean_data)-2):length(clean_data)]
      data_avg=(0.2*data_1[1])+(0.3*data_1[2])+(0.5*data_1[3])
      data_res=c(data_res,data_avg)
      clean_data=c(data_1[2:length(data_1)],data_avg)
    }
    return(data_res)
  }
  ########Models used in the code######### 
  Overall_models="aefnstdmzp"
  a="a.arima"
  e="ets"
  f="thetam"
  n="nnetar"
  s="stl"
  t="tbats"
  d="Combi Holt"
  m="Holt winters"
  z="Combination Holt winters"
  p="Combintation ARIMA"
  lambda = NULL
  a.args = NULL
  e.args = NULL
  n.args = NULL
  s.args = NULL
  t.args = NULL
  weights = c("equal", "insample.errors", "cv.errors")
  weights="equal"
  h=h_fore
  y <- as.ts(y)
  models="aefnst"
  # Match the specified models
  expandedModels <- unique(tolower(unlist(strsplit(models, split = ""))))
  
  modelResults <- list()
  forecast_r=list()
  #######ARIMA model###############
  if(is.element("a", expandedModels)){
    if(is.null(a.args)){
      a.args <- list(lambda = lambda)
    } else if(is.null(a.args$lambda)){
      a.args$lambda <- lambda
    }
    modelResults$auto.arima <- do.call(auto.arima,c(list(y),a.args))
    forecast.arima=do.call(forecast, c(list(modelResults$auto.arima,h=h)))
    forecast_r$forecast.arima=forecast.arima$mean
  }
  
  ########ETS model###############  
  
  if(is.element("e", expandedModels)){
    if(is.null(e.args)){
      e.args <- list(lambda = lambda)
    } else if(is.null(e.args$lambda)){
      e.args$lambda <- lambda
    }
    modelResults$ets <- do.call(ets, c(list(y), e.args))
    forecast.ets=do.call(forecast, c(list(modelResults$ets,h=h)))
    forecast_r$forecast.ets=forecast.ets$mean
  }
  ########## thetam()##########
  if(is.element("f", expandedModels)){
    modelResults$thetam <- thetam(y)
    forecast.thetam=forecast(modelResults$thetam,h=h)
    forecast_r$forecast.thetam=forecast.thetam$mean
  }
  # nnetar()
  if(is.element("n", expandedModels)){
    
    if(is.null(n.args)){
      n.args <- list(lambda = lambda)
    } else if(is.null(n.args$lambda)){
      n.args$lambda <- lambda
    }
    set.seed(260)
    modelResults$nnetar <- do.call(nnetar, c(list(y), n.args))
    forecast.nnetar=do.call(forecast, c(list(modelResults$nnetar,h=h)))
    forecast_r$forecast.nnetar=forecast.nnetar$mean
  }
  #########stlm() ###########
  if(is.element("s", expandedModels)){
    
    if(is.null(s.args)){
      s.args <- list(lambda = lambda)
    } else if(is.null(s.args$lambda)){
      s.args$lambda <- lambda
    }
    modelResults$stlm <- do.call(stlm, c(list(y), s.args))
    forecast.stlm=do.call(forecast, c(list(modelResults$stlm,h=h)))
    forecast_r$forecast.stl=forecast.stlm$mean
  }
  ########## tbats()##########
  if(is.element("t", expandedModels)){
    modelResults$tbats <- do.call(tbats, c(list(y), t.args))
    forecast.tbats=do.call(forecast, c(list(modelResults$tbats,h=h)))
    forecast_r$forecast.tbats=forecast.tbats$mean
  }
  ##########Combi TS Function ############ 
  if(is.element("d", expandedModels)){
    forecast.desi_ts=Combi_holt(y,h=h)
    forecast_r$forecast.desi=unlist(as.vector(forecast.desi_ts[1]))
  }  
  ######Holt winters##############
  if(is.element("m", expandedModels)){
    modelResults$hw <- HoltWinters(y)
    forecast.hw=forecast(modelResults$hw ,h=h)
    forecast_r$forecast.hw= forecast.hw$mean
  } 
  
  ######combination Holt winter ###########
  
  if(is.element("z", expandedModels)){
    d.ts=Combi_holt(y,h=h)
    alpha_d=d.ts[2]
    beta_d=d.ts[3]
    alpha_d=as.numeric(alpha_d)
    beta_d=as.numeric(beta_d)
    modelResults$hw_dts <- HoltWinters(y,alpha=alpha_d,beta=beta_d)
    forecast.hw_dts=forecast(modelResults$hw_dts,h=h)
    forecast_r$forecast.hw_dts= forecast.hw_dts$mean
  } 
  
  ##################Combination ARIMA############## 
  if(is.element("p",expandedModels)){
    modelResults$ar_c= Combi_Arima(y,h)
    forecast_r$forecast.ar_c= modelResults$ar_c$mean
  }
  
  # Set the model weights
  includedModels <- names(modelResults)
  # Weighting methods would go here, equal weighting for now
  if(weights == "equal"){
    modelResults$weights <- rep(1 / length(expandedModels), length(expandedModels))
  } else if(weights %in% c("insample.errors", "cv.errors")){
    modelResults$weights <- rep(0, length(expandedModels))
    index <- 1
    modResults <- modelResults
    if(weights == "cv.errors"){
      #modResults <-
      for(i in expandedModels){
        if(i == "a"){
          modResults$auto.arima <- cvts(y, FUN = auto.arima,
                                        maxHorizon = cvHorizon,
                                        horizonAverage = horizonAverage,
                                        verbose = FALSE,
                                        windowSize = windowSize)
        } else if(i == "e"){
          modResults$ets <- cvts(y, FUN = ets,
                                 maxHorizon = cvHorizon,
                                 horizonAverage = horizonAverage,
                                 verbose = FALSE,
                                 windowSize = windowSize)
        } else if(i == "f"){
          modResults$thetam <- cvts(y, FUN = thetam,
                                    maxHorizon = cvHorizon,
                                    horizonAverage = horizonAverage,
                                    verbose = FALSE,
                                    windowSize = windowSize)
        } else if(i == "n"){
          modResults$nnetar <- cvts(y, FUN = nnetar,windowSize=30,maxHorizon=4)
        } else if(i == "s"){
          modResults$stlm <- cvts(y, FUN = stlm,
                                  maxHorizon = cvHorizon,
                                  horizonAverage = horizonAverage,
                                  verbose = FALSE,
                                  windowSize = windowSize)
        } else if(i == "t"){
          modResults$tbats <- cvts(y, FUN = tbats,
                                   maxHorizon = cvHorizon,
                                   horizonAverage = horizonAverage,
                                   verbose = FALSE,
                                   windowSize = windowSize)
        }
      }
    }
    # If horizonAverage == TRUE, the resulting accuracy object will have only one row
    cvHorizon <- ifelse(horizonAverage, 1, cvHorizon)
    cvHorizon <- ifelse(weights != "cv.errors", 1, cvHorizon)
    for(i in expandedModels){
      if(i == "a"){
        modelResults$weights[index] <- accuracy(modResults$auto.arima)[cvHorizon, errorMethod]
      } else if(i == "e"){
        modelResults$weights[index] <- accuracy(modResults$ets)[cvHorizon, errorMethod]
      } else if(i == "f"){
        modelResults$weights[index] <- accuracy(modResults$thetam)[cvHorizon, errorMethod]
      } else if(i == "n"){
        modelResults$weights[index] <- accuracy(modResults$nnetar)[cvHorizon, errorMethod]
      } else if(i == "s"){
        modelResults$weights[index] <- accuracy(modResults$stlm)[cvHorizon, errorMethod]
      } else if(i == "t"){
        modelResults$weights[index] <- accuracy(modResults$tbats)[cvHorizon, errorMethod]
      }
      index <- index + 1
    }
    # Scale the weights
    modelResults$weights <- (1 / modelResults$weights) / sum(1 / modelResults$weights)
  }
  
  # Check for valid weights when weights = "insample.errors" and submodels produce perfect fits
  if(is.element(NaN, modelResults$weights) & weights %in% c("insample.errors", "cv.errors")){
    warning('At least one model perfectly fit the series, so accuracy measures cannot be used for weights. Reverting to weights = "equal".')
    modelResults$weights <- rep(1/ length(includedModels), length(includedModels))
  }
  names(modelResults$weights) <- includedModels
  
  # Apply the weights to construct the fitted values
  fits <- sapply(includedModels, FUN = function(x) fitted(modelResults[[x]]))
  fitsWeightsMatrix <- matrix(rep(modelResults$weights[includedModels], times = nrow(fits)),
                              nrow = nrow(fits), byrow = TRUE)
  fits <- rowSums(fits * fitsWeightsMatrix)
  resid <- y - fits
  if (!is.null(tsp(y))){
    fits <- ts(fits)
    resid <- ts(fits)
    tsp(fits) <- tsp(resid) <- tsp(y)
  }
  class(modelResults) <- "E
  Eforecast"
  modelResults$frequency <- frequency(y)
  modelResults$x <- y
  modelResults$models <- includedModels
  modelResults$fitted <- fits
  modelResults$residuals <- resid
  f1=as.data.frame(do.call(rbind,forecast_r ))
  forecast_value = t(t(f1)*modelResults$weights)
  d=as.matrix(forecast_value)
  dd=colSums(d)
  dd=as.numeric(dd)
  if(is.null(dd))
  {
    dd=rep(y[length(y)],h)*runif(h,1,1.3)
  }
  return(dd)
}


setwd("C://Users//310284953//Documents//python")
# Outlier Function
Out_function= function(data_sku)
{
  ddd=data_sku
  ff=vector()
  ff_1=vector()
  fff=vector()
  fff_1=vector()
  f=data_sku
  f=as.integer(f)
  rr=which(f>0)
  rr_1=which(f==0)
  data_sku[is.na(data_sku)]=0
  if(length(rr_1)!=0)
  {
    f=f[rr[1]:length(f)]
  } else{
    f=f
  } 
  ut <- function(x) {m = median(x); median(x) + 3 *median(abs(x-m))}
  ut1 <- function(x) {m = median(x); median(x)-2*median(abs(x-m))}
  utf <- function(x) {m = median(x); median(x) + 2 *median(abs(x-m))}
  ut1f <- function(x) {m = median(x); median(x)-1.5*median(abs(x-m))}
  x=which(f==0)
  tt=1:length(data_sku)
  mod=supsmu(tt,data_sku)
  f[x]=mod$y[x]
  for(i in 1:(length(f)-6))
  {
    f_temp=c(f[(i):(i+2)],f[(i+4):(i+6)])
    ff[i+3]=ut(f_temp)
    ff_1[i+3]=ut1(f_temp)
    fff[i+3]=utf(f_temp)
    fff_1[i+3]=ut1f(f_temp)
  }
  for(i in 1:3)
  {
    f_temp=c(f[(i+1):(i+6)])
    ff[i]=ut(f_temp)
    ff_1[i]=ut1(f_temp)
    fff[i]=utf(f_temp)
    fff_1[i]=ut1f(f_temp)
  }
  for(i in 1:3)
  {
    f_temp=c(f[(length(f)-i):(length(f)-5-i)])
    ff[length(f)-i+1]=ut(f_temp)
    ff_1[length(f)-i+1]=ut1(f_temp)
    fff[length(f)-i+1]=utf(f_temp)
    fff_1[length(f)-i+1]=ut1f(f_temp)
  } 
  if(length(rr_1)!=0)
  {
    z1=c(rep(0,(rr[1]-1)),ff)
    z11=c(rep(0,(rr[1]-1)),ff_1)
    z1f=c(rep(0,(rr[1]-1)),fff)
    z11f=c(rep(0,(rr[1]-1)),fff_1)
  } else {
    z1=ff
    z11=ff_1
    z1f=fff
    z11f=fff_1
  }
  outliers= data_sku > z1
  outliers1=data_sku<z11
  a=which(outliers=="TRUE")
  b=which(outliers1=="TRUE")
  data_sku[a]=z1f[a]
  data_sku[b]=z11f[b]
  return(data_sku)
}


movngavg=function(clean_data,h)
{
  data_res=vector()
  for(i in 1:h)
  {
    data_1=clean_data[(length(clean_data)-2):length(clean_data)]
    data_avg=(0.2*data_1[1])+(0.3*data_1[2])+(0.5*data_1[3])
    data_res=c(data_res,data_avg)
    clean_data=c(data_1[2:length(data_1)],data_avg)
  }
  return(data_res)
}
#Execution File
library(forecast)
forecast_period = function() {
  h = readline(" Enter the forecast period ")
  h = as.numeric(h)
  return(h)
}
#Forecast Period
h = 18
###########Enter the file name
masterdataset1 = read.csv("cee_apr_ts_in.csv")

data_master_t=masterdataset1
frequency=12
vector=rep(0,25)
n=nrow(data_master_t)
df_total = data.frame()
final_df = data.frame()
start.time <- Sys.time()


#Before running the model. CHange 'h' in the future forecast.
#Before running the model. CHange 'h' in the future forecast.
for(r in 1:n)
{
  data_sku=as.numeric(data_master_t[r,(2:(ncol(data_master_t)))])
  sum_data_sku=sum(data_sku)
  if(sum_data_sku!=0)
  {
    data_sku[is.na(data_sku)]=0
    data_sku[data_sku<0] = 0
    #####Outlier 
    a=which(data_sku>0)
    b=a[1]
    data_1=data_sku[(b):length(data_sku)]
    data_1=as.numeric(data_1)
    data_1[data_1<0]=0
    d_v=c(1:length(data_1))
    dd=supsmu(d_v,data_1)
    z_m=length(data_1[data_1==0])
    Z_m_allowed=floor((0.3*length(data_1)))
    if(z_m<=Z_m_allowed)
    {
      data_1[data_1==0]=dd$y[data_1==0]
    }
    data_sku=data_1
    data_sku[data_sku<0]=0
    forecast_key = data_master_t[r,1]
    forecast_key = as.data.frame(forecast_key)
    len=length(data_sku)
    if(len>9 & z_m<=Z_m_allowed)
    {
      tryCatch({
        data_sku=Out_function(data_sku)}, error=function(e){})
    }
    #############Outlier end###############
    set.seed(580)
    clean_data=ts(data_sku,frequency=12)
    if(length(clean_data)>7)
    {
      data_result=tryCatch({
        Eforecast(clean_data,h)},error=function(e){})
    } else if(length(clean_data)<4 & length(clean_data)>=7)
    {
      data_result=movngavg(clean_data,h)
    } else {
      data_result=(rep(clean_data[length(clean_data)],h))*runif(h,1,1.2)
    }
    
    data_result = as.numeric(data_result)
    sm_data=sum(data_result)
    if(is.null(data_result) | sm_data==0)
    {
      data_result=rep(clean_data[length(clean_data)],h)*runif(h,1,1.3)
    }
    #Mention to be forecasted period h="n"
    data_result = as.matrix(data_result)
    data_result[data_result<0]=0
    data_result = t(data_result)
  } else 
  {
    data_result=rep(0,h)
    data_result=t(data_result)
  }
  data_result=data_result*runif(length(data_result),1,1.04)
  # p_allowed=(length(data_sku))*0.3
  #p_zero=(data_sku[data_sku==0])
  #if(length(data_sku)>=24 & p_allowed>=p_zero)
  #{
  # season_data=season_calculator(data_sku)
  #season_data_full=rep(season_data,8)
  #} else 
  #{
  # ro=rep(1,12)
  # season_data_full=rep(ro,8)
  # }
  #season_req=season_data_full[1:(length(data_result))]
  #data_result_ses=data_result*season_req
  data_f = cbind(forecast_key, data_result)
  #data_f_ses=cbind(forecast_key, data_result_ses)
  final_df = rbind(final_df,data_f)
  #final_df_ses=rbind(final_df_ses,data_f_ses)
  end.time1 <- Sys.time()
  time.taken <- end.time1 - start.time
  cat(paste0(round(r / n * 100), '% completed'))
  time.taken
  cat("\n")
  cat("\n")
  cat("\n")
  cat("\n")
  cat("\n")
  cat("\n")
  cat("\n")
  cat("\n")
  cat("\n")
  cat("\n")
  cat("\n")
  cat("\n")
  cat("\n")
}
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
final_df1 = final_df
write.csv(final_df1, "cee_apr_ts_op.csv")
































