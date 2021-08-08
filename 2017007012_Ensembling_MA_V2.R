library(dplyr)

base.directory = c("C:\\Users\\310272603\\Documents\\MasterData")

# Set working Directory to pick TS Outputs
setwd("Z:\\2017\\2017007\\2017007.Forecasts\\2017007.TS.Output")
ts_data1 = read.csv("Forecast_TS_LATAM_data.till.Feb.csv") # Standalone 1
ts_data2 = read.csv("Forecast_TS_LATAM_data.till.Mar.csv") # Standalone 2
ts_data3 = read.csv("Forecast_TS_LATAM_data.till.April.csv") # Standalone 3
ts_forecast=read.csv("Forecast_TS_LATAM_data.till.June.csv") # Future Forecast

# Set working Directory to pick ML Outputs
setwd("Z:\\2017\\2017007\\2017007.Forecasts\\2017007.ML.Output") 

ml_data1 = read.csv("Forecast_ML_LATAM_data.till.Feb.csv") # Standalone 1
ml_data2 = read.csv("Forecast_ML_LATAM_data.till.Mar.csv") # Standalone 2
ml_data3 = read.csv("Forecast_ML_LATAM_data.till.April.csv") # Standalone 3
ml_forecast=read.csv("Forecast_ML_LATAM_data.till.June.csv") # Future Forecast

market_name = 'LATAM' # Enter Market Name
# Set working Directory to pick Model Input Sheet

setwd("C:\\Users\\310272603\\Documents\\MasterData")
actual_data = read.csv("20170707.Model.input.All.Markets.csv") # Model Input Sheet with Market in the 1st Column
actual_data=actual_data[actual_data[1]==market_name,]
master_actuals = actual_data=actual_data[,-1]
n=ncol(actual_data)
actual_data = actual_data[,c(1,(ncol(actual_data)-2):(ncol(actual_data)))]

  #########Dates start and End to be mentioned in the code thats is stand alones###################
  start=as.Date("2017-04-01") ###The order is "YYYY-MM-DD"
  End=as.Date("2017-06-01") ###The order is "YYYY-MM-DD"
  dates_seq=as.character(seq(start,End, by="month"))
  ######### dates start and End in the master sheet#######
  start_m=as.Date("2013-06-01")
  End_m=as.Date("2017-06-01")
  dates_seq_m=as.character(seq(start_m,End_m, by="month"))
  colnames(actual_data)=c("keys",dates_seq)
  
  actual_1=actual_data[,(c("keys",as.character(dates_seq)))]


  sma_f=function(x,h_f)
  {
    v_sma=sma(x,h=h_f,silent=c("graph"))
    v_forecast=as.numeric(v_sma$forecast)
    return(v_forecast)
  }
  
ts_data1 = ts_data1[,c(1,3)]
ts_data2 = ts_data2[,c(1,3)]
ts_data3 = ts_data3[,c(1,3)]
ml_data1 = ml_data1[,c(1,3)]
ml_data2 = ml_data2[,c(1,3)]
ml_data3 = ml_data3[,c(1,3)]

colnames(ts_data1) = c("keys","TS.1")
colnames(ts_data2) = c("keys","TS.2")
colnames(ts_data3) = c("keys","TS.3")

colnames(ml_data1) = c("keys","ML.1")
colnames(ml_data2) = c("keys","ML.2")
colnames(ml_data3) = c("keys","ML.3")

ts_data = merge.data.frame(ts_data1, ts_data2, all = TRUE)
ts_data = merge.data.frame(ts_data, ts_data3,all = TRUE)
ts_data[is.na(ts_data)] = 0

ml_data = merge.data.frame(ml_data1, ml_data2, all = TRUE)
ml_data = merge.data.frame(ml_data, ml_data3,all = TRUE)
ml_data[is.na(ml_data)] = 0


ml.col = c("keys", "ML.1","ML.2","ML.3")
act.col = c("keys", "ACT.1","ACT.2","ACT.3")
ts.col = c("keys", "TS.1","TS.2","TS.3")

colnames(actual_1) = act.col
colnames(ts_data) = ts.col
colnames(ml_data) = ml.col


master.data = merge.data.frame(actual_1, ts_data,all = TRUE)

master.data = merge.data.frame(master.data, ml_data,all = TRUE)

master.data[is.na(master.data)] = 0
master.data[master.data<1] = 0

col.nor = tail(colnames(master.data),9)

get.intersect.keys = intersect(ts_data$keys,ml_data$keys)


master_working1 = master.data[apply(master.data[,c(col.nor)],1, function(x) all(x > 0)), ]


master_working1$ts.dev1 = abs(master_working1$ACT.1 - master_working1$TS.1)
master_working1$ml.dev1 = abs(master_working1$ACT.1 - master_working1$ML.1)
master_working1$ts.dev2 = abs(master_working1$ACT.2 - master_working1$TS.2)
master_working1$ml.dev2 = abs(master_working1$ACT.2 - master_working1$ML.2)
master_working1$ts.dev3 = abs(master_working1$ACT.3 - master_working1$TS.3)
master_working1$ml.dev3 = abs(master_working1$ACT.3 - master_working1$ML.3)

master_working1$w1.ts=(1/master_working1$ts.dev1)/((1/master_working1$ts.dev1)+(1/master_working1$ml.dev1))
master_working1$w2.ts=(1/master_working1$ts.dev2)/((1/master_working1$ts.dev2)+(1/master_working1$ml.dev2))
master_working1$w3.ts=(1/master_working1$ts.dev3)/((1/master_working1$ts.dev3)+(1/master_working1$ml.dev3))

master_working1$w1.ml=(1/master_working1$ml.dev1)/((1/master_working1$ts.dev1)+(1/master_working1$ml.dev1))
master_working1$w2.ml=(1/master_working1$ml.dev2)/((1/master_working1$ts.dev2)+(1/master_working1$ml.dev2))
master_working1$w3.ml=(1/master_working1$ml.dev3)/((1/master_working1$ts.dev3)+(1/master_working1$ml.dev3))
master_working1[is.na(master_working1)] = 1

master_working1$wm.ts=(master_working1$w1.ts+master_working1$w2.ts+master_working1$w3.ts)/3
master_working1$wm.ml=(master_working1$w1.ml+master_working1$w2.ml+master_working1$w3.ml)/3


#master_working1_wts=cbind(master_working1$keys,master_working1$wm.ts,master_working1$wm.ml)
#mf_ts=merge.data.frame(ts_forecast,master_working1$wm.ts,by="keys")
#mf_ml=merge.data.frame(ml_forecast,master_working1$wm.ml,by="keys")
#mf_ts=
#m_com=merge.data.frame()


ts.input = ts_forecast
ml.input = ml_forecast
colnames(ml.input) = c("keys", paste("ml",c(1:18),sep = "."))
colnames(ts.input) = c("keys", paste("ts",c(1:18),sep = "."))

#Refining the Dataset.
ts.input[ts.input<1] = 0
ml.input[ml.input<1] = 0


ts.input.base= ts.input = ts.input[apply(ts.input[,c(tail(colnames(ts.input),18))],1, function(x) all(x > 0)), ]
ml.input.base = ml.input = ml.input[apply(ml.input[,c(tail(colnames(ml.input),18))],1, function(x) all(x > 0)), ]

###### Keys ########

common.keys = intersect(ts.input$keys,ml.input$keys)

ts.input = ts.input[(ts.input$keys %in% common.keys), ]
ml.input = ml.input[(ml.input$keys %in% common.keys), ]

Final_wts.dataframe = master_working1[c("keys","w3.ml","wm.ts","wm.ml")]
append.wts.keys = data.frame()
append.wts.keys = cbind.data.frame(get.intersect.keys,c(0.5),c(0.5),c(0.5))
colnames(append.wts.keys) = c("keys","w3.ml","wm.ts","wm.ml")
data.wts.keys = append.wts.keys[!(append.wts.keys$keys %in% Final_wts.dataframe$keys),]
Final_wts.dataframe = rbind.data.frame(Final_wts.dataframe, data.wts.keys)

colnames(Final_wts.dataframe) = c("keys","Method","WTS.TS","WTS.ML")
temp1 = merge.data.frame(ts.input,Final_wts.dataframe)
weight.dataframe = merge.data.frame(temp1, ml.input)

ff.name = paste("ESF", c(1:18),sep = ".")
cut.ts = grep("ts", colnames(weight.dataframe), value = TRUE)
cut.ml = grep("ml", colnames(weight.dataframe), value = TRUE)

weight.dataframe[,c(ff.name)] = ((weight.dataframe$WTS.TS * weight.dataframe[,c(cut.ts)]) + (weight.dataframe$WTS.ML * weight.dataframe[,c(cut.ml)]))
ensembled.output = weight.dataframe[,c("keys",ff.name)]

#setwd("C:\\Users\\310272603\\Documents\\MasterData")
#write.csv(weight.dataframe,"weight.dataframedet.csv",row.names = FALSE)


colnames(master_actuals)=c("keys",dates_seq_m)
model.output = ensembled.output
model_input.data = master_actuals
###### Name of the Market ######
data.months = grep("2",colnames(master_actuals),value = TRUE)
model_input.data[,c(data.months)] = lapply(model_input.data[,c(data.months)],as.numeric)
model_input.data[is.na(model_input.data)] = 0
model_input.data[model_input.data<0] = 0
working_dataframe = model_input.data
##### Selection of SKU's ####
Final_Forecast =Final_ESF = Model_run.ESF=data.frame()
length(working_dataframe$keys) - length(model.output$keys)
working_dataframe <- working_dataframe[!working_dataframe$keys %in% as.vector(model.output[,1]), ]
length(working_dataframe$keys)
wer = colnames(working_dataframe)
message("Calculation MA of Remaining SKU's...")

wer = tail(wer,12)
l2m = tail(wer,2)
working_dataframe$nonzerocount <- rowSums(working_dataframe[,c(wer)]>0)
working_dataframe$nonzerocount_l2m <- rowSums(working_dataframe[,c(l2m)]>0)
rawdata_lowdata <- working_dataframe[((working_dataframe$nonzerocount_l2m>0)),]
rawdata_lowdata <- rawdata_lowdata[,1:(ncol(rawdata_lowdata)-2)]

NAMES_list=rawdata_lowdata$keys
h_f = 18
main_data_m=as.matrix(rawdata_lowdata[,-1])
sma_forecast=apply(main_data_m,1,function(x) sma_f(x,h_f))
df_remain=as.data.frame(t(matrix(sma_forecast,nrow=h_f)))
ma_output=cbind(rawdata_lowdata$keys,df_remain)

colnames(ma_output) = c("keys",ff.name)
ma_output[ma_output<1]=0

Final_merged.output = rbind.data.frame(ensembled.output,ma_output)
Sum = rowSums(Final_merged.output[,c(ff.name)])
Final_merged.output = subset.data.frame(Final_merged.output, Sum > 0)


######## Trend part ########
message("Running Trend COrrection")

colnames(Final_merged.output)[1]="keys"
data_tt=merge.data.frame(Final_merged.output,model_input.data)

for(i in nrow(data_tt))
{
  get.ff.name1 = ff.name[1:6]
  get.ff.name2 = ff.name[7:12]
  get.ff.name3 = ff.name[13:18]
  get.cbh.month = tail(colnames(data_tt), 6)
  sum.ff.name1 = sum(as.numeric(data_tt[i,c(get.ff.name1)]))/rowSums(data_tt[i,c(get.ff.name1)]>0)
  sum.ff.name2 = sum(as.numeric(data_tt[i,c(get.ff.name2)]))/rowSums(data_tt[i,c(get.ff.name2)]>0)
  sum.ff.name3 = sum(as.numeric(data_tt[i,c(get.ff.name3)]))/rowSums(data_tt[i,c(get.ff.name3)]>0)
  sum.cbh.name = sum(as.numeric(data_tt[i,c(get.cbh.month)]))/rowSums(data_tt[i,c(get.cbh.month)]>0)
  if((sum.cbh.name/sum.ff.name1) < (1/6) || (sum.cbh.name / sum.ff.name1) > 6)
  {
    output.data = sum.cbh.name*runif(18,0.9,1.1)
    data_tt[i,c(ff.name)] = output.data
  } else if((sum.cbh.name/sum.ff.name2) < (1/6) || (sum.cbh.name / sum.ff.name2) > 6)
  {
    output.data = sum.cbh.name*runif(12,0.9,1.1)
    data_tt[i,c(get.ff.name2,get.ff.name3)] = output.data
  } else if((sum.cbh.name/sum.ff.name3) < (1/6) || (sum.cbh.name / sum.ff.name3) > 6)
  {
    output.data = sum.cbh.name*runif(6,0.9,1.1)
    data_tt[i,c(get.ff.name3)] = output.data
  } else { weter = c("You just got lucky, Enjoy!")}
  
}

ESF.data = data_tt[,c("keys",ff.name)]
ESF.data[ESF.data<1] = 0
ESF.data[,c(ff.name)] = ceiling(ESF.data[,c(ff.name)])
setwd(base.directory)
output.file = paste(paste(gsub("-", "", Sys.Date(), fixed = TRUE), "MA.Appended", market_name,sep = "_"),"csv", sep = ".")
write.csv(ESF.data,output.file,row.names = FALSE)
write.csv(data_tt, "data_tt.csv",row.names = FALSE)
write.csv(master_working1,paste(paste(gsub("-", "", Sys.Date(), fixed = TRUE), "Weights.Working", market_name,sep = "_"),"csv", sep = "."),row.names = FALSE)
write.csv(Final_wts.dataframe[,c(1,3,4)],paste(paste(gsub("-", "", Sys.Date(), fixed = TRUE), "Ensemble.Weights", market_name,sep = "_"),"csv", sep = "."),row.names = FALSE)


