setwd("C:\\Users\\310287871\\Desktop\\Forecast Generation")
#model_input.data = read.csv("Model-Input-June.csv")
BG_Channel=read.csv("BG_Material_Mapping.csv")
BG_Channel$market = toupper(BG_Channel$market)


setwd("C:\\Users\\310287871\\Desktop\\Forecast Generation\\CEE")
#########################Upload of the file ##################################
data_master=read.csv("CEE_Actuals-Model.csv")
data_master[is.na(data_master)] = 0
data_master_dd=data_master
ml.input = read.csv("CEE_ml_output.csv")
ts.input=read.csv("CEE_ts_output.csv")
Ensemble_Output=read.csv("CEE_ensemble_output.csv")
model_input.data = read.csv("CEE-Model-Input.csv")
market.input=market.input.name=toupper(c("CEE"))

## "INDIA"  "MET"    "UKI"    "US"     "APAC"   "GRC"    "JAPAN"  "IIG"    "
## "CEE"    "NORDIC" "DACH"  
## "FRANCE" "BNLX"   "LATAM"  "AFRICA" "RCA"    "CANADA" "IBERIA"

library(tibble)
library(splitstackshape) ;library(plyr) ;library(dplyr);library(dtplyr);library(reshape);library(reshape2);library(lubridate)

p=2

######Ensembling #######
colnames(ml.input) = c("keys", paste("ML",c(1:18),sep = "."))
colnames(ts.input) = c("keys", paste("TS",c(1:18),sep = "."))
ts.input[is.na(ts.input)] = 0
ml.input[is.na(ml.input)] = 0
ts.input[ts.input<1] = 0
ml.input[ml.input<1] = 0

ts.input = ts.input[apply(ts.input[,c(tail(colnames(ts.input),18))],1, function(x) all(x > 0)), ]
ml.input = ml.input[apply(ml.input[,c(tail(colnames(ml.input),18))],1, function(x) all(x > 0)), ]


###### Keys ########
common.keys = intersect(ts.input$keys,ml.input$keys)

ts.input = ts.input[(ts.input$keys %in% common.keys), ]
ml.input = ml.input[(ml.input$keys %in% common.keys), ]

derwer = merge.data.frame(ml.input,ts.input, all = TRUE)
derwer = na.omit(derwer)
keys = unique(derwer$keys)



colnames(Ensemble_Output) = c("keys","Method","wt.ml","wt.ts")
temp1 = merge.data.frame(Ensemble_Output, ts.input)
weight.dataframe = merge.data.frame(temp1, ml.input)

ff.name = paste(c(1:18),sep = ".")
cut.ts = grep("TS", colnames(weight.dataframe), value = TRUE)
cut.ml = grep("ML", colnames(weight.dataframe), value = TRUE)

weight.dataframe[,c(ff.name)] = (weight.dataframe$wt.ts * weight.dataframe[,c(cut.ts)]) + (weight.dataframe$wt.ml * weight.dataframe[,c(cut.ml)])
weight.dataframe.output = weight.dataframe[,c("keys",ff.name)]
data_master_output=weight.dataframe.output




######## Trend part ########
colnames(data_master_dd)[1]="keys"
data_tt=merge.data.frame(data_master_output,data_master_dd,by="keys")

for(i in nrow(data_tt))
{
  k_1=(ncol(data_tt)-5)
  k_2=ncol(data_tt)
  data_act3sum=mean(as.numeric(data_tt[i,(k_1:k_2)]))
  data_fut3sum=mean(as.numeric(data_tt[i,(1:5)]))
  if((data_fut3sum>3*data_act3sum) | (data_fut3sum<0.33*data_act3sum))
  {
    
    data_master_output[i,(2:ncol(data_master_output))]=(data_act3sum*runif(18,0.9,1.1))
  } else wweret = 1
}


model.output = data_master_output


##### Working ######
model.output[model.output<1] = 0
output.month = colnames(model.output)[2:19]
Sum = rowSums(model.output[,c(output.month)])
model.output = subset.data.frame(model.output, Sum > 0)



###### Name of the Market ######
data.months = grep("sales", colnames(model_input.data), value = TRUE)
data.factors = setdiff(colnames(model_input.data), data.months)
model_input.data[,c(data.months)] = lapply(model_input.data[,c(data.months)], as.numeric)
model_input.data[,c(data.factors)] = lapply(model_input.data[,c(data.factors)], as.factor)
model_input.data[is.na(model_input.data)] = 0
model_input.data[model_input.data<0] = 0
model_input.data$market = toupper(model_input.data$market)
market_names = unique(model_input.data$market)
options(warn=-1)

##### Selection of SKU's ####
Final_Forecast =Final_ESF = Model_run.ESF=data.frame()
working_dataframe = subset.data.frame(model_input.data, market == market.input.name)
length(working_dataframe$keys) - length(model.output$keys)
working_dataframe <- working_dataframe[!working_dataframe$keys %in% as.vector(model.output[,1]), ]
length(working_dataframe$keys)
name_market = unique(working_dataframe$market)
name_market = as.data.frame(name_market)
wer = colnames(working_dataframe)

wer = tail(wer,12)
l2m = tail(wer,2)
working_dataframe = subset.data.frame(working_dataframe, select = -c(market))
working_dataframe$nonzerocount <- rowSums(working_dataframe[,c(wer)]>0)
working_dataframe$nonzerocount_l2m <- rowSums(working_dataframe[,c(l2m)]>0)
rawdata_lowdata <- working_dataframe[((working_dataframe$nonzerocount_l2m>0)),]
rawdata_lowdata <- rawdata_lowdata[,1:(ncol(rawdata_lowdata)-2)]
rawdata_lowdata$MA1 <- rowSums(rawdata_lowdata[,(ncol(rawdata_lowdata)-5):ncol(rawdata_lowdata)])/
  rowSums(rawdata_lowdata[,(ncol(rawdata_lowdata)-5):ncol(rawdata_lowdata)]>0)
rawdata_lowdata$MA1[rawdata_lowdata$MA1<1] = 0 
non_w = tail(wer,5)
rawdata_lowdata$nonzerocount <- rowSums(rawdata_lowdata[,c(non_w)]>0)
rawdata_lowdata$MA1 = ifelse(rawdata_lowdata$nonzerocount==1,rawdata_lowdata$MA1*runif(1,0.75,0.92),rawdata_lowdata$MA1)
rawdata_lowdata$MA1[rawdata_lowdata$MA1<1] = 0
forecast_lowdata = rawdata_lowdata[,c("keys","MA1")]
output.month = paste(c(1:18), sep = ".")
market.output.month = paste("M",c(1:18), sep = ".")
rev.forecast = as.data.frame(rawdata_lowdata[,"MA1"])
forecast_lowdata[,c(market.output.month)] <- runif(18,0.85,1.15)
forecast_lowdata[forecast_lowdata$MA1<= 1.2,c(market.output.month)] = 1

forecast_lowdata[,c(output.month)] = forecast_lowdata$MA1 *forecast_lowdata[,c(market.output.month)]
Final_Forecast = forecast_lowdata[,c("keys",output.month)]
Final_Forecast[Final_Forecast<1] = 0
colnames(model.output) = c("keys",output.month)
Final_merged.output = rbind.data.frame(model.output,Final_Forecast)
maarket.name = paste(market.input.name,"Merged.Forecast.Output","csv",sep = ".")

Sum = rowSums(Final_merged.output[,c(output.month)])
Final_merged.output = subset.data.frame(Final_merged.output, Sum > 0)





##### Seas #####

## data_master=data_master_dd

colnames(data_master)[1]="Key"
material_1=as.data.frame(cSplit(data_master,"Key","_"))
material=as.factor(material_1[,ncol(material_1)])
channel=as.factor(material_1[,(ncol(material_1)-p)])
dum_2=as.factor(material)
dum_3=c(rep("p",nrow(data_master)))
dum_4=c(rep("q",nrow(data_master)))
dum_5=c(rep("q",nrow(data_master)))
data_master=add_column(data_master, MaterialID1 =dum_2,.after = 1)
data_master=add_column(data_master, MaterialID2 =dum_2,.after = 2)
data_master=add_column(data_master, MaterialID3 =channel,.after = 3)
data_master=add_column(data_master, MaterialID4 =dum_4,.after = 4)
data_master=add_column(data_master, MaterialID5 =dum_5,.after = 5)
colnames(BG_Channel_markt)[2]=colnames(data_master)[2]="Material"
BG_Channel_markt=BG_Channel[BG_Channel$market==market.input,]

data_master_2=merge.data.frame(data_master,BG_Channel_markt,by="Material")



data_master_2[,5]=data_master_2[,(ncol(data_master_2)-1)]
data_master_2=select(data_master_2,Key,Material,everything())

data_master_2=data_master_2[,-ncol(data_master_2)]
data_master_2=data_master_2[,-ncol(data_master_2)]



data_master=data_master_2 
key_1=paste(data_master[,4],data_master[,5],sep="_")
data_master_dummy=cbind(key_1,data_master)
a_1=ncol(data_master)-5
a_2=ncol(data_master)-5-23
data_master_du_1=data_master[,(a_2:a_1)]
data_master_du=cbind(key_1,data_master_du_1)
dataColumns=colnames(data_master_du)[-1]
data_actuals_1=cbind(data_master[,(1:6)],data_master_du[-1])
temp_df = ddply(data_master_du,c("key_1"),function(x) colSums(x[dataColumns]))
actual_data=data_actuals_1
actual1_data=temp_df
Shape_data=actual_data[,((ncol(actual_data)-23):(ncol(actual_data)))]
#RCA_data=read.csv("AvF.csv")
col_name=colnames(Shape_data)
cum_dev=vector()
Full_ses=data.frame()
Full_data=data.frame()
ses_data=data.frame()
Seas_function=function(S_Index_1,S_index_2,CH_BG_SI_1,CH_BG_SI_2,len)
{
  Ses1=vector()
  for(i in 1:length(S_Index_1))
  {
    if(S_Index_2[i]!=0)    
    {
      
      if(len>=24)
      {
        
        mean_ses=(S_Index_1[i]+S_Index_2[i])/2
        vector_SI=c(S_Index_1[i],S_Index_2[i],CH_BG_SI_1[i],CH_BG_SI_2[i])
        v_SI_v_g=length(which(vector_SI >=1))
        if(v_SI_v_g==4)
        {
          cov=sd(c(S_Index_1[i],S_Index_2[i]))/mean(c(S_Index_1[i],S_Index_2[i]))
          if(cov<=0.09)
          {
            Ses1[i]=mean(c(S_Index_1[i],S_Index_2[i]))
          } else if(mean_ses<1.6)
          {
            Ses1[i]=mean_ses
          } else 
          {
            Ses1[i]=1.6
          }
        } else if(v_SI_v_g==3)
        {
          if(S_Index_1[i]> 1 & S_Index_2[i] > 1)
          {
            mean_ses1=mean(S_Index_1[i],S_Index_2[i])
          } else 
          {
            n=c(S_Index_1[i],S_Index_2[i])
            mean_ses1=n[which(n>=1)]
          }
          if(mean_ses1<1.3)
          {
            Ses1[i]=mean_ses1
          } else 
          {
            Ses1[i]=1.3
          }
        } else if((v_SI_v_g==2)) 
        {
          if(S_Index_1[i]>1 & S_Index_2[i]>1)
          {
            if(mean_ses<1.3)
            {
              Ses1[i]=mean_ses
            } else
            {
              Ses1[i]=1
            }
          } else
          {
            Ses1[i]=1
          }
        } else if(S_Index_1[i]<1 & S_Index_2[i]<1)
        {
          if(mean_ses>0.8)
          {
            Ses1[i]=mean_ses
          } else
          {
            Ses1[i]=0.8
          }
          
        } else
        {
          Ses1[i]=1
        }
        
      } else if (len>=12 & len<24)
      {
        s_vector=c(S_Index_2[i],CH_BG_SI_1[i],CH_BG_SI_2[i])
        count_1=length(which(s_vector>1))
        if(count_1==3)
        {
          if(S_Index_2[i]<1.3)
          {
            Ses1[i]=S_Index_2[i]
          } else 
          {
            Ses1[i]=1.3
          }
        } else
        {
          Ses1[i]=1
        }  
        
        
      } else
      {
        Ses1[i]=1
      }
      
    } else 
    {
      Ses1[i]=1
    }  
  }
  return(Ses1)
}
d_str_month=function(d)
{
  h=substr(d,2,(nchar(d)-5))
  return(h)
}
#d_str_year=function(d)
#{
#h=substr(d,(nchar(d)-3),(nchar(d)))
#return(h)
#}
Ses_data=data.frame()
S_Index_1_data=data.frame()
S_Index_2_data=data.frame()
FACC_SKU_vec=vector()
for(i in 1:nrow(actual_data))
{
  Shape_data_vector=as.numeric(Shape_data[i,])
  if(sum(Shape_data_vector)>200)
  {
    Shape_data_vector[Shape_data_vector<0]=0
    y1_avg=mean(Shape_data_vector[1:12])
    y2_avg=mean(Shape_data_vector[13:24])
    len=length(Shape_data_vector)
    S_Index_1=Shape_data_vector[1:12]/y1_avg
    S_Index_2=Shape_data_vector[13:24]/y2_avg
    S_Index_mean=(S_Index_1+S_Index_2)/2
    key=paste(actual_data[i,4],actual_data[i,5],sep="_")
    key=as.character(key)
    CH_BG_SI_total=as.numeric(actual1_data[actual1_data[,1]==key,])
    CH_BG_SI_sub_total=CH_BG_SI_total[(length(CH_BG_SI_total)-23):length(CH_BG_SI_total)]
    CH_BG_SI_1=CH_BG_SI_sub_total[1:12]
    CH_BG_SI_2=CH_BG_SI_sub_total[13:24]
    yt1_avg=mean(CH_BG_SI_1)
    yt2_avg=mean(CH_BG_SI_2)
    CH_BG_SI_1_index=CH_BG_SI_1/yt1_avg
    CH_BG_SI_2_index=CH_BG_SI_2/yt2_avg
    S_Index_2[is.na(S_Index_2)]=0
    S_Index_1[is.na(S_Index_1)]=0
    Shape_data_len=which(Shape_data_vector>0)
    shape_new=Shape_data_vector[Shape_data_len[1]:length(Shape_data_vector)]
    
    len=length(shape_new)
    if( y2_avg!=0)
    {
      Ses_vector=Seas_function(S_Index_1,S_Index_2,CH_BG_SI_1_index,CH_BG_SI_2_index,len=len)
    } else 
    {
      Ses_vector=rep(1,12)
    }
  } else
  {
    Ses_vector=rep(1,12)
  }
  Ses_data=rbind(Ses_data,Ses_vector)
  Full_ses=rbind(Full_ses,S_Index_mean)
  S_Index_1_data=rbind(S_Index_1_data,S_Index_1)
  S_Index_2_data=rbind(S_Index_2_data,S_Index_2)
  
}

Full_ses=cbind(actual_data[,1],Full_ses)
S_Index_1_data=cbind(actual_data[,1],S_Index_1_data)
S_Index_2_data=cbind(actual_data[,1],S_Index_2_data)
Full_ses[is.na(Full_ses)]=1
S_Index_1_data[is.na(S_Index_1_data)]=1
S_Index_2_data[is.na(S_Index_2_data)]=1
h=colnames(Shape_data)
month_vector=as.numeric(d_str_month(h))
#year_vector=as.numeric(d_str_year(h))
colnames(Ses_data)=month_vector[1:12]
Ses_data_1=cbind(actual_data[,1],Ses_data)
Ses_data_1[Ses_data_1=="NA"]=0
kk=colnames(Ses_data_1)
kkk=c("Key",kk[2:length(kk)])
colnames(Ses_data_1)=kkk
k_1=colnames(Full_ses)
colnames(Full_ses)=colnames(Ses_data_1)
colnames(S_Index_1_data)=colnames(Ses_data_1)
colnames(S_Index_2_data)=colnames(Ses_data_1)


##### Data frame from the Seasonality function #####
Ses_data_1=cbind(Ses_data_1[,(2:ncol(Ses_data_1))],Ses_data_1[,(2:ncol(Ses_data_1))])[1:18]
Ses_data_1=cbind(actual_data[,1],Ses_data_1)
gg.name1 = paste("Seas1",1:18,sep = ".")
gg.name2 = paste("Seas2",1:12,sep = ".")
gg = paste(1:18)
colnames(Ses_data_1) = c("keys",1:18)


#### Dup Seas ####
Ses_data_2 = Ses_data_1

#Ses_data_1 = Ses_data_2
Ses_data_1[,gg.name1] = Ses_data_1[,c(gg)]
update.colnames = colnames(Ses_data_1)
update.colnames
Ses_data_5 = Ses_data_1[,c(update.colnames[1],update.colnames[7:24])]
colnames(Ses_data_5)[1] =  c("keys")


seas.common.keys = intersect(Final_merged.output$keys,Ses_data_5$keys)
length(seas.common.keys)

###### Rbind Data #######
colnames(Final_merged.output)[2:19] = c(paste("ESF",1:18,sep = "."))
col.seas = colnames(Ses_data_5)[2:19]
col.ff = colnames(Final_merged.output)[2:19]
seas.working= merge.data.frame(Final_merged.output,Ses_data_5,all = TRUE)
seas.working1 = seas.working[(seas.working$keys %in% Final_merged.output$keys),]
seas.working1[is.na(seas.working1)] = 1
tt.name = paste("FF", c(1:18),sep = ".")

seas.working1[,c(tt.name)]  = seas.working1[,c(col.seas)] * seas.working1[,c(col.ff)] 

seas.working.output = seas.working1[,c("keys",tt.name)]
maarket.name = paste(market.input.name,"Merged.Forecast.Output3","csv",sep = ".")

write.csv(seas.working.output,maarket.name,row.names = FALSE)


