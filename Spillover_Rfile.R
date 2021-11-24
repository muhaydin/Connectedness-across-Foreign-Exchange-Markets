#### Packages Used in the Estimation ####

packages <- c("tvReg","readxl","vars", "xts", "bvarsv", "tsbox", "tseries", "vars", "devtools","SciViews", "base")
for(i in packages) {
  if(i %in% rownames(installed.packages())){
    library(i, character.only = T)
  } else {
    install.packages(i)
    library(i, character.only = T)
  }}
install_github("tomaskrehlik/frequencyConnectedness", tag = "dev") 
library(frequencyConnectedness)

#### Data Used in the Project #### 

dataset<-read_excel("dataset2.xlsx")
colnames(dataset)<-c("DATE","B","T","Z","C","E","G")
dataset<-dataset[!is.na(dataset$DATE),]

#### Unit Root Testing #### 

data.ts<-as.ts(dataset[,-1])
colnames(data.ts)<-c("B","T","Z","C","E","G")
adf<-lapply(1:ncol(data.ts), function(x) adf.test(na.omit(data.ts[,x])))
adf[1:ncol(data.ts)]

####  Optimum Lag Selection #### 

opt.lag<-VARselect(data.ts, lag.max=10)$selection
aic<-as.numeric(VARselect(data.ts, lag.max=10)$selection[1])
hq<-as.numeric(VARselect(data.ts, lag.max=10)$selection[2])
sc<-as.numeric(VARselect(data.ts, lag.max=10)$selection[3])
fpe<-as.numeric(VARselect(data.ts, lag.max=10)$selection[4])
opt.lag

####  Estimation of VAR ####  

est <- VAR(data.ts, p = sc, type = "const")

####   Estimation of Traditional Connectedness: Connectedness Table ####  
sptable <- spilloverDY12(est, n.ahead = 50, no.corr = F)

####   Spillover Tables  ####  
print(sptable)
print(to(sptable))
print(from(sptable))
print(net(sptable))
print(pairwise(sptable))


####  Estimation of Dynamic Connectedness: Spillover Indices   ####  

params_est = list(p = sc, type = "const")
spindex2 <- spilloverRollingDY12(data.ts, n.ahead = 50, no.corr=F, "VAR", 
                                 params_est = params_est, window = 200, cluster = NULL)


####   Plotting Results of Rolling Estimation ####  

plotOverall(spindex2)
plotTo(spindex2)
plotFrom(spindex2)
plotNet(spindex2)
plotPairwise(spindex2)

