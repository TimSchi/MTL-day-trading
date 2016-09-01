
##
##Trading profit on testing data with MT 3R weights
##

#set stocks for indexes by official quotes
dax    = c("ALV","BAS","BAYN","BMW","CBK","DBK","LHA","HEN","LIN","RWE","SIE","VOW","SAP","MUV2","DTE","ADS","DAI","TKA","FME","EOAN")
cac    = c("AC","AI","CA","OR","MC","ML","SGO","GLE","CS","BNP","EN","BN","ENGI","RI","UG","SU","FP","VIE","VIV","CAP","AIR","KER","ORA","ACA")
djia   = c("GE","XOM","PG","DD","MMM","UTX","IBM","MRK","AXP","MCD","BA","KO","CAT","DIS","JPM","JNJ","WMT","HD","INTC","MSFT")
ftse   = c("CPI","REX","BG","AGK","BA","AZN","AAL","AMFW","CNA","LGEN","BARC","BP","REL","STAN","GSK","PRU","LAND","MGGT","RR","DGE","SAB","NG","BLND","RIO","WTB","SMIN","RB","RDSB","LLOY","SGE","SHP","NXT","IMI","BLT","MKS","MRW","RBS","WEIR","WOS","HMSO","SRP","IMB","GKN","ADN","SVT","SKY","VOD","TSCO","SDR","UU","GFS","HSBA","RSA","OML","TATE","SN","AV","KGF","PSON","BATS","ULVR")
stocks = list(FTSE = ftse, DAX = dax, DJIA = djia, CAC = cac)

abs.prof = list()

#define profit function
month.profit = function(W,X,Y,i) {
  
  sum(sapply(1:i, function(c) 
    t(tanh(X[[c]] %*% W[,c])) %*% Y[[c]]), na.rm = T)
}

#load stock & ROI data into R  
for (i in seq(1,length(stocks))) {
  index = names(stocks)[[i]]
  X1=list()
  Y1=list()
  
  for (c in seq(1,length(stocks[[i]]))) {
    comp = paste(stocks[[i]][c])
    
    #load X data
    file = file.path("C:/Users/.../Documents/Data_TA/",index,"/",comp,".csv", fsep = '')
    f = read.csv(file)
    f = gbm(f)
    X1[[c]] = f
    
    #load Y data
    file = file.path("C:/Users/.../Documents/Data_TA/",index,"/ROI_",comp,".csv", fsep = '')
    f = read.csv(file)
    f = gbm(f)
    Y1[[c]] = f
  }
  
  window.profits = vector()
  
  for (m in seq(1,length(TS))) {
    
    testing.x = list()
    testing.y = list()
    
    for (c in seq(1,length(stocks[[i]]))) {
        
        #creation of testing datasets of X & Y
        testing.x[[c]] = X1[[c]][[length(X1[[c]])-length(TS)+m]] 
        testing.y[[c]] = Y1[[c]][[length(X1[[c]])-length(TS)+m]]
    }
    #profits per testing month
    window.profits[m] = month.profit(W=We[[m]],X=testing.x,Y=testing.y,i=length(stocks[[i]]))
  }
  #absolute profit over all months per index  
  abs.prof[[index]] = sum(window.profits)
}
