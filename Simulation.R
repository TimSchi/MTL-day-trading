
##
## Simulation
##

#set stocks for indexes by official quotes
dax    = c("ALV","BAS","BAYN","BMW","CBK","DBK","LHA","HEN","LIN","RWE","SIE","VOW","SAP","MUV2","DTE","ADS","DAI","TKA","FME","EOAN")
cac    = c("AC","AI","CA","OR","MC","ML","SGO","GLE","CS","BNP","EN","BN","ENGI","RI","UG","SU","FP","VIE","VIV","CAP","AIR","KER","ORA","ACA")
djia   = c("GE","XOM","PG","DD","MMM","UTX","IBM","MRK","AXP","MCD","BA","KO","CAT","DIS","JPM","JNJ","WMT","HD","INTC","MSFT")
ftse   = c("CPI","REX","BG","AGK","BA","AZN","AAL","AMFW","CNA","LGEN","BARC","BP","REL","STAN","GSK","PRU","LAND","MGGT","RR","DGE","SAB","NG","BLND","RIO","WTB","SMIN","RB","RDSB","LLOY","SGE","SHP","NXT","IMI","BLT","MKS","MRW","RBS","WEIR","WOS","HMSO","SRP","IMB","GKN","ADN","SVT","SKY","VOD","TSCO","SDR","UU","GFS","HSBA","RSA","OML","TATE","SN","AV","KGF","PSON","BATS","ULVR")
stocks = list(FTSE = ftse, DAX = dax, DJIA = djia, CAC = cac)

#define a function to order stock data by date and group by month
gbm = function(f) {
  f = xts(f,order.by = as.Date(f$Date))
  f = split(f, f = "months")
  f = sapply(1:length(f), function(i) f[[i]] = subset(f[[i]][,-1]))  #remove the Date column
  for (m in seq(1,length(f))) {
    storage.mode(f[[m]]) = "numeric"
  } 
  return(f)
}

L.TC             = list()
balance.trad.pos = list()
balance.reb      = list()
balance.lot      = list()
balance.reb.lot  = list()
P                = list() 
dates            = list()
y                = list()

for (i in seq(1,length(stocks))) {
  index = names(stocks)[[i]]
  
  #load stock & ROI data into R
  X = list()
  Y = list()
  for (c in seq(1,length(stocks[[i]]))) {
    comp = paste(stocks[[i]][c])
    
    #load X data
    file   = file.path("C:/Users/.../Documents/Data_TA/",index,"/",comp,".csv", fsep = '')
    f      = read.csv(file)
    f      = gbm(f)
    X[[c]] = f
    
    #load Y data
    file                   = file.path("C:/Users/.../Documents/Data_TA/",index,"/ROI_",comp,".csv", fsep = '')
    f                      = read.csv(file)
    dates[[index]][[comp]] = f$Date
    f                      = gbm(f)
    Y[[c]]                 = f
    
    #create p values on weights for a chosen model
    p = vector() 
    s = vector()
    for (m in seq(26,length(X[[c]]))) {
      v = tanh(X[[c]][[m]] %*% We[[m-25]][,c])
      p = append(p,v)
      s = append(s,as.vector(Y[[c]][[m]]))
    }
    P[[index]][[comp]] = p
    y[[index]][[comp]] = s
  
    trad.pos.cash  = vector()
    trad.pos.stock = vector()
    reb.cash       = vector()
    reb.stock      = vector()
    reb.lot.cash   = vector()
    reb.lot.stock  = vector()
    lot.cash       = vector()
    lot.stock      = vector()
    
    #evenly split start budget for each security
    B.cash.start   = 0.5 
    B.stock.start  = 0.5
    
    #simulate the four trading strategies
    for (d in seq_along(P[[index]][[comp]])) {
      
      if (d==1) {
        trad.pos.cash[d]  = if (P[[index]][[comp]][[d]]>0) B.cash.start * (1 - P[[index]][[comp]][[d]]*(1 + TC)) else 
          if (P[[index]][[comp]][[d]]<0) B.cash.start - P[[index]][[comp]][[d]]* B.stock.start * (1 - TC) else B.cash.start
        trad.pos.stock[d] = if (P[[index]][[comp]][[d]]>0) (B.stock.start + P[[index]][[comp]][[d]] * B.cash.start) * (1 + y[[index]][[comp]][[d]]) else 
          if (P[[index]][[comp]][[d]]<0) B.stock.start * (1 + P[[index]][[comp]][[d]]) * (1 + y[[index]][[comp]][[d]]) else B.stock.start
        
        
        lot.cash[d]  = if (P[[index]][[comp]][[d]]>0) B.cash.start * (1 - lotsize * P[[index]][[comp]][[d]]*(1 + TC)) else 
          if (P[[index]][[comp]][[d]]<0) B.cash.start - lotsize * P[[index]][[comp]][[d]]* B.stock.start * (1 - TC) else B.cash.start
        lot.stock[d] = if (P[[index]][[comp]][[d]]>0) (B.stock.start + lotsize * P[[index]][[comp]][[d]] * B.cash.start) * (1 + y[[index]][[comp]][[d]]) else 
          if (P[[index]][[comp]][[d]]<0) B.stock.start * (1 + lotsize * P[[index]][[comp]][[d]]) * (1 + y[[index]][[comp]][[d]]) else B.stock.start
        
        reb.cash[d]  = trad.pos.cash[d]
        reb.stock[d] = trad.pos.stock[d]
        
        reb.lot.cash  = lot.cash[d]
        reb.lot.stock = lot.stock[d]
      }
      else  {
        trad.pos.cash[d]  = if (P[[index]][[comp]][[d]]>0) trad.pos.cash[d-1] * (1 - P[[index]][[comp]][[d]]*(1 + TC)) else 
          if (P[[index]][[comp]][[d]]<0) trad.pos.cash[d-1] - P[[index]][[comp]][[d]]* trad.pos.stock[d-1] * (1 - TC) else trad.pos.cash[d-1]
        trad.pos.stock[d] = if (P[[index]][[comp]][[d]]>0) (trad.pos.stock[d-1] + P[[index]][[comp]][[d]] * trad.pos.cash[d-1]) * (1 + y[[index]][[comp]][[d]]) else 
          if (P[[index]][[comp]][[d]]<0) trad.pos.stock[d-1] * (1 + P[[index]][[comp]][[d]]) * (1 + y[[index]][[comp]][[d]]) else trad.pos.stock[d-1]
        
        lot.cash[d]  = if (P[[index]][[comp]][[d]]>0) lot.cash[d-1] * (1 - lotsize * P[[index]][[comp]][[d]]*(1 + TC)) else 
          if (P[[index]][[comp]][[d]]<0) lot.cash[d-1] - lotsize * P[[index]][[comp]][[d]]* lot.stock[d-1] * (1 - TC) else lot.cash[d-1]
        lot.stock[d] = if (P[[index]][[comp]][[d]]>0) (lot.stock[d-1] + lotsize * P[[index]][[comp]][[d]] * lot.cash[d-1]) * (1 + y[[index]][[comp]][[d]]) else 
          if (P[[index]][[comp]][[d]]<0) lot.stock[d-1] * (1 + lotsize * P[[index]][[comp]][[d]]) * (1 + y[[index]][[comp]][[d]]) else lot.stock[d-1]
        
        reb          = (reb.cash[d-1] + reb.stock[d-1] - TC * abs(reb.cash[d-1] - reb.stock[d-1])/2)/2
        reb.cash[d]  = if (P[[index]][[comp]][[d]]>0) reb * (1 - P[[index]][[comp]][[d]]*(1 + TC)) else 
          if (P[[index]][[comp]][[d]]<0) reb - P[[index]][[comp]][[d]] * reb * (1 - TC) else reb
        reb.stock[d] = if (P[[index]][[comp]][[d]]==0) reb else reb * (1 + P[[index]][[comp]][[d]]) * (1 + y[[index]][[comp]][[d]])
        
        reb              = (reb.lot.cash[d-1] + reb.lot.stock[d-1] - TC * abs(reb.lot.cash[d-1] - reb.lot.stock[d-1])/2)/ 2
        reb.lot.cash[d]  = if (P[[index]][[comp]][[d]]>0) reb * (1 - lotsize * P[[index]][[comp]][[d]]*(1 + TC)) else 
          if (P[[index]][[comp]][[d]]<0) reb - lotsize * P[[index]][[comp]][[d]] * reb * (1 - TC) else reb
        reb.lot.stock[d] = if (P[[index]][[comp]][[d]]==0) reb else reb * (1 + lotsize * P[[index]][[comp]][[d]]) * (1 + y[[index]][[comp]][[d]])
        
      }
    }
    
    L.TC[[index]][[comp]] = cbind(Positional_Cash=trad.pos.cash, Positional_Stock=trad.pos.stock, Rebalance_Cash=reb.cash, Rebalance_Stock=reb.stock, Rebalance_lot_Cash=reb.lot.cash, Rebalance_lot_Stock=reb.lot.stock, FixedLot_Cash=lot.cash, FixedLot_Stock=lot.stock)
  }
  #end balances of each strategy for every index
  balance.trad.pos[[index]] = sum(sapply(1:length(stocks[[i]]), function(c) tail(L.TC[[index]][[c]][,1],1) + tail(L.TC[[index]][[c]][,2],1)))
  balance.reb[[index]]      = sum(sapply(1:length(stocks[[i]]), function(c) tail(L.TC[[index]][[c]][,3],1) + tail(L.TC[[index]][[c]][,4],1)))
  balance.lot[[index]]      = sum(sapply(1:length(stocks[[i]]), function(c) tail(L.TC[[index]][[c]][,7],1) + tail(L.TC[[index]][[c]][,8],1)))
  balance.reb.lot[[index]]  = sum(sapply(1:length(stocks[[i]]), function(c) tail(L.TC[[index]][[c]][,5],1) + tail(L.TC[[index]][[c]][,6],1)))
}

