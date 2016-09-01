
##
## Model MT GR&MR optimization on FTSE data
##

rm(list = ls())

#install required packages
install.packages("xts")
install.packages("doParallel")
install.packages("foreach")
library(xts)
library(doParallel)
library(foreach)


# Data preparation --------------------------------------------------------------

#set stock sample of FTSE by official quotes
aktien = c("CPI","REX","BG","AGK","BA","AZN","AAL","AMFW","CNA","LGEN","BARC","BP","REL","STAN","GSK","PRU","LAND","MGGT","RR","DGE","SAB","NG","BLND","RIO","WTB","SMIN","RB","RDSB","LLOY","SGE","SHP","NXT","IMI","BLT","MKS","MRW","RBS","WEIR","WOS","HMSO","SRP","IMB","GKN","ADN","SVT","SKY","VOD","TSCO","SDR","UU","GFS","HSBA","RSA","OML","TATE","SN","AV","KGF","PSON","BATS","ULVR")

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

#load stock & ROI data into R
X = list()
Y = list()

for (c in seq(1,length(aktien))) {
  comp = paste(aktien[c])
  
  #load X data
  file      = file.path("C:/Users/.../Documents/Data_TA/FTSE/",comp,".csv", fsep = '')
  f         = read.csv(file)
  f         = gbm(f)
  X[[comp]] = f
  
  #load Y data
  file      = file.path("C:/Users/.../Documents/Data_TA/FTSE/ROI_",comp,".csv", fsep = '')
  f         = read.csv(file)
  f         = gbm(f)
  Y[[comp]] = f
}


# Functions ---------------------------------------------------------------

#define the objective with two regularizers
objective = function(W, lambda.g, lambda.m) {
  
  W = array(W,c(indicators,length(training.x[[c]]),length(aktien)))
  
  -sum(sapply(1:length(training.x), function(c) 
    sapply(1:length(training.x[[c]]), function(m) 
      t(tanh(training.x[[c]][[m]] %*% W[,m,c])) %*% training.y[[c]][[m]])),na.rm = T) + 
      lambda.g * sum(sapply(1:length(training.x),function(c) sum(diag(sqrt(t(W[,,c]) %*% W[,,c]))))) +
      lambda.m * sum(sapply(2:length(training.x[[1]]), function(m) norm(W[,m,] - W[,m-1,])^2))
} 

#define the gradient 
gradient = function(W, lambda.g, lambda.m){
  
  W = array(W,c(indicators,length(training.x[[c]]),length(aktien)))
  
  sapply(1:length(training.x), function(c) 
    sapply(1:length(training.x[[c]]), function(m) 
      sapply(1:ncol(training.x[[c]][[m]]), function(i) 
        -sum(sapply(1:nrow(training.x[[c]][[m]]), function(d) 
          (cosh(training.x[[c]][[m]][d,] %*% W[,m,c])^-2) * training.y[[c]][[m]][d] * training.x[[c]][[m]][d,i]),na.rm = T) + 
        lambda.g * W[i,m,c] / norm(as.matrix(W[,m,c]),'F') +
        if (m==1) 2 * lambda.m * (W[i,m,c] - W[i,m+1,c]) else if (m==length(training.x[[c]])) 2 * lambda.m * (W[i,m,c] - W[i,m-1,c]) else 2 * lambda.m * (2*W[i,m,c] - W[i,m-1,c] - W[i,m+1,c]))))
}

# Hyperparameter tuning ------------------------------------------------------------

indicators   = ncol(X[[1]][[1]])  #number of technical analysis features
window       = 18                 #training window length
boot         = 6                  #first 6 months of data reserved for feature calculation
start_vector = runif(indicators * length(aktien) * window,-5,5) #random initial weights
lambda.g     = c(10^-4,10^-2,1,10,100,10^3,10^4)
lambda.m     = c(10^-4,10^-2,1,10,100,10^3,10^4)
grid         = expand.grid(lg = lambda.g, lm = lambda.m)

training.x   = list()
training.y   = list()
validation.x = list()
validation.y = list()
W            = list()   

for (c in seq(1,length(aktien))) {
  
  #creation of first training dataset of 18 months of X & Y
  training.x[[c]] = lapply((boot+1):(boot+window), function(m) X[[c]][[m]])
  training.y[[c]] = lapply((boot+1):(boot+window), function(m) Y[[c]][[m]])
  
  #creation of validation dataset of X & Y
  validation.x[[c]] = X[[c]][[boot+window+1]]
  validation.y[[c]] = Y[[c]][[boot+window+1]]
}

#initialize clusters for parallel computing
no_cores = detectCores() - 1
cl       = makeCluster(no_cores)
registerDoParallel(cl)

#optimization for all grid rows
out = foreach(i = 1:nrow(grid))  %dopar%  {
  optim(start_vector, objective, gradient, lambda.g = grid$lg[i], lambda.m = grid$lm[i], method = "L-BFGS-B")
}
stopCluster(cl)

#check algorithm convergence
for (i in seq(1,length(out))) {
  print(out[[i]]$convergence)
}

#save weights of last training month for every lambda combination
for (i in seq(1,length(out))) {
  f      = array(out[[i]]$par, c(indicators, window, length(aktien)))
  W[[i]] = cbind(sapply(1:length(aktien), function(c) f[,window,c]))
}

#compute profits on validation data 
profits = sapply(1:length(W), function(i) month.profit(W = W[[i]], X = validation.x, Y = validation.y))

#find optimal lambdas
opt.lambda = grid[which.max(profits),]


# Model training ----------------------------------------------------------

#first training window of 19 months (incl. validation month)
for (c in seq(1,length(aktien))) {
  
  #creation of training datasets X & Y incl. the validation set
  training.x[[c]] = lapply((boot+1):(boot+window+1), function(m) X[[c]][[m]])
  training.y[[c]] = lapply((boot+1):(boot+window+1), function(m) Y[[c]][[m]])
}

#optimization via L-BFGS
out1 = optim(runif(indicators * length(aktien) * length(training.x[[1]]),-5,5), objective, gradient, lambda.g = opt.lambda$lg, lambda.m = opt.lambda$lm, method = 'L-BFGS-B')

#initialize clusters for parallel computing
cl = makeCluster(no_cores)
registerDoParallel(cl)

#model training via sliding window approach, optimization via L-BFGS
TS = foreach (i=1:131) %dopar% {
  training.x = lapply(1:length(aktien), function(c) lapply((8+i):(25+i), function(m) X[[c]][[m]]))
  training.y = lapply(1:length(aktien), function(c) lapply((8+i):(25+i), function(m) Y[[c]][[m]]))
  optim(start_vector, objective, gradient, lambda.g = opt.lambda$lg, lambda.m = opt.lambda$lm, method = 'L-BFGS-B')
}
stopCluster(cl)

#check algorithm convergence
for (i in seq(1,length(TS))) {
  print(TS[[i]]$convergence)
}

We = list()   
#merge all training output
TS = c(list(out1),TS) 

#save produced weights of last month in each training window
for (i in seq(1,length(TS))) {
  if (i==1) f = array(TS[[i]]$par, c(indicators, window+1, length(aktien))) else f = array(TS[[i]]$par, c(indicators, window, length(aktien)))
  if (i==1) We[[i]] = cbind(sapply(1:length(aktien), function(c) f[, window+1, c])) else We[[i]] = cbind(sapply(1:length(aktien), function(c) f[, window, c])) 
}
