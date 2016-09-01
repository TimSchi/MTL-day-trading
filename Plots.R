
##
## Plot generation
##

for (i in seq(1,length(stocks))) {
  index = names(stocks)[[i]]
  c     = 19   #indicates random company in index
  d     = dates[[index]][[c]][which(dates[[index]][[c]]=='2002-05-01'):length(dates[[index]][[c]])]
  
  #plot simulation results
  #initialize plot sheet  
  pdf(file  = file.path('C:/Users/.../Documents/',index,'_sim1.pdf',fsep = ''))
  par(mfrow = c(2,1))
  
  #TC, Positional 
  plot(P[[i]][[c]], type = 'l', col = 1, ylim = c(-1.3,4), main = 'TC, Positional', ylab = '', xlab = 'Dates', xaxt = 'n')
  axis(1, at = seq(1,length(d), by = 400), labels = d[seq(1,length(d), by = 400)], tick = T, las = 1, cex.axis = 0.7)
  lines(L.TC[[i]][[c]][,1], col = 2)
  lines(L.TC[[i]][[c]][,2], col = 3)
  lines(cumsum(y[[i]][[c]]), col = 4)
  legend('topleft', c('P value','Cash balance','Stock balance','Cumulated daily return'), fill = c(1:4), cex = 0.65)
  
  #TC, Lot 
  plot(P[[i]][[c]], type = 'l', col = 1, ylim = c(-1.3,3), main = 'TC, Lot', ylab = '', xlab = 'Dates', xaxt = 'n')
  axis(1, at = seq(1,length(d), by = 400), labels = d[seq(1,length(d), by = 400)], tick = T, las = 1, cex.axis = 0.7)
  lines(L.TC[[i]][[c]][,7], col=2)
  lines(L.TC[[i]][[c]][,8], col=3)
  lines(cumsum(y[[i]][[c]]), col=4)
  legend('topleft', c('P value','Cash balance','Stock balance','Cumulated daily return'), fill = c(1:4), cex = 0.65)
  
  dev.off()
  
  #initialize plot sheet 
  pdf(file  = file.path('C:/Users/.../Documents/',index,'_sim2.pdf',fsep = ''))
  par(mfrow = c(2,1))
  
  #TC, Rebalance 
  plot(P[[i]][[c]], type = 'l', col = 1, ylim = c(-1.3,3), main = 'TC, Rebalance', ylab = '', xlab = 'Dates', xaxt = 'n')
  axis(1, at = seq(1,length(d), by = 400), labels = d[seq(1,length(d), by = 400)], tick = T, las = 1, cex.axis = 0.7)
  lines(L.TC[[i]][[c]][,3], col = 2)
  lines(L.TC[[i]][[c]][,4], col = 3)
  lines(cumsum(y[[i]][[c]]), col = 4)
  legend('topleft', c('P value','Cash balance','Stock balance','Cumulated daily return'), fill = c(1:4), cex = 0.65)
  
  #TC, Rebalance, Lot 
  plot(P[[i]][[c]], type = 'l', col = 1, ylim = c(-1.3,3), main = 'TC, Rebalance, Lot', ylab = '', xlab = 'Dates', xaxt = 'n')
  axis(1, at = seq(1,length(d), by = 400), labels = d[seq(1,length(d), by = 400)], tick = T, las = 1, cex.axis = 0.7)
  lines(L.TC[[i]][[c]][,5], col = 2)
  lines(L.TC[[i]][[c]][,6], col = 3)
  lines(cumsum(y[[i]][[c]]), col = 4)
  legend('topleft', c('P value','Cash balance','Stock balance','Cumulated daily return'), fill = c(1:4), cex = 0.65)
  
  dev.off()
  
  v = vector()
  
  for (c in seq(1, length(stocks[[i]]))) {
    #generate vector of end balances from TC, Positional strategy
    v = append(v, tail(L.TC[[i]][[c]][,1],1) + tail(L.TC[[i]][[c]][,2],1))
  }  
  
  #initialize plot sheet
  pdf(file  = file.path('C:/Users/.../Documents/',index,'_comp_bal_tradpos.pdf',fsep = ''))
  par(mfrow = c(2,1))
  
  #histogram of stock end balances 
  plot(v, type = 'h', xaxt = 'n', xlab = 'Company', ylab = 'End balance')
  abline(h = 1, col = 1, lty = 3)
  axis(1, at = seq(1,length(v)), labels = stocks[[index]], tick = T, las = 2, cex.axis = 0.7)
  
  #random sample of stocks
  a = 1
  b = 12
  c = 19
  
  #return curves
  plot(cumsum(y[[index]][[a]]), type = 'l', xaxt = 'n', xlab = 'Dates', ylab = 'y', col = 1, ylim = c(min(cumsum(y[[index]][[a]]), cumsum(y[[index]][[b]]), cumsum(y[[index]][[c]])), max(cumsum(y[[index]][[a]]), cumsum(y[[index]][[b]]), cumsum(y[[index]][[c]]))))
  lines(cumsum(y[[index]][[b]]), type = 'l', col = 2)
  lines(cumsum(y[[index]][[c]]), type = 'l', col = 3)
  abline(h = 0)
  axis(1, at = seq(1,length(d), by = 400), labels = d[seq(1,length(d), by = 400)], tick = T, las = 1, cex.axis = 0.7)
  legend('topleft', c(stocks[[index]][a], stocks[[index]][b], stocks[[index]][c]), fill = c(1:3), cex = 0.65)
  
  dev.off()
}

# Evolution of testing set profits -----------------------------------------------

#initialize plot sheet
pdf(file = file.path('C:/Users/.../Documents/windowprofits.pdf',fsep = ''))
plot(cumsum(window.profits), type = 'l', xlab = 'Window/Month', ylab = 'Profit', xaxt = 'n')
da = seq(as.Date("2002/06/01"), as.Date("2013/05/01"), by = "month")
axis(1, at = seq(1,length(da), by = 24),labels = da[seq(1,length(da), by = 24)], tick = T, las = 1, cex.axis = 0.8)
dev.off()


# Signals -----------------------------------------------------------------

#random stocks for the plots
a = 20
b = 4
c = 14
d = 1

# Model MT 3R signals-------------------------------------------------------------

#initialize plot sheet
pdf(file = file.path('C:/Users/.../Documents/3R_pvalues.pdf',fsep = ''))
par(mfrow=c(2,2))

for (i in seq(1,length(stocks))) {
  index = names(stocks)[[i]]  
  da    = dates[[index]][[a]][which(dates[[index]][[a]]=='2002-05-01'):length(dates[[index]][[a]])]
  plot(P[[i]][[a]], type = 'l', ylim = c(-1.1,1.8), main = index, xlab = 'Dates', ylab = '', xaxt = 'n', col = 1)
  lines(P[[i]][[b]], col = 2)
  lines(P[[i]][[c]], col = 3)
  lines(P[[i]][[d]], col = 4)
  axis(1, at = seq(1,length(da), by = 400), labels = da[seq(1,length(da), by = 400)], tick = T, las = 1, cex.axis = 0.7)
  legend('topleft', c(stocks[[index]][a], stocks[[index]][b], stocks[[index]][c], stocks[[index]][d]), fill = c(1:4), cex = 0.65)
  
}
dev.off()

# Model MT GR signals ---------------------------------------------------------------

#initialize plot sheet
pdf(file = file.path('C:/Users/.../Documents/NR_pvalues.pdf',fsep = ''))
par(mfrow=c(2,2))

for (i in seq(1,length(stocks))) {
  index = names(stocks)[[i]] 
  da    = dates[[index]][[a]][which(dates[[index]][[a]]=='2002-05-01'):length(dates[[index]][[a]])]
  plot(P[[i]][[a]], type = 'l', ylim = c(-1.1,1.8), main = index, xlab = 'Dates', ylab = '', xaxt = 'n', col = 1)
  lines(P[[i]][[b]], col = 2)
  lines(P[[i]][[c]], col = 3)
  lines(P[[i]][[d]], col = 4)
  axis(1, at = seq(1,length(da), by = 400), labels = da[seq(1,length(da), by = 400)], tick = T, las = 1, cex.axis = 0.7)
  legend('topleft', c(stocks[[index]][a], stocks[[index]][b], stocks[[index]][c], stocks[[index]][d]), fill = c(1:4), cex = 0.65)
  
}
dev.off()

# Model MT GR&MR signals ----------------------------------------------------------

#initialize plot sheet
pdf(file = file.path('C:/Users/.../Documents/GR+MR_pvalues.pdf',fsep = ''))
par(mfrow=c(2,2))

for (i in seq(1,length(stocks))) {
  index = names(stocks)[[i]]  
  da    = dates[[index]][[c]][which(dates[[index]][[c]]=='2002-05-02'):length(dates[[index]][[c]])]
  plot(P[[i]][[c]], type = 'l', ylim = c(-1.5,1.8), main = index, xlab = 'Dates', ylab = '', xaxt = 'n', col = 1)
  lines(cumsum(y[[i]][[c]]), col = 2)
  axis(1, at = seq(1,length(da), by = 400), labels = da[seq(1,length(da), by = 400)], tick = T, las = 1, cex.axis = 0.7)
  legend('topleft', c(stocks[[index]][c],'Cumulated daily return'), fill = c(1,2), cex = 0.65)
  
}
dev.off()
