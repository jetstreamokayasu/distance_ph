#プロット用関数群まとめ

#PD描画関数

plot_diag<-function(pd, lim, ...){
  
  oldpar<-par(no.readonly = T)
  
  symb <- pd[, 1]
  for (i in seq(along = symb)) {
    if (symb[i] == 0) {
      symb[i] <- 16
    }
    else if (symb[i] == 1) {
      symb[i] <- 2
    }
    else if (symb[i] == 2) {
      symb[i] <- 5
    }
    else if (symb[i] == 5) {
      symb[i] <- 1
    }
  }
  
  col <- pd[, 1] + 1
  for (i in seq(along = pd[, 1])) {
    if (pd[i, 1] == 2) {
      col[i] <- 4
    }
    if (pd[i, 1] == 3) {
      col[i] <- 3
    }
  }
  
  if(missing(lim)){lim<-c(0, max(pd[, 2:3]))}
  
  par(mgp = c(2.2, 0.7, 0))
  plot(pd[,2], pd[,3], xlim = lim, ylim = lim, col = col, pch = symb, lwd = 2, bty = "l", 
       xlab = "Birth", ylab = "Death", cex.lab = 1.7, ...)
  abline(a = 0, b = 1)
  legend("bottomright", legend = paste0("dim", 0:(max(pd[,1]))), col=unique(col), pch=unique(symb), cex=1.5, pt.lwd = 2)
  
  par(oldpar)
  
}
