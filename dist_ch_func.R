#再帰的にcellに入っている点が作るcellを消す
cell_cnct<-function(i, cell){
  
  if(length(cell[[i]]) > 1){
    
    cell<-cell[-cell[[i]][-1]]
    
  }
  
  if(i+1 < length(cell)){cell_cnct(i+1, cell)}
  
  return(cell)
  
}

#ベッチ数自動推定関数群を距離行列変更に対応させる
#proposedMethodOnlyから変形
distance_change_method <- function(X,maxdim,maxscale,samples, const.size=0){
  aggr1 <- matrix(0,length(X),1)
  aggr2 <- matrix(0,length(X),1)
  dimnames(aggr1) <- list(paste0("data-set", 1:length(X)),"proposed")
  dimnames(aggr2) <- dimnames(aggr1)
  
  for(t in 1:length(X)){
    
    cat("data set", t, "calculating\n")
    if(const.size==0){size<-X[[t]]$nsample*(4/5)}
    B <- bootstrapper(X[[t]]$noizyX,size,samples)
    speak <- bootstrap.homology.mk3(B,maxdim,maxscale)
    m5 <- sapply(1:maxdim,function(d)speak[[paste0("dim",d,"dhole")]])

    aggr1[t,1] <- m5[1]

    aggr2[t,1] <- m5[2]

  }

  aggrs <- list(aggr1,aggr2)
  
  Xsize<-sapply(1:length(X), function(l){return(nrow(X[[l]][["noizyX"]]))})
  if(const.size==0){Bsize<-sapply(1:length(X), function(l){return(nrow(X[[l]][["noizyX"]])*(4/5))})}
  else{Bsize<-const.size}
  
  aggrs <- append(aggrs,list(Xsize=Xsize,Xsamples=length(X),
                             Bsize=Bsize,Bsamples=samples,
                             maxdim=maxdim,maxscale=maxscale))
  class(aggrs) <- "bettiComp"
  
  return(aggrs)
}

#距離行列変更後、PH計算
#bootstrap.homology.mk2から変形
dist_changed_pl_peak_count <-function(X,maxdim,maxscale,const.band=0,maximum.thresh = F){
  require(TDA)
  # require(pracma)
  if(!("bootsSamples" %in% class(X))) stop("input must be bootsSamples")
  peak <- matrix(0,maxdim,length(X))
  # band <- ifelse(const.band > 0,const.band,hausdInterval(X, m=sample.size, B=times, alpha = (1-confidence)))
  tseq <- seq(0,maxscale,length.out = 1000)
  diags <- lapply(X,function(x)calc_dist_changed_pd(x,maxdim,maxscale))
  print(sapply(diags,function(diag)calcDiagCentroid.mk2(diag)[1]))
  band <- ifelse(const.band==0,max(sapply(diags,function(diag)calcDiagCentroid.mk2(diag)[1])),const.band)
  print(band)
  
  for (t in 1:length(X)) {
    land <- lapply(1:maxdim,function(d)landscape(diags[[t]][[1]],dimension = d,KK = 1,tseq = tseq))
    if(maximum.thresh) band <- max(sapply(land,max))/4
    for(d in 1:maxdim){
      peak[d,t] <- calc.landscape.peak(X=land[[d]], thresh = (band/d), tseq=tseq)
    }
  }
  
  dimnames(peak) <- list(paste0("dim",1:maxdim),paste0("sample",1:length(X)))
  bootstrap.summary <- list(peak=peak)
  bootstrap.summary <- append(bootstrap.summary,c(band=band,show.hole.density(peak)))
  class(bootstrap.summary) <- "smoothPhom"
  return(bootstrap.summary)
}

#距離関数変更後のパーシステント図を返す
calc_dist_changed_pd<-function(X, maxdim, maxscale, th_rate=0.8){
  
  require(TDA)
  
  thesh<-quantile_threshold(th_rate, X)
  cell<-cell_set(X, thresh)
  cnct<-connect2(1, cell, all = 1:nrow(X))
  red<-reduce_points(X, cnct)
  
  idx<-1:nrow(X) %>% .[-red[[2]]]
  
  X_dist<-dist(X) %>% as.matrix()
  X_dist[idx, ]<-X_dist[idx, ]-thresh
  X_dist[-idx, idx]<-X_dist[-idx, idx]-thresh
  X_dist[X_dist < 0]<-0
  
  filt<-ripsFiltration(X = X_dist, maxdimension = 2, maxscale = 3, dist = "arbitrary", library = "Dionysus", 
                             printProgress = T)
  
  pd<-filtrationDiag(filtration = filt, maxdimension = 2, library = "Dionysus", printProgress = T)
 
  return(pd)
   
}