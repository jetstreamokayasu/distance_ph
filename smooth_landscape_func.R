#TDAstasを使ってCTIC2019手法を書き換え
smooth_landscape_method<-function(X,maxdim,maxscale,samples, const.size=0, spar=seq(0, 1, 0.1)){
  aggr1 <- matrix(0,length(X),1)
  aggr2 <- matrix(0,length(X),1)
  dimnames(aggr1) <- list(paste0("data-set", 1:length(X)), "proposed")
  dimnames(aggr2) <- dimnames(aggr1)
  
  for(t in 1:length(X)){
    
    cat("data set", t, "calculating\n")
    if(const.size==0){size<-X[[t]]$nsample*(4/5)}
    else{size<-const.size}
    
    B <- seephacm:::bootstrapper(X[[t]]$noizyX,size,samples)
    speak <- smoothed_landscape_homology(X = B, maxdim = maxdim, maxscale = maxscale, spar = spar)
    m5 <- sapply(1:maxdim,function(d)speak[[paste0("dim",d,"dhole")]])
    
    aggr1[t,1] <- m5[1]
  
    aggr2[t,1] <- m5[2]
  }
  
  aggrs <- list(aggr1,aggr2)
  aggrs <- append(aggrs,list(Xsize=sapply(1:length(X), function(l)nrow(X[[l]][["noizyX"]])),Xsamples=length(X),
                             Bsize=size,Bsamples=samples,
                             maxdim=maxdim,maxscale=maxscale))
  class(aggrs) <- "bettiComp"
  
  return(aggrs)
}

#--------------------------------------------------------
smoothed_landscape_homology<-function(X,maxdim,maxscale,const.band=0,maximum.thresh = F, spar=seq(0, 1, 0.1)){
  
  if(!("bootsSamples" %in% class(X))) stop("input must be bootsSamples")
  peak <- matrix(0,maxdim,length(X))
  
  tseq <- seq(0,maxscale,length.out = 1000)
  diags <- lapply(X,function(x)TDAstats::calculate_homology(mat = x, dim = maxdim, threshold = maxscale))
  bands<-sapply(diags,function(diag)seephacm:::calc_diag_centroid(diag)[1])
  print(bands)
  band <- ifelse(const.band==0, max(bands),const.band)
  print(band)
  
  for (t in 1:length(X)) {
    land <- lapply(1:maxdim,function(d)landscape(diags[[t]],dimension = d,KK = 1,tseq = tseq))
    if(maximum.thresh) band <- max(sapply(land,max))/4
    for(d in 1:maxdim){
      peak[d,t] <- calc.landscape.peak(X=land[[d]], thresh = (band/d), tseq=tseq, spar = spar)
    }
  }
  
  dimnames(peak) <- list(paste0("dim",1:maxdim),paste0("sample",1:length(X)))
  bootstrap.summary <- list(peak=peak)
  bootstrap.summary <- append(bootstrap.summary,band=band)
  class(bootstrap.summary) <- "smoothPhom"
  return(bootstrap.summary)
}