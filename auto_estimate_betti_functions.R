#ベッチ数自動推定関数群をまとめたスクリプト

#-------------------------------------------------------------
#ベッチ数自動推定関数群をFRIによる距離行列変更に対応させる----
#proposedMethodOnlyから変形
#witness複体のランドマーク点を使用

fri_distance_change_method <- function(X,maxdim,maxscale,samples, const.size=0, l_rate=0.15, eta=1, spar = seq(0,1,0.1)){
  
  aggrs<-lapply(1:maxdim, function(k){
    
    aggr<-matrix(0,length(X),1)
    dimnames(aggr) <- list(paste0("data-set", 1:length(X)), "proposed")
    
    return(aggr)
    
  })
  
  for(t in 1:length(X)){
    
    cat("data set", t, "calculating\n")
    if(const.size==0){size<-X[[t]]$nsample*(4/5)}
    else{size<-const.size}
    
    B <- seephacm:::bootstrapper(X[[t]]$noizyX,size,samples)
    speak <- fri_dist_changed_pl_peak_count(X = B, maxdim = maxdim, maxscale = maxscale, l_rate = l_rate, eta = eta, spar = spar)
    m5 <- sapply(1:maxdim,function(d)speak[[paste0("dim",d,"mhole")]])
    
    for (i in 1:maxdim) {
      
      aggrs[[i]][t,1]<-m5[i]
      
    }
    
  }
  
  aggrs <- append(aggrs,list(Xsize=sapply(1:length(X), function(l)nrow(X[[l]][["noizyX"]])),Xsamples=length(X),
                             Bsize=size,Bsamples=samples,
                             maxdim=maxdim,maxscale=maxscale))
  class(aggrs) <- "bettiComp"
  
  return(aggrs)
  
}


#----------------------------------------------------
#距離行列変更後、PH計算・PLの局所最大値をカウント----
#bootstrap.homology.mk2から変形
#witness複体のランドマーク点を使用
#calc.landscape.peak(BootstrapHomology-mk1.R)をパッケージ化して置き換えるべし
#seephacm:::calc_diag_centroid(diag)からpersistence_weighted_mean(diag)へ変更
fri_dist_changed_pl_peak_count <-function(X, maxdim, maxscale, const.band=0, maximum.thresh = F, l_rate=0.15, eta=1, spar = seq(0,1,0.1)){
  require(TDA)
  
  if(!("bootsSamples" %in% class(X))) stop("input must be bootsSamples")
  peak <- matrix(0,maxdim,length(X))
  
  tseq <- seq(0,maxscale,length.out = 1000)
  diags <- lapply(X,function(x)multiresolut_homology(x, maxdim, l_rate, eta)[[1]])
  bands<-sapply(diags,function(diag)seephacm:::persistence_weighted_mean(diag))
  print(bands)
  band <- ifelse(const.band==0, max(bands),const.band)
  print(band)
  
  for (t in 1:length(X)) {
    land <- lapply(1:maxdim,function(d)landscape(diags[[t]],dimension = d,KK = 1,tseq = tseq))
    if(maximum.thresh) band <- max(sapply(land,max))/4
    for(d in 1:maxdim){
      peak[d,t] <- calc.landscape.peak(X=land[[d]], thresh = (band*(2*pi)/surface_nshpere(d)), tseq=tseq, spar = spar)
    }
  }
  
  dimnames(peak) <- list(paste0("dim",1:maxdim),paste0("sample",1:length(X)))
  bootstrap.summary <- list(peak=peak)
  bootstrap.summary <- append(bootstrap.summary,c(band=band,show.hole.density(peak)))
  class(bootstrap.summary) <- "smoothPhom"
  return(bootstrap.summary)
}



#-------------------------------------------------------------
#ベッチ数自動推定関数群をWVRによる距離行列変更に対応させる----
#proposedMethodOnlyから変形
#witness複体のランドマーク点を使用
#可変数引数によってPH計算を引数に指定するだけで任意のものを使えるように
#func=PH計算用関数、...=funcで使う変数
calc_distance_change_betti <- function(X,maxdim,maxscale,samples, const.size=0, spar = seq(0,1,0.1), ph_func, ...){
  
  extra_v<-list(...)
  
  aggrs<-lapply(1:maxdim, function(k){
    
    aggr<-matrix(0,length(X),1)
    dimnames(aggr) <- list(paste0("data-set", 1:length(X)), "proposed")
    
    return(aggr)
    
  })
  
  for(t in 1:length(X)){
    
    cat("data set", t, "calculating\n")
    if(const.size==0){size<-X[[t]]$nsample*(4/5)}else{size<-const.size}
    
    B <- seephacm:::bootstrapper(X[[t]]$noizyX,size,samples)
    speak <- distmat_changed_pl_peak_count(X = B, maxdim = maxdim, maxscale = maxscale, spar = spar, ph_func = ph_func, extra_v = extra_v)
    m5 <- sapply(1:maxdim,function(d)speak[[paste0("dim",d,"mhole")]])
    
    for (i in 1:maxdim) {
      
      aggrs[[i]][t,1]<-m5[i]
      
    }
    
  }
  
  aggrs <- append(aggrs,list(Xsize=sapply(1:length(X), function(l)nrow(X[[l]][["noizyX"]])),Xsamples=length(X),
                             Bsize=size,Bsamples=samples,
                             maxdim=maxdim,maxscale=maxscale))
  class(aggrs) <- "bettiComp"
  
  return(aggrs)
  
}

#------------------------------------------------------
#距離行列変更後、PH計算・PLの局所最大値をカウント------
#bootstrap.homology.mk2から変形
#witness複体のランドマーク点を使用
#calc.landscape.peak(BootstrapHomology-mk1.R)をパッケージ化して置き換えるべし
#seephacm:::persistence_weighted_mean(diag)使用
#funcで指定して任意のPH計算関数が使えるように。extra_vはfuncで使う変数リスト
distmat_changed_pl_peak_count <-function(X, maxdim, maxscale, const.band=0, maximum.thresh = F, spar = seq(0,1,0.1), ph_func, extra_v){
  require(TDA)
  
  if(!("bootsSamples" %in% class(X))) stop("input must be bootsSamples")
  peak <- matrix(0,maxdim,length(X))
  
  tseq <- seq(0,maxscale,length.out = 1000)
  diags <- lapply(X,function(x)ph_func(X=x, maxdim=maxdim, maxscale=maxscale, extra_v=extra_v)[["pd"]])
  bands<-sapply(diags,function(diag)seephacm:::persistence_weighted_mean(diag))
  print(bands)
  band <- ifelse(const.band==0, max(bands),const.band)
  print(band)
  
  for (t in 1:length(X)) {
    land <- lapply(1:maxdim,function(d)landscape(diags[[t]],dimension = d,KK = 1,tseq = tseq))
    if(maximum.thresh) band <- max(sapply(land,max))/4
    for(d in 1:maxdim){
      peak[d,t] <- calc.landscape.peak(X=land[[d]], thresh = (band*(2*pi)/surface_nshpere(d)), tseq=tseq, spar = spar)
    }
  }
  
  dimnames(peak) <- list(paste0("dim",1:maxdim),paste0("sample",1:length(X)))
  bootstrap.summary <- list(peak=peak)
  
  #show.hole.density関数を使わない。密度推定によりベッチ数を求めないため
  for(d in 1:maxdim){
    mhole<-mean(peak[d,])
    bootstrap.summary[[paste0("dim",d,"mhole")]] <- mhole
    print(paste0("dimension ",d,", ",round(mhole,digits = 2)," mean hole"))
  }
  
  bootstrap.summary <- append(bootstrap.summary,list(band=band))
  
  class(bootstrap.summary) <- "smoothPhom"
  return(bootstrap.summary)
}


#-------------------------------------------------------------
#ベッチ数自動推定関数群をWVRによる距離行列変更に対応させる(parallel使用)----
#proposedMethodOnlyから変形
#witness複体のランドマーク点を使用
#可変数引数によってPH計算を引数に指定するだけで任意のものを使えるように
#func=PH計算用関数、...=funcで使う変数
calc_paral_distance_change_betti <- function(X,maxdim,maxscale,samples, const.size=0, spar = seq(0,1,0.1), ph_func, ...){
  
  extra_v<-list(...)
  
  aggrs<-lapply(1:maxdim, function(k){
    
    aggr<-matrix(0,length(X),1)
    dimnames(aggr) <- list(paste0("data-set", 1:length(X)), "proposed")
    
    return(aggr)
    
  })
  
  for(t in 1:length(X)){
    
    cat("data set", t, "calculating\n")
    if(const.size==0){size<-X[[t]]$nsample*(4/5)}else{size<-const.size}
    
    B <- seephacm:::bootstrapper(X[[t]]$noizyX,size,samples)
    speak <- distmat_changed_pl_peak_count_paral(X = B, maxdim = maxdim, maxscale = maxscale, spar = spar, ph_func = ph_func, extra_v = extra_v)
    m5 <- sapply(1:maxdim,function(d)speak[[paste0("dim",d,"mhole")]])
    
    for (i in 1:maxdim) {
      
      aggrs[[i]][t,1]<-m5[i]
      
    }
    
  }
  
  aggrs <- append(aggrs,list(Xsize=sapply(1:length(X), function(l)nrow(X[[l]][["noizyX"]])),Xsamples=length(X),
                             Bsize=size,Bsamples=samples,
                             maxdim=maxdim,maxscale=maxscale))
  class(aggrs) <- "bettiComp"
  
  return(aggrs)
  
}

#------------------------------------------------------
#距離行列変更後、PH計算・PLの局所最大値をカウント(parallel使用)------
#bootstrap.homology.mk2から変形
#witness複体のランドマーク点を使用
#calc.landscape.peak(BootstrapHomology-mk1.R)をパッケージ化して置き換えるべし
#seephacm:::persistence_weighted_mean(diag)使用
#funcで指定して任意のPH計算関数が使えるように。extra_vはfuncで使う変数リスト
distmat_changed_pl_peak_count_paral <-function(X, maxdim, maxscale, const.band=0, maximum.thresh = F, spar = seq(0,1,0.1), ph_func, extra_v){
  require(TDA)
  
  if(!("bootsSamples" %in% class(X))) stop("input must be bootsSamples")
  peak <- matrix(0,maxdim,length(X))
  
  cl <- makeCluster(4, outfile="")
  
  clusterEvalQ(cl,{
    library(phacm)
    library(tidyverse)
    library(myfs)
    library(seephacm)
  })
  
  clusterEvalQ(cl, {
    source('~/R/distance_ph/auto_estimate_betti_functions.R', encoding = 'UTF-8')
    source('~/R/distance_ph/dist_ch_func.R', encoding = 'UTF-8')
    source('~/R/ph_jikken2/new-okayasu/BootstrapHomology-mk1.R', encoding = 'UTF-8')
  })
  
  tseq <- seq(0,maxscale,length.out = 1000)
  diags <- parLapply(cl, X, function(x)ph_func(X=x, maxdim=maxdim, maxscale=maxscale, extra_v=extra_v)[["pd"]])
  bands<-sapply(diags,function(diag)seephacm:::persistence_weighted_mean(diag))
  print(bands)
  band <- ifelse(const.band==0, max(bands),const.band)
  print(band)
  
  for (t in 1:length(X)) {
    land <- lapply(1:maxdim,function(d)landscape(diags[[t]],dimension = d,KK = 1,tseq = tseq))
    if(maximum.thresh) band <- max(sapply(land,max))/4
    for(d in 1:maxdim){
      peak[d,t] <- calc.landscape.peak(X=land[[d]], thresh = (band*(2*pi)/surface_nshpere(d)), tseq=tseq, spar = spar)
    }
  }
  
  dimnames(peak) <- list(paste0("dim",1:maxdim),paste0("sample",1:length(X)))
  bootstrap.summary <- list(peak=peak)
  
  #show.hole.density関数を使わない。密度推定によりベッチ数を求めないため
  for(d in 1:maxdim){
    mhole<-mean(peak[d,])
    bootstrap.summary[[paste0("dim",d,"mhole")]] <- mhole
    print(paste0("dimension ",d,", ",round(mhole,digits = 2)," mean hole"))
  }
  
  bootstrap.summary <- append(bootstrap.summary,list(band=band))
  
  class(bootstrap.summary) <- "smoothPhom"
  
  stopCluster(cl)
  
  return(bootstrap.summary)
}
