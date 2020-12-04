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
#func=PH計算用関数、(...)=funcで使う変数
#noizyX, nsampleを使わない
calc_distance_change_betti <- function(X,maxdim,maxscale,samples, const.size=0, spar = seq(0,1,0.1), ph_func, ...){
  
  aggrs<-lapply(1:maxdim, function(k){
    
    aggr<-matrix(0,length(X),1)
    dimnames(aggr) <- list(paste0("data-set", 1:length(X)), "proposed")
    
    return(aggr)
    
  })
  
  for(t in 1:length(X)){
    
    cat("data set", t, "calculating\n")
    if(const.size==0){size<-nrow(X[[t]])*0.8}else{size<-const.size}
    
    B <- seephacm:::bootstrapper(X[[t]],size,samples)
    speak <- distmat_changed_pl_peak_count(X = B, maxdim = maxdim, maxscale = maxscale, spar = spar, ph_func = ph_func, ...)
    m5 <- sapply(1:maxdim,function(d)speak[[paste0("dim",d,"mhole")]])
    
    for (i in 1:maxdim) {
      
      aggrs[[i]][t,1]<-m5[i]
      
    }
    
  }
  
  aggrs <- append(aggrs,list(Xsize=sapply(1:length(X), function(l)nrow(X[[l]])),Xsamples=length(X),
                             Bsize=size,Bsamples=samples,
                             maxdim=maxdim,maxscale=maxscale))
  class(aggrs) <- "dist_changed"
  
  return(aggrs)
  
}

#------------------------------------------------------
#距離行列変更後、PH計算・PLの局所最大値をカウント------
#bootstrap.homology.mk2から変形
#witness複体のランドマーク点を使用
#calc.landscape.peak(BootstrapHomology-mk1.R)をパッケージ化して置き換えるべし
#seephacm:::persistence_weighted_mean(diag)使用
#funcで指定して任意のPH計算関数が使えるように。extra_vはfuncで使う変数リスト
distmat_changed_pl_peak_count <-function(X, maxdim, maxscale, const.band=0, maximum.thresh = F, spar = seq(0,1,0.1), ph_func, ...){
  require(TDA)
  
  start_t<-Sys.time()
  
  if(!("bootsSamples" %in% class(X))) stop("input must be bootsSamples")
  peak <- matrix(0,maxdim,length(X))
  
  tseq <- seq(0,maxscale,length.out = 1000)
  diags <- lapply(X,function(x)ph_func(X=x, maxdim=maxdim, maxscale=maxscale, ...)[["pd"]])
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
  
  end_t<-Sys.time()
  
  bootstrap.summary <- append(bootstrap.summary,list(band=band, time=difftime(end_t, start_t, units = "secs")))
  
  class(bootstrap.summary) <- "smoothPhom"
  return(bootstrap.summary)
}


#-------------------------------------------------------------
#ベッチ数自動推定関数群をWVRによる距離行列変更に対応させる(parallel使用)----
#proposedMethodOnlyから変形
#witness複体のランドマーク点を使用
#可変数引数によってPH計算を引数に指定するだけで任意のものを使えるように
#func=PH計算用関数、...=funcで使う変数
#noizyX, nsampleを使わない
#parallel使用
#cluster設定をこちらで行うようにした
#この関数でparLapplyによりdistmat_changed_pl_peak_countを実行するようにした
calc_distance_change_betti_paral <- function(X,maxdim,maxscale,samples, const.size=0, spar = seq(0,1,0.1), ph_func, ...){
  
  cl <- makeCluster(4, outfile="")
  
  clusterEvalQ(cl,{
    library(phacm)
    library(tidyverse)
    library(myfs)
    library(seephacm)
    library(TDAstats)
    library(TDA)
  })
  
  clusterEvalQ(cl, {
    source('~/R/distance_ph/auto_estimate_betti_functions.R', encoding = 'UTF-8')
    source('~/R/distance_ph/dist_ch_func.R', encoding = 'UTF-8')
    source('~/R/ph_jikken2/new-okayasu/BootstrapHomology-mk1.R', encoding = 'UTF-8')
    source('~/R/distance_ph/smooth_landscape_func.R', encoding = 'UTF-8')
  })
  
  aggrs<-lapply(1:maxdim, function(k){
    
    aggr<-matrix(0,length(X),1)
    dimnames(aggr) <- list(paste0("data-set", 1:length(X)), "proposed")
    
    return(aggr)
    
  })
  
  #サブサンプルリスト作成
  B_list <- lapply(X, function(Y){
    
    if(const.size==0){size<-nrow(Y)*(0.8)}
    else{size<-const.size}
    
    B<-seephacm:::bootstrapper(Y,size,samples)
    
    return(B)
    
  })
  
  speak_list<-parLapply(cl, B_list, function(B){
    
    speak<-distmat_changed_pl_peak_count(X = B, maxdim = maxdim, maxscale = maxscale, spar = spar, ph_func = ph_func, ...)
    
    m5 <- sapply(1:maxdim,function(d)speak[[paste0("dim",d,"mhole")]]) 
    
    o_mat<-matrix(0, 1, maxdim+1)
    colnames(o_mat)<-c(paste0("H", 1:maxdim), "time")
    o_mat[1, ]<-c(m5, speak[["time"]])
    
    #parallelフォルダにcsvを出力
    write.csv(as.data.frame(o_mat), file = paste0("./parallel/", "H", maxdim, "_", 
                                                  gsub("\\.", "", round(speak[[paste0("dim", maxdim, "mhole")]], digits = 2)), 
                                                  "_", format(Sys.time(), "%m%d_%H%M"), ".csv"))
    
    return(speak)
    
  })
  
  for(t in 1:length(X)){
    
    m5 <- sapply(1:maxdim,function(d)speak_list[[t]][[paste0("dim",d,"mhole")]])
    
    for (i in 1:maxdim) {
      
      aggrs[[i]][t,1]<-m5[i]
      
    }

  }
  
  #サブサンプル点数
  Bsize<-sapply(B_list, function(B)nrow(B[[1]]))
  
  #計算時間
  times<-sapply(speak_list, function(s)s[["time"]])
  
  aggrs <- append(aggrs,list(Xsize=sapply(1:length(X), function(l)nrow(X[[l]])),Xsamples=length(X),
                             Bsize=Bsize,Bsamples=samples, times=times,
                             maxdim=maxdim,maxscale=maxscale))
  
  class(aggrs) <- "dist_changed"
  
  stopCluster(cl)
  
  return(aggrs)
  
}

#------------------------------------------------------
#距離行列変更後、PH計算・PLの局所最大値をカウント(parallel使用)------
#bootstrap.homology.mk2から変形
#witness複体のランドマーク点を使用
#calc.landscape.peak(BootstrapHomology-mk1.R)をパッケージ化して置き換えるべし
#seephacm:::persistence_weighted_mean(diag)使用
#funcで指定して任意のPH計算関数が使えるように。extra_vはfuncで使う変数リスト
#parallel使用
distmat_changed_pl_peak_count_paral<-function(cl, X, maxdim, maxscale, const.band=0, maximum.thresh = F, spar = seq(0,1,0.1), ph_func, ...){
  require(TDA)
  
  if(!("bootsSamples" %in% class(X))) stop("input must be bootsSamples")
  peak <- matrix(0,maxdim,length(X))
  
  tseq <- seq(0,maxscale,length.out = 1000)
  diags <- parLapply(cl, X, function(x)ph_func(X=x, maxdim=maxdim, maxscale=maxscale, ...)[["pd"]])
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
