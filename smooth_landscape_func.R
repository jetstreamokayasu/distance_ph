#TDAstasを使ってCTIC2019手法を書き換え
#次元の数を2に限定しない形に書き換え
smooth_landscape_method<-function(X,maxdim,maxscale,samples, const.size=0, spar=seq(0, 1, 0.1)){
  
  aggrs<-lapply(1:maxdim, function(k){
    
    aggr<-matrix(0,length(X),1)
    dimnames(aggr) <- list(paste0("data-set", 1:length(X)), "proposed")
    
    return(aggr)
    
  })
  
  for(t in 1:length(X)){
    
    cat("data set", t, "calculating\n")
    if(const.size==0){size<-nrow(X[[t]])*(0.8)}
    else{size<-const.size}
    
    B <- seephacm:::bootstrapper(X[[t]],size,samples)
    speak <- smoothed_landscape_homology(X = B, maxdim = maxdim, maxscale = maxscale, spar = spar)
    m5 <- sapply(1:maxdim,function(d)speak[[paste0("dim",d,"mhole")]])
    
    for (i in 1:maxdim) {
      
      aggrs[[i]][t,1]<-m5[i]
      
    }
    
  }
  
  aggrs <- append(aggrs,list(Xsize=sapply(1:length(X), function(l)nrow(X[[l]])),Xsamples=length(X),
                             Bsize=size,Bsamples=samples,
                             maxdim=maxdim,maxscale=maxscale))
  class(aggrs) <- "bettiComp"
  
  return(aggrs)
}

#--------------------------------------------------------
#bootstrap.homology.mk2をTDAstatsを使う形に書き換え
smoothed_landscape_homology<-function(X,maxdim,maxscale,const.band=0,maximum.thresh = F, spar=seq(0, 1, 0.1)){
  
  start_t<-Sys.time()
  
  if(!("bootsSamples" %in% class(X))) stop("input must be bootsSamples")
  peak <- matrix(0,maxdim,length(X))
  
  tseq <- seq(0,maxscale,length.out = 1000)
  diags <- lapply(X,function(x)TDAstats::calculate_homology(mat = x, dim = maxdim, threshold = maxscale))
  bands<-sapply(diags,function(diag)persistence_weighted_mean(diag))
  print(bands)
  band <- ifelse(const.band==0, max(bands),const.band)
  print(band)
  
  for (t in 1:length(X)) {
    land <- lapply(1:maxdim,function(d)landscape(diags[[t]],dimension = d,KK = 1,tseq = tseq))
    if(maximum.thresh) band <- max(sapply(land,max))/4
    for(d in 1:maxdim){
      peak[d,t] <- calc.landscape.peak(X=land[[d]], thresh = (band*((2*pi)/surface_nshpere(d))), tseq=tseq, spar = spar)
    }
  }
  
  dimnames(peak) <- list(paste0("dim",1:maxdim),paste0("sample",1:length(X)))
  bootstrap.summary <- list(peak=peak)
  
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

#------------------------------------------------
#n次元単位球の表面積を求める関数
#r=半径
surface_nshpere<-function(n, r=1){
  
  s<-(2*pi^((n+1)/2)*(r^n))/gamma((n+1)/2)
  
  return(s)
  
}

#----------------------------------------------
#パーシステンスの平均を求める
#n次元パーシステンスに対して(n次元単位球面積/1次元単位球面積(=2*pi))をかけている
persistence_weighted_mean<-function(diag){
  
  if(class(diag)=="list"){diag<-diag[[1]]}
  
  maxdim<-max(diag[,1])
  diag <- diag[-which(diag[,1]==0),]
  
  centroid<-lapply(1:maxdim, function(k){
    
    per<-(diag[diag[,1]==k,3]-diag[diag[,1]==k,2])*(surface_nshpere(k)/(2*pi))
    return(per)
    
  })
  
  cpers<-unlist(centroid) %>% mean()
  
  names(cpers)<-"cpersistence"
  
  return(cpers)
  
}

#-------------------------------------------------------
#TDAstasを使ってCTIC2019手法を書き換え
#次元の数を2に限定しない形に書き換え
#parallel使用
#cluster設定をこちらで行うようにした
#この関数でparLapplyによりsmoothed_landscape_homologyを実行するようにした
smooth_landscape_method_paral<-function(X,maxdim,maxscale,samples, const.size=0, spar=seq(0, 1, 0.1)){
  
  cl <- makeCluster(4, outfile="")
  
  clusterEvalQ(cl,{
    library(phacm)
    library(tidyverse)
    library(myfs)
    library(seephacm)
    library(TDAstats)
    library(parallel)
    library(TDA)
  })
  
  clusterEvalQ(cl, {
    source('~/R/distance_ph/smooth_landscape_func.R', encoding = 'UTF-8')
    source('~/R/ph_jikken2/new-okayasu/BootstrapHomology-mk1.R', encoding = 'UTF-8')
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
    
    speak<-smoothed_landscape_homology(X = B, maxdim = maxdim, maxscale = maxscale, spar = spar)
    
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
  
  aggrs <- append(aggrs,list(Xsize=sapply(1:length(X), function(l)nrow(X[[l]])), Xsamples=length(X), 
                             Bsize=Bsize, Bsamples=samples, times=times, 
                             maxdim=maxdim,maxscale=maxscale))
  
  class(aggrs) <- "bettiComp"
  
  stopCluster(cl)
  
  return(aggrs)
}

#--------------------------------------------------------
#bootstrap.homology.mk2をTDAstatsを使う形に書き換え
#parallel使用
smoothed_landscape_homology_paral<-function(cl, X,maxdim,maxscale,const.band=0,maximum.thresh = F, spar=seq(0, 1, 0.1)){
  
  if(!("bootsSamples" %in% class(X))) stop("input must be bootsSamples")
  peak <- matrix(0,maxdim,length(X))
  
  tseq <- seq(0,maxscale,length.out = 1000)
  diags <- parLapply(cl, X, function(x)TDAstats::calculate_homology(mat = x, dim = maxdim, threshold = maxscale))
  bands<-sapply(diags,function(diag)persistence_weighted_mean(diag))
  print(bands)
  band <- ifelse(const.band==0, max(bands),const.band)
  print(band)
  
  for (t in 1:length(X)) {
    land <- lapply(1:maxdim,function(d)landscape(diags[[t]],dimension = d,KK = 1,tseq = tseq))
    if(maximum.thresh) band <- max(sapply(land,max))/4
    for(d in 1:maxdim){
      peak[d,t] <- calc.landscape.peak(X=land[[d]], thresh = (band*((2*pi)/surface_nshpere(d))), tseq=tseq, spar = spar)
    }
  }
  
  dimnames(peak) <- list(paste0("dim",1:maxdim),paste0("sample",1:length(X)))
  bootstrap.summary <- list(peak=peak)
  
  for(d in 1:maxdim){
    mhole<-mean(peak[d,])
    bootstrap.summary[[paste0("dim",d,"mhole")]] <- mhole
    print(paste0("dimension ",d,", ",round(mhole,digits = 2)," mean hole"))
  }
  
  bootstrap.summary <- append(bootstrap.summary,list(band=band))
  class(bootstrap.summary) <- "smoothPhom"
  
  return(bootstrap.summary)
}
