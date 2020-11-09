#再帰的にcellに入っている点が作るcellを消す
cell_cnct<-function(i, cell){
  
  if(length(cell[[i]]) > 1){
    
    cell<-cell[-cell[[i]][-1]]
    
  }
  
  if(i+1 < length(cell)){cell_cnct(i+1, cell)}
  
  return(cell)
  
}

#-------------------------------------------------------------
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
    speak <- bootstrap.homology.mk2(B,maxdim,maxscale)
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

#------------------------------------------------------
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
      peak[d,t] <- calc.landscape.peak(X=land[[d]], thresh = (band*(2*pi)/surface_nshpere(d)), tseq=tseq)
    }
  }
  
  dimnames(peak) <- list(paste0("dim",1:maxdim),paste0("sample",1:length(X)))
  bootstrap.summary <- list(peak=peak)
  bootstrap.summary <- append(bootstrap.summary,c(band=band,show.hole.density(peak)))
  class(bootstrap.summary) <- "smoothPhom"
  return(bootstrap.summary)
}

#--------------------------------------------------------------
#距離関数変更後のパーシステント図を返す
calc_dist_changed_pd<-function(X, maxdim, maxscale, th_rate=0.8, const_th=0, idx=0){
  
  require(TDA)
  require(tidyverse)
  
  if(const_th==0){thresh<-quantile_threshold(th_rate, X)}
  
  if(idx==0){
    
  cell<-cell_set2(X, thresh)
  cnct<-connect2(1, cell, all = 1:nrow(X))
  red<-reduce_points(X, cnct)
  
  idx<-1:nrow(X) %>% .[-red[[2]]]
  
  }
  
  X_dist<-dist(X) %>% as.matrix()
  X_dist[idx, ]<-X_dist[idx, ]-thresh
  X_dist[-idx, idx]<-X_dist[-idx, idx]-thresh
  X_dist[X_dist < 0]<-0
  
  filt<-ripsFiltration(X = X_dist, maxdimension = 2, maxscale = 3, dist = "arbitrary", library = "Dionysus", 
                             printProgress = T)
  
  pd<-filtrationDiag(filtration = filt, maxdimension = 2, library = "Dionysus", printProgress = T)
 
  return(pd)
   
}

#--------------------------------------------------------------
#距離行列において指定したインデックスの値を変化させる
dist_mat_change<-function(X_dist, idx, thresh){
  
  X_dist[idx, ]<-X_dist[idx, ]-thresh
  X_dist[-idx, idx]<-X_dist[-idx, idx]-thresh
  X_dist[idx, idx]<-X_dist[idx, idx]-thresh
  X_dist[X_dist < 0]<-0
  
  return(X_dist)
  
}

#-----------------------------------------------------------------
#距離操作量固定、操作点数の割合を変えてPDを計算する関数
#ratesは操作点数の割合の集合。すべて同一の割合にすれば割合を固定し、操作対象点を変えて計算できる。
#ratesの要素数分PDを計算
select_rate_change_pd<-function(X, rates, thresh){
  
  idx_list<-lapply(rates, function(rate)sample(nrow(X), nrow(X)*rate))
  
  X_dist<-dist(X) %>% as.matrix()
  
  X_dists<-lapply(idx_list, function(idx)dist_mat_change(X_dist = X_dist, idx = idx, thresh = thresh))
  
  pds<-lapply(X_dists, function(dist){
  
    pd<-ripsFiltration(X = dist, maxdimension = 2, maxscale = 3, dist = "arbitrary", library = "Dionysus", 
                       printProgress = T) %>% 
      
      filtrationDiag(filtration = ., maxdimension = 2, library = "Dionysus", printProgress = T)
    
    return(c(pd, list(idx)))
    
  })
  
  return(pds)
  
}

#-----------------------------------------------------------------
#操作対象点固定、操作量を変化させてPH計算する関数
#thesは操作量の集合
manupilate_dist_change_pd<-function(X, idx, thes){
  
  X_dist<-dist(X) %>% as.matrix()
  
  X_dists<-lapply(thes, function(idx)dist_mat_change(X_dist = X_dist, idx = idx, thresh = thes))
  
  pds<-lapply(X_dists, function(dist){
    
    pd<-ripsFiltration(X = dist, maxdimension = 2, maxscale = 3, dist = "arbitrary", library = "Dionysus", 
                       printProgress = T) %>% 
      
      filtrationDiag(filtration = ., maxdimension = 2, library = "Dionysus", printProgress = T)
    
    return(pd)
    
  })
  
  return(pds)
  
}

#-----------------------------------------------------------------------
#変化後の距離行列からサブサンプルを抽出。PDを計算する関数
#操作量固定
#sub_rateはサブサンプルの割合、n_pdは計算するPDの数
manupulated_dist_mat_subs_pd<-function(X, threth, sub_rate, n_pd){
  
  X_red<-cell_set2(x = X, thresh = threth) %>% 
         connect2(i = 1, cell_p = ., all = 1:nrow(X)) %>% 
         reduce_points(X, .)
  
  X_rme<-1:nrow(X) %>% .[-X_red[[2]]]
  
  X_ched_dist<-dist(X) %>% as.matrix() %>% 
               dist_mat_change(X_dist = ., idx = X_rme, thresh = thresh)
  
  pds<-lapply(1:n_pd, function(k){
    
    idx<-sample(nrow(X), nrow(X)*sub_rate)
    
    pd<-ripsFiltration(X = X_ched_dist[idx, idx], maxdimension = 2, maxscale = 3, dist = "arbitrary", library = "Dionysus", 
                       printProgress = T) %>% 
      
      filtrationDiag(filtration = ., maxdimension = 2, library = "Dionysus", printProgress = T)
    
    return(pd)
    
  })
  
  return(pds)
  
}

#---------------------------------------------
#ランドマーク点を決定する関数
#Wittness複体を参考に
#Xはポイントクラウドデータ、n_landはランドマーク点の数
#d_mat=TならXに距離行列を入れられる
landmark_points<-function(X, n_land, d_mat=F){
  
  if(d_mat){X_dist<-X}else{X_dist<-dist(X) %>% as.matrix()}
  
  l_idx<-sample(nrow(X), 1)
  
  l_idx<-which.max(X_dist[l_idx, ]) %>% c(., l_idx)

  
  for (i in 1:(n_land-2)) {

    l_idx<-apply(X_dist[-l_idx, l_idx], 1, min) %>% which.max() %>% attributes() %$% as.integer(.) %>% c(., l_idx)

  }
  
  return(l_idx)
  
}

#-------------------------------------------------------------
#ベッチ数自動推定関数群を距離行列変更に対応させる
#proposedMethodOnlyから変形
#witness複体のランドマーク点を使用

maxmin_distance_change_method <- function(X,maxdim,maxscale,samples, const.size=0, l_rate=0.15, n_vic=10, spar = seq(0,1,0.1)){
  
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
    speak <- maxmin_dist_changed_pl_peak_count(X = B, maxdim = maxdim, maxscale = maxscale, l_rate = l_rate, n_vic = n_vic, spar = spar)
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

#------------------------------------------------
#距離行列変更後、PH計算・PLの局所最大値をカウント
#bootstrap.homology.mk2から変形
#witness複体のランドマーク点を使用
#calc.landscape.peak(BootstrapHomology-mk1.R)をパッケージ化して置き換えるべし
#seephacm:::calc_diag_centroid(diag)からpersistence_weighted_mean(diag)へ変更
maxmin_dist_changed_pl_peak_count <-function(X, maxdim, maxscale, const.band=0, maximum.thresh = F, l_rate=0.15, n_vic=10, spar = seq(0,1,0.1)){
  require(TDA)
  
  if(!("bootsSamples" %in% class(X))) stop("input must be bootsSamples")
  peak <- matrix(0,maxdim,length(X))
  
  tseq <- seq(0,maxscale,length.out = 1000)
  diags <- lapply(X,function(x)maxmin_dist_changed_pd(x, maxdim, maxscale, l_rate, n_vic)[[1]])
  bands<-sapply(diags,function(diag)persistence_weighted_mean(diag))
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

#--------------------------------------------------------------
#距離関数変更後のパーシステント図を返す
#witness複体のランドマーク点に関する距離行列の要素を変化させる
#l_rate=ランドマーク点の割合、n_vics=近傍点の数
#PDとランドマーク点のインデックスを返す
#TDAstats(ripser)で書き換えた
maxmin_dist_changed_pd<-function(X, maxdim, maxscale, l_rate=0.15, n_vic=10){
  
  require(TDA)
  require(tidyverse)
  require(TDAstats)
  
  X_dist<-dist(X) %>% as.matrix()
  
  #ランドマーク点を求める。l_idx=ランドマーク点のインデックス
  l_idx<-landmark_points(X = X_dist, n_land = nrow(X)*l_rate, d_mat = T)
  
  #ランドマーク点の近傍n_vics点の距離の平均を求める
  vics_dmean<-sapply(l_idx, function(k){
    
    vic_dmean<-X_dist[k, ] %>% sort() %>% .[2:(nvic+1)] %>% mean()
    names(vic_dmean)<-k
    
    return(vic_dmean)
    
  })
  
  #ランドマーク点に関する距離行列の要素を変更
  for (i in 1:length(l_idx)) {
    
    X_dist[l_idx[i], ]<-X_dist[l_idx[i], ]-vics_dmean[i]/2
    X_dist[, l_idx[i]]<-X_dist[, l_idx[i]]-vics_dmean[i]/2
    
  }
  
  X_dist[X_dist < 0]<-0 
  
  pd<-TDAstats::calculate_homology(mat = X_dist, dim = maxdim, threshold = maxscale, format = "distmat")
    
  class(pd)<-"diagram"
  
  
  return(list(pd=pd, l_idx=l_idx))
  
}


#-----------------------------------------------
#TDAstasのPDからTDAのPD(diagramクラス)へ変換
pd_conv_stats2tda<-function(pd){
  
  class(pd)<-"diagram"
  attr(pd, "maxdimension")<-max(pd[,1])
  attr(pd, "scale")<-c(0, max(pd[,3]))
  
  return(pd)
  
}

#------------------------------------------------
#MPH(?)を計算する関数
#ランドマーク点に関する要素において、元々の距離rを使って1-exp(-(x/a)^2)に置き換える
#aはハイパラ
#l_rate=ランドマーク点の割合
#PDとランドマーク点のインデックス、計算時間を返す

multiresolut_homology<-function(X, maxdim, l_rate=0.3, a=1){
  
  X_dist<-dist(X) %>% as.matrix()
  
  #ランドマーク点を求める。l_idx=ランドマーク点のインデックス
  l_idx<-landmark_points(X = X_dist, n_land = nrow(X)*l_rate, d_mat = T)
  
  normed_Xdist<-X_dist/max(X_dist)
  
  for (i in l_idx) {
    
    normed_Xdist[i, ]<-1-exp(-(X_dist[i, ]/a)^2)
    normed_Xdist[, i]<-1-exp(-(X_dist[, i]/a)^2)
    
  }
  
  time<-system.time(pd<-TDAstats::calculate_homology(mat = normed_Xdist, dim = maxdim, threshold = 1, format = "distmat"))
  
  return(list(pd=pd, l_idx=l_idx, time=time))
  
}

#------------------------------------------------------
#図中のx-y点間に直線を引く関数
#lines関数を書き換えただけ
draw_line<-function(x, y){
  
  lines(c(x[1], y[1]), c(x[2], y[2]))
  
}

#--------------------------------------------------------------
#距離行列において指定したインデックスの値を変化させる
#全体は正規化。変化はFRIによる
#X_dist=距離行列, lands=ランドマーク点, eta=FRIのハイパラ
dist_fri_change<-function(X_dist, lands, eta){
  
  X_dist<-X_dist/max(X_dist)
  
  for (i in lands) {
    
    X_dist[i, ]<-1-exp(-(X_dist[i, ]/eta)^2)
    X_dist[, i]<-1-exp(-(X_dist[, i]/eta)^2)
    
  }
  
  X_dist[X_dist < 0]<-0
  
  return(X_dist)
  
}
