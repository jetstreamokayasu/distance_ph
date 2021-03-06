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
#bootstrap.homology.mk2を使っているので対応していない

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
#距離行列変更後、PH計算--------------------------------
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
#距離関数変更後のパーシステント図を返す------------------------
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
#距離操作量固定、操作点数の割合を変えてPDを計算する関数-----------
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
#操作対象点固定、操作量を変化させてPH計算する関数----------------
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

#変化後の距離行列からサブサンプルを抽出。PDを計算する関数--------------------
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
#ランドマーク点を決定する関数-----------------
#Wittness複体を参考に
#Xはポイントクラウドデータ、n_landはランドマーク点の数
#d_mat=TならXに距離行列を入れられる
landmark_points<-function(X, n_land, d_mat=F){
  
  n_land<-as.integer(n_land)
  
  if(d_mat){X_dist<-X}else{X_dist<-dist(X) %>% as.matrix()}
  
  if(n_land == 0){return(numeric(0))}
    
  l_idx<-sample(nrow(X), 1)
  
  if(n_land >= 2){l_idx<-which.max(X_dist[l_idx, ]) %>% c(., l_idx)}

  if(n_land > 2){
    for (i in 1:(n_land-2)) {
  
      l_idx<-apply(X_dist[-l_idx, l_idx], 1, min) %>% which.max() %>% attributes() %$% as.integer(.) %>% c(., l_idx)
  
    }
  }
  
  return(l_idx)
  
}

#-------------------------------------------------------------
#ベッチ数自動推定関数群を距離行列変更に対応させる------------
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
    
    B <- usephacm:::bootstrapper(X[[t]]$noizyX,size,samples)
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
#usephacm:::calc_diag_centroid(diag)からpersistence_weighted_mean(diag)へ変更
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
as_diag<-function(pd){
  
  class(pd)<-"diagram"
  attr(pd, "maxdimension")<-max(pd[,1])
  attr(pd, "scale")<-c(0, max(pd[,3]))
  colnames(pd)<-c("dimension", "Birth", "Death")
  
  return(pd)
  
}

#------------------------------------------------
#MPH(?)を計算する関数----------------------------
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
#図中のx-y点間に直線を引く関数-------------------------
#lines関数を書き換えただけ
draw_line<-function(x, y, ...){
  
  lines(c(x[1], y[1]), c(x[2], y[2]), ...)
  
}


#--------------------------------------------------------------
#距離行列において指定したインデックスの値を変化させる----------
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


#--------------------------------------------------------------
#距離行列において指定したインデックスの値を変化させる
#変化はもとの距離に1-exp(-(d_ij/eta)^2)を掛ける(d_ij=元の距離)
#X_dist=距離行列, lands=ランドマーク点, eta=FRIのハイパラ

dist_wvr_change<-function(X_dist, lands, eta){
  
  if(length(lands)==0){return(X_dist)}
  
  X_chng_dist<-X_dist
  
  for (i in lands) {
    
    X_chng_dist[i, ]<-X_dist[i, ]*(1-exp(-(X_dist[i, ]/eta)^2))
    X_chng_dist[, i]<-X_dist[, i]*(1-exp(-(X_dist[, i]/eta)^2))
    
  }
  
  X_chng_dist[X_chng_dist < 0]<-0
  
  return(X_chng_dist)
  
}


#------------------------------------------------
#WPH(?)を計算する関数----------------------------
#ランドマーク点に関する要素において、元々の距離dに1-exp(-(d/eta)^2)を掛ける
#etaはハイパラ、l_rate=ランドマーク点の割合
#PDとランドマーク点のインデックス、計算時間を返す

weighted_homology<-function(X, maxdim, maxscale, l_rate, eta, ...){
  
  extra_v<-list(...)
  
  if(missing(l_rate)){l_rate<-extra_v$l_rate}
  if(missing(eta)){eta<-extra_v$eta}
  
  X_dist<-dist(X) %>% as.matrix()
  
  #ランドマーク点を求める。l_idx=ランドマーク点のインデックス
  l_idx<-landmark_points(X = X_dist, n_land = nrow(X)*l_rate, d_mat = T)
  
  X_chng_dist<-dist_wvr_change(X_dist = X_dist, lands = l_idx, eta = eta)
  
  time<-system.time(pd<-TDAstats::calculate_homology(mat = X_chng_dist, dim = maxdim, threshold = maxscale, format = "distmat"))
  
  pds<-list(pd=pd, l_idx=l_idx, time=time)
  
  attr(pds, "l_rate")<-l_rate
  attr(pds, "eta")<-eta
  
  return(pds)
  
}


#----------------------------------
#パーシステンス計算関数----
#usephacmの修正版
calc_per<-function (pd, dim){
  
  assertthat::assert_that((length(dim) == 1) && is_numeric(dim))
  
  pers <- pd[pd[, 1] == dim, 3] - pd[pd[, 1] == dim, 2]
  
  attr(pers, "pers_dim") <- dim
  
  return(pers)

}


#---------------------------------------
#試験的な関数-------------
#上から順に距離を入れ替える----------
dist_element_replace1<-function(pd, dim, distmat){
  
  h2_rows<-which(pd[,1]==dim)
  
  dist_cp<-distmat
  dist_cp[upper.tri(dist_cp)]<-0
  
  birth_e<-sapply(h2_rows, function(i)which(dist_cp == pd[i,2], arr.ind = T))
  
  dist_cp<-distmat
  
  for (i in 1:ncol(birth_e)) {
    
    c_eta<-pd[h2_rows[i], 3]/sqrt(log(10))
    
    dist_cp[birth_e[1,i], ]<-distmat[birth_e[1,i],]*( 1-exp( -(distmat[birth_e[1,i], ]/c_eta)^2 ) )
    
    dist_cp[, birth_e[1,i]]<-distmat[, birth_e[1,i]]*( 1-exp( -(distmat[, birth_e[1,i]]/c_eta)^2 ) )
    
    dist_cp[birth_e[2,i], ]<-distmat[birth_e[2,i], ]*( 1-exp( -(distmat[birth_e[2,i], ]/c_eta)^2 ) )
    
    dist_cp[, birth_e[2,i]]<-distmat[, birth_e[2,i]]*( 1-exp( -(distmat[, birth_e[2,i]]/c_eta)^2 ) )
    
    
  }
  
  return(dist_cp)
  
}

#---------------------------------------
#重複を取り除いて距離操作----
dist_element_replace_nodupl<-function(pd, dim, distmat){
  
  h2_rows<-which(pd[,1]==dim)
  
  dist_cp<-distmat
  dist_cp[upper.tri(dist_cp)]<-0
  
  birth_e<-sapply(h2_rows, function(i)which(dist_cp == pd[i,2], arr.ind = T))
  
  dist_cp<-distmat
  
  p_set<-c()
  for (j in 1:ncol(birth_e)) {
    
    pers<-pd[h2_rows[j], 3] - pd[h2_rows[j], 2]
    
    if(birth_e[1, j] %in% p_set[, 1]){
      
      if(pers > p_set[p_set[, 1]==birth_e[1, j], 3]){
        
        death<-pd[h2_rows[j], 3]
        
        p_set[p_set[, 1]==birth_e[1, j], ]<-c(birth_e[1, j], death, pers)
        
      }
      
    }else{
      
      death<-pd[h2_rows[j], 3]
      
      p_set<-rbind(p_set, c(birth_e[1, j], death, pers))
      
    }
    
    if(birth_e[2, j] %in% p_set[, 1]){
      
      if(pers > p_set[p_set[, 1]==birth_e[2, j], 3]){
        
        death<-pd[h2_rows[j], 3]
        
        p_set[p_set[, 1]==birth_e[2, j], ]<-c(birth_e[2, j], death, pers)
        
      }
      
    }else{
      
      death<-pd[h2_rows[j], 3]
      
      p_set<-rbind(p_set, c(birth_e[2, j], death, pers))
      
    }
    
  }
  
  colnames(p_set)<-c("p_idx", "death", "persistence")
  p_set %<>% as_tibble()
  
  for (i in 1:nrow(p_set)) {
    
    c_eta<-p_set$death[i]/sqrt(log(10))
    
    dist_cp[p_set$p_idx[i], ]<-distmat[p_set$p_idx[i], ]*( 1-exp( -(distmat[p_set$p_idx[i], ]/c_eta)^2 ) )
    
    dist_cp[, p_set$p_idx[i]]<-distmat[, p_set$p_idx[i]]*( 1-exp( -(distmat[, p_set$p_idx[i]]/c_eta)^2 ) )
    
  }
  
  return(dist_cp)
  
}


#------------------------------
#発生時刻と消滅時刻が入ったセルが塗られた、データ点間距離のヒストグラムを描画----
#breaks=ヒストグラムの
colored_birth_death_cell_hist<-
  function(data, pd, dim, breaks, m_title, distmat = F, eta_line = T, eta, barcode = F,
           inflec_line = F, inflec = eta*sqrt(3/2), tile_line = F, ninty_tile=eta*sqrt(log(10)), pd_line = F){

  if(inherits(data, "DistmatPD")){
    
    pd<-data$get_pd()
    data<-data$distmat
    distmat<-T
    
  }
    
  if(missing(breaks)){stop("breaks is missing.")}
  if( !("dimension" %in% colnames(pd)) ){stop("pd isn't diagram.")}
  
  if(missing(m_title)){m_title<-substitute(data)}
  
  #dim次元のパーシステントホモロジー群を抽出
  pd_Hd<-pd[pd[,1]==dim, ]
  
  if( !(is.matrix(pd_Hd)) ){pd_Hd<-as.matrix(pd_Hd) %>% t()}
  
  #発生時刻の距離が含まれるセルを求める
  birth_cell<-map_lgl(seq_along(breaks[-1]), function(i){some(pd_Hd[, 2], ~{(.x > breaks[i]) & (.x <= breaks[i+1])})}) %>% which()
  
  #消滅時刻の距離が含まれるセルを求める
  death_cell<-map_lgl(seq_along(breaks[-1]), function(i){some(pd_Hd[, 3], ~{(.x > breaks[i]) & (.x <= breaks[i+1])})}) %>% which()
  
  #cell_col_birth=発生時刻の距離が含まれるセルの色。NAは無色
  #"#e4007f=マゼンダ"、4D＝アルファ値30%
  birth_cell_col<-rep(NA, length = (length(breaks)-1))
  birth_cell_col[birth_cell]<-"#e4007f4d"
  
  #cell_col_death=発生時刻の距離が含まれるセルの色。NAは無色。
  #"#00a0e94d"=シアン、4D＝アルファ値30%
  death_cell_col<-rep(NA, length = (length(breaks)-1))
  death_cell_col[death_cell]<-"#00a0e94d"
  
  #ヒストグラムを作成する
  if(distmat){
    
    #発生時刻が含まれるセルをマゼンダで塗る
    hist_birth<-data %>% as.dist() %>% hist(breaks = breaks, col = birth_cell_col, main = m_title)
    
    #消滅時刻が含まれるセルをマゼンダで塗る
    hist_death<-data %>% as.dist() %>% hist(breaks = breaks, col = death_cell_col, main = "", add = T)
    
    }
  
  else{
    
    #発生時刻が含まれるセルをマゼンダで塗る
    hist_birth<-data %>% dist() %>% hist(breaks = breaks, col = birth_cell_col, main = m_title)
    
    #消滅時刻が含まれるセルをマゼンダで塗る
    hist_death<-data %>% dist() %>% hist(breaks = breaks, col = death_cell_col, main = "", add = T)
    
  }
  
  #距離がetaと等しい
  if(eta_line && !missing(eta)){
    
    abline(v = eta, col = "green3", lwd = 2)
    text(x = eta*1.1, y = max(hist_birth$counts)*0.9, labels = expression(plain(distance) == eta), pos = 3)
    
  }
  
  #距離が変曲点
  if(inflec_line){
    
    abline(v = inflec, col = "deeppink", lwd = 2)
    text(x = inflec*1.1, y = max(hist_birth$counts)*0.8, labels = "inflection point", pos = 3)
    
  }
  
  #距離が90%点
  if(tile_line){
    
    abline(v = ninty_tile, col = "darkviolet", lwd = 2)
    text(x = ninty_tile*1.1, y = max(hist_birth$counts)*0.7, labels = "90% point", pos = 3)
    
  }
  
  
  #生成時刻と消滅時刻をセットで垂直線をプロット
  if(pd_line){
    
      for (i in seq_len(nrow(pd_Hd))) {
        draw_line(x = c(pd_Hd[i, 2], 0), y = c(pd_Hd[i, 2], max(hist_birth$counts)*0.6), col = rainbow(nrow(pd_Hd))[i] )
        draw_line(x = c(pd_Hd[i, 3], 0), y = c(pd_Hd[i, 3], max(hist_birth$counts)*0.6), col = rainbow(nrow(pd_Hd))[i] )
      }
    
  }
  
  if(barcode){
    
    par(new = T)
    plot_per_barc(pd = pd, dim = dim, xlim = range(breaks), col = "red")
    
  }
  
  return(lst(hist_birth, hist_death))
  
}

#---------------------------------------
#パーシステントバーコードを描く関数------
#graphicを使い、後からいろいろ操作できるようにする

plot_per_barc<-function(pd, dim, xlim, ylim, col, lwd = 2, ...){
  
  if( !("dimension" %in% colnames(pd)) ){stop("pd mayn't be persistence diagram.")}
  
  if(missing(dim)){dim<-unique(pd[, 1])}
  if(!all(dim %in% pd[, 1])){stop("dim isn't correct dimension in persistence diagram.")}
  
  pd_Hd<-pd[(pd[, 1] %in% dim), ]
  
  if( !(is.matrix(pd_Hd)) ){pd_Hd<-as.matrix(pd_Hd) %>% t()}
  
  # if(missing(xlim)){xlim <- c(min(pd_Hd[, 2]), max(pd_Hd[, 3]))}
  # if(missing(ylim)){ylim <- c(0, nrow(pd_Hd)+1)}
  
  fill_ifmissing(xlim = c(min(pd_Hd[, 2]), max(pd_Hd[, 3])), ylim = c(0, nrow(pd_Hd)+1), 
                 col = c(1, 2, 4, 3, 5:(5+max(0, max(dim)-3)) )[1:(max(dim)+1)] )
  
  plot(x = pd_Hd[, 2:3], xlim = xlim, ylim = ylim, type = "n", xlab = "", ylab = "", 
       xaxt = "n", yaxt = "n")
  graphics::axis(1)
  graphics::title(xlab = "time")
  
  if(length(col) == 1){col<-rep(col, max(dim)+1)}
  
  for (j in seq_len(nrow(pd_Hd))) {
    draw_line(x = c(pd_Hd[j, 2], j), y = c(pd_Hd[j, 3], j), col = col[pd_Hd[j, 1]+1], lwd = lwd, ...)
  }
  
  
}

#-----------------------------------------------
#距離減衰度etaを「発生時刻と消滅時刻の中点」の中央値として距離行列操作----
#中央値・平均値、さらに発生時刻の平均値、消滅時刻の平均値を選択できるようにする
#dim=指定次元。1つのみ指定
mid_median_attenu<-function(pd, dim, distmat, type = c("median", "mean", "birth", "death")){
  
  assertthat::assert_that((length(dim)==1) && is.numeric(dim))
  
  pd_Hd<-pd[pd[,1]==dim, ]
  
  pd_Hd_mid<-apply(pd_Hd, 1, function(x){(x[2]+x[3])/2})
  
  pd_Hd_mid_med<-median(pd_Hd_mid)
  pd_Hd_mid_mean<-mean(pd_Hd_mid)
  
  pd_Hd_birth_mean<-mean(pd_Hd[, 2])
  pd_Hd_death_mean<-mean(pd_Hd[, 3])
  
  type<-match.arg(type)
  eta<-switch(type,
              median = pd_Hd_mid_med,
              mean = pd_Hd_mid_mean,
              birth = pd_Hd_birth_mean,
              death = pd_Hd_death_mean
              )
  
  distmat[distmat <= eta] <- distmat[distmat <= eta]*( 1-exp(-(distmat[distmat <= eta]/eta)^2) )
  
  return(lst(altdist=distmat, median=pd_Hd_mid_med, mean=pd_Hd_mid_mean, birth = pd_Hd_birth_mean, death = pd_Hd_death_mean, type=type))
  
}

#---------------------------------------
#フィルトレーション距離速度変化のための関数------
#d*(1-exp(-(d/eta)^2))

mph_exp<-function(d, eta){
  
  return(d*(1-exp(-(d/eta)^2)))
  
}

#-----------------------------------------------
#距離減衰度etaを「発生時刻と消滅時刻の中点」の平均値として距離行列操作----
#dim=指定次元。1つのみ指定
mid_mean_attenu_slope<-function(pd, dim, distmat, type = c("mean", "birth")){
  
  assertthat::assert_that((length(dim)==1) && is.numeric(dim))
  
  pd_Hd<-pd[pd[,1]==dim, ]
  
  pd_Hd_mid<-apply(pd_Hd, 1, function(x){(x[2]+x[3])/2})
  pd_Hd_death_mean<-mean(pd_Hd[, 3])
  pd_Hd_birth_mean<-mean(pd_Hd[, 2])
  
  type<-match.arg(type)
  eta<-switch (type,
    mean = mean(pd_Hd_mid),
    birth = pd_Hd_birth_mean
  )
  
  slp_seg<-solve(matrix(c(eta, pd_Hd_death_mean, 1, 1), 2, 2), matrix(c(mph_exp(eta, eta), pd_Hd_death_mean)))
  
  distmat[distmat <= eta] %<>% mph_exp(eta) 
  distmat[(distmat > eta) & (distmat <= pd_Hd_death_mean)] %<>% multiply_by(slp_seg[1]) %>% add(slp_seg[2])
  
  return(lst(altdist=distmat, mid_mean=mean(pd_Hd_mid), birth_mean=pd_Hd_birth_mean, death_mean=pd_Hd_death_mean, type=type))
  
}
