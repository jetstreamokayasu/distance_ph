#それぞれの操作対象点における操作量を各々異なる値にする
#trs300_1_10_distは既にある

trs300_1_10_lidx<-landmark_points(X = torus300_colle_set[[1]][[10]][["noizyX"]], n_land = 300*0.1)
trs300_1_10_lidx2<-landmark_points(X = trs300_1_10_dist, n_land = nrow(trs300_1_10_dist)*0.15, d_mat = T)


#ランドマーク点の近傍10点の距離の平均を求める
nvic<-10
trs300_1_10_land_vics_dsum<-sapply(trs300_1_10_lidx, function(k){
  
  
  vics_dmean<-trs300_1_10_dist[k, ] %>% sort() %>% .[1:(nvic+1)] %>% sum()/10
  names(vics_dmean)<-k
  
  return(vics_dmean)
  
})

trs300_1_10_land_vics_dmean<-sapply(trs300_1_10_lidx2, function(k){
  
  vic_dmean<-trs300_1_10_dist[k, ] %>% sort() %>% .[2:(nvic+1)] %>% mean()
  names(vic_dmean)<-k
  
  return(vic_dmean)
  
})

#ランドマークの近傍10点の距離の平均の1/2を操作量として距離行列を変化
#操作対象点における操作量を各点で異なる値にする
trs300_1_10_vdmean_dist<-trs300_1_10_dist
  
for (i in 1:length(trs300_1_10_lidx)) {
  
  trs300_1_10_vdmean_dist[trs300_1_10_lidx[i], ]<-trs300_1_10_vdmean_dist[trs300_1_10_lidx[i], ]-trs300_1_10_land_vics_dsum[i]/2
  trs300_1_10_vdmean_dist[, trs300_1_10_lidx[i]]<-trs300_1_10_vdmean_dist[, trs300_1_10_lidx[i]]-trs300_1_10_land_vics_dsum[i]/2
  
}

trs300_1_10_vdmean_dist[trs300_1_10_vdmean_dist < 0] <- 0

trs300_1_10_vdmean_pd<-ripsFiltration(X = trs300_1_10_vdmean_dist, maxdimension = 2, maxscale = 3, dist = "arbitrary", library = "Dionysus", 
                                      printProgress = T) %>% 
                          filtrationDiag(filtration = ., maxdimension = 2, library = "Dionysus", printProgress = T)

trs300_1_10_vdmean_pl<-calc_landscape(diag = trs300_1_10_vdmean_pd, maxscale = 3)

#ランドマークの近傍10点の距離の中央値の1/2を操作量として距離行列を変化
#一括変化。各点ごとに操作量を変えない
trs300_1_10_vdmedi_dist<-dist_mat_change(trs300_1_10_dist, idx = trs300_1_10_lidx, thresh = median(trs300_1_10_land_vics_dsum)/2)

trs300_1_10_vdmedi_pd<-ripsFiltration(X = trs300_1_10_vdmedi_dist, maxdimension = 2, maxscale = 3, dist = "arbitrary", library = "Dionysus", 
                                      printProgress = T) %>% 
  filtrationDiag(filtration = ., maxdimension = 2, library = "Dionysus", printProgress = T)

trs300_1_10_vdmedi_pl<-calc_landscape(diag = trs300_1_10_vdmedi_pd, maxscale = 3)

#ランドマーク点に関する距離から0.4を引く
#一括変化。各点ごとに操作量を変えない
trs300_1_10_land2_dist<-dist_mat_change(trs300_1_10_dist, idx = trs300_1_10_lidx, thresh = 0.4)

trs300_1_10_land2_pd<-ripsFiltration(X = trs300_1_10_land2_dist, maxdimension = 2, maxscale = 3, dist = "arbitrary", library = "Dionysus", 
                                      printProgress = T) %>% 
  filtrationDiag(filtration = ., maxdimension = 2, library = "Dionysus", printProgress = T)

trs300_1_10_land2_pl<-calc_landscape(diag = trs300_1_10_land2_pd, maxscale = 3)

#----------------------------------------------------------------------
#他のランドマーク点を選択した場合どうなるか
#ランドマークの近傍10点の距離の平均の1/2を操作量として距離行列を変化
#操作対象点における操作量を各点で異なる値にする

trs300_1_10_lidx2<-landmark_points(X = torus300_colle_set[[1]][[10]][["noizyX"]], n_land = 300*0.1)

#ランドマーク点の近傍10点の距離の平均を求める
nvic<-10
trs300_1_10_land2_vics_dsum<-sapply(trs300_1_10_lidx2, function(k){
  
  vics_dmean<-trs300_1_10_dist[k, ] %>% sort() %>% .[1:(nvic+1)] %>% sum()/nvic
  names(vics_dmean)<-k
  
  return(vics_dmean)
  
})

trs300_1_10_vdmean_dist2<-trs300_1_10_dist

for (i in 1:length(trs300_1_10_lidx2)) {
  
  trs300_1_10_vdmean_dist2[trs300_1_10_lidx2[i], ]<-trs300_1_10_vdmean_dist2[trs300_1_10_lidx2[i], ]-trs300_1_10_land2_vics_dsum[i]/2
  trs300_1_10_vdmean_dist2[, trs300_1_10_lidx2[i]]<-trs300_1_10_vdmean_dist2[, trs300_1_10_lidx2[i]]-trs300_1_10_land2_vics_dsum[i]/2
  
}

trs300_1_10_vdmean_dist2[trs300_1_10_vdmean_dist2 < 0] <- 0

trs300_1_10_vdmean_pd2<-ripsFiltration(X = trs300_1_10_vdmean_dist2, maxdimension = 2, maxscale = 3, dist = "arbitrary", library = "Dionysus", 
                                      printProgress = T) %>% 
  filtrationDiag(filtration = ., maxdimension = 2, library = "Dionysus", printProgress = T)

trs300_1_10_vdmean_pl2<-calc_landscape(diag = trs300_1_10_vdmean_pd2, maxscale = 3)


#ランドマーク点の距離変更後のPD
trs300_1_10_sub_pd<-maxmin_dist_changed_pd(X = torus300_colle_set[[1]][[10]][["noizyX"]][sample(300, 300*0.8), ], maxdim = 2, maxscale = 3)
trs300_1_10_sub_pl2<-calc_landscape(diag = trs300_1_10_sub_pd[[1]], maxscale = 3)

trs300_1_10_subs<-seephacm:::bootstrapper(X = torus300_colle_set[[1]][[10]][["noizyX"]], size = 240, samples = 10)

trs300_1_10_subs1_time<-system.time(trs300_1_10_subs_1<-maxmin_dist_changed_pd(X = trs300_1_10_subs[[1]], maxdim = 2, maxscale = 3))

trs300_1_10_subs_1_pl<-calc_landscape(diag = trs300_1_10_subs_1[[1]], maxscale = 3)

trs300_1_10_subs1_time2<-system.time(trs300_1_10_subs1_pd2<-dist(trs300_1_10_subs[[1]]) %>% as.matrix() %>% 
                                       ripsFiltration(X = ., maxdimension = 2, maxscale = 3, dist = "arbitrary", library = "Dionysus", printProgress = T) %>% 
                                       filtrationDiag(filtration = ., maxdimension = 2, library = "Dionysus", printProgress = T) )

trs300_1_10_subs1_pl2<-calc_landscape(trs300_1_10_subs1_pd2, 3)

trs300_1_10_subs1_time3<-system.time(trs300_1_10_subs1_pd3<-ripsDiag(X = trs300_1_10_subs[[1]], maxdimension = 2, maxscale = 3))
trs300_1_10_subs1_pl3<-calc_landscape(diag = trs300_1_10_subs1_pd3, maxscale = 3)

#--------------------------------------------------------------------
#サブサンプルにおいて、素のPDと距離を変化させた場合のPDを比較
#素のPD
trs300_1_10_subs_time<-system.time(trs300_1_10_subs_pd<-lapply(trs300_1_10_subs, function(X)ripsDiag(X = X, maxdimension = 2, maxscale = 3, printProgress = T)))

trs300_1_10_subs_pl<-lapply(trs300_1_10_subs_pd, function(X)calc_landscape(X, 3))

#距離を変化させた場合のPD
trs300_1_10_subs_time<-system.time(trs300_1_10_subs_dcpd<-lapply(trs300_1_10_subs, function(X)maxmin_dist_changed_pd(X = X, 2, 3)))

trs300_1_10_subs_dcpl<-lapply(trs300_1_10_subs_dcpd, function(X)calc_landscape(X[[1]], 3))

#距離を変化させた場合のPD
#ランドマーク点の数を変化
trs300_1_10_subs_time2<-system.time(trs300_1_10_subs_dcpd2<-lapply(trs300_1_10_subs, function(X)maxmin_dist_changed_pd(X = X, maxdim = 2, maxscale = 3, l_rate = 0.2)))

trs300_1_10_subs_dcpl2<-lapply(trs300_1_10_subs_dcpd2, function(X)calc_landscape(X[[1]], 3))

#距離を変化させた場合のPD
#ランドマーク点の数20% 近傍点の数5
trs300_1_10_subs_time3<-system.time(trs300_1_10_subs_dcpd3<-lapply(trs300_1_10_subs, function(X)maxmin_dist_changed_pd(X = X, maxdim = 2, maxscale = 3, l_rate = 0.2, n_vic = 5)))

trs300_1_10_subs_dcpl3<-lapply(trs300_1_10_subs_dcpd3, function(X)calc_landscape(X[[1]], 3))

#距離を変化させた場合のPD
#ランドマーク点の数20% 近傍点の数8
trs300_1_10_subs_time4<-system.time(trs300_1_10_subs_dcpd4<-lapply(trs300_1_10_subs, function(X)maxmin_dist_changed_pd(X = X, maxdim = 2, maxscale = 3, l_rate = 0.2, n_vic = 8)))

trs300_1_10_subs_dcpl4<-lapply(trs300_1_10_subs_dcpd4, function(X)calc_landscape(X[[1]], 3))

#距離を変化させた場合のPD
#85%サブサンプル、ランドマーク点の数20%、近傍点の数10
#最も1次ランドスケープの形がいい
trs300_1_10_subs2<-seephacm:::bootstrapper(X = torus300_colle_set[[1]][[10]][["noizyX"]], size = 300*0.85, samples = 10)

trs300_1_10_subs_time5<-system.time(trs300_1_10_subs_dcpd5<-lapply(trs300_1_10_subs2, function(X)maxmin_dist_changed_pd(X = X, maxdim = 2, maxscale = 3, l_rate = 0.2, n_vic = 10)))

trs300_1_10_subs_dcpl5<-lapply(trs300_1_10_subs_dcpd5, function(X)calc_landscape(X[[1]], 3))

#距離を変化させた場合のPD
#85%サブサンプル、ランドマーク点の数20%、近傍点の数8
trs300_1_10_subs_time6<-system.time(trs300_1_10_subs_dcpd6<-lapply(trs300_1_10_subs2, function(X)maxmin_dist_changed_pd(X = X, maxdim = 2, maxscale = 3, l_rate = 0.2, n_vic = 8)))

trs300_1_10_subs_dcpl6<-lapply(trs300_1_10_subs_dcpd6, function(X)calc_landscape(X[[1]], 3))

#距離を変化させた場合のPD
#85%サブサンプル、ランドマーク点の数15%、近傍点の数10
trs300_1_10_subs_time7<-system.time(trs300_1_10_subs_dcpd7<-lapply(trs300_1_10_subs2, function(X)maxmin_dist_changed_pd(X = X, maxdim = 2, maxscale = 3, l_rate = 0.15, n_vic = 10)))

trs300_1_10_subs_dcpl7<-lapply(trs300_1_10_subs_dcpd7, function(X)calc_landscape(X[[1]], 3))

#素のPD
#85%サブサンプル
trs300_1_10_subs_timeB<-system.time(trs300_1_10_subs_pd2<-lapply(trs300_1_10_subs2, function(X)ripsDiag(X = X, maxdimension = 2, maxscale = 3, printProgress = T)))

trs300_1_10_subs_pl2<-lapply(trs300_1_10_subs_pd2, function(X)calc_landscape(X, 3))

#DionysusとGUDHIの計算時間比較
trs300_1_10_subs_2_time<-system.time(trs300_1_10_subs_2_pd<-ripsDiag(X = trs300_1_10_subs2[[2]], maxdimension = 2, maxscale = 3, library = "Dionysus"))
trs300_1_10_subs_timeB<-system.time(trs300_1_10_subs_pd2<-lapply(trs300_1_10_subs2, function(X)ripsDiag(X = X, maxdimension = 2, maxscale = 3, printProgress = T)))


#-----------------------------------------------------
#TDAstats(ripser)を使って計算し直し
#サブサンプルにおいて、素のPDと距離を変化させた場合のPDを比較

#距離を変化させた場合のPD
#85%サブサンプル、ランドマーク点の数20%、近傍点の数10
trs300_1_10_subs_time5B<-system.time(trs300_1_10_subs_dcpd5B<-lapply(trs300_1_10_subs2, function(X)maxmin_dist_changed_pd(X = X, maxdim = 2, maxscale = 3, l_rate = 0.2, n_vic = 10)))

trs300_1_10_subs2_dcpl5B<-lapply(trs300_1_10_subs_dcpd5B, function(X)calc_landscape(list(X[[1]]), 3))

#距離を変化させた場合のPD
#85%サブサンプル、ランドマーク点の数20%、近傍点の数8
trs300_1_10_subs2_time6B<-system.time(trs300_1_10_subs2_dcpd6B<-lapply(trs300_1_10_subs2, function(X)maxmin_dist_changed_pd(X = X, maxdim = 2, maxscale = 3, l_rate = 0.2, n_vic = 8)))

trs300_1_10_subs2_dcpl6B<-lapply(trs300_1_10_subs2_dcpd6B, function(X)calc_landscape(list(X[[1]]), 3))

#距離を変化させた場合のPD
#85%サブサンプル、ランドマーク点の数15%、近傍点の数10
#最も1次ランドスケープの形がいい
trs300_1_10_subs2_time7B<-system.time(trs300_1_10_subs2_dcpd7B<-lapply(trs300_1_10_subs2, function(X)maxmin_dist_changed_pd(X = X, maxdim = 2, maxscale = 3, l_rate = 0.15, n_vic = 10)))

trs300_1_10_subs2_dcpl7B<-lapply(trs300_1_10_subs2_dcpd7B, function(X)calc_landscape(list(X[[1]]), 3))

#素のPD
#85%サブサンプル
trs300_1_10_subs_timeC<-system.time(trs300_1_10_subs_pd2B<-lapply(trs300_1_10_subs2, function(X)calculate_homology(mat = X, dim = 2, threshold = 3)))

trs300_1_10_subs2_pl2B<-lapply(trs300_1_10_subs_pd2B, function(X)calc_landscape(list(X), 3))
