#それぞれの操作対象点における操作量を各々異なる値にする
#trs300_1_10_distは既にある

trs300_1_10_lidx<-landmark_points(X = torus300_colle_set[[1]][[10]][["noizyX"]], n_land = 300*0.1)

#ランドマーク点の近傍10点の距離の平均を求める
nvic<-10
trs300_1_10_land_vics_dsum<-sapply(trs300_1_10_lidx, function(k){
  
  
  vics_dmean<-trs300_1_10_dist[k, ] %>% sort() %>% .[1:(nvic+1)] %>% sum()/10
  names(vics_dmean)<-k
  
  return(vics_dmean)
  
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

