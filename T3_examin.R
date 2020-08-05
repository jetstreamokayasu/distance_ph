#-------------------------------------------------
#3Dトーラス一様分布
t3orus<-x3Dtorus_unif(1000, 1, 2.5, 5)

trs<-torusUnif(1000, 1, 2.5)

t3orus_pd<-calculate_homology(mat = t3orus, dim = 3, threshold = 6)

t3orus2<-x3Dtorus_unif(n = 500, r = 1, R1 = 5, R2 = 2)

t3orus2_pd<-calculate_homology(mat = t3orus2, dim = 3, threshold = 6)

t3orus2_pl<-landscape(t3orus2_pd, dimension = 1)
t3orus2_pl_dim2<-landscape(t3orus2_pd, dimension = 2)
t3orus2_pl_dim3<-landscape(t3orus2_pd, dimension = 3)
tseq <- seq(min(t3orus2_pd[,2:3]), max(t3orus2_pd[,2:3]), length=500)

#3Dトーラス一様データセット
t3orus_colle1<-lapply(1:3, function(k){
  
  nsample<-500
  torus<-x3Dtorus_unif(n = nsample, r = 1, R1 = 5, R2 = 2)
  return(list(nsample=nsample, noizyX=torus, diag=0))
  
})

t3orus_col1_aggr<-smooth_landscape_method(X = t3orus_colle1, maxdim = 3, maxscale = 6, samples = 10)


#----------------------------------------------
#3次元トーラス距離行列変化実験1回目
#t3orus2
#ランドマーク点20%、近傍点10
t3orus2_lidx<-landmark_points(X = t3orus2, n_land = 500*0.2)

t3orus2_dist<-dist(t3orus2) %>% as.matrix()

t3orus2_vdmean_dist<-t3orus2_dist

nvics<-15

t3orus2_land_vics_dmean<-sapply(t3orus2_lidx, function(k){
  
  vic_dmean<-t3orus2_dist[k, ] %>% sort() %>% .[2:(nvic+1)] %>% mean()
  names(vic_dmean)<-k
  
  return(vic_dmean)
  
})


for (i in 1:length(t3orus2_lidx)) {
  
  t3orus2_vdmean_dist[t3orus2_lidx[i], ]<-t3orus2_vdmean_dist[t3orus2_lidx[i], ]-t3orus2_land_vics_dmean[i]/2
  t3orus2_vdmean_dist[, t3orus2_lidx[i]]<-t3orus2_vdmean_dist[, t3orus2_lidx[i]]-t3orus2_land_vics_dmean[i]/2
  
}

t3orus2_vdmean_dist[t3orus2_vdmean_dist < 0] <- 0

t3orus2_dpd<-calculate_homology(mat = t3orus2_vdmean_dist, dim = 3, threshold = 6, format = "distmat")
t3orus2_dpl

#3次元トーラス距離行列変化実験2回目
#ランドマーク点15%、近傍点10
t3orus2_lidx2<-landmark_points(X = t3orus2, n_land = 500*0.15)

t3orus2_vdmean_dist2<-t3orus2_dist

nvic<-10

t3orus2_land_vics_dmean2<-sapply(t3orus2_lidx2, function(k){
  
  vic_dmean<-t3orus2_dist[k, ] %>% sort() %>% .[2:(nvic+1)] %>% mean()
  names(vic_dmean)<-k
  
  return(vic_dmean)
  
})


for (i in 1:length(t3orus2_lidx2)) {
  
  t3orus2_vdmean_dist2[t3orus2_lidx2[i], ]<-t3orus2_vdmean_dist2[t3orus2_lidx2[i], ]-t3orus2_land_vics_dmean2[i]/2
  t3orus2_vdmean_dist2[, t3orus2_lidx2[i]]<-t3orus2_vdmean_dist2[, t3orus2_lidx2[i]]-t3orus2_land_vics_dmean2[i]/2
  
}

t3orus2_vdmean_dist2[t3orus2_vdmean_dist2 < 0] <- 0

t3orus2_dpd2<-calculate_homology(mat = t3orus2_vdmean_dist2, dim = 3, threshold = 6, format = "distmat")
t3orus2_dpl2<-calcLandscape(diag =  t3orus2_dpd2, maxscale = 6)

#3次元トーラス距離行列変化実験3回目
#ランドマーク点20%、近傍点10
t3orus2_lidx3<-landmark_points(X = t3orus2, n_land = 500*0.2)

t3orus2_vdmean_dist3<-t3orus2_dist

nvic<-10

t3orus2_land_vics_dmean3<-sapply(t3orus2_lidx3, function(k){
  
  vic_dmean<-t3orus2_dist[k, ] %>% sort() %>% .[2:(nvic+1)] %>% mean()
  names(vic_dmean)<-k
  
  return(vic_dmean)
  
})


for (i in 1:length(t3orus2_lidx3)) {
  
  t3orus2_vdmean_dist3[t3orus2_lidx3[i], ]<-t3orus2_vdmean_dist2[t3orus2_lidx3[i], ]-t3orus2_land_vics_dmean3[i]/2
  t3orus2_vdmean_dist3[, t3orus2_lidx3[i]]<-t3orus2_vdmean_dist2[, t3orus2_lidx3[i]]-t3orus2_land_vics_dmean3[i]/2
  
}

t3orus2_vdmean_dist3[t3orus2_vdmean_dist3 < 0] <- 0

t3orus2_dpd3<-calculate_homology(mat = t3orus2_vdmean_dist3, dim = 3, threshold = 6, format = "distmat")
t3orus2_dpl3<-calcLandscape(diag =  t3orus2_dpd2, maxscale = 6)

#3次元トーラス距離行列変化実験4回目
#ランドマーク点20%、近傍点15
t3orus2_dpd4<-maxmin_dist_changed_pd(X = t3orus2, maxdim = 3, maxscale = 6, l_rate = 0.2, n_vic = 15)
t3orus2_dpl4<-calcLandscape(diag =  t3orus2_dpd4[[1]], maxscale = 6)

#3次元トーラス距離行列変化実験5回目
#ランドマーク点30%、近傍点20
t3orus2_dpd5<-maxmin_dist_changed_pd(X = t3orus2, maxdim = 3, maxscale = 6, l_rate = 0.3, n_vic = 20)
t3orus2_dpl5<-calcLandscape(diag =  t3orus2_dpd5[[1]], maxscale = 6)

#3次元トーラス距離行列変化実験6回目
#ランドマーク点30%、近傍点15
t3orus2_dpd6<-maxmin_dist_changed_pd(X = t3orus2, maxdim = 3, maxscale = 6, l_rate = 0.3, n_vic = 15)
t3orus2_dpl6<-calcLandscape(diag =  t3orus2_dpd6[[1]], maxscale = 6)

#3次元トーラス距離行列変化実験7回目
#ランドマーク点30%、近傍点10
t3orus2_dpd7<-maxmin_dist_changed_pd(X = t3orus2, maxdim = 3, maxscale = 6, l_rate = 0.3, n_vic = 10)
t3orus2_dpl7<-calcLandscape(diag =  t3orus2_dpd7[[1]], maxscale = 6)

#1000点3次元トーラスのPD計算
g<-c()

tp<-expand.grid(seq(0, 2*pi, length=1000), seq(0, 2*pi, length=1000))

g<-apply(tp, 1, function(i){
  
  g0<-(1/(4*(pi^2)))*rad_distribute(1, 5, 2, i[1], i[2])
  
  return(g0)
  
})

tp2<-expand.grid(seq(0, 2*pi, length=100), seq(0, 2*pi, length=100))

g2<-apply(tp2, 1, function(i){
  
  g0<-(1/(4*(pi^2)))*rad_distribute(1, 5, 2, i[1], i[2])
  
  return(g0)
  
})


