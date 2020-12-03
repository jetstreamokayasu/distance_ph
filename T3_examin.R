#3Dトーラス一様分布------------
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



#3次元トーラス距離行列変化実験1回目-----------
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

#3次元トーラス距離行列変化実験8回目
#ランドマーク点40%、近傍点10
t3orus2_dpd8<-maxmin_dist_changed_pd(X = t3orus2, maxdim = 3, maxscale = 6, l_rate = 0.4, n_vic = 10)
t3orus2_dpl8<-calcLandscape(diag =  t3orus2_dpd8[[1]], maxscale = 6)

#3次元トーラス距離行列変化実験9回目
#ランドマーク点50%、近傍点10
t3orus2_time9<-system.time(t3orus2_dpd9<-maxmin_dist_changed_pd(X = t3orus2, maxdim = 3, maxscale = 6, l_rate = 0.5, n_vic = 10))
t3orus2_dpl9<-calcLandscape(diag =  t3orus2_dpd9[[1]], maxscale = 6)

#3次元トーラス距離行列変化実験10回目
#ランドマーク点40%、近傍点15
t3orus2_time10<-system.time(t3orus2_dpd10<-maxmin_dist_changed_pd(X = t3orus2, maxdim = 3, maxscale = 6, l_rate = 0.4, n_vic = 15))
t3orus2_dpl10<-calcLandscape(diag =  t3orus2_dpd10[[1]], maxscale = 6)

#3次元トーラス距離行列変化実験12回目
#ランドマーク点40%、近傍点15
t3orus2_time10<-system.time(t3orus2_dpd10<-maxmin_dist_changed_pd(X = t3orus2, maxdim = 3, maxscale = 6, l_rate = 0.4, n_vic = 15))
t3orus2_dpl10<-calcLandscape(diag =  t3orus2_dpd10[[1]], maxscale = 6)



#--------------------------------------------------
#1000点3次元トーラスのPD計算
t3orus1000<-x3Dtorus_unif(n = 1000, r = 1, R1 = 5, R2 = 2)
t3orus1000_pd<-calculate_homology(mat = t3orus1000, dim = 3, threshold = 6)

t3orus1000_pl<-calcLandscape(diag = t3orus1000_pd, maxscale = 6)

#1000点3次元トーラス距離行列変化実験1回目
#ランドマーク点15%、近傍点10
t3orus1000_pd1<-maxmin_dist_changed_pd(X = t3orus1000, maxdim = 3, maxscale = 6, l_rate = 0.15, n_vic = 10)
t3orus1000_pl1<-calcLandscape(diag =  t3orus1000_pd1[[1]], maxscale = 6)

#800点3次元トーラスのPD計算
t3orus800<-x3Dtorus_unif(n = 800, r = 1, R1 = 5, R2 = 2)
t3orus800_pd<-calculate_homology(mat = t3orus800, dim = 3, threshold = 6)
t3orus800_pl<-calcLandscape(diag = t3orus800_pd, maxscale = 6)

#------------------------------------------------------
#600点3次元トーラスのPD計算---------------------------
t3orus600<-x3Dtorus_unif(n = 600, r = 1, R1 = 5, R2 = 2)
t3orus600_time<-system.time(t3orus600_pd<-calculate_homology(mat = t3orus600, dim = 3, threshold = 6))
t3orus600_pl<-calcLandscape(diag = t3orus600_pd, maxscale = 6)

#600点3次元トーラス距離行列変化実験1回目
#ランドマーク点15%、近傍点10
t3orus600_time_dpd1<-system.time(t3orus600_dpd1<-maxmin_dist_changed_pd(X = t3orus600, maxdim = 3, maxscale = 6, l_rate = 0.15, n_vic = 10))
t3orus600_dpl1<-calcLandscape(t3orus600_dpd1[[1]], 6)

#600点3次元トーラス距離行列変化実験2回目
#ランドマーク点20%、近傍点10
t3orus600_time_dpd2<-system.time(t3orus600_dpd2<-maxmin_dist_changed_pd(X = t3orus600, maxdim = 3, maxscale = 6, l_rate = 0.20, n_vic = 10))
t3orus600_dpl2<-calcLandscape(t3orus600_dpd2[[1]], 6)

#-----------------------------------------
t3orus2_pd3<-calculate_homology(mat = t3orus2, dim = 3)
t3orus2_pl3<-calcLandscape(diag = t3orus2_pd3, maxscale = 5)

plot(t3orus2_pl3[["tseq"]], t3orus2_pl3[["3-land"]], type = "l", col=4, xlim = c(0, 5), ylim = c(0, 0.09))
abline(h=t3orus2_pl3[["thresh"]]*(2*pi)/surface_nshpere(3))

plot(t3orus2_pl3[["tseq"]], t3orus2_pl3[["2-land"]], type = "l", col=3, xlim = c(0, 5), ylim = c(0, 0.4))
abline(h=t3orus2_pl3[["thresh"]]/2)

#---------------------------------------
#500点3次元トーラスのPD計算-------------
#r=2, R1=8, R2=4
t3orus3<-x3Dtorus_unif(n = 500, r = 2, R1 = 8, R2 = 4)
t3orus3_time<-system.time(t3orus3_pd<-calculate_homology(mat = t3orus3, dim = 3, threshold = 9))
t3orus3_pl<-calcLandscape(diag = t3orus600_pd, maxscale = 9)

#500点3次元トーラス距離行列変化実験1回目(実質11回目)
#r=2, R1=8, R2=4
#ランドマーク点40%、近傍点10
t3orus3_time11<-system.time(t3orus3_dpd11<-maxmin_dist_changed_pd(X = t3orus3, maxdim = 3, maxscale = 9, l_rate = 0.4, n_vic = 10))
t3orus3_dpl11<-calcLandscape(diag = t3orus3_dpd11[["pd"]], maxscale = 9)

#500点3次元トーラス距離行列変化実験2回目
#r=2, R1=8, R2=4
#ランドマーク点45%、近傍点8
t3orus3_time2<-system.time(t3orus3_dpd2<-maxmin_dist_changed_pd(X = t3orus3, maxdim = 3, maxscale = 9, l_rate = 0.45, n_vic = 8))
t3orus3_dpl2<-calcLandscape(diag = t3orus3_dpd2[["pd"]], maxscale = 9)

#500点3次元トーラス距離行列変化実験3回目
#r=2, R1=8, R2=4
#ランドマーク点50%、近傍点7
t3orus3_dpd3<-maxmin_dist_changed_pd(X = t3orus3, maxdim = 3, maxscale = 9, l_rate = 0.5, n_vic = 7)
t3orus3_dpl3<-calcLandscape(diag = t3orus3_dpd3[["pd"]], maxscale = 9)

#500点3次元トーラス距離行列変化実験4回目
#r=2, R1=8, R2=4
#ランドマーク点20%、近傍点10
t3orus3_dpd4<-maxmin_dist_changed_pd(X = t3orus3, maxdim = 3, maxscale = 9, l_rate = 0.2, n_vic = 10)
t3orus3_dpl4<-calcLandscape(diag = t3orus3_dpd4[["pd"]], maxscale = 9)

#500点3次元トーラス距離行列変化実験5回目
#r=2, R1=8, R2=4
#ランドマーク点45%、近傍点10
t3orus3_dpd5<-maxmin_dist_changed_pd(X = t3orus3, maxdim = 3, maxscale = 9, l_rate = 0.45, n_vic = 10)
t3orus3_dpl5<-calcLandscape(diag = t3orus3_dpd5[["pd"]], maxscale = 9)

#500点3次元トーラス距離行列変化実験6回目
#r=2, R1=8, R2=4
#ランドマーク点45%、近傍点15
t3orus3_dpd6<-maxmin_dist_changed_pd(X = t3orus3, maxdim = 3, maxscale = 9, l_rate = 0.45, n_vic = 15)
t3orus3_dpl6<-calcLandscape(diag = t3orus3_dpd6[["pd"]], maxscale = 9)

#500点3次元トーラス距離行列変化実験7回目
#r=2, R1=8, R2=4
#ランドマーク点50%、近傍点8
#最もランドマークが大きくなる、はず
t3orus3_dpd7<-maxmin_dist_changed_pd(X = t3orus3, maxdim = 3, maxscale = 9, l_rate = 0.5, n_vic = 8)
t3orus3_dpl7<-calcLandscape(diag = t3orus3_dpd7[["pd"]], maxscale = 9)

#---------------------------------------------------------------
#500点3次元トーラス距離行列変化実験----------------------------
#サブサンプルで実験1回目
#r=2, R1=8, R2=4
#ランドマーク点50%、近傍点8
t3orus3_sub_dpd1<-maxmin_dist_changed_pd(X = t3orus3[sample(nrow(t3orus3), nrow(t3orus3)*0.8), ], maxdim = 3, maxscale = 9, l_rate = 0.5, n_vic = 8)
t3orus3_sub_dpl1<-calcLandscape(diag = t3orus3_sub_dpd1[["pd"]], maxscale = 9)

#500点3次元トーラス距離行列変化実験
#サブサンプルで実験2回目
#r=2, R1=8, R2=4
#ランドマーク点50%、近傍点10
t3orus3_sub_dpd2<-maxmin_dist_changed_pd(X = t3orus3[sample(nrow(t3orus3), nrow(t3orus3)*0.8), ], maxdim = 3, maxscale = 9, l_rate = 0.5, n_vic = 10)
t3orus3_sub_dpl2<-calcLandscape(diag = t3orus3_sub_dpd2[["pd"]], maxscale = 9)

#500点3次元トーラス距離行列変化実験
#サブサンプルで実験3回目
#r=2, R1=8, R2=4
#ランドマーク点50%、近傍点9
t3orus3_sub_dpd3<-maxmin_dist_changed_pd(X = t3orus3[sample(nrow(t3orus3), nrow(t3orus3)*0.8), ], maxdim = 3, maxscale = 9, l_rate = 0.5, n_vic = 9)
t3orus3_sub_dpl3<-calcLandscape(diag = t3orus3_sub_dpd3[["pd"]], maxscale = 9)

#500点3次元トーラス距離行列変化実験
#サブサンプルで実験4回目
#r=2, R1=8, R2=4
#ランドマーク点55%、近傍点8
t3orus3_sub_dpd4<-maxmin_dist_changed_pd(X = t3orus3[sample(nrow(t3orus3), nrow(t3orus3)*0.8), ], maxdim = 3, maxscale = 9, l_rate = 0.55, n_vic = 8)
t3orus3_sub_dpl4<-calcLandscape(diag = t3orus3_sub_dpd4[["pd"]], maxscale = 9)

#500点3次元トーラス距離行列変化実験
#サブサンプルで実験5回目
#r=2, R1=8, R2=4
#ランドマーク点60%、近傍点9
t3orus3_sub_dpd5<-maxmin_dist_changed_pd(X = t3orus3[sample(nrow(t3orus3), nrow(t3orus3)*0.8), ], maxdim = 3, maxscale = 9, l_rate = 0.6, n_vic = 9)
t3orus3_sub_dpl5<-calcLandscape(diag = t3orus3_sub_dpd5[["pd"]], maxscale = 9)

#500点3次元トーラス距離行列変化実験
#サブサンプルで実験6回目
#r=2, R1=8, R2=4
#ランドマーク点60%、近傍点10
t3orus3_sub_dpd6<-maxmin_dist_changed_pd(X = t3orus3[sample(nrow(t3orus3), nrow(t3orus3)*0.8), ], maxdim = 3, maxscale = 9, l_rate = 0.6, n_vic = 10)
t3orus3_sub_dpl6<-calcLandscape(diag = t3orus3_sub_dpd6[["pd"]], maxscale = 9)

#サブサンプル作成
t3orus3_sub<-t3orus3[sample(nrow(t3orus3), nrow(t3orus3)*0.8), ]

#500点3次元トーラス距離行列変化実験
#サブサンプルで実験7回目
#r=2, R1=8, R2=4
#ランドマーク点60%、近傍点8
t3orus3_sub_dpd7<-maxmin_dist_changed_pd(X = t3orus3_sub, maxdim = 3, maxscale = 9, l_rate = 0.6, n_vic = 8)
t3orus3_sub_dpl7<-calcLandscape(diag = t3orus3_sub_dpd6[["pd"]], maxscale = 9)

#500点3次元トーラス距離行列変化実験
#サブサンプルで実験8回目
#r=2, R1=8, R2=4
#ランドマーク点65%、近傍点8
#ランドマーク点を増やしていく
#最もパーシステンスが大きくなる
t3orus3_sub_dpd8<-maxmin_dist_changed_pd(X = t3orus3_sub, maxdim = 3, maxscale = 9, l_rate = 0.65, n_vic = 8)
t3orus3_sub_dpl8<-calcLandscape(diag = t3orus3_sub_dpd8[["pd"]], maxscale = 9)

#500点3次元トーラス距離行列変化実験
#サブサンプルで実験9回目
#r=2, R1=8, R2=4
#ランドマーク点70%、近傍点8
#ランドマーク点を増やしていく
t3orus3_sub_dpd9<-maxmin_dist_changed_pd(X = t3orus3_sub, maxdim = 3, maxscale = 9, l_rate = 0.7, n_vic = 8) 
t3orus3_sub_dpl9<-calcLandscape(diag = t3orus3_sub_dpd9[["pd"]], maxscale = 9)

#------------------------------------------
#T3で100個の推定を試す---------------------
t3orus4_list<- lapply(1:100, function(i){
  nsample <- 500
  torus <- x3Dtorus_unif(n = nsample, r = 2, R1 = 8, R2 = 4)
  return(list(nsample = nsample, noizyX = torus, diag = 0))
})

t3orus4_aggr<-maxmin_distance_change_method(X = t3orus4_list, maxdim = 3, maxscale = 9, samples = 10, l_rate = 0.65, n_vic = 8)

#---------------------------------------------------------------
#maxmin_dist_changed_pl_peak_count()関数の修正後のテスト--------
t3orus3_sublist<-seephacm:::bootstrapper(X = t3orus3, size = nrow(t3orus3)*0.8, samples = 2)
t3orus3_subpeak<-maxmin_dist_changed_pl_peak_count(X = t3orus3_sublist, maxdim = 3, maxscale = 9, l_rate = 0.65, n_vic = 8)

#500点3次元トーラス距離行列変化実験
#サブサンプルで実験10回目
#r=2, R1=8, R2=4
#ランドマーク点65%、近傍点8
t3orus3_sublist1_dpd10<-maxmin_dist_changed_pd(X = t3orus3_sublist[[1]], maxdim = 3, maxscale = 9, l_rate = 0.65, n_vic = 8)
t3orus3_sublist1_dpl10<-calcLandscape(diag = t3orus3_sublist1_dpd10[["pd"]], maxscale = 9)

#500点3次元トーラス距離行列変化実験
#サブサンプルで実験11回目
#r=2, R1=8, R2=4
#ランドマーク点65%、近傍点8
t3orus3_sublist1_dpd11<-maxmin_dist_changed_pd(X = t3orus3_sublist[[2]], maxdim = 3, maxscale = 9, l_rate = 0.65, n_vic = 8)
t3orus3_sublist1_dpl11<-calcLandscape(diag = t3orus3_sublist1_dpd11[["pd"]], maxscale = 9)

#----------------------------------------------------------------------------
#正規化せず、ランドマーク点に関する距離に1-exp(-(d_ij/eta)^2)を掛けてみる----
#ランドマーク点としてt4_land1使用。eta=10
#1回目
t3orus4_distH<-t3orus4_dist

for (i in t4_land1) {
  
  t3orus4_distH[i, ]<-t3orus4_dist[i, ]*(1-exp(-(t3orus4_dist[i, ]/10)^2))
  t3orus4_distH[, i]<-t3orus4_dist[, i]*(1-exp(-(t3orus4_dist[, i]/10)^2))
  
}

t3orus4_pdH_time<-system.time( t3orus4_pdH<-calculate_homology(mat = t3orus4_distH, dim = 3, format = "distmat") )
t3orus4_plH<-calc_landscape(t3orus4_pdH, maxscale = 5)
plot_landscape(land = trs300_1_10_plE, dim = 2, ylim = c(0, 0.3))

#正規化せず、ランドマーク点に関する距離に1-exp(-(d_ij/eta)^2)を掛けてみる--------
#ランドマーク点としてt4_land2(全データ点の30%)使用。eta=５
#2回目

t4_land2<-landmark_points(t3orus4, n_land = nrow(t3orus4)*0.3)

t3orus4_distI<-t3orus4_dist

for (i in t4_land2) {
  
  t3orus4_distI[i, ]<-t3orus4_dist[i, ]*(1-exp(-(t3orus4_dist[i, ]/5)^2))
  t3orus4_distI[, i]<-t3orus4_dist[, i]*(1-exp(-(t3orus4_dist[, i]/5)^2))
  
}

t3orus4_pdI_time<-system.time( t3orus4_pdI<-calculate_homology(mat = t3orus4_distI, dim = 3, threshold = 9, format = "distmat") )
t3orus4_plI<-calc_landscape(t3orus4_pdI, maxscale = 9)
calc.landscape.peak(X = t3orus4_plI[["2-land"]], dimension = 2, thresh = t3orus4_plI[["thresh"]]/2, tseq = t3orus4_plI[["tseq"]], show = T)
calc.landscape.peak(X = t3orus4_plI[["3-land"]], dimension = 3, thresh = t3orus4_plI[["thresh"]]*(2*pi)/surface_nshpere(3), tseq = t3orus4_plI[["tseq"]], show = T)
plot_landscape(land = t3orus4_plI, dim = 3, ylim = c(0, 1.0))
plot_landscape(land = t3orus4_plI, dim = 2, ylim = c(0, 1.5))

#---------------------
#正規化せず、ランドマーク点に関する距離に1-exp(-(d_ij/eta)^2)を掛けてみる----------
#ランドマーク点100%。eta=５
#3回目


t3orus4_distJ<-t3orus4_dist*(1-exp(-(t3orus4_dist/5)^2))

t3orus4_pdJ_time<-system.time( t3orus4_pdJ<-calculate_homology(mat = t3orus4_distJ, dim = 3, threshold = 9, format = "distmat") )
t3orus4_plJ<-calc_landscape(t3orus4_pdJ, maxscale = 9)
calc.landscape.peak(X = t3orus4_plJ[["2-land"]], dimension = 2, thresh = t3orus4_plJ[["thresh"]]/2, tseq = t3orus4_plJ[["tseq"]], show = T)
calc.landscape.peak(X = t3orus4_plJ[["3-land"]], dimension = 3, thresh = t3orus4_plJ[["thresh"]]*(2*pi)/surface_nshpere(3), tseq = t3orus4_plJ[["tseq"]], show = T)

#---------------------
#正規化せず、ランドマーク点に関する距離に1-exp(-(d_ij/eta)^2)を掛けてみる----------
#ランドマーク点100%。eta=3
#4回目


t3orus4_distK<-t3orus4_dist*(1-exp(-(t3orus4_dist/3)^2))

t3orus4_pdK_time<-system.time( t3orus4_pdK<-calculate_homology(mat = t3orus4_distK, dim = 3, threshold = 9, format = "distmat") )
t3orus4_plK<-calc_landscape(t3orus4_pdK, maxscale = 9)
calc.landscape.peak(X = t3orus4_plK[["2-land"]], dimension = 2, thresh = t3orus4_plK[["thresh"]]/2, tseq = t3orus4_plK[["tseq"]], show = T)
calc.landscape.peak(X = t3orus4_plK[["3-land"]], dimension = 3, thresh = t3orus4_plK[["thresh"]]*(2*pi)/surface_nshpere(3), tseq = t3orus4_plK[["tseq"]], show = T)



#距離行列変化なしの3次元トーラスt3orus4のPD計算時間を求める----
#threshold=9
#maxscaleは9必要
t3orus4_time0B<-system.time(t3orus4_dpd0B<-calculate_homology(mat = t3orus4_dist, dim = 3, threshold = 9, format = "distmat"))
t3orus4_dpl0B<-calc_landscape(diag = t3orus4_dpd0B, maxscale = 9)
calc.landscape.peak(X = t3orus4_dpl0B[["2-land"]], dimension = 2, thresh = t3orus4_dpl0B[["thresh"]]/2, tseq = t3orus4_dpl0B[["tseq"]], show = T)
plot_landscape(t3orus4_dpl0B, 3, ylim = c(0, 1.0))
plot_landscape(t3orus4_dpl0B, 2, ylim = c(0, 1.5))

#-----------------
#距離行列変化なしの3次元トーラスt3orus4のPD計算時間を求める-----
#threshold=5
t3orus4_time0C<-system.time(t3orus4_dpd0C<-calculate_homology(mat = t3orus4_dist, dim = 3, threshold = 5, format = "distmat"))
t3orus4_dpl0C<-calc_landscape(diag = t3orus4_dpd0C, maxscale = 5)

#load("D:/okayasu/D_documents/R/distance_ph/vars/t3orus4_time0B.RData")



#----------------------------------------------
#3次元トーラスで成功率を求めてみる-------------
#r = 2, R1 = 8, R2 = 4
#全データセット数5、サブサンプル数5
#eta=5, ランドマーク点30%

t3orus4_list2<- lapply(1:5, function(i){
  nsample <- 500
  torus <- x3Dtorus_unif(n = nsample, r = 2, R1 = 8, R2 = 4)
  return(list(nsample = nsample, noizyX = torus, diag = 0))
})

t3orus4_aggrs_time<-system.time( t3orus4_aggrs<-calc_distance_change_betti(X = t3orus4_list2[1:3], maxdim = 3, maxscale = 9, samples = 3, ph_func = weighted_homology, l_rate=0.5, eta=7) )

t3ours4_list2_2_sub<-seephacm:::bootstrapper(X = t3orus4_list2[[2]][["noizyX"]], size = nrow(t3orus4_list2[[2]][["noizyX"]])*0.8, 1)

t3ours4_list2_2_sub_pd<-weighted_homology(X = t3ours4_list2_2_sub[[1]], maxdim = 3, maxscale = 9, extra_v = list(l_rate=0.5, eta=6.5))
t3ours4_list2_2_sub_pl<-calc_landscape(diag = t3ours4_list2_2_sub_pd[[1]], maxscale = 9)

t3ours4_list2_2_sub_dist<-dist(t3ours4_list2_2_sub[[1]]) %>% as.matrix()

t3ours4_list2_2_sub_normal_time<-system.time( t3ours4_list2_2_sub_normal_pd<-calculate_homology(mat = t3ours4_list2_2_sub_dist, dim = 3, threshold = 9, format = "distmat") )
t3ours4_list2_2_sub_normal_pl<-calc_landscape(diag = t3ours4_list2_2_sub_normal_pd, maxscale = 9)

t3ours4_list2_2_sub_wvr_dist<-dist_wvr_change(X_dist = t3ours4_list2_2_sub_dist, lands = t3ours4_list2_2_sub_pd[["l_idx"]], eta = 7)

t3ours4_list2_2_sub_lidx<-t3ours4_list2_2_sub_pd[["l_idx"]]
t3ours4_list2_2_sub_wvr_dist2<-dist_wvr_change(X_dist = t3ours4_list2_2_sub_dist, lands = t3ours4_list2_2_sub_lidx, eta = 4)
t3ours4_list2_2_sub_pd2<-calculate_homology(mat = t3ours4_list2_2_sub_wvr_dist2, dim = 3, threshold = 9, format = "distmat")
t3ours4_list2_2_sub_pl2<-calc_landscape(diag = t3ours4_list2_2_sub_pd2, maxscale = 9)
calc.landscape.peak(X = t3ours4_list2_2_sub_pl2[["3-land"]], dimension = 3, thresh = t3ours4_list2_2_sub_pl2[["thresh"]]*(2*pi)/surface_nshpere(3), tseq = t3ours4_list2_2_sub_pl2[["tseq"]], show = T)


plot(t3ours4_list2_2_sub_dist[t3ours4_list2_2_sub_pd[["l_idx"]][1], ], t3ours4_list2_2_sub_dist[t3ours4_list2_2_sub_pd[["l_idx"]][1], ])
points(t3ours4_list2_2_sub_dist[t3ours4_list2_2_sub_pd[["l_idx"]][1], ], t3ours4_list2_2_sub_wvr_dist[t3ours4_list2_2_sub_pd[["l_idx"]][1], ], col=2)
