#密度が低い場合にスケールを変化させた場合のPHがどうなるかを調べる------
half_trs1<-torus1/2

hf_trs1_pd<-calculate_homology(half_trs1, dim = 2, threshold = 3)
hf_trs1_pl<-calcLandscape(diag = hf_trs1_pd, maxscale = 3)


#torus300_colle_set[[1]]を使って300点トーラス100セットのFRI距離行列変化の実験をする-----
trs300_1_fri_aggr_time<-system.time( trs300_1_fri_aggr<-fri_distance_change_method(torus300_colle_set[[1]], maxdim = 2, maxscale = 3, samples = 10, l_rate = 0.3, eta = 1) )

trs300_1_fri_aggr2_time<-system.time( trs300_1_fri_aggr2<-fri_distance_change_method(torus300_colle_set[[1]], maxdim = 2, maxscale = 3, samples = 10, l_rate = 0.45, eta = 1) )



#グリッドサーチのようにランドマーク点割合とハイパラetaを試す------------

land_rate_set<-seq(0.1, 0.8, by=0.05)
eta_set<-seq(0.5, 2.5, by=0.1)

para_set<-expand.grid(land_rate_set, eta_set)
colnames(para_set)<-c("l_rate", "eta")

trs300_1_10_para_test_time<-system.time( trs300_1_10_para_test<-lapply(1:nrow(para_set), function(i){
  
  cat("para_set=", i, "\n")
  cat("rate=", para_set[i, 1], "\n")
  cat("eta=", para_set[i, 2], "\n")
  mpd<-multiresolut_homology(X = torus300_colle_set[[1]][[10]][["noizyX"]], maxdim = 2, l_rate = para_set[i, 1], a = para_set[i, 2])
  
  return(mpd)
  
  }) )

#グリッドサーチ後の平滑化PLを計算、局所最大値を数える
trs300_1_10_para_test_pls<-lapply(trs300_1_10_para_test, function(pd)calc_landscape(diag = pd[[1]], maxscale = 1))
trs300_1_10_para_test_counts<-lapply(trs300_1_10_para_test_pls, function(pl)calc.landscape.peak(X = pl[["2-land"]], dimension = 2, tseq = pl[["tseq"]], thresh = pl[["thresh"]]/2, show = T))

#局所最大値の数をパラメータごとにプロット
plot(para_set, xlab="landmark rate", ylab="eta", type="n")
text(para_set[, 1], para_set[, 2], labels = round(unlist(trs300_1_10_para_test_counts), 2), 
     col = ifelse(unlist(trs300_1_10_para_test_counts) >= 0.5 & unlist(trs300_1_10_para_test_counts) < 1.5, 2, 4))

#一部のパラメータについてPLを一覧表示
para_part_set<-expand.grid(c(0.2, 0.5, 0.8), rev(c(0.5, 1.5, 2.5)))
colnames(para_part_set)<-c("l_rate", "eta")

#グラフ描画画面を分割
oldpar <- par(no.readonly = TRUE) 
par(mfrow=c(3, 3))
par(mai = c(0.3, 0.3, 0.3, 0.3))

for (p in 1:nrow(para_part_set)){
  
  pl_idx<-which(para_set[,1]==para_part_set[p, 1] & para_set[,2]==para_part_set[p, 2])
  plot(trs300_1_10_para_test_pls[[pl_idx]][["tseq"]], trs300_1_10_para_test_pls[[pl_idx]][["2-land"]], ylim = c(0, 0.08), col=3, type="l", main = paste0("rate=", para_part_set[p, 1], " eta=", para_part_set[p, 2]), xlab = "", ylab = "")
  abline(h=trs300_1_10_para_test_pls[[pl_idx]][["thresh"]]/2)

}

plot(trs300_1_10_para_test_pls[[3]][["tseq"]], trs300_1_10_para_test_pls[[3]][["2-land"]], ylim = c(0, 0.1), col=3, type="l", xlab = "")



#正規化せず、ランドマーク点に関する距離に1-exp(-(d_ij/eta)^2)を掛けてみる------------
#ランドマーク点30%
trs300_1_10_distE<-trs300_1_10_dist

for (i in land10F) {
  
  trs300_1_10_distE[i, ]<-trs300_1_10_dist[i, ]*(1-exp(-(trs300_1_10_dist[i, ]/3)^2))
  trs300_1_10_distE[, i]<-trs300_1_10_dist[, i]*(1-exp(-(trs300_1_10_dist[, i]/3)^2))
  
}

trs300_1_10_pdE_time<-system.time(trs300_1_10_pdE<-calculate_homology(mat = trs300_1_10_distE, dim = 2, threshold = 3, format = "distmat"))
trs300_1_10_plE<-calc_landscape(trs300_1_10_pdE, maxscale = 3)
plot_landscape(land = trs300_1_10_plE, dim = 2, ylim = c(0, 0.3))

#正規化せず、ランドマーク点に関する距離に1-exp(-(d_ij/eta)^2)を掛けてみる
#ランドマーク点100%
trs300_1_10_distF<-trs300_1_10_dist*(1-exp(-(trs300_1_10_dist/3)^2))

trs300_1_10_pdF_time<-system.time(trs300_1_10_pdF<-calculate_homology(mat = trs300_1_10_distF, dim = 2, threshold = 3, format = "distmat"))
trs300_1_10_plF<-calc_landscape(trs300_1_10_pdF, maxscale = 3)
plot_landscape(land = trs300_1_10_plE, dim = 2, ylim = c(0, 0.3))

#各パラメータごとにおける計算時間をプロット
trs300_1_10_para_test_t<-map_dbl(trs300_1_10_para_test, ~{.[["time"]][3]})
par(mai=c(0.7, 0.7, 0.3, 0.3))
par(mgp=c(2, 1, 0))
plot(para_set, xlab="landmark rate", ylab="eta", type="n", cex.lab = 1.3)
text(para_set$l_rate, para_set$eta, labels = round(trs300_1_10_para_test_t, digits = 2), 
     col = smoothPalette(x = round(trs300_1_10_para_test_t-trs300_1_10_pd_time[3], digits = 2), palfunc = blue2red), cex=0.8)



#------------------------------------------------------------------------
#グリッドサーチのようにランドマーク点割合とハイパラetaを試す-------------
#2次元トーラス2回目
#1-exp(-(d_ij/eta)^2)を掛けてみる

land_rate_set<-seq(0.1, 0.8, by=0.05)
eta_set2<-seq(1, 4, by=0.1)

para_set2<-expand.grid(land_rate_set, eta_set2)
colnames(para_set2)<-c("l_rate", "eta")


trs300_1_10_para_test_time2<-system.time( trs300_1_10_para_test2<-lapply(1:nrow(para_set2), function(i){
  
  cat("para_set=", i, "\n")
  cat("rate=", para_set2$l_rate[i], "\n")
  cat("eta=", para_set2$eta[i], "\n")
  
  lands<-landmark_points(X = trs300_1_10_dist, n_land = 300*para_set2$l_rate[i], d_mat = T)
  
  chng_dist<-dist_wvr_change(trs300_1_10_dist, lands = lands, eta = para_set2$eta[i])
  
  time<-system.time( mpd<-calculate_homology(mat = chng_dist, dim = 2, threshold = 3, format = "distmat") )
  
  return(list(mpd=mpd, time=time))
  
}) )

#グリッドサーチ後の平滑化PLを計算、局所最大値を数える
trs300_1_10_para_test2_pls<-lapply(trs300_1_10_para_test2, function(pd)calc_landscape(diag = pd[[1]], maxscale = 3, plot = F))
trs300_1_10_para_test2_counts<-lapply(trs300_1_10_para_test2_pls, function(pl)calc.landscape.peak(X = pl[["2-land"]], dimension = 2, tseq = pl[["tseq"]], thresh = pl[["thresh"]]/2, show = T))

#局所最大値の数をパラメータごとにプロット
par(mai=c(0.7, 0.7, 0.3, 0.3))
par(mgp=c(2, 1, 0))
plot(para_set2, xlab="landmark rate", ylab="eta", type="n", cex.lab = 1.3)
text(para_set2[, 1], para_set2[, 2], labels = round(unlist(trs300_1_10_para_test2_counts), 2),
     col = ifelse(unlist(trs300_1_10_para_test2_counts) >= 0.5 & unlist(trs300_1_10_para_test2_counts) < 1.5, 2, 4))

#一部のパラメータについてPLを一覧表示
para_part_set2<-expand.grid(c(0.2, 0.5, 0.8), rev(c(1.5, 2.5, 3.0)))
colnames(para_part_set2)<-c("l_rate", "eta")


#グラフ描画画面を分割
oldpar <- par(no.readonly = TRUE) 
par(mfrow=c(3, 3))
par(mai = c(0.3, 0.3, 0.3, 0.3))

for (p in 1:nrow(para_part_set2)){
  
  pl_idx<-which(para_set2[,1]==para_part_set2[p, 1] & para_set2[,2]==para_part_set2[p, 2])
  plot(trs300_1_10_para_test2_pls[[pl_idx]][["tseq"]], trs300_1_10_para_test2_pls[[pl_idx]][["2-land"]], ylim = c(0, 0.2), col=3, type="l", main = paste0("rate=", para_part_set2[p, 1], " eta=", para_part_set2[p, 2]), xlab = "", ylab = "")
  abline(h=trs300_1_10_para_test2_pls[[pl_idx]][["thresh"]]/2)
  
}

pl_idx<-which(para_set2$l_rate==0.8 & para_set2$eta==2.5)
plot_landscape(trs300_1_10_para_test2_pls[[pl_idx]], 2, ylim = c(0, 0.2))

#各パラメータごとにおける計算時間をプロット
trs300_1_10_para_test2_times<-sapply(trs300_1_10_para_test2, function(pd)pd[["time"]][3])

library(tagcloud)
par(mai=c(0.7, 0.7, 0.3, 0.3))
par(mgp=c(2, 1, 0))
plot(para_set2, xlab="landmark rate", ylab="eta", type="n", cex.lab = 1.3)
text(para_set2[, 1], para_set2[, 2], labels = round(trs300_1_10_para_test2_times, digits = 2), col = smoothPalette(x = trs300_1_10_para_test2_times-round(trs300_1_10_pd_time[3], digits = 2), palfunc = blue2red))

#------------------------------------------------
#2次元トーラス100セットをWVRで推定してみる-------

trs300_colle1_aggr_test<-calc_distance_change_betti(X = torus300_colle_set[[1]][1:2], maxdim = 2, maxscale = 3, samples = 5, ph_func = weighted_homology, l_rate=0.8, eta=3)

trs300_colle1_wvr_time<-system.time( trs300_colle1_wvr_aggr<-calc_distance_change_betti(X = torus300_colle_set[[1]], maxdim = 2, maxscale = 3, samples = 10, ph_func = weighted_homology, l_rate=0.8, eta=3) )

#----------------------------
#改良したベッチ数推定関数を試す

#noizyXを外す
torus300_colle_set_1to2<-map(torus300_colle_set[[1]][1:2], ~{.[["noizyX"]]})

trs300_colle1_wvr_aggr_test<-calc_distance_change_betti(X = torus300_colle_set_1to2, 
                                                        maxdim = 2, maxscale = 3, samples = 2, 
                                                        ph_func = weighted_homology, l_rate=0.8, eta=3)


trs300_colle1_aggr_test<-smooth_landscape_method_paral(X = torus300_colle_set_1to2, maxdim = 2, maxscale = 3, samples = 4)

#---------------------------------------
search_load(trs300_1_10_pd_time, path="D:/okayasu/D_documents/R/distance_ph/vars")
var_path="D:/okayasu/D_documents/R/distance_ph/vars"

