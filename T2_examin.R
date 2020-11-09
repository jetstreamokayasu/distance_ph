#密度が低い場合にスケールを変化させた場合のPHがどうなるかを調べる
half_trs1<-torus1/2

hf_trs1_pd<-calculate_homology(half_trs1, dim = 2, threshold = 3)
hf_trs1_pl<-calcLandscape(diag = hf_trs1_pd, maxscale = 3)

#torus300_colle_set[[1]]を使って300点トーラス100セットのFRI距離行列変化の実験をする
trs300_1_fri_aggr_time<-system.time( trs300_1_fri_aggr<-fri_distance_change_method(torus300_colle_set[[1]], maxdim = 2, maxscale = 3, samples = 10, l_rate = 0.3, eta = 1) )

trs300_1_fri_aggr2_time<-system.time( trs300_1_fri_aggr2<-fri_distance_change_method(torus300_colle_set[[1]], maxdim = 2, maxscale = 3, samples = 10, l_rate = 0.45, eta = 1) )


#--------------------------------------------
#グリッドサーチのようにランドマーク点割合とハイパラetaを試す

land_rate_set<-seq(0.1, 0.8, by=0.05)
eta_set<-seq(0.5, 2.5, by=0.1)

para_set<-expand.grid(land_rate_set, eta_set)

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
     col = ifelse(unlist(trs300_1_10_para_test_counts) >= 1.5 & unlist(trs300_1_10_para_test_counts) < 2.5, 2, 4))

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
