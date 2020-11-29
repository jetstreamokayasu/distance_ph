#３次元トーラス推定用のハイパラグリッドサーチ

#-------------------
#並列化準備------
#parallel使用
library(parallel)

cl <- makeCluster(4, outfile="")

clusterEvalQ(cl,{
  library(phacm)
  library(tidyverse)
  library(myfs)
  library(seephacm)
  library(TDAstats)
  library(R6)
})

clusterEvalQ(cl, {
  source('~/R/distance_ph/auto_estimate_betti_functions.R', encoding = 'UTF-8')
  source('~/R/distance_ph/dist_ch_func.R', encoding = 'UTF-8')
  source('~/R/distance_ph/R6trial.R', encoding = 'UTF-8')
  source('~/R/ph_jikken2/new-okayasu/BootstrapHomology-mk1.R', encoding = 'UTF-8')
}
)

#parallelで並列計算時に使うオジェクト読み込み
#clusterExport(cl, varlist = c("t3ours4_list2_2_sub", "para_set3"))
#clusterExport(cl, varlist = "t3orus4_list3_5")

stopCluster(cl)

#-----------------------------
#グリッドサーチのようにランドマーク点割合とハイパラetaを試す-------------
#3次元トーラス1回目
#サブサンプルt3ours4_list2_2_sub使用
#1-exp(-(d_ij/eta)^2)を掛けてみる
#parallel使用

land_rate_set2<-seq(0.05, 0.9, by=0.05)
eta_set3<-seq(3, 7, by=0.1)

para_set3<-expand.grid(land_rate_set2, eta_set3)
colnames(para_set3)<-c("l_rate", "eta")

##並列化を試す

t3ours4_list2_2_sub_para_test1_time<-system.time( t3ours4_list2_2_sub_para_test1<-parLapply(cl, 1:nrow(para_set3), function(i){
  
  wpd<-weighted_homology(X = t3ours4_list2_2_sub[[1]], maxdim = 3, maxscale = 9, extra_v = list(l_rate=para_set3$l_rate[i], eta=para_set3$eta[i]))
  
  #parallelフォルダにtxtを出力
  sink(paste0("./parallel/", "l_rate", gsub("\\.", "", para_set3$l_rate[i]), "eta", gsub("\\.", "", para_set3$eta[i]), "_", format(Sys.time(), "%m%d_%H%M"), ".txt"))
  print(paste0("l_rate=", para_set3$l_rate[i]))
  print(paste0("eta=", para_set3$eta[i]))
  sink()
  
  return(wpd)
  
}) )

#グリッドサーチ後の平滑化PLを計算、局所最大値を数える
t3ours4_list2_2_sub_para_test1_pls<-lapply(t3ours4_list2_2_sub_para_test1, function(pd)calc_landscape(diag = pd[[1]], maxscale = 9, plot = F))
t3ours4_list2_2_sub_para_test1_counts<-lapply(t3ours4_list2_2_sub_para_test1_pls, function(pl)calc.landscape.peak(X = pl[["2-land"]], dimension = 2, tseq = pl[["tseq"]], thresh = pl[["thresh"]]/2))
t3ours4_list2_2_sub_para_test1_counts_dim3<-lapply(t3ours4_list2_2_sub_para_test1_pls, function(pl)calc.landscape.peak(X = pl[["3-land"]], dimension = 2, tseq = pl[["tseq"]], thresh = pl[["thresh"]]*(2*pi)/surface_nshpere(3)))

par(mai=c(0.7, 0.7, 0.3, 0.3))
par(mgp=c(2, 1, 0))
plot(para_set3, xlab="landmark rate", ylab="eta", type="n", cex.lab = 1.3, main = "T^3 H_2")
text(para_set3$l_rate, para_set3$eta, labels = round(unlist(t3ours4_list2_2_sub_para_test1_counts), 2),
     col = ifelse(unlist(t3ours4_list2_2_sub_para_test1_counts) >= 2.5 & unlist(t3ours4_list2_2_sub_para_test1_counts) < 3.5, 2, 4), cex=0.8)

plot(para_set3, xlab="landmark rate", ylab="eta", type="n", cex.lab = 1.3, main = "T^3 H_3")
text(para_set3$l_rate, para_set3$eta, labels = round(unlist(t3ours4_list2_2_sub_para_test1_counts_dim3), 2),
     col = ifelse(unlist(t3ours4_list2_2_sub_para_test1_counts_dim3) >= 0.5 & unlist(t3ours4_list2_2_sub_para_test1_counts_dim3) < 1.5, 2, 4), cex=0.8)

h2_crect_idx<-range_index(unlist(t3ours4_list2_2_sub_para_test1_counts), 2.5, 3.5)
h3_crect_idx<-range_index(unlist(t3ours4_list2_2_sub_para_test1_counts_dim3), 0.5, 1.5)
h2h3_crect_idx<-intersect(h2_crect_idx, h3_crect_idx)

plot_landscape(t3ours4_list2_2_sub_para_test1_pls[[703]], 2)
plot_landscape(t3ours4_list2_2_sub_para_test1_pls[[703]], 3)

#各パラメータごとにおける計算時間をプロット
library(colorRamps)
t3ours4_list2_2_sub_para_test1_t<-map_dbl(t3ours4_list2_2_sub_para_test1, ~{.[["time"]][3]})
par(mai=c(0.7, 0.7, 0.3, 0.3))
par(mgp=c(2, 1, 0))
plot(para_set3, xlab="landmark rate", ylab="eta", type="n", cex.lab = 1.3)
text(para_set3$l_rate, para_set3$eta, labels = round(t3ours4_list2_2_sub_para_test1_t), 
     col = smoothPalette(x = t3ours4_list2_2_sub_para_test1_t-round(t3ours4_list2_2_sub_normal_time)[3], palfunc = blue2red), cex=0.8)

#一部のパラメータについてPLを一覧表示
para_part_set3<-expand.grid(c(0.2, 0.5, 0.8),rev(c(3.0, 5.0, 7.0)))
colnames(para_part_set3)<-c("l_rate", "eta")


#グラフ描画画面を分割
par(mfrow=c(3, 3))
par(mai = c(0.3, 0.3, 0.3, 0.3))

#2次ランドスケープ
for (p in 1:nrow(para_part_set3)){
  
  pl_idx<-which(para_set3$l_rate==para_part_set3$l_rate[p] & para_set3$eta==para_part_set3$eta[p])
  plot(t3ours4_list2_2_sub_para_test1_pls[[pl_idx]][["tseq"]], t3ours4_list2_2_sub_para_test1_pls[[pl_idx]][["2-land"]], ylim = c(0, 1.5), col=3, type="l", main = paste0("rate=", para_part_set3$l_rate[p], " eta=", para_part_set3$eta[p]), xlab = "", ylab = "")
  abline(h=t3ours4_list2_2_sub_para_test1_pls[[pl_idx]][["thresh"]]/2)
  
}

#3次ランドスケープ
for (p in 1:nrow(para_part_set3)){
  
  pl_idx<-which(para_set3$l_rate==para_part_set3$l_rate[p] & para_set3$eta==para_part_set3$eta[p])
  plot(t3ours4_list2_2_sub_para_test1_pls[[pl_idx]][["tseq"]], t3ours4_list2_2_sub_para_test1_pls[[pl_idx]][["3-land"]], ylim = c(0, 0.4), col=4, type="l", main = paste0("rate=", para_part_set3$l_rate[p], " eta=", para_part_set3$eta[p]), xlab = "", ylab = "")
  abline(h=t3ours4_list2_2_sub_para_test1_pls[[pl_idx]][["thresh"]]*(2*pi)/surface_nshpere(3))
  
}

#グリッドサーチでH2もH3も正しい値だったハイパラセットのPLを描画

par(mfrow=c(2, 5))
par(mai = c(0.3, 0.3, 0.3, 0.3))

for (i in h2h3_crect_idx){
  
  plot(t3ours4_list2_2_sub_para_test1_pls[[i]][["tseq"]], t3ours4_list2_2_sub_para_test1_pls[[i]][["3-land"]], ylim = c(0, 0.4), col=4, type="l", main = paste0("rate=", para_set3$l_rate[i], " eta=", para_set3$eta[i]), xlab = "", ylab = "")
  abline(h=t3ours4_list2_2_sub_para_test1_pls[[i]][["thresh"]]*(2*pi)/surface_nshpere(3))
  
}

for (i in h2h3_crect_idx){
  
  plot(t3ours4_list2_2_sub_para_test1_pls[[i]][["tseq"]], t3ours4_list2_2_sub_para_test1_pls[[i]][["2-land"]], ylim = c(0, 1.5), col=3, type="l", main = paste0("rate=", para_set3$l_rate[i], " eta=", para_set3$eta[i]), xlab = "", ylab = "")
  abline(h=t3ours4_list2_2_sub_para_test1_pls[[i]][["thresh"]]/2)
  
}

#----------------------------------------------------
#3次元トーラスで成功率を求めてみる-------------
#r = 2, R1 = 8, R2 = 4
#全データセット数5、サブサンプル数5
#eta=3.7, ランドマーク点65%
t3orus4_test_aggrs_time<-system.time( t3orus4_test_aggrs<-calc_paral_distance_change_betti(X = t3orus4_list2[1:2], maxdim = 3, maxscale = 9, samples = 3, ph_func = weighted_homology, l_rate=0.65, eta=3.7) )

t3orus4_list3<- lapply(1:100, function(i){
  nsample <- 500
  torus <- x3Dtorus_unif(n = nsample, r = 2, R1 = 8, R2 = 4)
  return(list(nsample = nsample, noizyX = torus, diag = 0))
})

t3orus4_list3_1to50aggrs_time<-system.time( t3orus4_list3_1to50aggrs<-calc_paral_distance_change_betti(X = t3orus4_list3[1:50], maxdim = 3, maxscale = 9, samples = 10, ph_func = weighted_homology, l_rate=0.8, eta=4.7) )

#-------------------------------
#3次元トーラスで成功率を求めてみる----
#H3の推定がうまくいかないので試す
#r = 2, R1 = 8, R2 = 4
#全データセット数30、サブサンプル数10
#eta=3.8, ランドマーク点50%
t3orus4_list3_1to30aggrs_time<-system.time(
  t3orus4_list3_1to30aggrs<-calc_distance_change_betti_paral(X = t3orus4_list3[1:30], maxdim = 3, maxscale = 9, samples = 10, 
                                                             ph_func = weighted_homology, l_rate=0.5, eta=3.8) )
#------------------------------
#サブサンプルのPLを観察する----
t3orus4_list3_5<-TDAdataset$new(data = t3orus4_list3[[5]][["noizyX"]])

#サブサンプル作成
t3orus4_list3_5$create_subsample(sub_size = t3orus4_list3_5$n_points*0.8, n_subs = 10)

#距離行列変化
tmp<-lapply(t3orus4_list3_5$subsamples, 
            function(X){X$distmat_c$change_dist(l_rate=0.5, eta=3.8)})

#parallel用いてPD計算
t3orus4_list3_5_subs_pd<-parLapply(cl, t3orus4_list3_5$subsamples, function(X){
  
  X$distmat_c$calc_pd(maxdim=3, maxscale=9)
  
  #parallelフォルダにtxtを出力
  sink(paste0("./parallel/", "l_rate", gsub("\\.", "", X$distmat_c$l_rate), "eta", gsub("\\.", "", X$distmat_c$eta), "_", format(Sys.time(), "%m%d_%H%M"), ".txt"))
  print(paste0("l_rate=", X$distmat_c$l_rate))
  print(paste0("eta=", X$distmat_c$eta))
  print(paste0("time=", X$distmat_c$get_time()[3]))
  sink()
  
  return(X$distmat_c$get_pd())
  
  })

t3orus4_list3_5_subs_pl<-lapply(t3orus4_list3_5_subs_pd, function(pd)calc_landscape(diag = pd, maxscale = 9))

#----------------------------------------
#サブサンプルでハイパラグリッドサーチ----

eta_set4<-seq(5, 8, by=0.2)
land_rate_set3<-seq(0.1, 0.8, by=0.1)
para_set4<-expand.grid(land_rate_set3, eta_set4)
colnames(para_set4)<-c("l_rate", "eta")

t3orus4_list3_5_subs<-map(t3orus4_list3_5$subsamples, ~{.$data})

#parallelで並列計算時に使うオジェクト読み込み
clusterExport(cl, varlist = c("t3orus4_list3_5_subs", "para_set4"))

t3ours4_list3_5_sub_gs1_time<-system.time( t3ours4_list3_5_sub_gs1<-parLapply(cl, 1:nrow(para_set4), function(p){
  
  wpd<-weighted_homology(X = t3orus4_list3_5_subs[[1]], maxdim = 3, maxscale = 9, l_rate = para_set4$l_rate[p], eta = para_set4$eta[p])
  
  #parallelフォルダにtxtを出力
  sink(paste0("./parallel/", "l_rate", gsub("\\.", "", para_set4$l_rate[p]), "eta", gsub("\\.", "", para_set4$eta[p]), "_", format(Sys.time(), "%m%d_%H%M"), ".txt"))
  print(paste0("l_rate=", para_set4$l_rate[p]))
  print(paste0("eta=", para_set4$eta[p]))
  print(paste0("time=", wpd[["time"]][3]))
  sink()
  
  return(wpd)
  
}) )
