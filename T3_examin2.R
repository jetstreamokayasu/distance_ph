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

#サブセット1をグリッドサーチ
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

#サブセット2~5をグリッドサーチ
t3ours4_list3_5_sub_gs2to5_time<-system.time( 
  
  t3ours4_list3_5_sub_gs2to5<-lapply(2:5, function(i){
    
    sub_gs<-parLapply(cl, 1:nrow(para_set4), function(p){
      
      wpd<-weighted_homology(X = t3orus4_list3_5_subs[[i]], maxdim = 3, maxscale = 9, l_rate = para_set4$l_rate[p], eta = para_set4$eta[p])
      
      #parallelフォルダにtxtを出力
      sink(paste0("./parallel/", "data", i, "_", "lrate", gsub("\\.", "", para_set4$l_rate[p]), "eta", gsub("\\.", "", para_set4$eta[p]), "_", format(Sys.time(), "%m%d_%H%M"), ".txt"))
      print(paste0("l_rate=", para_set4$l_rate[p]))
      print(paste0("eta=", para_set4$eta[p]))
      print(paste0("time=", wpd[["time"]][3]))
      sink()
      
      return(wpd)
      
    })
    
    return(sub_gs)
    
  })
  
  )

#サブセット6~10をグリッドサーチ
t3ours4_list3_5_sub_gs6to10_time<-system.time( 
  
  t3ours4_list3_5_sub_gs6to10<-lapply(6:10, function(i){
    
    sub_gs<-parLapply(cl, 1:nrow(para_set4), function(p){
      
      wpd<-weighted_homology(X = t3orus4_list3_5_subs[[i]], maxdim = 3, maxscale = 9, l_rate = para_set4$l_rate[p], eta = para_set4$eta[p])
      
      #parallelフォルダにtxtを出力
      sink(paste0("./parallel/", "data", i, "_", "lrate", gsub("\\.", "", para_set4$l_rate[p]), "eta", gsub("\\.", "", para_set4$eta[p]), "_", format(Sys.time(), "%m%d_%H%M"), ".txt"))
      print(paste0("l_rate=", para_set4$l_rate[p]))
      print(paste0("eta=", para_set4$eta[p]))
      print(paste0("time=", wpd[["time"]][3]))
      sink()
      
      return(wpd)
      
    })
    
    return(sub_gs)
    
  })
  
)


#------------------------------------
#グリッドサーチ計算結果まとめ-----

#サブサンプル2~5のPLをまとめる
t3ours4_list3_5_sub_gs2to5_pls<-lapply(t3ours4_list3_5_sub_gs2to5, function(X){
  
  sub_pls<-lapply(X, function(Y){calc_landscape(diag = Y[["pd"]], maxscale = 9, plot = F)})
  
})

#サブサンプル2~5のPLの局所最大値をまとめる
t3ours4_list3_5_sub_gs2to5_peaks_H3<-lapply(t3ours4_list3_5_sub_gs2to5_pls, function(X){
  
  sub_pls<-sapply(X, function(Y){calc.landscape.peak(X = Y[["3-land"]], dimension = 3, thresh = Y[["thresh"]]*(2*pi)/surface_nshpere(3), tseq = Y[["tseq"]])})
  
})

#局所最大値の数をパラメータごとにプロット
par(mai=c(0.7, 0.7, 0.3, 0.3))
par(mgp=c(2, 1, 0))
#サブサンプル2
plot(para_set4, xlab="landmark rate", ylab="eta", type="n", cex.lab = 1.3)
text(para_set4$l_rate, para_set4$eta, labels = round(unlist(t3ours4_list3_5_sub_gs2to5_peaks_H3[[1]]), 2),
     col = ifelse(t3ours4_list3_5_sub_gs2to5_peaks_H3[[1]] >= 0.5 & t3ours4_list3_5_sub_gs2to5_peaks_H3[[1]] < 1.5, 2, 4))

#サブサンプル3
plot(para_set4, xlab="landmark rate", ylab="eta", type="n", cex.lab = 1.3)
text(para_set4$l_rate, para_set4$eta, labels = round(unlist(t3ours4_list3_5_sub_gs2to5_peaks_H3[[3]]), 2),
     col = ifelse(t3ours4_list3_5_sub_gs2to5_peaks_H3[[3]] >= 0.5 & t3ours4_list3_5_sub_gs2to5_peaks_H3[[3]] < 1.5, 2, 4))

#サブサンプル1
plot(para_set4, xlab="landmark rate", ylab="eta", type="n", cex.lab = 1.3)
text(para_set4$l_rate, para_set4$eta, labels = round(unlist(t3ours4_list3_5_sub_gs1_6to10_peaks_H3[[1]]), 2),
     col = ifelse(t3ours4_list3_5_sub_gs1_6to10_peaks_H3[[1]] >= 0.5 & t3ours4_list3_5_sub_gs1_6to10_peaks_H3[[1]] < 1.5, 2, 4))


#サブサンプル2~5のPLの局所最大値の平均
t3ours4_list3_5_sub_gs2to5_peaks_ave_H3<-sapply(1:length(t3ours4_list3_5_sub_gs2to5_peaks_H3[[1]]), function(i){
  mpeaks<-sapply(t3ours4_list3_5_sub_gs2to5_peaks_H3, function(PL){PL[[i]]}) %>% mean()
})

#局所最大値の数をパラメータごとにプロット
#サブサンプル2~5平均
plot(para_set4, xlab="landmark rate", ylab="eta", type="n", cex.lab = 1.3)
text(para_set4$l_rate, para_set4$eta, labels = round(t3ours4_list3_5_sub_gs2to5_peaks_ave_H3, 2),
     col = ifelse(t3ours4_list3_5_sub_gs2to5_peaks_ave_H3 >= 0.5 & t3ours4_list3_5_sub_gs2to5_peaks_ave_H3 < 1.5, 2, 4))

#サブサンプルのGS結果をまとめる
t3ours4_list3_5_sub_gs<-c(list(t3ours4_list3_5_sub_gs1), t3ours4_list3_5_sub_gs2to5, t3ours4_list3_5_sub_gs6to10)

#サブサンプル1, 6~10のPLをまとめる
t3ours4_list3_5_sub_gs1_6to10_pls<-lapply(t3ours4_list3_5_sub_gs[c(1, 6:10)], function(X){
  
  sub_pls<-lapply(X, function(Y){calc_landscape(diag = Y[["pd"]], maxscale = 9, plot = F)})
  
})

#サブサンプル1, 6~10のPLの局所最大値をまとめる
t3ours4_list3_5_sub_gs1_6to10_peaks_H3<-lapply(t3ours4_list3_5_sub_gs1_6to10_pls, function(X){
  
  sub_pls<-sapply(X, function(Y){calc.landscape.peak(X = Y[["3-land"]], dimension = 3, thresh = Y[["thresh"]]*(2*pi)/surface_nshpere(3), tseq = Y[["tseq"]])})
  
})

#サブサンプル1~10のPLの局所最大値をまとめる
t3ours4_list3_5_sub_gs_peaks_H3<-c(t3ours4_list3_5_sub_gs1_6to10_peaks_H3[1], t3ours4_list3_5_sub_gs2to5_peaks_H3, t3ours4_list3_5_sub_gs1_6to10_peaks_H3[2:6])

#サブサンプル1~10のPLの局所最大値の平均
t3ours4_list3_5_sub_gs_peaks_H3_ave<-sapply(1:length(t3ours4_list3_5_sub_gs_peaks_H3[[1]]), function(i){
  mpeaks<-sapply(t3ours4_list3_5_sub_gs_peaks_H3, function(PL){PL[[i]]}) %>% mean()
})

#局所最大値の数をパラメータごとにプロット
#サブサンプル1~10平均
plot(para_set4, xlab="landmark rate", ylab="eta", type="n", cex.lab = 1.3)
text(para_set4$l_rate, para_set4$eta, labels = round(t3ours4_list3_5_sub_gs_peaks_H3_ave, 2),
     col = ifelse(t3ours4_list3_5_sub_gs_peaks_H3_ave >= 0.5 & t3ours4_list3_5_sub_gs_peaks_H3_ave < 1.5, 2, 4))

#-------------------------------
#GS結果をH2に関してまとめる-----

#サブサンプル2~5のPLのH2局所最大値をまとめる
t3ours4_list3_5_sub_gs2to5_peaks_H2<-lapply(t3ours4_list3_5_sub_gs2to5_pls, function(X){
  
  sub_pls<-sapply(X, function(Y){calc.landscape.peak(X = Y[["2-land"]], dimension = 2, thresh = Y[["thresh"]]*(2*pi)/surface_nshpere(2), tseq = Y[["tseq"]])})
  
})

#サブサンプル1, 6~10のPLのH2局所最大値をまとめる
t3ours4_list3_5_sub_gs1_6to10_peaks_H2<-lapply(t3ours4_list3_5_sub_gs1_6to10_pls, function(X){
  
  sub_pls<-sapply(X, function(Y){calc.landscape.peak(X = Y[["2-land"]], dimension = 2, thresh = Y[["thresh"]]*(2*pi)/surface_nshpere(2), tseq = Y[["tseq"]])})
  
})

#サブサンプル1~10のPLのH2局所最大値をまとめる
t3ours4_list3_5_sub_gs_peaks_H2<-c(t3ours4_list3_5_sub_gs1_6to10_peaks_H2[1], t3ours4_list3_5_sub_gs2to5_peaks_H2, t3ours4_list3_5_sub_gs1_6to10_peaks_H2[2:6])

#サブサンプル1~10のPLのH2局所最大値の平均
t3ours4_list3_5_sub_gs_peaks_H2_ave<-sapply(1:length(t3ours4_list3_5_sub_gs_peaks_H2[[1]]), function(i){
  mpeaks<-sapply(t3ours4_list3_5_sub_gs_peaks_H2, function(PL){PL[[i]]}) %>% mean()
})

#H2局所最大値の数をパラメータごとにプロット
#サブサンプル1~10平均
plot(para_set4, xlab="landmark rate", ylab="eta", type="n", cex.lab = 1.3)
text(para_set4$l_rate, para_set4$eta, labels = round(t3ours4_list3_5_sub_gs_peaks_H2_ave, 2),
     col = ifelse(t3ours4_list3_5_sub_gs_peaks_H2_ave >= 2.5 & t3ours4_list3_5_sub_gs_peaks_H2_ave < 3.5, 2, 4))


#-------------------
#サブサンプル1~10の計算時間をまとめる------
t3ours4_list3_5_sub_gs_times<-lapply(t3ours4_list3_5_sub_gs, function(X){
  
  sub_pls<-sapply(X, function(Y){Y$time[3]})
  
})

#サブサンプル1~10の計算時間平均
t3ours4_list3_5_sub_gs_times_ave<-sapply(1:length(t3ours4_list3_5_sub_gs_times[[1]]), function(i){
  mtimes<-sapply(t3ours4_list3_5_sub_gs_times, function(PL){PL[[i]]}) %>% mean()
})

#計算時間プロット
plot(para_set4, xlab="landmark rate", ylab="eta", type="n", cex.lab = 1.3)
text(para_set4$l_rate, para_set4$eta, labels = round(t3ours4_list3_5_sub_gs_times_ave), col = smoothPalette(x = t3ours4_list3_5_sub_gs_times_ave, palfunc = blue2red))


#----------------------------
#距離の分布をプロット--------
t3orus4_list3_5_subs2_dist<-t3orus4_list3_5$subsamples[[2]]$distmat_c$distmat
t3orus4_list3_5_subs2_altdist<-dist_wvr_change(X_dist = t3orus4_list3_5_subs2_dist, lands = t3ours4_list3_5_sub_gs2to5[[1]][[125]][["l_idx"]], eta = 8.0)

t3orus4_list_3_5_sub2_plt1<-ggplot() + geom_point(aes(x = t3orus4_list3_5_subs2_dist[51, ], y = t3orus4_list3_5_subs2_dist[51, ]), size=3, shape = 1)
t3orus4_list_3_5_sub2_plt2<-t3orus4_list_3_5_sub2_plt1 + theme(axis.text=element_text(size=14))
t3orus4_list_3_5_sub2_plt3<-t3orus4_list_3_5_sub2_plt2 + geom_point(aes(x = t3orus4_list3_5_subs2_dist[51, ], y = t3orus4_list3_5_subs2_altdist[51, ]), size=3, shape = 1, color = 2)




#----------------------------------------
#サブサンプルでハイパラグリッドサーチ。H2成功率確認用----

eta_set5<-seq(1, 4, by=0.2)
land_rate_set4<-seq(0.1, 0.9, by=0.1)
para_set5<-expand.grid(land_rate_set4, eta_set5)
colnames(para_set5)<-c("l_rate", "eta")


#parallelで並列計算時に使うオジェクト読み込み
clusterExport(cl, varlist = c("t3orus4_list3_5_subs", "para_set5"))

#サブセット1~10をグリッドサーチ
t3ours4_list3_5_sub_gsH2_time<-system.time( 
  
  t3ours4_list3_5_sub_gsH2<-lapply(1:10, function(i){
    
    sub_gs<-parLapply(cl, 1:nrow(para_set5), function(p){
      
      wpd<-weighted_homology(X = t3orus4_list3_5_subs[[i]], maxdim = 3, maxscale = 9, l_rate = para_set5$l_rate[p], eta = para_set5$eta[p])
      
      #parallelフォルダにcsvを出力
      write.csv(as.data.frame(wpd[["pd"]]), 
                file = paste0("./parallel/", "data", i, "_", "lrate", 
                              gsub("\\.", "", para_set5$l_rate[p]), "eta", gsub("\\.", "", para_set5$eta[p]), 
                              "_", format(Sys.time(), "%m%d_%H%M"), ".csv"))
      
      return(wpd)
      
    })
    
    return(sub_gs)
    
  })
)

#サブサンプルのPLをまとめる
t3ours4_list3_5_sub_gsH2_pls<-lapply(t3ours4_list3_5_sub_gsH2, function(X){
  
  sub_pls<-lapply(X, function(Y){calc_landscape(diag = Y[["pd"]], maxscale = 9, plot = F)})
  
})

#サブサンプルのPLのH2局所最大値をまとめる
t3ours4_list3_5_sub_gsH2_peaks_H2<-lapply(t3ours4_list3_5_sub_gsH2_pls, function(X){
  
  sub_pls<-sapply(X, function(Y){calc.landscape.peak(X = Y[["2-land"]], dimension = 2, thresh = Y[["thresh"]]*(2*pi)/surface_nshpere(2), tseq = Y[["tseq"]], show = F)})
  
})

#サブサンプル1~10のPLのH2局所最大値の平均
t3ours4_list3_5_sub_gsH2_peaks_H2_ave<-sapply(1:length(t3ours4_list3_5_sub_gsH2_peaks_H2[[1]]), function(i){
  mpeaks<-sapply(t3ours4_list3_5_sub_gsH2_peaks_H2, function(PL){PL[[i]]}) %>% mean()
})

#H2局所最大値の数をパラメータごとにプロット
#サブサンプル1~10平均
par(mai=c(0.7, 0.7, 0.3, 0.3))
par(mgp=c(2, 1, 0))
plot(para_set5, xlab="landmark rate", ylab="eta", type="n", cex.lab = 1.3)
text(para_set5$l_rate, para_set5$eta, labels = round(t3ours4_list3_5_sub_gsH2_peaks_H2_ave, 2),
     col = ifelse(t3ours4_list3_5_sub_gsH2_peaks_H2_ave >= 2.5 & t3ours4_list3_5_sub_gsH2_peaks_H2_ave < 3.5, 2, 4), cex = 0.8)

plot(rbind(para_set4, para_set5), xlab="landmark rate", ylab="eta", type="n", cex.lab = 1.3)
text(para_set4$l_rate, para_set4$eta, labels = round(t3ours4_list3_5_sub_gs_peaks_H2_ave, 2),
     col = ifelse(t3ours4_list3_5_sub_gs_peaks_H2_ave >= 2.5 & t3ours4_list3_5_sub_gs_peaks_H2_ave < 3.5, 2, 4), cex = 0.8)

#---------------------------------
#ggplotでH2局所最大値の数をパラメータごとにプロット----
#サブサンプル1~10平均
para_gs_plt<-ggplot(data = para_set5) + geom_text(aes(x = l_rate, y = eta, label = round(t3ours4_list3_5_sub_gsH2_peaks_H2_ave, 2), color = as.factor( if_else(t3ours4_list3_5_sub_gsH2_peaks_H2_ave >= 2.5 & t3ours4_list3_5_sub_gsH2_peaks_H2_ave < 3.5, 1, 2) )))
para_gs_plt3<-para_gs_plt + geom_text(data = para_set4, aes(x = l_rate, y = eta, label = round(t3ours4_list3_5_sub_gs_peaks_H2_ave, 2), col = as.factor( if_else(t3ours4_list3_5_sub_gs_peaks_H2_ave >= 2.5 & t3ours4_list3_5_sub_gs_peaks_H2_ave < 3.5, 1, 2) )))
para_gs_plt2<-para_gs_plt3 + scale_color_brewer(palette = "Set1") + guides(color="none") + theme_test()
