#n次元直方体のベッチ数推定実験

#----------------------------
#３次元直方体
cuboid3d_1<-xRect_unif(100, sides = c(1, 1, 3))

cuboid3d_2<-xRect_unif(50, sides = c(1, 1, 1))

cuboid3d_2$rect[, 2] %<>% add(3) 
cuboid3d_1and2<-rbind(cuboid3d_1$rect, cuboid3d_2$rect)

cuboid3d_1and2_inst<-TDAdataset$new(cuboid3d_1and2)

{pd_start<-Sys.time()
calculate_homology(mat = cuboid3d_1and2, dim = 2) %>% cuboid3d_1and2_inst$input_pd()
pd_end<-Sys.time()}

#-------------------------------
#7次元直方体(８次元空間に埋め込まれた)---------

cuboid7d_1<-xRect_unif(n = 200, sides = c(rep(1, length = 7), 8), d = 8)

cuboid7d_1_inst<-TDAdataset$new(cuboid7d_1$rect)

{
pd_start<-Sys.time()
calculate_homology(mat = cuboid7d_1$rect, dim = 7) %>% cuboid7d_1_inst$input_pd()
pd_end<-Sys.time()
}

#-------------------------------
#5次元直方体(6次元空間に埋め込まれている)---------

cuboid5d_1<-xRect_unif(n = 100, sides = c(rep(1, length = 5), 5), d = 6)

cuboid5d_1_inst<-TDAdataset$new(cuboid5d_1$rect)

{
  pd_start<-Sys.time()
  calculate_homology(mat = cuboid5d_1$rect, dim = 5) %>% cuboid5d_1_inst$input_pd()
  pd_end<-Sys.time()
}

#-------------------------------
#4次元直方体(5次元空間に埋め込まれている)---------

#４次元直方体1つ目
cuboid4d_1<-xRect_unif(n = 100, sides = c(rep(1, length = 4), 5), d = 5)

cuboid4d_1_inst<-TDAdataset$new(cuboid4d_1$rect)


{
  pd_start<-Sys.time()
  calculate_homology(mat = cuboid4d_1$rect, dim = 4) %>% cuboid4d_1_inst$input_pd()
  pd_end<-Sys.time()
}

#４次元直方体2つ目
cuboid4d_2<-xRect_unif(n = 200, sides = c(rep(1, length = 4), 5), d = 5)

cuboid4d_2_inst<-TDAdataset$new(cuboid4d_2)

{ 
  pd_start<-Sys.time()
  calculate_homology(mat = cuboid4d_2, dim = 4) %>% cuboid4d_2_inst$input_pd()
  pd_end<-Sys.time()
}

#４次元直方体3つ目
cuboid4d_3<-xRect_unif(n = 150, sides = c(rep(1, length = 4), 5), d = 5)

cuboid4d_3_inst<-TDAdataset$new(cuboid4d_3)
cuboid4d_3_inst$calc_pd(maxdim = 4, maxscale = 3)

#4次元直方体4つ目
cuboid4d_4<-xRect_unif(n = 180, sides = c(rep(1, length = 4), 5), d = 5)

cuboid4d_4_inst<-TDAdataset$new(cuboid4d_4)
cuboid4d_4_inst$calc_pd(maxdim = 4, maxscale = 2)

cuboid4d_4_inst$create_changed_distmat(l_rate = 0.5, eta = 1.2)
cuboid4d_4_inst$alt_distmat[[1]]$calc_pd(maxdim = 4, maxscale = 2)

cuboid4d_4_inst$get_pd() %>% as_diag() %>% plot(diagLim=c(0, 1.5))
cuboid4d_4_inst$alt_distmat[[1]]$get_pd() %>% as_diag() %>% plot(diagLim=c(0, 1.5))

cuboid4d_4_inst$plot_pl(dim = 4, ylim = c(0, 0.1))
cuboid4d_4_inst$alt_distmat[[1]]$plot_pl(dim = 4, ylim = c(0, 0.1))

#4次元直方体5つ目
cuboid4d_5<-xRect_unif(n = 250, sides = c(rep(1, length = 4), 5), d = 5)

#------------------------------------
#CTIC手法で４次元直方体成功率実験----------
#180点の4次元立方体

cube4d_180_list1<-map(1:100, ~xRect_unif(n = 180, sides = c(rep(1, length = 4), 5), d = 5))
save2RData(cube4d_180_list1)

{
cube4d_180_lst1_1to50_time<-system.time(
cube4d_180_lst1_1to50_aggr<-smooth_landscape_method(X = cube4d_180_list1[1:50], maxdim = 4, maxscale = 2, samples = 10)
)

save2RData(cube4d_180_lst1_1to50_time)
save2RData(cube4d_180_lst1_1to50_aggr)
}

{
  cube4d_180_lst1_51to100_time<-system.time(
    cube4d_180_lst1_51to100_aggr<-smooth_landscape_method_paral(X = cube4d_180_list1[51:100], maxdim = 4, maxscale = 2, samples = 10, ncl = 4)
  )
  
  save2RData(cube4d_180_lst1_51to100_time)
  save2RData(cube4d_180_lst1_51to100_aggr)
}

#-------------------------
#結合時刻変化手法で４次元直方体成功率実験----------
#180点の4次元立方体
#l_rate=0.5, eta=1.2
{
  cube4d_180_lst1_51to100_wvr_time<-system.time(
    cube4d_180_lst1_51to100_wvr_aggr<-calc_distance_change_betti_paral(X = cube4d_180_list1[51:100], maxdim = 4, maxscale = 2, samples = 10, ncl = 4, 
                                                                       ph_func = weighted_homology, l_rate=0.5, eta=1.2)
  )
  
  save2RData(cube4d_180_lst1_51to100_wvr_time)
  save2RData(cube4d_180_lst1_51to100_wvr_aggr)
}

#-----------------------
#結合時刻変化手法のハイパラチューニング

cb4d_180_lst1_84_inst<-TDAdataset$new(cube4d_180_list1[[84]])
cb4d_180_lst1_84_inst$create_subsample(sub_size = cb4d_180_lst1_84_inst$n_points*0.8, n_subs = 1)

cb4d_180_lst1_84_inst$subsamples[[1]]$calc_pd(maxdim = 4, maxscale = 2)
cb4d_180_lst1_84_inst$subsamples[[1]]$create_changed_distmat(l_rate = 0.5, eta = 1.24)
cb4d_180_lst1_84_inst$subsamples[[1]]$alt_distmat[[1]]$calc_pd(maxdim = 4, maxscale = 2)

cb4d_180_lst1_84_inst$subsamples[[1]]$create_changed_distmat(l_rate = 0.8, eta = 1.24)
cb4d_180_lst1_84_inst$subsamples[[1]]$alt_distmat[[2]]$calc_pd(maxdim = 4, maxscale = 2)

cb4d_180_lst1_84_inst$subsamples[[1]]$create_changed_distmat(l_rate = 0.3, eta = 1.24)
cb4d_180_lst1_84_inst$subsamples[[1]]$alt_distmat[[3]]$calc_pd(maxdim = 4, maxscale = 2)

cb4d_180_lst1_84_inst$subsamples[[1]]$create_changed_distmat(l_rate = 0.4, eta = 1.24)
cb4d_180_lst1_84_inst$subsamples[[1]]$alt_distmat[[4]]$calc_pd(maxdim = 4, maxscale = 2)

#-------------------------
#結合時刻変化手法で４次元直方体成功率実験----------
#180点の4次元立方体。リスト1つ目の1~20セット使用
#l_rate=0.4, eta=1.24

{
  cube4d_180_lst1_1to20_wvr_time<-system.time(
    cube4d_180_lst1_1to20_wvr_aggr<-calc_distance_change_betti_paral(X = cube4d_180_list1[1:20], maxdim = 4, maxscale = 2, samples = 10, ncl = 6, 
                                                                       ph_func = weighted_homology, l_rate=0.4, eta=1.24)
  )
  
  save2RData(cube4d_180_lst1_1to20_wvr_time)
  save2RData(cube4d_180_lst1_1to20_wvr_aggr)
}

cube4d_180_lst1_1to20_wvr_aggrB<-calc_distance_change_betti_paral(X = cube4d_180_list1[1:20], maxdim = 4, maxscale = 2, samples = 10, ncl = 6, 
                                                                 ph_func = weighted_homology, l_rate=0.5, eta=1.3)

#----------------------------------------
#4次元直方体のサブサンプルでハイパラグリッドサーチ。------
#H4成功率確認用

cl <- makeCluster(4, outfile="")
registerDoParallel(cl)

clusterEvalQ(cl,{
  library(phacm)
  library(tidyverse)
  library(myfs)
  library(usephacm)
  library(TDAstats)
  library(TDA)
})


stopCluster(cl)

#グリッドサーチ用ハイパラ
cube_eta1<-seq(1.0, 1.4, by=0.05)
cube_lrate1<-seq(0.1, 0.8, by=0.05)
cube_para1<-expand.grid(cube_eta1, cube_lrate1)
colnames(cube_para1)<-c("eta", "l_rate")


#4D直方体のサブサンプルリスト
#cube4d_180_lst1_84_subs<-usephacm:::bootstrapper(X = cube4d_180_list1[[84]], size = nrow(cube4d_180_list1[[84]])*0.8, samples = 5)

#parallelで並列計算時に使うオジェクト読み込み
clusterExport(cl, varlist = c("cube4d_180_lst1_84_subs", "cube_para1"))

#サブセット1~5をグリッドサーチ
cube4d_180_lst1_84_subs_time<-system.time( 
  
  cube4d_180_lst1_84_subs_gs<-lapply(1:length(cube4d_180_lst1_84_subs), function(i){
    
    sub_gs<-parApply(cl, cube_para1, 1, function(p){
      
      wpd<-weighted_homology(X = cube4d_180_lst1_84_subs[[i]], maxdim = 4, maxscale = 2, l_rate = p[2], eta = p[1])
      
      #parallelフォルダにcsvを出力
      write.csv(as.data.frame(wpd[["pd"]]), 
                file = paste0("./parallel/", "data", i, "_", "lrate", 
                              gsub("\\.", "", p[2]), "eta", gsub("\\.", "", p[1]), 
                              "_", format(Sys.time(), "%m%d_%H%M"), ".csv"))
      
      return(wpd)
      
    })
    
    return(sub_gs)
    
  })
)

#途中で落ちたので5セット目のみ計算しなおす
cube4d_180_lst1_84_subs_5_gs_time<-system.time(
cube4d_180_lst1_84_subs_5_gs<-parApply(cl, cube_para1, 1, function(p){
  
  wpd<-weighted_homology(X = cube4d_180_lst1_84_subs[[5]], maxdim = 4, maxscale = 2, l_rate = p[2], eta = p[1])
  
  #parallelフォルダにcsvを出力
  write.csv(as.data.frame(wpd[["pd"]]), 
            file = paste0("./parallel/", "data", 5, "_", "lrate", 
                          gsub("\\.", "", p[2]), "eta", gsub("\\.", "", p[1]), 
                          "_", format(Sys.time(), "%m%d_%H%M"), ".csv"))
  
  return(wpd)
  
})
)

#---------------------------------
#csvに出力されたPDを読み込む---------

list.files(path = "./parallel", pattern = paste0("data", 1, "_", "lrate", gsub("\\.", "", cube_para1$l_rate[27]), "eta", gsub("\\.", "", cube_para1$eta[27]), "_0305"), full.names = T)

cube4d_180_lst1_84_subs_gs_pd1<-list.files(path = "./parallel", pattern = paste0("data", 1, "_", "lrate", gsub("\\.", "", cube_para1$l_rate[27]), "eta", gsub("\\.", "", cube_para1$eta[27]), "_0305"), full.names = T) %>% 
  read_csv(col_types = "_idd")

cube4d_180_lst1_84_subs_gs_1to3<-map(1:3, function(i){map(1:nrow(cube_para1), ~{
  
  wpd<-list.files(path = "./parallel", pattern = paste0("data", i, "_", "lrate", gsub("\\.", "", cube_para1$l_rate[.x]), "eta", gsub("\\.", "", cube_para1$eta[.x]), "_0305"), full.names = T) %>% 
  read_csv(col_types = "_idd")
  
  return(lst(wpd=wpd, l_rate=cube_para1$l_rate[.x], cube_para1$eta[.x]))
  
  })})

#サブサンプル4つ目
cube4d_180_lst1_84_subs_gs_4<-map(1:nrow(cube_para1), ~{
  
  file<-list.files(path = "./parallel", pattern = paste0("data", 4, "_", "lrate", gsub("\\.", "", cube_para1$l_rate[.x]), "eta", gsub("\\.", "", cube_para1$eta[.x]), "_0305"), full.names = T)
  
  if(is_empty(file)){return(file)}
  else{
    
    wpd<-read_csv(file, col_types = "_idd")
    return(lst(wpd=wpd, l_rate=cube_para1$l_rate[.x], cube_para1$eta[.x]))
  }
  
})

every(cube4d_180_lst1_84_subs_gs_4, ~{equals(length(.), 3)}) #%>% which()

#サブサンプル5のグリッドサーチ結果リストのname変更、ハイパラ追加

cube4d_180_lst1_84_subs_5_gs_rename<-map(seq_along(cube4d_180_lst1_84_subs_5_gs), ~{
  
  names(cube4d_180_lst1_84_subs_5_gs[[.x]])[1]<-"wpd"
  return(append(cube4d_180_lst1_84_subs_5_gs[[.x]], lst(l_rate=cube_para1$l_rate[.x], cube_para1$eta[.x])))
  
})

#サブサンプル1~5のグリッドサーチ結果のPDまとめ
cube4d_180_lst1_84_subs_gs<-append(cube4d_180_lst1_84_subs_gs_1to3, lst(cube4d_180_lst1_84_subs_gs_4, cube4d_180_lst1_84_subs_5_gs_rename))

#サブサンプルのPLをまとめる
cube4d_180_lst1_84_subs_gs_pls<-lapply(cube4d_180_lst1_84_subs_gs, function(X){
  
  sub_pls<-lapply(X, function(Y){calc_landscape(diag = as.matrix(Y[["wpd"]]), maxscale = 2, plot = F)})
  
})

#サブサンプルのPLのH4局所最大値をまとめる
cube4d_180_lst1_84_subs_gs_peaks<-lapply(cube4d_180_lst1_84_subs_gs_pls, function(X){
  
  sub_pls<-sapply(X, function(Y){calc.landscape.peak(X = Y[["4-land"]], dimension = 2, thresh = Y[["thresh"]]*(2*pi)/surface_nshpere(4), tseq = Y[["tseq"]], show = F)})
  
})

#サブサンプル1~10のPLのH2局所最大値の平均
cube4d_180_lst1_84_subs_gs_peaks_ave<-sapply(seq_along(cube4d_180_lst1_84_subs_gs_peaks[[1]]), function(i){
  mpeaks<-sapply(cube4d_180_lst1_84_subs_gs_peaks, function(PL){PL[[i]]}) %>% mean()
})

#H2局所最大値の数をパラメータごとにプロット
#サブサンプル1~10平均
par(mai=c(0.7, 0.7, 0.3, 0.3))
par(mgp=c(2, 1, 0))
plot(cube_para1$l_rate, cube_para1$eta, xlab="landmark rate", ylab="eta", type="n", cex.lab = 1.3)
text(cube_para1$l_rate, cube_para1$eta, labels = round(cube4d_180_lst1_84_subs_gs_peaks_ave, 2),
     col = ifelse(cube4d_180_lst1_84_subs_gs_peaks_ave >= 0.5 & cube4d_180_lst1_84_subs_gs_peaks_ave < 1.5, 2, 4), cex = 0.8)

#-------------------------
#結合時刻変化手法で４次元直方体成功率実験----------
#180点の4次元立方体。リスト1つ目の1~20セット使用
#l_rate=0.4, eta=1.4

{
  cube4d_180_lst1_1to20_wvr_time2<-system.time(
    cube4d_180_lst1_1to20_wvr_aggr2<-calc_distance_change_betti_paral(X = cube4d_180_list1[1:20], maxdim = 4, maxscale = 2, samples = 10, ncl = 4, 
                                                                     ph_func = weighted_homology, l_rate=0.4, eta=1.4)
  )
  
  save2RData(cube4d_180_lst1_1to20_wvr_time2)
  save2RData(cube4d_180_lst1_1to20_wvr_aggr2)
}


#----------------------------------------
#4次元直方体のサブサンプルでハイパラグリッドサーチ。その2------
#H4成功率確認用
#グリッドサーチ用ハイパラ。その2
cube_eta2<-seq(1.3, 1.6, by=0.05)
cube_lrate2<-seq(0.2, 0.8, by=0.05)
cube_para2<-expand.grid(cube_eta2, cube_lrate2)
colnames(cube_para2)<-c("eta", "l_rate")

#parallelで並列計算時に使うオジェクト読み込み
clusterExport(cl, varlist = c("cube4d_180_lst1_84_subs", "cube_para2"))

#サブセット1~5をグリッドサーチ
cube4d_180_lst1_84_subs_time2<-system.time( 
  
  cube4d_180_lst1_84_subs_gs2<-lapply(1:length(cube4d_180_lst1_84_subs), function(i){
    
    sub_gs<-parLapply(cl, 1:nrow(cube_para2), function(p){
      
      wpd<-weighted_homology(X = cube4d_180_lst1_84_subs[[i]], maxdim = 4, maxscale = 2, l_rate = cube_para2$l_rate[p], eta = cube_para2$eta[p])
      
      #parallelフォルダにcsvを出力
      write.csv(as.data.frame(wpd[["pd"]]), 
                file = paste0("./parallel/", "data", i, "_", "lrate", 
                              gsub("\\.", "", cube_para2$l_rate[p]), "eta", gsub("\\.", "", cube_para2$eta[p]), 
                              "_", format(Sys.time(), "%m%d_%H%M"), ".csv"))
      
      return(wpd)
      
    })
    
    return(sub_gs)
    
  })
)

#サブサンプルのPLをまとめる
cube4d_180_lst1_84_subs_gs2_pls<-lapply(cube4d_180_lst1_84_subs_gs2, function(X){
  
  sub_pls<-lapply(X, function(Y){calc_landscape(diag = as.matrix(Y[["pd"]]), maxscale = 2, plot = F)})
  
})

#サブサンプルのPLのH4局所最大値をまとめる
cube4d_180_lst1_84_subs_gs2_peaks<-lapply(cube4d_180_lst1_84_subs_gs2_pls, function(X){
  
  sub_pls<-sapply(X, function(Y){calc.landscape.peak(X = Y[["4-land"]], dimension = 2, thresh = Y[["thresh"]]*(2*pi)/surface_nshpere(4), tseq = Y[["tseq"]], show = F)})
  
})

#サブサンプル1~5のPLのH4局所最大値の平均
cube4d_180_lst1_84_subs_gs2_peaks_ave<-sapply(seq_along(cube4d_180_lst1_84_subs_gs2_peaks[[1]]), function(i){
  mpeaks<-sapply(cube4d_180_lst1_84_subs_gs2_peaks, function(PL){PL[[i]]}) %>% mean()
})

#H2局所最大値の数をパラメータごとにプロット
#サブサンプル1~10平均
par(mai=c(0.7, 0.7, 0.3, 0.3))
par(mgp=c(2, 1, 0))
plot(cube_para2$l_rate, cube_para2$eta, xlab="landmark rate", ylab="eta", type="n", cex.lab = 1.3)
text(cube_para2$l_rate, cube_para2$eta, labels = round(cube4d_180_lst1_84_subs_gs2_peaks_ave, 2),
     col = ifelse(cube4d_180_lst1_84_subs_gs2_peaks_ave >= 0.5 & cube4d_180_lst1_84_subs_gs2_peaks_ave < 1.5, 2, 4), cex = 0.8)

#-------------------------
#結合時刻変化手法で4次元直方体成功率実験----------
#250点の4次元立方体。リスト1つ目の1~20セット使用
#l_rate=0.5, eta=1.4

cube4d_250_lst1<-map(1:100, ~xRect_unif(n = 250, sides = c(rep(1, length = 4), 5), d = 5))

{
  cube4d_250_lst1_1to20_wvr_time<-system.time(
    cube4d_250_lst1_1to20_wvr_aggr<-calc_distance_change_betti_paral(X = cube4d_250_lst1[1:20], maxdim = 4, maxscale = 2, samples = 10, ncl = 6, 
                                                                      ph_func = weighted_homology, l_rate=0.5, eta=1.4)
  )
  
  save2RData(cube4d_250_lst1_1to20_wvr_time)
  save2RData(cube4d_250_lst1_1to20_wvr_aggr)
}

#サブサンプル数を5にして再実験
{
  cube4d_250_lst1_1to20_wvr_time2<-system.time(
    cube4d_250_lst1_1to20_wvr_aggr2<-calc_distance_change_betti_paral(X = cube4d_250_lst1[1:20], maxdim = 4, maxscale = 2, samples = 5, ncl = 6, 
                                                                     ph_func = weighted_homology, l_rate=0.5, eta=1.4)
  )
  
  save2RData(cube4d_250_lst1_1to20_wvr_time2)
  save2RData(cube4d_250_lst1_1to20_wvr_aggr2)
}

#ctic手法

{
  cube4d_250_lst1_1to20_time<-system.time(
    cube4d_250_lst1_1to20_aggr<-smooth_landscape_method_paral(X = cube4d_250_lst1[1:20], maxdim = 4, maxscale = 2, samples = 5, ncl = 6)
  )
  
  save2RData(cube4d_250_lst1_1to20_time)
  save2RData(cube4d_250_lst1_1to20_aggr)
}

#計算時間のテスト
cube4d_250_lst1_15_inst<-TDAdataset$new(cube4d_250_lst1[[15]])
cube4d_250_lst1_15_inst$create_subsample(sub_size = cube4d_250_lst1_15_inst$n_points*0.8, n_subs = 2)

cube4d_250_lst1_15_inst$subsamples[[1]]$calc_pd(maxdim = 4, maxscale = 2)#740sec

#250点の4次元立方体。リスト1つ目の21~30セット使用
#l_rate=0.5, eta=1.4、サブサンプル数5
{
  cube4d_250_lst1_21to30_wvr_time<-system.time(
    cube4d_250_lst1_21to30_wvr_aggr<-calc_distance_change_betti_paral(X = cube4d_250_lst1[21:30], maxdim = 4, maxscale = 2, samples = 5, ncl = 6, 
                                                                      ph_func = weighted_homology, l_rate=0.5, eta=1.4)
  )
  
  save2RData(cube4d_250_lst1_21to30_wvr_time)
  save2RData(cube4d_250_lst1_21to30_wvr_aggr)
}

#250点の4次元立方体。リスト1つ目の21~30セット使用
#ctic手法
# 
# {
#   cube4d_250_lst1_21to30_time<-system.time(
#     cube4d_250_lst1_21to30_aggr<-smooth_landscape_method_paral(X = cube4d_250_lst1[21:30], maxdim = 4, maxscale = 2, samples = 5, ncl = 6)
#   )
#   
#   save2RData(cube4d_250_lst1_21to30_time)
#   save2RData(cube4d_250_lst1_21to30_aggr)
# }

#250点の4次元立方体。リスト1つ目の31~40セット使用
{#結合時刻早期化手法
  cube4d_250_lst1_31to40_wvr_time<-system.time(
    cube4d_250_lst1_31to40_wvr_aggr<-calc_distance_change_betti_paral(X = cube4d_250_lst1[31:40], maxdim = 4, maxscale = 2, samples = 5, ncl = 6, 
                                                                      ph_func = weighted_homology, l_rate=0.5, eta=1.4)
  )
  
  save2RData(cube4d_250_lst1_31to40_wvr_time)
  save2RData(cube4d_250_lst1_31to40_wvr_aggr)
}

{#ctic手法
  cube4d_250_lst1_31ot40_time<-system.time(
    cube4d_250_lst1_31to40_aggr<-smooth_landscape_method_paral(X = cube4d_250_lst1[31:40], maxdim = 4, maxscale = 2, samples = 5, ncl = 4)
  )
  
  save2RData(cube4d_250_lst1_31ot40_time)
  save2RData(cube4d_250_lst1_31to40_aggr)
}

#-------------------------
#結合時刻変化手法で4次元直方体成功率実験----------
#240点の4次元立方体。リスト1つ目の1~20セット使用
#l_rate=0.5, eta=1.4、サブサンプル数5

cube4d_240_lst1<-map(1:100, ~xRect_unif(n = 240, sides = c(rep(1, length = 4), 5), d = 5))

{
  cube4d_240_lst1_1to20_wvr_time<-system.time(
    cube4d_240_lst1_1to20_wvr_aggr<-calc_distance_change_betti_paral(X = cube4d_240_lst1[1:20], maxdim = 4, maxscale = 2, samples = 5, ncl = 6, 
                                                                     ph_func = weighted_homology, l_rate=0.5, eta=1.4)
  )
  
  save2RData(cube4d_240_lst1_1to20_wvr_time)
  save2RData(cube4d_240_lst1_1to20_wvr_aggr)
}

#ctic手法

{
  cube4d_240_lst1_1to20_time<-system.time(
    cube4d_240_lst1_1to20_aggr<-smooth_landscape_method_paral(X = cube4d_240_lst1[1:20], maxdim = 4, maxscale = 2, samples = 5, ncl = 6)
  )
  
  save2RData(cube4d_240_lst1_1to20_time)
  save2RData(cube4d_240_lst1_1to20_aggr)
}

#240点の4次元立方体。リスト1つ目の21~30セット使用
{{#結合時刻早期化手法
  cube4d_240_lst1_21to30_wvr_time<-system.time(
    cube4d_240_lst1_21to30_wvr_aggr<-calc_distance_change_betti_paral(X = cube4d_240_lst1[21:30], maxdim = 4, maxscale = 2, samples = 5, ncl = 6, 
                                                                     ph_func = weighted_homology, l_rate=0.5, eta=1.4)
  )
  
  save2RData(cube4d_240_lst1_21to30_wvr_time)
  save2RData(cube4d_240_lst1_21to30_wvr_aggr)
}

#ctic手法

{
  cube4d_240_lst1_21to30_time<-system.time(
    cube4d_240_lst1_21to30_aggr<-smooth_landscape_method_paral(X = cube4d_240_lst1[21:30], maxdim = 4, maxscale = 2, samples = 5, ncl = 6)
  )
  
  save2RData(cube4d_240_lst1_21to30_time)
  save2RData(cube4d_240_lst1_21to30_aggr)
}
}

#240点の4次元立方体。リスト1つ目の31~40セット使用
{
{#結合時刻早期化手法
  cube4d_240_lst1_31to40_wvr_time<-system.time(
    cube4d_240_lst1_31to40_wvr_aggr<-calc_distance_change_betti_paral(X = cube4d_240_lst1[31:40], maxdim = 4, maxscale = 2, samples = 5, ncl = 6, 
                                                                      ph_func = weighted_homology, l_rate=0.5, eta=1.4)
  )
  
  save2RData(cube4d_240_lst1_31to40_wvr_time)
  save2RData(cube4d_240_lst1_31to40_wvr_aggr)
}

{#ctic手法
  cube4d_240_lst1_31ot40_time<-system.time(
    cube4d_240_lst1_31to40_aggr<-smooth_landscape_method_paral(X = cube4d_240_lst1[31:40], maxdim = 4, maxscale = 2, samples = 5, ncl = 6)
  )
  
  save2RData(cube4d_240_lst1_31ot40_time)
  save2RData(cube4d_240_lst1_31to40_aggr)
}
}
#-------------------------
#結合時刻変化手法で4次元直方体成功率実験----------
#230点の4次元立方体。リスト1つ目の1~20セット使用
#l_rate=0.5, eta=1.4、サブサンプル数5

cube4d_230_lst1<-map(1:100, ~xRect_unif(n = 230, sides = c(rep(1, length = 4), 5), d = 5))

{
  cube4d_230_lst1_1to20_wvr_time<-system.time(
    cube4d_230_lst1_1to20_wvr_aggr<-calc_distance_change_betti_paral(X = cube4d_230_lst1[1:20], maxdim = 4, maxscale = 2, samples = 5, ncl = 6, 
                                                                     ph_func = weighted_homology, l_rate=0.5, eta=1.4)
  )
  
  save2RData(cube4d_230_lst1_1to20_wvr_time)
  save2RData(cube4d_230_lst1_1to20_wvr_aggr)
}

#ctic手法

{
  cube4d_230_lst1_1to20_time<-system.time(
    cube4d_230_lst1_1to20_aggr<-smooth_landscape_method_paral(X = cube4d_230_lst1[1:20], maxdim = 4, maxscale = 2, samples = 5, ncl = 6)
  )
  
  save2RData(cube4d_230_lst1_1to20_time)
  save2RData(cube4d_230_lst1_1to20_aggr)
}

#230点の4次元立方体。リスト1つ目の21~30セット使用
{{#結合時刻早期化手法
  cube4d_230_lst1_21to30_wvr_time<-system.time(
    cube4d_230_lst1_21to30_wvr_aggr<-calc_distance_change_betti_paral(X = cube4d_230_lst1[21:30], maxdim = 4, maxscale = 2, samples = 5, ncl = 6, 
                                                                      ph_func = weighted_homology, l_rate=0.5, eta=1.4)
  )
  
  save2RData(cube4d_230_lst1_21to30_wvr_time)
  save2RData(cube4d_230_lst1_21to30_wvr_aggr)
}
  
  #ctic手法
  
  {
    cube4d_230_lst1_21to30_time<-system.time(
      cube4d_230_lst1_21to30_aggr<-smooth_landscape_method_paral(X = cube4d_230_lst1[21:30], maxdim = 4, maxscale = 2, samples = 5, ncl = 6)
    )
    
    save2RData(cube4d_230_lst1_21to30_time)
    save2RData(cube4d_230_lst1_21to30_aggr)
  }
}

#230点の4次元立方体。リスト1つ目の31~40セット使用
{
  {#結合時刻早期化手法
    cube4d_230_lst1_31to40_wvr_time<-system.time(
      cube4d_230_lst1_31to40_wvr_aggr<-calc_distance_change_betti_paral(X = cube4d_230_lst1[31:40], maxdim = 4, maxscale = 2, samples = 5, ncl = 6, 
                                                                        ph_func = weighted_homology, l_rate=0.5, eta=1.4)
    )
    
    save2RData(cube4d_230_lst1_31to40_wvr_time)
    save2RData(cube4d_230_lst1_31to40_wvr_aggr)
  }
  
  {#ctic手法
    cube4d_230_lst1_31ot40_time<-system.time(
      cube4d_230_lst1_31to40_aggr<-smooth_landscape_method_paral(X = cube4d_230_lst1[31:40], maxdim = 4, maxscale = 2, samples = 5, ncl = 6)
    )
    
    save2RData(cube4d_230_lst1_31ot40_time)
    save2RData(cube4d_230_lst1_31to40_aggr)
  }
}

#-------------------------
#結合時刻変化手法で4次元直方体成功率実験----------
#220点の4次元立方体。リスト1つ目の1~20セット使用
#l_rate=0.5, eta=1.4、サブサンプル数5

cube4d_220_lst1<-map(1:100, ~xRect_unif(n = 220, sides = c(rep(1, length = 4), 5), d = 5))

{
{
  cube4d_220_lst1_1to20_wvr_time<-system.time(
    cube4d_220_lst1_1to20_wvr_aggr<-calc_distance_change_betti_paral(X = cube4d_220_lst1[1:20], maxdim = 4, maxscale = 2, samples = 5, ncl = 6, 
                                                                     ph_func = weighted_homology, l_rate=0.5, eta=1.4)
  )
  
  save2RData(cube4d_220_lst1_1to20_wvr_time)
  save2RData(cube4d_220_lst1_1to20_wvr_aggr)
}

#ctic手法

{
  cube4d_220_lst1_1to20_time<-system.time(
    cube4d_220_lst1_1to20_aggr<-smooth_landscape_method_paral(X = cube4d_220_lst1[1:20], maxdim = 4, maxscale = 2, samples = 5, ncl = 6)
  )
  
  save2RData(cube4d_220_lst1_1to20_time)
  save2RData(cube4d_220_lst1_1to20_aggr)
}
}

#220点の4次元立方体。リスト1つ目の21~30セット使用
{{#結合時刻早期化手法
  cube4d_220_lst1_21to30_wvr_time<-system.time(
    cube4d_220_lst1_21to30_wvr_aggr<-calc_distance_change_betti_paral(X = cube4d_220_lst1[21:30], maxdim = 4, maxscale = 2, samples = 5, ncl = 6, 
                                                                      ph_func = weighted_homology, l_rate=0.5, eta=1.4)
  )
  
  save2RData(cube4d_220_lst1_21to30_wvr_time)
  save2RData(cube4d_220_lst1_21to30_wvr_aggr)
}
  
  #ctic手法
  
  {
    cube4d_220_lst1_21to30_time<-system.time(
      cube4d_220_lst1_21to30_aggr<-smooth_landscape_method_paral(X = cube4d_220_lst1[21:30], maxdim = 4, maxscale = 2, samples = 5, ncl = 6)
    )
    
    save2RData(cube4d_220_lst1_21to30_time)
    save2RData(cube4d_220_lst1_21to30_aggr)
  }
}

#220点の4次元立方体。リスト1つ目の31~40セット使用
{
  {#結合時刻早期化手法
    cube4d_220_lst1_31to40_wvr_time<-system.time(
      cube4d_220_lst1_31to40_wvr_aggr<-calc_distance_change_betti_paral(X = cube4d_220_lst1[31:40], maxdim = 4, maxscale = 2, samples = 5, ncl = 6, 
                                                                        ph_func = weighted_homology, l_rate=0.5, eta=1.4)
    )
    
    save2RData(cube4d_220_lst1_31to40_wvr_time)
    save2RData(cube4d_220_lst1_31to40_wvr_aggr)
  }
  
  {#ctic手法
    cube4d_220_lst1_31ot40_time<-system.time(
      cube4d_220_lst1_31to40_aggr<-smooth_landscape_method_paral(X = cube4d_220_lst1[31:40], maxdim = 4, maxscale = 2, samples = 5, ncl = 6)
    )
    
    save2RData(cube4d_220_lst1_31ot40_time)
    save2RData(cube4d_220_lst1_31to40_aggr)
  }
}

#-------------------------
#結合時刻変化手法で4次元直方体成功率実験----------
#210点の4次元立方体。リスト1つ目の1~20セット使用
#l_rate=0.5, eta=1.4、サブサンプル数5

cube4d_210_lst1<-map(1:100, ~xRect_unif(n = 210, sides = c(rep(1, length = 4), 5), d = 5))

{
  {
    cube4d_210_lst1_1to20_wvr_time<-system.time(
      cube4d_210_lst1_1to20_wvr_aggr<-calc_distance_change_betti_paral(X = cube4d_210_lst1[1:20], maxdim = 4, maxscale = 2, samples = 5, ncl = 6, 
                                                                       ph_func = weighted_homology, l_rate=0.5, eta=1.4)
    )
    
    save2RData(cube4d_210_lst1_1to20_wvr_time)
    save2RData(cube4d_210_lst1_1to20_wvr_aggr)
  }
  
  #ctic手法
  
  {
    cube4d_210_lst1_1to20_time<-system.time(
      cube4d_210_lst1_1to20_aggr<-smooth_landscape_method_paral(X = cube4d_210_lst1[1:20], maxdim = 4, maxscale = 2, samples = 5, ncl = 6)
    )
    
    save2RData(cube4d_210_lst1_1to20_time)
    save2RData(cube4d_210_lst1_1to20_aggr)
  }
}

#210点の4次元立方体。リスト1つ目の21~30セット使用
{{#結合時刻早期化手法
  cube4d_210_lst1_21to30_wvr_time<-system.time(
    cube4d_210_lst1_21to30_wvr_aggr<-calc_distance_change_betti_paral(X = cube4d_210_lst1[21:30], maxdim = 4, maxscale = 2, samples = 5, ncl = 6, 
                                                                      ph_func = weighted_homology, l_rate=0.5, eta=1.4)
  )
  
  save2RData(cube4d_210_lst1_21to30_wvr_time)
  save2RData(cube4d_210_lst1_21to30_wvr_aggr)
}
  
  #ctic手法
  
  {
    cube4d_210_lst1_21to30_time<-system.time(
      cube4d_210_lst1_21to30_aggr<-smooth_landscape_method_paral(X = cube4d_210_lst1[21:30], maxdim = 4, maxscale = 2, samples = 5, ncl = 6)
    )
    
    save2RData(cube4d_210_lst1_21to30_time)
    save2RData(cube4d_210_lst1_21to30_aggr)
  }
}

#210点の4次元立方体。リスト1つ目の31~40セット使用
{
  {#結合時刻早期化手法
    cube4d_210_lst1_31to40_wvr_time<-system.time(
      cube4d_210_lst1_31to40_wvr_aggr<-calc_distance_change_betti_paral(X = cube4d_210_lst1[31:40], maxdim = 4, maxscale = 2, samples = 5, ncl = 6, 
                                                                        ph_func = weighted_homology, l_rate=0.5, eta=1.4)
    )
    
    save2RData(cube4d_210_lst1_31to40_wvr_time)
    save2RData(cube4d_210_lst1_31to40_wvr_aggr)
  }
  
  {#ctic手法
    cube4d_210_lst1_31ot40_time<-system.time(
      cube4d_210_lst1_31to40_aggr<-smooth_landscape_method_paral(X = cube4d_210_lst1[31:40], maxdim = 4, maxscale = 2, samples = 5, ncl = 6)
    )
    
    save2RData(cube4d_210_lst1_31ot40_time)
    save2RData(cube4d_210_lst1_31to40_aggr)
  }
}

#-------------------------
#結合時刻変化手法で4次元直方体成功率実験----------
#200点の4次元立方体。リスト1つ目の1~20セット使用
#l_rate=0.5, eta=1.4、サブサンプル数5

cube4d_200_lst1<-map(1:100, ~xRect_unif(n = 200, sides = c(rep(1, length = 4), 5), d = 5))

{
  {
    cube4d_200_lst1_1to20_wvr_time<-system.time(
      cube4d_200_lst1_1to20_wvr_aggr<-calc_distance_change_betti_paral(X = cube4d_200_lst1[1:20], maxdim = 4, maxscale = 2, samples = 5, ncl = 6, 
                                                                       ph_func = weighted_homology, l_rate=0.5, eta=1.4)
    )
    
    save2RData(cube4d_200_lst1_1to20_wvr_time)
    save2RData(cube4d_200_lst1_1to20_wvr_aggr)
  }
  
  #ctic手法
  
  {
    cube4d_200_lst1_1to20_time<-system.time(
      cube4d_200_lst1_1to20_aggr<-smooth_landscape_method_paral(X = cube4d_200_lst1[1:20], maxdim = 4, maxscale = 2, samples = 5, ncl = 6)
    )
    
    save2RData(cube4d_200_lst1_1to20_time)
    save2RData(cube4d_200_lst1_1to20_aggr)
  }
}

#200点の4次元立方体。リスト1つ目の21~30セット使用
{{#結合時刻早期化手法
  cube4d_200_lst1_21to30_wvr_time<-system.time(
    cube4d_200_lst1_21to30_wvr_aggr<-calc_distance_change_betti_paral(X = cube4d_200_lst1[21:30], maxdim = 4, maxscale = 2, samples = 5, ncl = 6, 
                                                                      ph_func = weighted_homology, l_rate=0.5, eta=1.4)
  )
  
  save2RData(cube4d_200_lst1_21to30_wvr_time)
  save2RData(cube4d_200_lst1_21to30_wvr_aggr)
}
  
  #ctic手法
  
  {
    cube4d_200_lst1_21to30_time<-system.time(
      cube4d_200_lst1_21to30_aggr<-smooth_landscape_method_paral(X = cube4d_200_lst1[21:30], maxdim = 4, maxscale = 2, samples = 5, ncl = 6)
    )
    
    save2RData(cube4d_200_lst1_21to30_time)
    save2RData(cube4d_200_lst1_21to30_aggr)
  }
}
#200点の4次元立方体。リスト1つ目の21~30セット使用
#結合時刻早期化手法2回目
#なぜ200点だけ精度が低いのか確かめる
{
  cube4d_200_lst1_21to30_wvr_time2<-system.time(
    cube4d_200_lst1_21to30_wvr_aggr2<-calc_distance_change_betti_paral(X = cube4d_200_lst1[21:30], maxdim = 4, maxscale = 2, samples = 5, ncl = 6, 
                                                                      ph_func = weighted_homology, l_rate=0.5, eta=1.4)
  )
  
  save2RData(cube4d_200_lst1_21to30_wvr_time2)
  save2RData(cube4d_200_lst1_21to30_wvr_aggr2)
}


#200点の4次元立方体。リスト1つ目の31~40セット使用
{
  {#結合時刻早期化手法
    cube4d_200_lst1_31to40_wvr_time<-system.time(
      cube4d_200_lst1_31to40_wvr_aggr<-calc_distance_change_betti_paral(X = cube4d_200_lst1[31:40], maxdim = 4, maxscale = 2, samples = 5, ncl = 6, 
                                                                        ph_func = weighted_homology, l_rate=0.5, eta=1.4)
    )
    
    save2RData(cube4d_210_lst1_31to40_wvr_time)
    save2RData(cube4d_210_lst1_31to40_wvr_aggr)
  }
  
  {#ctic手法
    cube4d_200_lst1_31ot40_time<-system.time(
      cube4d_200_lst1_31to40_aggr<-smooth_landscape_method_paral(X = cube4d_200_lst1[31:40], maxdim = 4, maxscale = 2, samples = 5, ncl = 6)
    )
    
    save2RData(cube4d_200_lst1_31ot40_time)
    save2RData(cube4d_200_lst1_31to40_aggr)
  }
}
