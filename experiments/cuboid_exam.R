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

#4次元直方体3つ目
cuboid4d_4<-xRect_unif(n = 180, sides = c(rep(1, length = 4), 5), d = 5)

cuboid4d_4_inst<-TDAdataset$new(cuboid4d_4)
cuboid4d_4_inst$calc_pd(maxdim = 4, maxscale = 2)

cuboid4d_4_inst$create_changed_distmat(l_rate = 0.5, eta = 1.2)
cuboid4d_4_inst$alt_distmat[[1]]$calc_pd(maxdim = 4, maxscale = 2)

cuboid4d_4_inst$get_pd() %>% as_diag() %>% plot(diagLim=c(0, 1.5))
cuboid4d_4_inst$alt_distmat[[1]]$get_pd() %>% as_diag() %>% plot(diagLim=c(0, 1.5))

cuboid4d_4_inst$plot_pl(dim = 4, ylim = c(0, 0.1))
cuboid4d_4_inst$alt_distmat[[1]]$plot_pl(dim = 4, ylim = c(0, 0.1))


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

cl <- makeCluster(6, outfile="")
registerDoParallel(cl)

clusterEvalQ(cl,{
  library(phacm)
  library(tidyverse)
  library(myfs)
  library(usephacm)
  library(TDAstats)
  library(TDA)
})

clusterEvalQ(cl, {
  source('~/R/ph_jikken2/new-okayasu/BootstrapHomology-mk1.R', encoding = 'UTF-8')
  source(list.files(path = getwd(), recursive=T, pattern="smooth_landscape_func.R", full.names = T), encoding = 'UTF-8')
  source(list.files(path = getwd(), recursive=T, pattern="auto_estimate_betti_functions.R", full.names = T), encoding = 'UTF-8')
  source(list.files(path = getwd(), recursive=T, pattern="dist_ch_func.R", full.names = T), encoding = 'UTF-8')
})

stopCluster(cl)

#グリッドサーチ用ハイパラ
cube_eta1<-seq(1.0, 1.4, by=0.05)
cube_lrate1<-seq(0.1, 0.8, by=0.05)
cube_para1<-expand.grid(cube_eta1, cube_lrate1)
colnames(cube_para1)<-c("eta", "l_rate")


#4D直方体のサブサンプルリスト
cube4d_180_lst1_84_subs<-usephacm:::bootstrapper(X = cube4d_180_list1[[84]], size = nrow(cube4d_180_list1[[84]])*0.8, samples = 5)

#parallelで並列計算時に使うオジェクト読み込み
clusterExport(cl, varlist = c("cube4d_180_lst1_84_subs", "cube_para1"))

#サブセット1~10をグリッドサーチ
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


