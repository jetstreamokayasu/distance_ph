#3次元トーラスに対して補間した場合の計算時間を求める

{
intrp_st<-Sys.time()
t3rs4_lst3_47_inted<-voronoi_interpo4d(figure = t3orus450_list1[[47]], n_vics = 25) %>% rbind(t3orus450_list1[[47]], .)
intrp_ed<-Sys.time()
}

figurePlot3d(t3rs4_lst3_47_inted[1:450, 1:3])
points3d(t3rs4_lst3_47_inted[451:nrow(t3rs4_lst3_47_inted), 1:3], col = 4)

t3rs4_lst3_47_inted_inst<-TDAdataset$new(t3rs4_lst3_47_inted)

t3rs4_lst3_47_inted_inst$create_subsample(sub_size = t3rs4_lst3_47_inted_inst$n_points*0.8, n_subs = 5)

t3rs4_lst3_47_inted_inst$subsamples[[1]]$calc_pd(maxdim = 3, maxscale = 9)

t3rs4_lst3_47_inted_inst$create_subsample(sub_size = t3rs4_lst3_47_inted_inst$n_points*0.6, n_subs = 1)
t3rs4_lst3_47_inted_inst$subsamples[[6]]$calc_pd(maxdim = 3, maxscale = 9)

#並列計算で行う
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

clusterEvalQ(cl, {
  source('~/R/ph_jikken2/new-okayasu/BootstrapHomology-mk1.R', encoding = 'UTF-8')
  source(list.files(path = getwd(), recursive=T, pattern="smooth_landscape_func.R", full.names = T), encoding = 'UTF-8')
  source(list.files(path = getwd(), recursive=T, pattern="auto_estimate_betti_functions.R", full.names = T), encoding = 'UTF-8')
  source(list.files(path = getwd(), recursive=T, pattern="dist_ch_func.R", full.names = T), encoding = 'UTF-8')
})

stopCluster(cl)

t3rs4_lst3_47_inted_sub2to5_pd<-parLapply(cl, t3rs4_lst3_47_inted_inst$subsamples[2:5], function(X){
  
  X$calc_pd(maxdim = 3, maxscale = 9)
  
  return(lst(pd=X$get_pd(), time=X$get_time()))
  
})

t3rs4_lst3_47_inted_sub2to5_time<-c(t3rs4_lst3_47_inted_inst$subsamples[[1]]$get_time()[3], map_dbl(t3rs4_lst3_47_inted_sub2to5_pd, ~{.[["time"]][3]}))

for (i in seq_along(t3rs4_lst3_47_inted_sub2to5_pd)) {
  t3rs4_lst3_47_inted_inst$subsamples[[i+1]]$input_pd(t3rs4_lst3_47_inted_sub2to5_pd[[i]][["pd"]])
}

t3rs4_lst3_47_inted_inst$subsamples[[2]]$plot_pd()
t3rs4_lst3_47_inted_inst$bind_sub_peaks()

#各サブサンプルにおける閾値を求める
t3rs4_lst3_47_inted_sub2to5_thresh<-map_dbl(t3rs4_lst3_47_inted_inst$subsamples[1:5], ~{.$get_thresh(3)})
