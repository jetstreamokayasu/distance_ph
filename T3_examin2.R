#３次元トーラス推定用のハイパラグリッドサーチ

#グリッドサーチのようにランドマーク点割合とハイパラetaを試す-------------
#3次元トーラス1回目
#サブサンプルt3ours4_list2_2_sub使用
#1-exp(-(d_ij/eta)^2)を掛けてみる
#parallel使用

land_rate_set2<-seq(0.05, 0.9, by=0.05)
eta_set3<-seq(3, 7, by=0.1)

para_set3<-expand.grid(land_rate_set2, eta_set3)
colnames(para_set3)<-c("l_rate", "eta")

#並列化準備
#parallel使用
library(parallel)

cl <- makeCluster(4, outfile="")

clusterEvalQ(cl,{
  library(phacm)
  library(tidyverse)
  library(myfs)
  library(seephacm)
  
})

clusterEvalQ(cl, {
  source('~/R/distance_ph/auto_estimate_betti_functions.R', encoding = 'UTF-8')
  source('~/R/distance_ph/dist_ch_func.R', encoding = 'UTF-8')
  source('~/R/ph_jikken2/new-okayasu/BootstrapHomology-mk1.R', encoding = 'UTF-8')
}
)

#parallelで並列計算時に使うオジェクト読み込み
clusterExport(cl, varlist = c("t3ours4_list2_2_sub", "para_set3"))

stopCluster(cl)

##並列化を試す
##350点トーラス

t3ours4_list2_2_sub_para_test1_time<-system.time( t3ours4_list2_2_sub_para_test1<-parLapply(cl, 1:nrow(para_set3), function(i){
  
  wpd<-weighted_homology(X = t3ours4_list2_2_sub[[1]], maxdim = 3, maxscale = 9, extra_v = list(l_rate=para_set3$l_rate[i], eta=para_set3$eta[i]))
  
  #parallelフォルダにtxtを出力
  sink(paste0("./parallel/", "l_rate", gsub("\\.", "", para_set3$l_rate[i]), "eta", gsub("\\.", "", para_set3$eta[i]), "_", format(Sys.time(), "%m%d_%H%M"), ".txt"))
  print(paste0("l_rate=", para_set3$l_rate[i]))
  print(paste0("eta=", para_set3$eta[i]))
  sink()
  
  return(wpd)
  
}) )
