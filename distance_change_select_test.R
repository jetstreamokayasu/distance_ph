#距離行列の変化のさせ方を色々試す

#---------------------------------------------------
#操作対象点固定。操作量を変える。
#0.1~1まで0.1刻みで操作量を変えている
trs300_1_7_dc_dist4<-lapply(seq(0.1, 1, 0.1), function(th)dist_mat_change(X_dist = trs300_1_7_dist, idx = trs300_1_7_dc_idx2[[5]], thresh = th))

trs300_1_7_dc_pd4<-lapply(trs300_1_7_dc_dist4, function(dist){
  
  pd<-ripsFiltration(X = dist, maxdimension = 2, maxscale = 3, dist = "arbitrary", library = "Dionysus", 
                     printProgress = T) %>% 
    
    filtrationDiag(filtration = ., maxdimension = 2, library = "Dionysus", printProgress = T)
  
  return(pd)
  
})


trs300_1_7_dc_pl4<-lapply(trs300_1_7_dc_pd4, function(pd)calc_landscape(diag = pd, maxscale = 3))

#---------------------------------------------------------
#変化後の距離行列から80%分のデータ点に関わる要素を抽出。PH計算

trs300_1_7_red<-cell_set2(torus300_colle_set[[1]][[7]][["noizyX"]], thresh = 0.4) %>% 
                connect2(1, ., all = 1:300) %>% 
                reduce_points(torus300_colle_set[[1]][[7]][["noizyX"]], .)

trs300_1_7_rme<-1:300 %>% .[-trs300_1_7_red[[2]]]

trs300_1_7_dc_dist5<-dist_mat_change(trs300_1_7_dist, idx = trs300_1_7_rme, thresh = 0.4)

trs300_1_7_dc_pd5<-lapply(1:10, function(k){
  
  idx<-sample(300, 300*0.8)
  
  pd<-ripsFiltration(X = trs300_1_7_dc_dist5[idx, idx], maxdimension = 2, maxscale = 3, dist = "arbitrary", library = "Dionysus", 
                     printProgress = T) %>% 
    
    filtrationDiag(filtration = ., maxdimension = 2, library = "Dionysus", printProgress = T)
  
  return(pd)
  
})

trs300_1_7_dc_pl5<-lapply(trs300_1_7_dc_pd5, function(pd)calc_landscape(diag = pd, maxscale = 3))

#-----------------------------------------------------------
#データセット7以外でも試す。7も含めて1~10セット目を計算する
#距離変化0.4固定、操作点の割合を変えてPDを計算
#10~100%まで割合を変えて計算する

trs300_1_1_10_manupi_chng_pds<-lapply(1:10, function(k)select_rate_change_pd(X = torus300_colle_set[[1]][[k]][["noizyX"]], rates = seq(0.1, 1, by = 0.1), thresh = 0.4))

trs300_1_1_10_manupi_chng_pls<-lapply(trs300_1_1_10_manupi_chng_pds, function(pds){
  
  pls<-lapply(pds, function(pd)calc_landscape(diag = pd, maxscale = 3))
  
  return(pls)
  
})


#操作点数の割合50%に固定。操作量0.4に固定。操作点を変化させてPH計算
trs300_1_1_10_rate_chng_pds<-lapply(1:10, function(k)select_rate_change_pd(X = torus300_colle_set[[1]][[k]][["noizyX"]], rates = rep(0.5, 10), thresh = 0.4))
  
trs300_1_1_rate_chng_pls<-lapply(trs300_1_1_10_rate_chng_pds[[1]], function(pd)calc_landscape(diag = pd, maxscale = 3))  

trs300_1_2_10_rate_chng_pls<-lapply(trs300_1_1_10_rate_chng_pds[2:10], function(pds){
  
  pls<-lapply(pds, function(pd)calc_landscape(diag = pd, maxscale = 3))
  
  return(pls)
  
})
  
#---------------------------------------------------
#オリジナルのPLをpng保存
png("./data/trs300_1_1_pl_dim2.png", width = 484, height = 425)
plot_landscape(land = trs300_1_1_10_pl[[1]])
dev.off()  


for (i in 2:10) {
  
  png(paste0("./data/trs300_1_", i, "_pl_dim2.png"), width = 484, height = 425)
  plot_landscape(land = trs300_1_1_10_pl[[i]])
  dev.off() 
  
}

#---------------------------------------------------------------
#データセット10で10%選択したとき山が大きくなるのはなぜなのか
trs300_1_10_dchng_pd<-select_rate_change_pd(X = torus300_colle_set[[1]][[10]][["noizyX"]], rates = rep(0.1, 10), thresh = 0.4) 

trs300_1_10_dchng_pls<-lapply(trs300_1_10_dchng_pd, function(pd)calc_landscape(diag = pd[1], maxscale = 3))



#-------------------------------------------------------------------
#Witness複体のランドマークの選び方に従って操作点を選ぶ

trs300_1_10_dist<-dist(torus300_colle_set[[1]][[10]][["noizyX"]]) %>% as.matrix()

set.seed(121)
l_idx1<-sample(300, 1)

land10F<-landmark_points(X = torus300_colle_set[[1]][[10]][["noizyX"]], n_land = 30)

#---------------------------------------------------
#maxminランドマーク選出により操作点を選出。距離行列を変更。PH計算

#変化量は0.4
trs300_1_10_dist2<-dist_mat_change(trs300_1_10_dist, idx = land10F, thresh = 0.4)

trs300_1_10_lmdc_pd<-ripsFiltration(X = trs300_1_10_dist2, maxdimension = 2, maxscale = 3, dist = "arbitrary", library = "Dionysus", 
                              printProgress = T) %>% 
  filtrationDiag(filtration = ., maxdimension = 2, library = "Dionysus", printProgress = T)

trs300_1_10_lmdc_pl<-calc_landscape(diag = trs300_1_10_lmdc_pd, maxscale = 3)

#変化量は0.5
trs300_1_10_dist3<-dist_mat_change(trs300_1_10_dist, idx = land10F, thresh = 0.5)

trs300_1_10_lmdc_pd2<-ripsFiltration(X = trs300_1_10_dist3, maxdimension = 2, maxscale = 3, dist = "arbitrary", library = "Dionysus", 
                                    printProgress = T) %>% 
  filtrationDiag(filtration = ., maxdimension = 2, library = "Dionysus", printProgress = T)

trs300_1_10_lmdc_pl2<-calc_landscape(diag = trs300_1_10_lmdc_pd2, maxscale = 3)

#変化量は1
trs300_1_10_dist4<-dist_mat_change(trs300_1_10_dist, idx = land10F, thresh = 1)

trs300_1_10_lmdc_pd3<-ripsFiltration(X = trs300_1_10_dist3, maxdimension = 2, maxscale = 3, dist = "arbitrary", library = "Dionysus", 
                                     printProgress = T) %>% 
  filtrationDiag(filtration = ., maxdimension = 2, library = "Dionysus", printProgress = T)

trs300_1_10_lmdc_pl3<-calc_landscape(diag = trs300_1_10_lmdc_pd3, maxscale = 3)

#サブサンプルで試す
#変化量0.5

trs300_1_10_sub<-torus300_colle_set[[1]][[10]][["noizyX"]][sample(300, 300*0.8), ]
trs300_1_10_sub_lidx<-landmark_points(X = trs300_1_10_sub, n_land = 300*0.8*0.1)

trs300_1_10_sub_dist<-dist(trs300_1_10_sub) %>% as.matrix()

trs300_1_10_sub_ched_dist<-dist_mat_change(trs300_1_10_sub_dist, idx = trs300_1_10_sub_lidx, thresh = 0.5)

trs300_1_10_sub_pd<-ripsFiltration(X = trs300_1_10_sub_ched_dist, maxdimension = 2, maxscale = 3, dist = "arbitrary", library = "Dionysus", 
                                     printProgress = T) %>% 
  filtrationDiag(filtration = ., maxdimension = 2, library = "Dionysus", printProgress = T)

trs300_1_10_sub_pl<-calc_landscape(diag = trs300_1_10_sub_pd, maxscale = 3)
