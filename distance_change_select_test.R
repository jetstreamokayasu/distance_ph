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

l_idx2<-which.max(trs300_1_10_dist[l_idx1,])

l_idx3_1<-which.max(trs300_1_10_dist[l_idx1, -l_idx2]) %>% attributes() %>% .$names %>% as.integer() 
l_idx3_2<-which.max(trs300_1_10_dist[l_idx2, -l_idx1]) %>% attributes() %>% .$names %>% as.integer()

l_idx<-c(l_idx1, l_idx2)

l_idx3s<-sapply(1:length(l_idx), function(k){
  
  which.max(trs300_1_10_dist[l_idx[k], -l_idx[-k]]) %>% attributes() %>% .$names %>% as.integer() 
  
})

l_idx3s_dist<-sapply(1:length(l_idx), function(k){trs300_1_10_dist[l_idx[k], l_idx3s[k]]})

l_idx3<-l_idx3s[which.min(l_idx3s_dist)]


land10C<-landmark_points(X = torus300_colle_set[[1]][[10]][["noizyX"]], n_land = 100)
land10D<-landmark_points(X = torus300_colle_set[[1]][[10]][["noizyX"]], n_land = 100)


#ランドマーク決定関数のデバック
land10E<-l_idx

cand<-apply(trs300_1_10_dist[-land10E, land10E], 1, min)



land10E<-which.max(cand) %>% attributes() %>% .$names %>% as.integer() %>% c(., land10E)
land10E<-apply(trs300_1_10_dist[-land10E, land10E], 1, min) %>%  which.max() %>% attributes() %>% .$names %>% as.integer() %>% c(., land10E)

land10F<-landmark_points(X = torus300_colle_set[[1]][[10]][["noizyX"]], n_land = 30)

x_rand<-rnorm(100)
y_rand<-rnorm(100)

xy_land<-landmark_points(X = cbind(x_rand, y_rand), n_land = 10)
