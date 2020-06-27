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

trs_list<-1:10 %>% .[-7]

#操作点数の割合50%に固定。操作量0.4に固定。操作点を変化させてPH計算
trs300_1_1_10_rate_chng_pds<-lapply(1:10, function(k)select_rate_change_pd(X = torus300_colle_set[[1]][[k]][["noizyX"]], rates = rep(0.5, 10), thresh = 0.4))
  
  
  
  
  
  
