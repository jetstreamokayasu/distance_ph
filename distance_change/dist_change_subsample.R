#サブサンプルでの距離関数変化がパーシステンスに与える影響を見る
#どのくらい変化しているのか

trs300_1_1subs<-lapply(1:10,function(i)torus300_colle_set[[1]][[1]][["noizyX"]][sample(300,300*0.8),])

#サブサンプルのPD計算
trs300_1_1subs_pd<-lapply(trs300_1_1subs, function(X)calc_dist_changed_pd(X = X, maxdim = 2, maxscale = 3))

#サブサンプル10個の距離閾値
trs300_1_1subs_th<-unlist(lapply(trs300_1_1subs, function(X)quantile_threshold(0.8, X)))

#サブサンプルのPL
trs300_1_1subs_pl<-lapply(trs300_1_1subs_pd, function(X)calc_landscape(diag = X, maxscale = 3))

#サブサンプルのパーシステンスの平均
trs300_1_1subs_mpc<-unlist(lapply(trs300_1_1subs_pd, function(X)usephacm:::calc_diag_centroid(diag = X)[[1]]))

#------------------------------------------------
#距離変化0.4固定、変化点の割合を変えてPDを計算
#10~100%まで割合を変えて計算する
#10~100点を選んでいる。割合ではない。プログラムミス

#データセット1
trs300_1_1_dc_idx<-lapply(seq(10, 100, 10), function(k)sample(1:300, k))

trs300_1_1_dist<-dist(torus300_colle_set[[1]][[1]][["noizyX"]]) %>% as.matrix()

trs300_1_1_dc_dist<-lapply(trs300_1_1_dc_idx, function(idx)dist_mat_change(X_dist = trs300_1_1_dist, idx = idx, thresh = 0.4))

trs300_1_1_dc_pd<-lapply(trs300_1_1_dc_dist, function(dist){
  
  pd<-ripsFiltration(X = dist, maxdimension = 2, maxscale = 3, dist = "arbitrary", library = "Dionysus", 
                       printProgress = T) %>% 
  
      filtrationDiag(filtration = ., maxdimension = 2, library = "Dionysus", printProgress = T)
  
  
})

trs300_1_1_dc_pl<-lapply(trs300_1_1_dc_pd, function(pd)calc_landscape(diag = pd, maxscale = 3))

#データセット7
#オリジナルで計算してもPLで山がはっきり出ないデータ
#10~100点を選んでいる。割合ではない
trs300_1_7_dc_idx<-lapply(seq(10, 100, 10), function(k)sample(1:300, k))

trs300_1_7_dist<-dist(torus300_colle_set[[1]][[7]][["noizyX"]]) %>% as.matrix()

trs300_1_7_dc_dist<-lapply(trs300_1_7_dc_idx, function(idx)dist_mat_change(X_dist = trs300_1_7_dist, idx = idx, thresh = 0.4))

trs300_1_7_dc_pd<-lapply(trs300_1_7_dc_dist, function(dist){
  
  pd<-ripsFiltration(X = dist, maxdimension = 2, maxscale = 3, dist = "arbitrary", library = "Dionysus", 
                     printProgress = T) %>% 
    
    filtrationDiag(filtration = ., maxdimension = 2, library = "Dionysus", printProgress = T)
  
  
})


trs300_1_7_dc_pl<-lapply(trs300_1_7_dc_pd, function(pd)calc_landscape(diag = pd, maxscale = 3))

#---------------------------------------------------------
#距離変化0.4固定、変化点の割合を変えてPDを計算
#10~100%まで割合を変えて計算する
#データセット7
#オリジナルで計算してもPLで山がはっきり出ないデータ

trs300_1_7_dc_idx3<-lapply(seq(0.1, 1, by=0.1), function(k)sample(1:300, 300*k))

trs300_1_7_dc_dist3<-lapply(trs300_1_7_dc_idx3, function(idx)dist_mat_change(X_dist = trs300_1_7_dist, idx = idx, thresh = 0.4))

trs300_1_7_dc_pd3<-lapply(trs300_1_7_dc_dist3, function(dist){
  
  pd<-ripsFiltration(X = dist, maxdimension = 2, maxscale = 3, dist = "arbitrary", library = "Dionysus", 
                     printProgress = T) %>% 
    
    filtrationDiag(filtration = ., maxdimension = 2, library = "Dionysus", printProgress = T)
  
    return(pd)
  
})


trs300_1_7_dc_pl3<-lapply(trs300_1_7_dc_pd3, function(pd)calc_landscape(diag = pd, maxscale = 3))


#データセット7
#選択点割合を50%に固定。選ぶ点を変えてパーシステンスがどうなるか
trs300_1_7_dc_idx2<-lapply(rep(0.5, 10), function(k)sample(1:300, 300*k))

trs300_1_7_dc_dist2<-lapply(trs300_1_7_dc_idx2, function(idx)dist_mat_change(X_dist = trs300_1_7_dist, idx = idx, thresh = 0.4))

trs300_1_7_dc_pd2<-lapply(trs300_1_7_dc_dist2, function(dist){
  
  pd<-ripsFiltration(X = dist, maxdimension = 2, maxscale = 3, dist = "arbitrary", library = "Dionysus", 
                     printProgress = T) %>% 
    
    filtrationDiag(filtration = ., maxdimension = 2, library = "Dionysus", printProgress = T)
  
    return(pd)
  
})


trs300_1_7_dc_pl2<-lapply(trs300_1_7_dc_pd, function(pd)calc_landscape(diag = pd, maxscale = 3))

#------------------------------------------------------------------
#オリジナルのPD計算
trs300_1_1_pd<-ripsFiltration(X = trs300_1_1_dist, maxdimension = 2, maxscale = 3, dist = "arbitrary", library = "Dionysus", 
                   printProgress = T) %>% 
                filtrationDiag(filtration = ., maxdimension = 2, library = "Dionysus", printProgress = T)
trs300_1_1_pl<-calc_landscape(trs300_1_1_pd, maxscale = 3)

trs300_1_1_10_pd<-lapply(1:10, function(k){
  
  pd<-dist(torus300_colle_set[[1]][[k]][["noizyX"]]) %>% as.matrix() %>% 
    
    ripsFiltration(X = ., maxdimension = 2, maxscale = 3, dist = "arbitrary", library = "Dionysus", 
                     printProgress = T) %>% 
    
    filtrationDiag(filtration = ., maxdimension = 2, library = "Dionysus", printProgress = T)
  
  
})

trs300_1_1_10_pl<-lapply(trs300_1_1_10_pd, function(pd)calc_landscape(pd, maxscale = 3))


#-----------------------------------------------------------------------
#300点トーラスで距離行列を変化させてPH計算
trs300_1_1_10_dc_pd<-lapply(1:10, function(k)calc_dist_changed_pd(X = torus300_colle_set[[1]][[k]][["noizyX"]], maxdim = 2, maxscale = 3))

trs300_1_1_10_dc_pl<-lapply(trs300_1_1_10_dc_pd, function(pd)calc_landscape(diag = pd, maxscale = 3))

trs300_1_1_10_th<-unlist(lapply(torus300_colle_set[[1]][1:10], function(X)quantile_threshold(0.8, X[["noizyX"]])))
