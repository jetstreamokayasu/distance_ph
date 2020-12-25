#2次元トーラスの超低密度領域のCTIC2019手法、補間手法、距離操作手法の比較

#-----------------
#200点トーラス------------

trs200_list1<-lapply(1:100, function(i)torusUnif(n = 200, 1, 2.5))

trs200_list1_inted<-lapply(trs200_list1, function(X)interpo3d:::voronoi_interpo(X, 10) %>% rbind(X, .))

trs200_list1_inted1<-TDAdataset$new(trs200_list1_inted[[1]])
trs200_list1_1<-TDAdataset$new(trs200_list1[[1]])
trs200_list1_1$create_changed_distmat(l_rate = 0.8, eta = 3)

trs200_list1_inted_aggr1<-smooth_landscape_method(X = trs200_list1_inted, maxdim = 2, maxscale = 3, samples = 10)

trs200_list1_wvr_aggr1<-calc_distance_change_betti(X = trs200_list1, maxdim = 2, maxscale = 3, samples = 10, 
                                                         ph_func = weighted_homology, l_rate=0.8, eta=3)
#---------------------
#100セットのリスト2～6個目----
trs200_list2to6<-lapply(1:5, function(k){lapply(1:100, function(i)torusUnif(n = 200, 1, 2.5))})

{#ctic2019手法
  trs200_list2to5_aggrs<-lapply(1:4, function(k){
    
    cat("list", k, "calc\n")
    time<-system.time(aggr<-smooth_landscape_method(trs200_list2to6[[k]], 2, 3, 10))
    return(append(aggr, list(time=time)))
    
  })
  save2RData(trs200_list2to5_aggrs)
}

#補間手法

trs200_list2to5_inted<-lapply(trs200_list2to6[1:4], function(Z){lapply(Z, function(X)interpo3d:::voronoi_interpo(X, 10) %>% rbind(X, .))})
  

{
  trs200_list2to5_inted_aggrs<-lapply(1:4, function(k){
    
    cat("list", k, "calc\n")
    time<-system.time(aggr<-smooth_landscape_method(trs200_list2to5_inted[[k]], 2, 3, 10))
    return(append(aggr, list(time=time)))
    
  })
  save2RData(trs200_list2to5_inted_aggrs)
}


{#距離操作手法
  trs200_list2to5_wvr_aggrs<-lapply(1:4, function(k){
    
    cat("list", k, "calc\n")
    time<-system.time(aggr<-calc_distance_change_betti(trs200_list2to6[[k]], maxdim = 2, maxscale = 3, samples = 10, ph_func = weighted_homology, l_rate=0.8, eta=3))
    return(append(aggr, list(time=time)))
    
  })
  save2RData(trs200_list2to5_wvr_aggrs)
}


#--------------------
#trs210_list1, trs220_list1が存在しないため210~220点計算し直し-----
#210点計算終了
#------------------------
#210点トーラス------------

trs210_list1<-lapply(1:100, function(i)torusUnif(n = 210, 1, 2.5))

trs210_list1_inted<-lapply(trs210_list1, function(X){interpo3d:::voronoi_interpo(X, 10) %>% rbind(X, .)})

{#補間手法
  trs210_list1_inted_time1<-system.time(
trs210_list1_inted_aggr1<-smooth_landscape_method(X = trs210_list1_inted, maxdim = 2, maxscale = 3, samples = 10)
)

save2RData(trs210_list1_inted_time1)  
save2RData(trs210_list1_inted_aggr1) 
}

{#距離操作手法
trs210_list1_wvr_time1<-system.time(
  trs210_list1_wvr_aggr1<-calc_distance_change_betti(X = trs210_list1, maxdim = 2, maxscale = 3, samples = 10, 
                                                         ph_func = weighted_homology, l_rate=0.8, eta=3) )

save2RData(trs210_list1_wvr_time1)
save2RData(trs210_list1_wvr_aggr1)
}

#---------------------
#210点トーラス100セットのリスト2～5個目----
trs210_list2to5<-lapply(1:4, function(k){lapply(1:100, function(i)torusUnif(n = 210, 1, 2.5))})

{#ctic2019手法
  trs210_list2to5_aggrs<-lapply(1:4, function(k){
    
    cat("list", k, "calc\n")
    time<-system.time(aggr<-smooth_landscape_method(trs210_list2to5[[k]], 2, 3, 10))
    return(append(aggr, list(time=time)))
    
  })
  save2RData(trs210_list2to5_aggrs)
}

#補間手法

trs210_list2to5_inted<-lapply(trs210_list2to5[1:4], function(Z){lapply(Z, function(X)interpo3d:::voronoi_interpo(X, 10) %>% rbind(X, .))})


{
  trs210_list2to5_inted_aggrs<-lapply(1:4, function(k){
    
    cat("list", k, "calc\n")
    time<-system.time(aggr<-smooth_landscape_method(trs210_list2to5_inted[[k]], 2, 3, 10))
    return(append(aggr, list(time=time)))
    
  })
  save2RData(trs210_list2to5_inted_aggrs)
}


{#距離操作手法
  trs200_list2to5_wvr_aggrs<-lapply(1:4, function(k){
    
    cat("list", k, "calc\n")
    time<-system.time(aggr<-calc_distance_change_betti(trs200_list2to6[[k]], maxdim = 2, maxscale = 3, samples = 10, ph_func = weighted_homology, l_rate=0.8, eta=3))
    return(append(aggr, list(time=time)))
    
  })
  save2RData(trs200_list2to5_wvr_aggrs)
}

#------------------------
#220点トーラス------------

trs220_list1<-lapply(1:100, function(i)torusUnif(n = 220, 1, 2.5))

#補間手法
trs220_list1_inted<-lapply(trs220_list1, function(X){interpo3d:::voronoi_interpo(X, 10) %>% rbind(X, .)})
{
trs220_list1_inted_time1<-system.time(
trs220_list1_inted_aggr1<-smooth_landscape_method(X = trs220_list1_inted, maxdim = 2, maxscale = 3, samples = 10) 
)
  save2RData(trs220_list1_inted_time1)
  save2RData(trs220_list1_inted_aggr1)
  
}

#距離操作手法
{
trs220_list1_wvr_time1<-system.time(
  trs220_list1_wvr_aggr1<-calc_distance_change_betti(X = trs220_list1, maxdim = 2, maxscale = 3, samples = 10, 
                                                           ph_func = weighted_homology, l_rate=0.8, eta=3) )
 
  save2RData(trs220_list1_wvr_time1)
  save2RData(trs220_list1_wvr_aggr1)
   
}


#------------------------
#230点トーラス------------

trs230_list1<-lapply(1:100, function(i)torusUnif(n = 230, 1, 2.5))

trs230_list1_inted<-lapply(trs230_list1, function(X){interpo3d:::voronoi_interpo(X, 10) %>% rbind(X, .)})

{
trs230_list1_inted_time1<-system.time(
  trs230_list1_inted_aggr1<-smooth_landscape_method(X = trs230_list1_inted, maxdim = 2, maxscale = 3, samples = 10) 
)

save2RData(trs230_list1_inted_time1)
save2RData(trs230_list1_inted_aggr1)
  
}

{
trs230_list1_wvr_time1<-system.time(
  trs230_list1_wvr_aggr1<-calc_distance_change_betti(X = trs230_list1, maxdim = 2, maxscale = 3, samples = 10, 
                                                     ph_func = weighted_homology, l_rate=0.8, eta=3) )

save2RData(trs230_list1_wvr_time1)
save2RData(trs230_list1_wvr_aggr1)
  
}

#------------------------
#240点トーラス------------

trs240_list1<-lapply(1:100, function(i)torusUnif(n = 240, 1, 2.5))

trs240_list1_inted<-lapply(trs240_list1, function(X){interpo3d:::voronoi_interpo(X, 10) %>% rbind(X, .)})

{
  trs240_list1_inted_time1<-system.time(
    trs240_list1_inted_aggr1<-smooth_landscape_method(X = trs240_list1_inted, maxdim = 2, maxscale = 3, samples = 10) 
  )
  
  save2RData(trs240_list1_inted_time1)
  save2RData(trs240_list1_inted_aggr1)
  
}

{
  trs240_list1_wvr_time1<-system.time(
    trs240_list1_wvr_aggr1<-calc_distance_change_betti(X = trs240_list1, maxdim = 2, maxscale = 3, samples = 10, 
                                                       ph_func = weighted_homology, l_rate=0.8, eta=3) )
  
  save2RData(trs240_list1_wvr_time1)
  save2RData(trs240_list1_wvr_aggr1)
  
}

#-----------------
#250点トーラス-----
trs250_list1<-lapply(1:100, function(i)torusUnif(n = 250, 1, 2.5))

trs250_list1_inted<-lapply(trs250_list1, function(X)interpo3d:::voronoi_interpo(X, 10) %>% rbind(X, .))

trs250_list1_inted1<-TDAdataset$new(trs250_list1_inted[[1]])

{
  trs250_list1_inted_time1<-system.time(
    trs250_list1_inted_aggr1<-smooth_landscape_method(X = trs250_list1_inted, maxdim = 2, maxscale = 3, samples = 10) 
  )
  
  save2RData(trs250_list1_inted_time1)
  save2RData(trs250_list1_inted_aggr1)
  
}

{
  trs250_list1_wvr_time1<-system.time(
    trs250_list1_wvr_aggr1<-calc_distance_change_betti(X = trs250_list1, maxdim = 2, maxscale = 3, samples = 10, 
                                                       ph_func = weighted_homology, l_rate=0.8, eta=3) )
  
  save2RData(trs250_list1_wvr_time1)
  save2RData(trs250_list1_wvr_aggr1)
  
}
