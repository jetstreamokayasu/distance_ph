#楕円体の成功率実験

#---------------------------------------
#楕円体の成功率を求める
#100点。a = 5, b = 1, c = 1
ellip100_list1<-map(1:100, ~{xEllip_unif(n = 100, a = 5, b = 1, c = 1)})

#CTIC2019手法
ellip100_aggr1<-smooth_landscape_method(X = ellip100_list1, maxdim = 2, maxscale = 3, samples = 10)

#expを掛ける距離変化
ellip100_wvr_aggr1<-calc_distance_change_betti(X = ellip100_list1, maxdim = 2, maxscale = 3, samples = 10, ph_func =  weighted_homology, l_rate=0.45, eta=4.0)

#補間手法
ellip100_inted_list1<-lapply(ellip100_list1, function(X){interpo3d:::voronoi_interpo(X, 10) %>% rbind(X, .)})

ellip100_inted_aggr1<-smooth_landscape_method(X = ellip100_inted_list1, maxdim = 2, maxscale = 3, samples = 10)

ellip100_list1_inted2<-lapply(ellip100_list1[1:20], function(X){interpo3d:::voronoi_interpo(X, 15) %>% rbind(X, .)})

#--------------------------------------
#楕円体の成功率を求める
#200点。a = 5, b = 1, c = 1
ellip200_list1<-map(1:100, ~{xEllip_unif(n = 200, a = 5, b = 1, c = 1)})

#CTIC2019手法
ellip200_aggr1<-smooth_landscape_method(X = ellip200_list1, maxdim = 2, maxscale = 3, samples = 10)

#---------------------------------
#楕円体を150～200点で推定----------
#--------------------------------------
#楕円体の成功率を求める
#150点。a = 5, b = 1, c = 1
ellip150_list1<-map(1:100, ~{xEllip_unif(n = 150, a = 5, b = 1, c = 1)})

#CTIC2019手法
ellip150_aggr1<-smooth_landscape_method(X = ellip150_list1, maxdim = 2, maxscale = 3, samples = 10)
save2RData(ellip150_aggr1)

#expを掛ける距離変化
ellip150_wvr_aggr1<-calc_distance_change_betti(X = ellip150_list1, maxdim = 2, maxscale = 3, samples = 10, ph_func =  weighted_homology, l_rate=0.45, eta=3.7)
save2RData(ellip150_wvr_aggr1)

{#補間手法
ellip150_list1_inted<-lapply(ellip150_list1, function(X){interpo3d:::voronoi_interpo(X, 10) %>% rbind(X, .)})
save2RData(ellip150_list1_inted)

ellip150_inted_aggr1<-smooth_landscape_method(X = ellip150_list1_inted, maxdim = 2, maxscale = 3, samples = 10)
save2RData(ellip150_inted_aggr1)
}

#2~5セット目
ellip150_list2to5<-map(1:4, ~{map(1:100, ~{xEllip_unif(n = 150, a = 5, b = 1, c = 1)})})

#100セットリスト2~5つ目
{#CTIC2019
  ellip150_list2to5_aggr<-lapply(1:4, function(k){
    
    cat("list", k, "calc\n")
    time<-system.time( aggr<-smooth_landscape_method(X = ellip150_list2to5[[k]], maxdim = 2, maxscale = 3, samples = 10) )
    return(append(aggr, list(time=time)))
    
  })
  save2RData(ellip150_list2to5_aggr)
}

{#expを掛ける距離変化
  ellip150_list2to5_wvr_aggr<-lapply(1:4, function(k){
    
    cat("list", k, "calc\n")
    time<-system.time( aggr<-calc_distance_change_betti(X = ellip150_list2to5[[k]], maxdim = 2, maxscale = 3, samples = 10, ph_func =  weighted_homology, l_rate=0.45, eta=3.7) )
    return(append(aggr, list(time=time)))
    
  })
  save2RData(ellip150_list2to5_wvr_aggr)
}

#補間手法
ellip150_list2to5_inted<-lapply(ellip150_list2to5, function(Z){lapply(Z, function(X)interpo3d:::voronoi_interpo(X, 10) %>% rbind(X, .))})

{
  ellip150_list2to5_inted_aggr<-lapply(1:4, function(k){
    
    cat("list", k, "calc\n")
    time<-system.time( aggr<-smooth_landscape_method(X = ellip150_list2to5_inted[[k]], maxdim = 2, maxscale = 3, samples = 10) )
    return(append(aggr, list(time=time)))
    
  })
  save2RData(ellip150_list2to5_inted_aggr)
}


#--------------------------------------
#楕円体の成功率を求める
#160点。a = 5, b = 1, c = 1
ellip160_list1to5<-map(1:5, ~{map(1:100, ~{xEllip_unif(n = 160, a = 5, b = 1, c = 1)})})

#100セットリスト1~5つ目
{#CTIC2019
  ellip160_list1to5_aggr<-lapply(1:5, function(k){
    
    cat("list", k, "calc\n")
    time<-system.time( aggr<-smooth_landscape_method(X = ellip160_list1to5[[k]], maxdim = 2, maxscale = 3, samples = 10) )
    return(append(aggr, list(time=time)))
    
  })
  save2RData(ellip160_list1to5_aggr)
}

{#expを掛ける距離変化
  ellip160_list1to5_wvr_aggr<-lapply(1:5, function(k){
    
    cat("list", k, "calc\n")
    time<-system.time( aggr<-calc_distance_change_betti(X = ellip160_list1to5[[k]], maxdim = 2, maxscale = 3, samples = 10, ph_func =  weighted_homology, l_rate=0.45, eta=3.7) )
    return(append(aggr, list(time=time)))
    
  })
  save2RData(ellip160_list1to5_wvr_aggr)
}

{#補間手法
  ellip160_list1to5_inted<-lapply(ellip160_list1to5, function(Z){lapply(Z, function(X)interpo3d:::voronoi_interpo(X, 10) %>% rbind(X, .))})
save2RData(ellip160_list1to5_inted)

  ellip160_list1to5_inted_aggr<-lapply(1:5, function(k){
    
    cat("list", k, "calc\n")
    time<-system.time( aggr<-smooth_landscape_method(X = ellip160_list1to5_inted[[k]], maxdim = 2, maxscale = 3, samples = 10) )
    return(append(aggr, list(time=time)))
    
  })
  save2RData(ellip160_list1to5_inted_aggr)
}

#--------------------------------------
#楕円体の成功率を求める
#170点。a = 5, b = 1, c = 1
ellip170_list1to5<-map(1:5, ~{map(1:100, ~{xEllip_unif(n = 170, a = 5, b = 1, c = 1)})})

#100セットリスト1~5つ目
{#CTIC2019
  ellip170_list1to5_aggr<-lapply(1:5, function(k){
    
    cat("list", k, "calc\n")
    time<-system.time( aggr<-smooth_landscape_method(X = ellip170_list1to5[[k]], maxdim = 2, maxscale = 3, samples = 10) )
    return(append(aggr, list(time=time)))
    
  })
  save2RData(ellip170_list1to5_aggr)
}

{{#expを掛ける距離変化
  ellip170_list1to5_wvr_aggr<-lapply(1:5, function(k){
    
    cat("list", k, "calc\n")
    time<-system.time( aggr<-calc_distance_change_betti(X = ellip170_list1to5[[k]], maxdim = 2, maxscale = 3, samples = 10, ph_func =  weighted_homology, l_rate=0.45, eta=3.7) )
    return(append(aggr, list(time=time)))
    
  })
  save2RData(ellip170_list1to5_wvr_aggr)
}

{#補間手法
  ellip170_list1to5_inted<-lapply(ellip170_list1to5, function(Z){lapply(Z, function(X)interpo3d:::voronoi_interpo(X, 10) %>% rbind(X, .))})
  save2RData(ellip170_list1to5_inted)
  
  ellip170_list1to5_inted_aggr<-lapply(1:5, function(k){
    
    cat("list", k, "calc\n")
    time<-system.time( aggr<-smooth_landscape_method(X = ellip170_list1to5_inted[[k]], maxdim = 2, maxscale = 3, samples = 10) )
    return(append(aggr, list(time=time)))
    
  })
  save2RData(ellip170_list1to5_inted_aggr)
}}


#--------------------------------------
#楕円体の成功率を求める
#180点。a = 5, b = 1, c = 1
ellip180_list1to5<-map(1:5, ~{map(1:100, ~{xEllip_unif(n = 180, a = 5, b = 1, c = 1)})})

#100セットリスト1~5つ目
{#CTIC2019
  ellip180_list1to5_aggr<-lapply(1:5, function(k){
    
    cat("list", k, "calc\n")
    time<-system.time( aggr<-smooth_landscape_method(X = ellip180_list1to5[[k]], maxdim = 2, maxscale = 3, samples = 10) )
    return(append(aggr, list(time=time)))
    
  })
  save2RData(ellip180_list1to5_aggr)
}

{#expを掛ける距離変化
  ellip180_list1to5_wvr_aggr<-lapply(1:5, function(k){
    
    cat("list", k, "calc\n")
    time<-system.time( aggr<-calc_distance_change_betti(X = ellip180_list1to5[[k]], maxdim = 2, maxscale = 3, samples = 10, ph_func =  weighted_homology, l_rate=0.45, eta=3.7) )
    return(append(aggr, list(time=time)))
    
  })
  save2RData(ellip180_list1to5_wvr_aggr)
}
  
{#補間手法
  ellip180_list1to5_inted<-lapply(ellip170_list1to5, function(Z){lapply(Z, function(X)interpo3d:::voronoi_interpo(X, 10) %>% rbind(X, .))})
  save2RData(ellip180_list1to5_inted)
  
  ellip180_list1to5_inted_aggr<-lapply(1:5, function(k){
    
    cat("list", k, "calc\n")
    time<-system.time( aggr<-smooth_landscape_method(X = ellip180_list1to5_inted[[k]], maxdim = 2, maxscale = 3, samples = 10) )
    return(append(aggr, list(time=time)))
    
  })
  save2RData(ellip180_list1to5_inted_aggr)
}
