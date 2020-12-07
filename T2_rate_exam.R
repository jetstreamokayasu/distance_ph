#2次元トーラスの成功率実験

#------------------------------------------------
#2次元トーラス100セットをWVRで推定してみる-------
#300点トーラス

trs300_colle1_aggr_test<-calc_distance_change_betti(X = torus300_colle_set[[1]][1:2], maxdim = 2, maxscale = 3, samples = 5, ph_func = weighted_homology, l_rate=0.8, eta=3)

#100セットリスト1つ目
trs300_colle1_wvr_time<-system.time( trs300_colle1_wvr_aggr<-calc_distance_change_betti(X = torus300_colle_set[[1]], maxdim = 2, maxscale = 3, samples = 10, ph_func = weighted_homology, l_rate=0.8, eta=3) )

trs300_colle1_wvr_aggr_test<-ccalc_distance_change_betti_paral(X = torus300_colle_set[[1]][1:2], 
                                                              maxdim = 2, maxscale = 3, samples = 2, ph_func = weighted_homology, l_rate=0.8, eta=3)
#100セットリスト2~3つ目
torus300_colle_setB<-lapply(torus300_colle_set, function(X)lapply(X, function(Y)Y[["noizyX"]]))


{
  trs300_colle2to3_wvr_aggr<-lapply(2:3, function(k){
    
    cat("list", k, "calc\n")
    time<-system.time( aggr<-calc_distance_change_betti(X = torus300_colle_setB[[k]], 
                                                        maxdim = 2, maxscale = 3, samples = 10, 
                                                        ph_func = weighted_homology, l_rate=0.8, eta=3) )
    return(append(aggr, list(time=time)))
    
  })
  save2RData(trs300_colle2to3_wvr_aggr)
}

#-----------------------
#2次元トーラス100セットをWVRで推定してみる-------
#350点トーラス

torus350_colle_set1<-map(torus350_colle_set[[1]], ~{.[["noizyX"]]})

trs350_colle1_wvr_time<-system.time( trs350_colle1_wvr_aggr<-calc_distance_change_betti(X = torus350_colle_set1, maxdim = 2, maxscale = 3, samples = 10, ph_func = weighted_homology, l_rate=0.8, eta=3) )

torus350_colle_setB<-lapply(torus350_colle_set, function(X)lapply(X, function(Y)Y[["noizyX"]]))

#100セットリスト2~3つ目
torus300_colle_setB<-lapply(torus300_colle_set, function(X)lapply(X, function(Y)Y[["noizyX"]]))


{
  trs350_colle2to3_wvr_aggr<-lapply(2:3, function(k){
    
    cat("list", k, "calc\n")
    time<-system.time( aggr<-calc_distance_change_betti(X = torus350_colle_setB[[k]], 
                                                        maxdim = 2, maxscale = 3, samples = 10, 
                                                        ph_func = weighted_homology, l_rate=0.8, eta=3) )
    return(append(aggr, list(time=time)))
    
  })
  save2RData(trs350_colle2to3_wvr_aggr)
}

#-----------------------
#2次元トーラス100セットをWVRで推定してみる-------
#340点トーラス

torus340_colle_set1<-map(torus340_colle_set[[1]], ~{.[["noizyX"]]})

trs340_colle1_wvr_time<-system.time( trs340_colle1_wvr_aggr<-calc_distance_change_betti(X = torus340_colle_set1, maxdim = 2, maxscale = 3, samples = 10, ph_func = weighted_homology, l_rate=0.8, eta=3) )

#100セットリスト2~3つ目
torus340_colle_setB<-lapply(torus340_colle_set, function(X)lapply(X, function(Y)Y[["noizyX"]]))


{
  trs340_colle2to3_wvr_aggr<-lapply(2:3, function(k){
    
    cat("list", k, "calc\n")
    time<-system.time( aggr<-calc_distance_change_betti(X = torus340_colle_setB[[k]], 
                                                        maxdim = 2, maxscale = 3, samples = 10, 
                                                        ph_func = weighted_homology, l_rate=0.8, eta=3) )
    return(append(aggr, list(time=time)))
    
  })
  save2RData(trs340_colle2to3_wvr_aggr)
}

#-----------------------
#2次元トーラス100セットをWVRで推定してみる-------
#330点トーラス

torus330_colle_set1<-map(torus330_colle_set[[1]], ~{.[["noizyX"]]})

trs330_colle1_wvr_time<-system.time( trs330_colle1_wvr_aggr<-calc_distance_change_betti(X = torus330_colle_set1, maxdim = 2, maxscale = 3, samples = 10, ph_func = weighted_homology, l_rate=0.8, eta=3) )

#100セットリスト2~3つ目
torus330_colle_setB<-lapply(torus330_colle_set, function(X)lapply(X, function(Y)Y[["noizyX"]]))


{
  trs330_colle2to3_wvr_aggr<-lapply(2:3, function(k){
    
    cat("list", k, "calc\n")
    time<-system.time( aggr<-calc_distance_change_betti(X = torus330_colle_setB[[k]], 
                                                        maxdim = 2, maxscale = 3, samples = 10, 
                                                        ph_func = weighted_homology, l_rate=0.8, eta=3) )
    return(append(aggr, list(time=time)))
    
  })
  save2RData(trs330_colle2to3_wvr_aggr)
}

#-----------------------
#2次元トーラス100セットをWVRで推定してみる-------
#320点トーラス

torus320_colle_set1<-map(torus320_colle_set[[1]], ~{.[["noizyX"]]})

trs320_colle1_wvr_time<-system.time( trs320_colle1_wvr_aggr<-calc_distance_change_betti(X = torus320_colle_set1, maxdim = 2, maxscale = 3, samples = 10, ph_func = weighted_homology, l_rate=0.8, eta=3) )

#100セットリスト2~3つ目
torus320_colle_setB<-lapply(torus320_colle_set, function(X)lapply(X, function(Y)Y[["noizyX"]]))

{
  trs320_colle2to3_wvr_aggr<-lapply(2:3, function(k){
    
    cat("list", k, "calc\n")
    time<-system.time( aggr<-calc_distance_change_betti(X = torus320_colle_setB[[k]], 
                                                        maxdim = 2, maxscale = 3, samples = 10, 
                                                        ph_func = weighted_homology, l_rate=0.8, eta=3) )
    return(append(aggr, list(time=time)))
    
  })
  save2RData(trs320_colle2to3_wvr_aggr)
}

#-----------------------
#2次元トーラス100セットをWVRで推定してみる-------
#310点トーラス

torus310_colle_set1<-map(torus310_colle_set[[1]], ~{.[["noizyX"]]})

trs310_colle1_wvr_time<-system.time( trs310_colle1_wvr_aggr<-calc_distance_change_betti(X = torus310_colle_set1, maxdim = 2, maxscale = 3, samples = 10, ph_func = weighted_homology, l_rate=0.8, eta=3) )

#100セットリスト2~3つ目
torus310_colle_setB<-lapply(torus310_colle_set, function(X)lapply(X, function(Y)Y[["noizyX"]]))

{
  trs310_colle2to3_wvr_aggr<-lapply(2:3, function(k){
    
    cat("list", k, "calc\n")
    time<-system.time( aggr<-calc_distance_change_betti(X = torus310_colle_setB[[k]], 
                                                        maxdim = 2, maxscale = 3, samples = 10, 
                                                        ph_func = weighted_homology, l_rate=0.8, eta=3) )
    return(append(aggr, list(time=time)))
    
  })
  save2RData(trs310_colle2to3_wvr_aggr)
}
