#2次元トーラスの成功率実験

#------------------------------------------------
#2次元トーラス100セットをWVRで推定してみる-------
#300点トーラス

trs300_colle1_aggr_test<-calc_distance_change_betti(X = torus300_colle_set[[1]][1:2], maxdim = 2, maxscale = 3, samples = 5, ph_func = weighted_homology, l_rate=0.8, eta=3)

trs300_colle1_wvr_time<-system.time( trs300_colle1_wvr_aggr<-calc_distance_change_betti(X = torus300_colle_set[[1]], maxdim = 2, maxscale = 3, samples = 10, ph_func = weighted_homology, l_rate=0.8, eta=3) )

trs300_colle1_wvr_aggr_test<-calc_paral_distance_change_betti(X = torus300_colle_set[[1]][1:2], 
                                                              maxdim = 2, maxscale = 3, samples = 2, ph_func = weighted_homology, l_rate=0.8, eta=3)


#-----------------------
#2次元トーラス100セットをWVRで推定してみる-------
#350点トーラス

torus350_colle_set1<-map(torus350_colle_set[[1]], ~{.[["noizyX"]]})

trs350_colle1_wvr_time<-system.time( trs350_colle1_wvr_aggr<-calc_distance_change_betti(X = torus350_colle_set1, maxdim = 2, maxscale = 3, samples = 10, ph_func = weighted_homology, l_rate=0.8, eta=3) )


#-----------------------
#2次元トーラス100セットをWVRで推定してみる-------
#340点トーラス

torus340_colle_set1<-map(torus340_colle_set[[1]], ~{.[["noizyX"]]})

trs340_colle1_wvr_time<-system.time( trs340_colle1_wvr_aggr<-calc_distance_change_betti(X = torus340_colle_set1, maxdim = 2, maxscale = 3, samples = 10, ph_func = weighted_homology, l_rate=0.8, eta=3) )
