#2次元トーラスのexp距離変化手法の再計算
torus300_colle_setB<-map(torus300_colle_set, ~{map(., ~{.[["noizyX"]]})})

trs300_colle1_wvr_time2<-system.time( trs300_colle1_wvr_aggr2<-calc_distance_change_betti(X = torus300_colle_setB[[1]], maxdim = 2, maxscale = 3, samples = 10, ph_func = weighted_homology, l_rate=0.8, eta=3.0) )
