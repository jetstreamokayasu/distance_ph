#密度が低い場合にスケールを変化させた場合のPHがどうなるかを調べる
half_trs1<-torus1/2

hf_trs1_pd<-calculate_homology(half_trs1, dim = 2, threshold = 3)
hf_trs1_pl<-calcLandscape(diag = hf_trs1_pd, maxscale = 3)

#torus300_colle_set[[1]]を使って300点トーラス100セットのFRI距離行列変化の実験をする
trs300_1_fri_aggr_time<-system.time( trs300_1_fri_aggr<-fri_distance_change_method(torus300_colle_set[[1]], maxdim = 2, maxscale = 3, samples = 10, l_rate = 0.3, eta = 1) )

trs300_1_fri_aggr2_time<-system.time( trs300_1_fri_aggr2<-fri_distance_change_method(torus300_colle_set[[1]], maxdim = 2, maxscale = 3, samples = 10, l_rate = 0.45, eta = 1) )


