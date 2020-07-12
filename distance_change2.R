#点数削減手法を用いた距離行列の変更の有効性を確かめる
#torus300_colle_set

trs300_coll1_aggr<-distance_change_method(X = torus300_colle_set[[1]], maxdim = 2, maxscale = 3, samples = 10)

#--------------------------------------------------------
#ランドマーク点の距離を変化させたときのベッチ数推定実験
#torus300_colle_set
trs300_coll1_aggr2<-maxmin_distance_change_method(X = torus300_colle_set[[1]], maxdim = 2, maxscale = 3, samples = 10)
