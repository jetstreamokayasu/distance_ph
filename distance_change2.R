#点数削減手法を用いた距離行列の変更の有効性を確かめる
#torus300_colle_set

trs300_coll1_aggr<-distance_change_method(X = torus300_colle_set[[1]], maxdim = 2, maxscale = 3, samples = 10)

#--------------------------------------------------------
#ランドマーク点の距離を変化させたときのベッチ数推定実験
#torus300_colle_set
trs300_coll1_aggr2<-maxmin_distance_change_method(X = torus300_colle_set[[1]], maxdim = 2, maxscale = 3, samples = 10)


#-------------------------------------------------------
#素の状態でのベッチ数推定実験
#サブサンプル85%
trs300_coll1_ori_time<-system.time(trs300_coll1_ori_aggr<-proposedMethodOnly(X = torus300_colle_set[[1]], maxdim = 2, maxscale = 3, samples = 10, const.size = 300*0.85))

#--------------------------------------
#TDAstat(ripserを使ったパッケージ)テスト
#ポイントクラウドデータ
trs300_1_10_subs_2_time2<-system.time(trs300_1_10_subs_2_pd2<-calculate_homology(mat = trs300_1_10_subs2[[2]], dim = 2))

#距離行列
trs300_1_10_subs2_2_time3<-system.time(trs300_1_10_subs2_2_pd3<-dist(trs300_1_10_subs2[[2]]) %>% as.matrix() %>% calculate_homology(mat = ., dim = 2, threshold = 3, format = "distmat"))
plot_persist(trs300_1_10_subs2_2_pd3)

#TDAstatからPLを求められるか？
#リストにすればできる
trs300_1_10_subs2_2_pl3<-calc_landscape(diag = list(trs300_1_10_subs2_2_pd3), maxscale = 3)

#距離行列を変化させて計算できるか？
trs300_1_10_vdmean_time<-system.time(trs300_1_10_vdmean_pd3<-calculate_homology(mat = trs300_1_10_vdmean_dist2, dim = 2, threshold = 3, format = "distmat"))
trs300_1_10_vdmean_pl3<-calc_landscape(list(trs300_1_10_vdmean_pd3), maxscale = 3)
trs300_1_10_vdmean_pl3B<-landscape(Diag = trs300_1_10_vdmean_pd3, dimension = 2)

tseq <-seq(min(trs300_1_10_vdmean_pd3[, 2:3]), max(trs300_1_10_vdmean_pd3[, 2:3]), length = 500)
plot(tseq, trs300_1_10_vdmean_pl3B, type = "l", col = 3)

#--------------------------------------------------------
#ランドマーク点の距離を変化させたときのベッチ数推定実験
#torus300_colle_set
#TDAstat(ripser)を使う
#85%サブサンプル
trs300_coll1_aggr3_time<-system.time(trs300_coll1_aggr3<-maxmin_distance_change_method(X = torus300_colle_set[[1]][1:10], maxdim = 2, maxscale = 3, samples = 10, const.size = 300*0.85))

#90%サブサンプル
trs300_coll1_aggr4_time<-system.time(trs300_coll1_aggr4<-maxmin_distance_change_method(X = torus300_colle_set[[1]][1:10], maxdim = 2, maxscale = 3, samples = 10, const.size = 300*0.9))

#80%サブサンプル
trs300_coll1_aggr5_time<-system.time(trs300_coll1_aggr5<-maxmin_distance_change_method(X = torus300_colle_set[[1]][1:10], maxdim = 2, maxscale = 3, samples = 10))

#85%サブサンプル、ランドマーク点15%、近傍点7
trs300_coll1_aggr6_time<-system.time(trs300_coll1_aggr6<-maxmin_distance_change_method(X = torus300_colle_set[[1]][1:10], maxdim = 2, maxscale = 3, samples = 10, const.size = 300*0.85, n_vic = 7))

#85%サブサンプル、ランドマーク点20%、近傍点10
trs300_coll1_aggr7_time<-system.time(trs300_coll1_aggr7<-maxmin_distance_change_method(X = torus300_colle_set[[1]][1:10], maxdim = 2, maxscale = 3, samples = 10, const.size = 300*0.85, l_rate = 0.2, n_vic = 10))

#85%サブサンプル、ランドマーク点20%、近傍点7
trs300_coll1_aggr8_time<-system.time(trs300_coll1_aggr8<-maxmin_distance_change_method(X = torus300_colle_set[[1]][1:10], maxdim = 2, maxscale = 3, samples = 10, const.size = 300*0.85, l_rate = 0.2, n_vic = 7))

#85%サブサンプル、ランドマーク点20%、近傍点8
trs300_coll1_aggr9_time<-system.time(trs300_coll1_aggr9<-maxmin_distance_change_method(X = torus300_colle_set[[1]][1:10], maxdim = 2, maxscale = 3, samples = 10, const.size = 300*0.85, l_rate = 0.2, n_vic = 8))

#85%サブサンプル、ランドマーク点20%、近傍点7
#結果のバランスとしては最も良い？
trs300_coll1_aggr8B_time<-system.time(trs300_coll1_aggr8B<-maxmin_distance_change_method(X = torus300_colle_set[[1]], maxdim = 2, maxscale = 3, samples = 10, const.size = 300*0.85, l_rate = 0.2, n_vic = 7))
