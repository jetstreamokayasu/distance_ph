#3次元トーラスの推定成功率実験

#--------------------------------
#データリストからnoizyXを外す----

t3orus4_list3B<-map(t3orus4_list3, ~{.[["noizyX"]]})

#------------------------------------
#素の3次元トーラスで成功率を求めてみる----
#r = 2, R1 = 8, R2 = 4
#全データセット数30、サブサンプル数5
t3orus4_list3_1to30_normal_aggrs_time<-system.time(
  t3orus4_list3_1to30_normal_aggrs<-smooth_landscape_method_paral(X = t3orus4_list3B[1:30], maxdim = 3, maxscale = 9, samples = 5) )

#------------------------------------
#素の550点3次元トーラスで成功率を求めてみる----
#r = 2, R1 = 8, R2 = 4
#全データセット数30、サブサンプル数5

t3orus550_list1<-lapply(1:100, function(i)x3Dtorus_unif(n = 550, r = 2, R1 = 8, R2 = 4))
  

t3orus550_list1_1to30_normal_aggrs_time<-system.time(
  t3orus550_list1_1to30_normal_aggrs<-smooth_landscape_method_paral(X = t3orus550_list1[1:30], maxdim = 3, maxscale = 9, samples = 5) )


#-------------------------------------
#3次元トーラスで成功率を求めてみる----
#H3の推定がうまくいかないので試す
#r = 2, R1 = 8, R2 = 4
#全データセット数30、サブサンプル数10
#eta=7.5, ランドマーク点50%
t3orus4_list3_1to30aggrs2_time<-system.time(
  t3orus4_list3_1to30aggrs2<-calc_distance_change_betti_paral(X = t3orus4_list3B[1:30], maxdim = 3, maxscale = 9, samples = 10, 
                                                             ph_func = weighted_homology, l_rate=0.5, eta=7.5) )

#-------------------------------------
#3次元トーラスで成功率を求めてみる----
#H3の推定がうまくいかないので試す
#r = 2, R1 = 8, R2 = 4
#全データセット数30、サブサンプル数5
#eta=6.5, ランドマーク点50%
t3orus4_list3_1to30aggrs3_time<-system.time(
  t3orus4_list3_1to30aggrs3<-calc_distance_change_betti_paral(X = t3orus4_list3B[1:30], maxdim = 3, maxscale = 9, samples = 5, 
                                                              ph_func = weighted_homology, l_rate=0.5, eta=6.5) )           

#------------------------------------
#calc_distance_change_betti_paralのテスト
t3orus4_list3_1to2aggrs_test_time<-system.time(
  t3orus4_list3_1to2aggrs_test<-calc_distance_change_betti_paral(X = t3orus4_list3B[1:2], maxdim = 3, maxscale = 9, samples = 2, 
                                                             ph_func = weighted_homology, l_rate=0.5, eta=6.2) )
