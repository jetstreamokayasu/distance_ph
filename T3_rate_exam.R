#-------------------------------
#3次元トーラスで成功率を求めてみる----
#H3の推定がうまくいかないので試す
#r = 2, R1 = 8, R2 = 4
#全データセット数30、サブサンプル数10
#eta=3.8, ランドマーク点50%
t3orus4_list3_1to30aggrs2_time<-system.time(
  t3orus4_list3_1to30aggrs2<-calc_distance_change_betti_paral(X = t3orus4_list3[1:30], maxdim = 3, maxscale = 9, samples = 10, 
                                                             ph_func = weighted_homology, l_rate=0.5, eta=7.5) )
