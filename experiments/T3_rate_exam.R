#3次元トーラスの推定成功率実験

#--------------------------------
#データリストからnoizyXを外す----

t3orus4_list3B<-map(t3orus4_list3, ~{.[["noizyX"]]})

#------------------------------------
#素の500点3次元トーラスで成功率を求めてみる----
#r = 2, R1 = 8, R2 = 4
#全データセット数30、サブサンプル数5
t3orus4_list3_1to30_normal_aggrs_time<-system.time(
  t3orus4_list3_1to30_normal_aggrs<-smooth_landscape_method_paral(X = t3orus4_list3B[1:30], maxdim = 3, maxscale = 9, samples = 5) )

{
  t3orus4_list3_31to100_normal_aggrs_time<-system.time(
    t3orus4_list3_31to100_normal_aggrs<-smooth_landscape_method_paral(X = t3orus4_list3B[31:100], maxdim = 3, maxscale = 9, samples = 5) )
  
  save2RData(t3orus4_list3_31to100_normal_aggrs_time)
  save2RData(t3orus4_list3_31to100_normal_aggrs)
}

#------------------------------------
#素の450点3次元トーラスで成功率を求めてみる----
#r = 2, R1 = 8, R2 = 4
#全データセット数30、サブサンプル数5

t3orus450_list1<-lapply(1:100, function(i)x3Dtorus_unif(n = 450, r = 2, R1 = 8, R2 = 4))


t3orus450_list1_1to30_normal_aggrs_time<-system.time(
  t3orus450_list1_1to30_normal_aggrs<-smooth_landscape_method_paral(X = t3orus450_list1[1:30], maxdim = 3, maxscale = 9, samples = 5) )

{
  t3orus450_list1_31to100_normal_aggrs_time<-system.time(
    t3orus450_list1_31to100_normal_aggrs<-smooth_landscape_method_paral(X = t3orus450_list1[31:100], maxdim = 3, maxscale = 9, samples = 5) )
  
  save2RData(t3orus450_list1_31to100_normal_aggrs_time)
  save2RData(t3orus450_list1_31to100_normal_aggrs)
}

#450点3次元トーラス100セット2つ目
t3orus450_list2<-lapply(1:100, function(i)x3Dtorus_unif(n = 450, r = 2, R1 = 8, R2 = 4))

#------------------------------------
#素の550点3次元トーラスで成功率を求めてみる----
#r = 2, R1 = 8, R2 = 4
#全データセット数30、サブサンプル数5

t3orus550_list1<-lapply(1:100, function(i)x3Dtorus_unif(n = 550, r = 2, R1 = 8, R2 = 4))
  

t3orus550_list1_1to30_normal_aggrs_time<-system.time(
  t3orus550_list1_1to30_normal_aggrs<-smooth_landscape_method_paral(X = t3orus550_list1[1:30], maxdim = 3, maxscale = 9, samples = 5) )

#------------------------------------
#素の490点3次元トーラスで成功率を求めてみる----
#r = 2, R1 = 8, R2 = 4
#全データセット数30、サブサンプル数5

t3orus490_list1<-lapply(1:100, function(i)x3Dtorus_unif(n = 490, r = 2, R1 = 8, R2 = 4))


t3orus490_list1_1to30_normal_aggrs_time<-system.time(
  t3orus490_list1_1to30_normal_aggrs<-smooth_landscape_method_paral(X = t3orus490_list1[1:30], maxdim = 3, maxscale = 9, samples = 5) )

{
  t3orus490_list1_31to100_normal_aggrs_time<-system.time(
    t3orus490_list1_31to100_normal_aggrs<-smooth_landscape_method_paral(X = t3orus490_list1[31:100], maxdim = 3, maxscale = 9, samples = 5) )
  
  save2RData(t3orus490_list1_31to100_normal_aggrs_time)
  save2RData(t3orus490_list1_31to100_normal_aggrs)
}

#------------------------------------
#素の480点3次元トーラスで成功率を求めてみる----
#r = 2, R1 = 8, R2 = 4
#全データセット数30、サブサンプル数5

t3orus480_list1<-lapply(1:100, function(i)x3Dtorus_unif(n = 480, r = 2, R1 = 8, R2 = 4))


t3orus480_list1_1to30_normal_aggrs_time<-system.time(
  t3orus480_list1_1to30_normal_aggrs<-smooth_landscape_method_paral(X = t3orus480_list1[1:30], maxdim = 3, maxscale = 9, samples = 5) )

{
  t3orus480_list1_31to100_normal_aggrs_time<-system.time(
    t3orus480_list1_31to100_normal_aggrs<-smooth_landscape_method_paral(X = t3orus480_list1[31:100], maxdim = 3, maxscale = 9, samples = 5) )
  
  save2RData(t3orus480_list1_31to100_normal_aggrs_time)
  save2RData(t3orus480_list1_31to100_normal_aggrs)
}

#480点3次元トーラス100セット2つ目
t3orus480_list2<-lapply(1:100, function(i)x3Dtorus_unif(n = 480, r = 2, R1 = 8, R2 = 4))

#------------------------------------
#素の470点3次元トーラスで成功率を求めてみる----
#r = 2, R1 = 8, R2 = 4
#全データセット数30、サブサンプル数5

t3orus470_list1<-lapply(1:100, function(i)x3Dtorus_unif(n = 470, r = 2, R1 = 8, R2 = 4))

{
  t3orus470_list1_1to30_normal_aggrs_time<-system.time(
    t3orus470_list1_1to30_normal_aggrs<-smooth_landscape_method_paral(X = t3orus470_list1[1:30], maxdim = 3, maxscale = 9, samples = 5) )

  save2RData(t3orus470_list1_1to30_normal_aggrs_time)
  save2RData(t3orus470_list1_1to30_normal_aggrs)
}

{
  t3orus470_list1_31to100_normal_aggrs_time<-system.time(
    t3orus470_list1_31to100_normal_aggrs<-smooth_landscape_method_paral(X = t3orus470_list1[31:100], maxdim = 3, maxscale = 9, samples = 5) )
  
  save2RData(t3orus470_list1_31to100_normal_aggrs_time)
  save2RData(t3orus470_list1_31to100_normal_aggrs)
}

t3orus470_list2<-lapply(1:100, function(i)x3Dtorus_unif(n = 470, r = 2, R1 = 8, R2 = 4))

#------------------------------------
#素の460点3次元トーラスで成功率を求めてみる----
#r = 2, R1 = 8, R2 = 4
#全データセット数30、サブサンプル数5

t3orus460_list1<-lapply(1:100, function(i)x3Dtorus_unif(n = 460, r = 2, R1 = 8, R2 = 4))

{
  t3orus460_list1_1to30_normal_aggrs_time<-system.time(
    t3orus460_list1_1to30_normal_aggrs<-smooth_landscape_method_paral(X = t3orus460_list1[1:30], maxdim = 3, maxscale = 9, samples = 5) )
  
  save2RData(t3orus460_list1_1to30_normal_aggrs_time)
  save2RData(t3orus460_list1_1to30_normal_aggrs)
}

{
  t3orus460_list1_31to100_normal_aggrs_time<-system.time(
    t3orus460_list1_31to100_normal_aggrs<-smooth_landscape_method_paral(X = t3orus460_list1[31:100], maxdim = 3, maxscale = 9, samples = 5) )
  
  save2RData(t3orus460_list1_31to100_normal_aggrs_time)
  save2RData(t3orus460_list1_31to100_normal_aggrs)
}

#460点3次元トーラス100セット2つ目
t3orus460_list2<-lapply(1:100, function(i)x3Dtorus_unif(n = 460, r = 2, R1 = 8, R2 = 4))

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
#500点3次元トーラスで成功率を求めてみる----
#H3の推定がうまくいかないので試す
#r = 2, R1 = 8, R2 = 4
#全データセット数30、サブサンプル数5
#eta=6.5, ランドマーク点50%
t3orus4_list3_1to30aggrs3_time<-system.time(
  t3orus4_list3_1to30aggrs3<-calc_distance_change_betti_paral(X = t3orus4_list3B[1:30], maxdim = 3, maxscale = 9, samples = 5, 
                                                              ph_func = weighted_homology, l_rate=0.5, eta=6.5) )   

#計算時間を再び求める
{t3orus4_list3_1to30aggrs4_time<-system.time(
  t3orus4_list3_1to30aggrs4<-calc_distance_change_betti_paral(X = t3orus4_list3B[1:30], maxdim = 3, maxscale = 9, samples = 5, 
                                                              ph_func = weighted_homology, l_rate=0.5, eta=6.5) )
  save2RData(t3orus4_list3_1to30aggrs4)
  save2RData(t3orus4_list3_1to30aggrs4_time)
  
  }   

 t3orus4_list3_31to100_wvr_aggrs3_time<-system.time(
  t3orus4_list3_31to100_wvr_aggrs3<-calc_distance_change_betti_paral(X = t3orus4_list3B[31:100], maxdim = 3, maxscale = 9, samples = 5, 
                                                              ph_func = weighted_homology, l_rate=0.5, eta=6.5) )   

#-------------------------------------
#550点3次元トーラスで成功率を求めてみる----
#r = 2, R1 = 8, R2 = 4
#全データセット数30、サブサンプル数5
#eta=6.5, ランドマーク点50%
t3orus550_list1_1to30_wvr_aggrs1_time<-system.time(
  t3orus550_list1_1to30_wvr_aggrs1<-calc_distance_change_betti_paral(X = t3orus550_list1[1:30], maxdim = 3, maxscale = 9, samples = 5, 
                                                              ph_func = weighted_homology, l_rate=0.5, eta=6.5) )   

#-------------------------------------
#450点3次元トーラスで成功率を求めてみる----
#r = 2, R1 = 8, R2 = 4
#全データセット数30、サブサンプル数5
#eta=6.5, ランドマーク点50%
t3orus450_list1_1to30_wvr_aggrs1_time<-system.time(
  t3orus450_list1_1to30_wvr_aggrs1<-calc_distance_change_betti_paral(X = t3orus450_list1[1:30], maxdim = 3, maxscale = 9, samples = 5, 
                                                                     ph_func = weighted_homology, l_rate=0.5, eta=6.5) )   

#31~100セット目
{
t3orus450_list1_31to100_wvr_aggrs1_time<-system.time(
  t3orus450_list1_31to100_wvr_aggrs1<-calc_distance_change_betti_paral(X = t3orus450_list1[31:100], maxdim = 3, maxscale = 9, samples = 5, 
                                                                     ph_func = weighted_homology, l_rate=0.5, eta=6.5) )   

save2RData(t3orus450_list1_31to100_wvr_aggrs1_time)
save2RData(t3orus450_list1_31to100_wvr_aggrs1)
}

#100セット2つ目
{
  t3orus450_list2_wvr_aggr1_time<-system.time(
    t3orus450_list2_wvr_aggr1<-calc_distance_change_betti_paral(X = t3orus450_list2, maxdim = 3, maxscale = 9, samples = 5, 
                                                                         ph_func = weighted_homology, l_rate=0.5, eta=6.5) )   
  
  save2RData(t3orus450_list2_wvr_aggr1_time)
  save2RData(t3orus450_list2_wvr_aggr1)
}

#-------------------------------------
#490点3次元トーラスで成功率を求めてみる----
#r = 2, R1 = 8, R2 = 4
#全データセット数30、サブサンプル数5
#eta=6.5, ランドマーク点50%
{
t3orus490_list1_1to30_wvr_aggrs1_time<-system.time(
  t3orus490_list1_1to30_wvr_aggrs1<-calc_distance_change_betti_paral(X = t3orus490_list1[1:30], maxdim = 3, maxscale = 9, samples = 5, 
                                                                     ph_func = weighted_homology, l_rate=0.5, eta=6.5) )   
save2RData(t3orus490_list1_1to30_wvr_aggrs1_time)
save2RData(t3orus490_list1_1to30_wvr_aggrs1)
}

#31~100セット目
{
  t3orus490_list1_31to100_wvr_aggrs1_time<-system.time(
    t3orus490_list1_31to100_wvr_aggrs1<-calc_distance_change_betti_paral(X = t3orus490_list1[31:100], maxdim = 3, maxscale = 9, samples = 5, 
                                                                         ph_func = weighted_homology, l_rate=0.5, eta=6.5) )   
  
  save2RData(t3orus490_list1_31to100_wvr_aggrs1_time)
  save2RData(t3orus490_list1_31to100_wvr_aggrs1)
}

#-------------------------------------
#480点3次元トーラスで成功率を求めてみる----
#r = 2, R1 = 8, R2 = 4
#全データセット数30、サブサンプル数5
#eta=6.5, ランドマーク点50%
{
  t3orus480_list1_1to30_wvr_aggrs1_time<-system.time(
    t3orus480_list1_1to30_wvr_aggrs1<-calc_distance_change_betti_paral(X = t3orus480_list1[1:30], maxdim = 3, maxscale = 9, samples = 5, 
                                                                       ph_func = weighted_homology, l_rate=0.5, eta=6.5) )   
  save2RData(t3orus480_list1_1to30_wvr_aggrs1_time)
  save2RData(t3orus480_list1_1to30_wvr_aggrs1)
}

#31~100セット目
{
  t3orus480_list1_31to100_wvr_aggrs1_time<-system.time(
    t3orus480_list1_31to100_wvr_aggrs1<-calc_distance_change_betti_paral(X = t3orus480_list1[31:100], maxdim = 3, maxscale = 9, samples = 5, 
                                                                         ph_func = weighted_homology, l_rate=0.5, eta=6.5) )   
  
  save2RData(t3orus480_list1_31to100_wvr_aggrs1_time)
  save2RData(t3orus480_list1_31to100_wvr_aggrs1)
}

#100セット2つ目
{
  t3orus480_list2_wvr_aggr1_time<-system.time(
    t3orus480_list2_wvr_aggr1<-calc_distance_change_betti_paral(X = t3orus480_list2, maxdim = 3, maxscale = 9, samples = 5, 
                                                                ph_func = weighted_homology, l_rate=0.5, eta=6.5) )   
  
  save2RData(t3orus480_list2_wvr_aggr1_time)
  save2RData(t3orus480_list2_wvr_aggr1)
}

#-------------------------------------
#470点3次元トーラスで成功率を求めてみる----
#r = 2, R1 = 8, R2 = 4
#全データセット数30、サブサンプル数5
#eta=6.5, ランドマーク点50%
{
  t3orus470_list1_1to30_wvr_aggrs1_time<-system.time(
    t3orus470_list1_1to30_wvr_aggrs1<-calc_distance_change_betti_paral(X = t3orus470_list1[1:30], maxdim = 3, maxscale = 9, samples = 5, 
                                                                       ph_func = weighted_homology, l_rate=0.5, eta=6.5) )   
  save2RData(t3orus470_list1_1to30_wvr_aggrs1_time)
  save2RData(t3orus470_list1_1to30_wvr_aggrs1)
}

#31~100セット目
{
  t3orus470_list1_31to100_wvr_aggrs1_time<-system.time(
    t3orus470_list1_31to100_wvr_aggrs1<-calc_distance_change_betti_paral(X = t3orus470_list1[31:100], maxdim = 3, maxscale = 9, samples = 5, 
                                                                         ph_func = weighted_homology, l_rate=0.5, eta=6.5) )   
  
  save2RData(t3orus470_list1_31to100_wvr_aggrs1_time)
  save2RData(t3orus470_list1_31to100_wvr_aggrs1)
}

#100セット2つ目
{
  t3orus470_list2_wvr_aggr1_time<-system.time(
    t3orus470_list2_wvr_aggr1<-calc_distance_change_betti_paral(X = t3orus470_list2, maxdim = 3, maxscale = 9, samples = 5, 
                                                                ph_func = weighted_homology, l_rate=0.5, eta=6.5) )   
  
  save2RData(t3orus470_list2_wvr_aggr1_time)
  save2RData(t3orus470_list2_wvr_aggr1)
}

#-------------------------------------
#460点3次元トーラスで成功率を求めてみる----
#r = 2, R1 = 8, R2 = 4
#全データセット数30、サブサンプル数5
#eta=6.5, ランドマーク点50%
{
  t3orus460_list1_1to30_wvr_aggrs1_time<-system.time(
    t3orus460_list1_1to30_wvr_aggrs1<-calc_distance_change_betti_paral(X = t3orus460_list1[1:30], maxdim = 3, maxscale = 9, samples = 5, 
                                                                       ph_func = weighted_homology, l_rate=0.5, eta=6.5) )   
  save2RData(t3orus460_list1_1to30_wvr_aggrs1_time)
  save2RData(t3orus460_list1_1to30_wvr_aggrs1)
}

#31~100セット目
{
  t3orus460_list1_31to100_wvr_aggrs1_time<-system.time(
    t3orus460_list1_31to100_wvr_aggrs1<-calc_distance_change_betti_paral(X = t3orus460_list1[31:100], maxdim = 3, maxscale = 9, samples = 5, 
                                                                         ph_func = weighted_homology, l_rate=0.5, eta=6.5) )   
  
  save2RData(t3orus460_list1_31to100_wvr_aggrs1_time)
  save2RData(t3orus460_list1_31to100_wvr_aggrs1)
}

#100セット2つ目
{
  t3orus460_list2_wvr_aggr1_time<-system.time(
    t3orus460_list2_wvr_aggr1<-calc_distance_change_betti_paral(X = t3orus460_list2, maxdim = 3, maxscale = 9, samples = 5, 
                                                                ph_func = weighted_homology, l_rate=0.5, eta=6.5) )   
  
  save2RData(t3orus460_list2_wvr_aggr1_time)
  save2RData(t3orus460_list2_wvr_aggr1)
}

#--------------------------------------
#calc_distance_change_betti_paralのテスト----
t3orus4_list3_1to2aggrs_test_time<-system.time(
  t3orus4_list3_1to2aggrs_test<-calc_distance_change_betti_paral(X = t3orus4_list3B[1:2], maxdim = 3, maxscale = 9, samples = 2, 
                                                             ph_func = weighted_homology, l_rate=0.5, eta=6.2) )



#-------------------------------------------------
#以下3次元トーラスの2次ベッチ数の推定実験----------

#-------------------------------------
#450点3次元トーラスで2次ベッチ数の成功率を求めてみる----
#r = 2, R1 = 8, R2 = 4
#全データセット数30、サブサンプル数5
#eta=3.9, ランドマーク点10%
t3orus450_list1_1to30_wvr_H2aggrs1_time<-system.time(
  t3orus450_list1_1to30_wvr_H2aggrs1<-calc_distance_change_betti_paral(X = t3orus450_list1[1:30], maxdim = 3, maxscale = 9, samples = 5, 
                                                                     ph_func = weighted_homology, l_rate=0.1, eta=3.9) )   

{
  t3orus450_list1_31to100_wvr_H2aggrs1_time<-system.time(
    t3orus450_list1_31to100_wvr_H2aggrs1<-calc_distance_change_betti_paral(X = t3orus450_list1[31:100], maxdim = 3, maxscale = 9, samples = 5, 
                                                                         ph_func = weighted_homology, l_rate=0.1, eta=3.9) )   
  
  save2RData(t3orus450_list1_31to100_wvr_H2aggrs1)
  save2RData(t3orus450_list1_31to100_wvr_H2aggrs1_time)
  
}

#-------------------------------------
#500点3次元トーラスで2次ベッチ数の成功率を求めてみる----
#r = 2, R1 = 8, R2 = 4
#全データセット数30、サブサンプル数5
#eta=3.6, ランドマーク点50%
t3orus4_list3_1to30_wvrH2_aggrs4_time<-system.time(
  t3orus4_list3_1to30_wvrH2_aggrs4<-calc_distance_change_betti_paral(X = t3orus4_list3B[1:30], maxdim = 3, maxscale = 9, samples = 5, 
                                                              ph_func = weighted_homology, l_rate=0.5, eta=3.6) )   

#-------------------------------------
#500点3次元トーラスで2次ベッチ数の成功率を求めてみる----
#r = 2, R1 = 8, R2 = 4
#全データセット数30、サブサンプル数5
#eta=6.0, ランドマーク点10%
t3orus4_list3_1to30_wvrH2_aggrs5_time<-system.time(
  t3orus4_list3_1to30_wvrH2_aggrs5<-calc_distance_change_betti_paral(X = t3orus4_list3B[1:30], maxdim = 3, maxscale = 9, samples = 5, 
                                                                     ph_func = weighted_homology, l_rate=0.1, eta=6.0) )   

#-------------------------------------
#500点3次元トーラスで2次ベッチ数の成功率を求めてみる----
#r = 2, R1 = 8, R2 = 4
#全データセット数30、サブサンプル数5
#eta=4.0, ランドマーク点50%
t3orus4_list3_1to30_wvrH2_aggrs6_time<-system.time(
  t3orus4_list3_1to30_wvrH2_aggrs6<-calc_distance_change_betti_paral(X = t3orus4_list3B[1:30], maxdim = 3, maxscale = 9, samples = 5, 
                                                                     ph_func = weighted_homology, l_rate=0.5, eta=4.0) )   

{#31~100セット目
  t3orus4_list3_31to100_wvrH2_aggrs6_time<-system.time(
  t3orus4_list3_31to100_wvrH2_aggrs6<-calc_distance_change_betti_paral(X = t3orus4_list3B[31:100], maxdim = 3, maxscale = 9, samples = 5, 
                                                                     ph_func = weighted_homology, l_rate=0.5, eta=4.0) )   
save2RData(t3orus4_list3_31to100_wvrH2_aggrs6_time)
save2RData(t3orus4_list3_31to100_wvrH2_aggrs6)
}
#-------------------------------------
#500点3次元トーラスで2次ベッチ数の成功率を求めてみる----
#r = 2, R1 = 8, R2 = 4
#全データセット数30、サブサンプル数5
#eta=4.5, ランドマーク点50%
t3orus4_list3_1to30_wvrH2_aggrs7_time<-system.time(
  t3orus4_list3_1to30_wvrH2_aggrs7<-calc_distance_change_betti_paral(X = t3orus4_list3B[1:30], maxdim = 3, maxscale = 9, samples = 5, 
                                                                     ph_func = weighted_homology, l_rate=0.5, eta=4.5) )   


#-------------------------------------
#450点3次元トーラスで2次ベッチ数の成功率を求めてみる----
#r = 2, R1 = 8, R2 = 4
#全データセット数30、サブサンプル数5
#eta=4.0, ランドマーク点50%
t3orus450_list1_1to30_wvr_aggrs2_time<-system.time(
  t3orus450_list1_1to30_wvrH2_aggrs2<-calc_distance_change_betti_paral(X = t3orus450_list1[1:30], maxdim = 3, maxscale = 9, samples = 5, 
                                                                     ph_func = weighted_homology, l_rate=0.5, eta=4.0) )   

{
  
  t3orus450_list1_31to100_wvr_aggrs2_time<-system.time(
    t3orus450_list1_31to100_wvrH2_aggrs2<-calc_distance_change_betti_paral(X = t3orus450_list1[31:100], maxdim = 3, maxscale = 9, samples = 5, 
                                                                         ph_func = weighted_homology, l_rate=0.5, eta=4.0) )   
  
  save2RData(t3orus450_list1_31to100_wvr_aggrs2_time)
  save2RData(t3orus450_list1_31to100_wvrH2_aggrs2)
  
}

#-------------------------------------
#490点3次元トーラスで2次ベッチ数の成功率を求めてみる----
#r = 2, R1 = 8, R2 = 4
#全データセット数30、サブサンプル数5
#eta=4.0, ランドマーク点50%
t3orus490_list1_1to30_wvr_aggrs2_time<-system.time(
  t3orus490_list1_1to30_wvrH2_aggrs2<-calc_distance_change_betti_paral(X = t3orus490_list1[1:30], maxdim = 3, maxscale = 9, samples = 5, 
                                                                       ph_func = weighted_homology, l_rate=0.5, eta=4.0) )   

{
  t3orus490_list1_31to100_wvr_aggrs2_time<-system.time(
    t3orus490_list1_31to100_wvrH2_aggrs2<-calc_distance_change_betti_paral(X = t3orus490_list1[31:100], maxdim = 3, maxscale = 9, samples = 5, 
                                                                           ph_func = weighted_homology, l_rate=0.5, eta=4.0) )   
  
  save2RData(t3orus490_list1_31to100_wvr_aggrs2_time)
  save2RData(t3orus490_list1_31to100_wvrH2_aggrs2)
  
}

#-------------------------------------
#480点3次元トーラスで2次ベッチ数の成功率を求めてみる----
#r = 2, R1 = 8, R2 = 4
#全データセット数30、サブサンプル数5
#eta=4.0, ランドマーク点50%
t3orus480_list1_1to30_wvr_aggrs2_time<-system.time(
  t3orus480_list1_1to30_wvrH2_aggrs2<-calc_distance_change_betti_paral(X = t3orus480_list1[1:30], maxdim = 3, maxscale = 9, samples = 5, 
                                                                       ph_func = weighted_homology, l_rate=0.5, eta=4.0) )   

{
  t3orus480_list1_31to100_wvr_aggrs2_time<-system.time(
    t3orus480_list1_31to100_wvrH2_aggrs2<-calc_distance_change_betti_paral(X = t3orus480_list1[31:100], maxdim = 3, maxscale = 9, samples = 5, 
                                                                           ph_func = weighted_homology, l_rate=0.5, eta=4.0) )   
  
  save2RData(t3orus480_list1_31to100_wvr_aggrs2_time)
  save2RData(t3orus480_list1_31to100_wvrH2_aggrs2)
  
}

#-------------------------------------
#470点3次元トーラスで2次ベッチ数の成功率を求めてみる----
#r = 2, R1 = 8, R2 = 4
#全データセット数30、サブサンプル数5
#eta=4.0, ランドマーク点50%
t3orus470_list1_1to30_wvr_aggrs2_time<-system.time(
  t3orus470_list1_1to30_wvrH2_aggrs2<-calc_distance_change_betti_paral(X = t3orus470_list1[1:30], maxdim = 3, maxscale = 9, samples = 5, 
                                                                       ph_func = weighted_homology, l_rate=0.5, eta=4.0) )   
{
  t3orus470_list1_31to100_wvr_aggrs2_time<-system.time(
    t3orus470_list1_31to100_wvrH2_aggrs2<-calc_distance_change_betti_paral(X = t3orus470_list1[31:100], maxdim = 3, maxscale = 9, samples = 5, 
                                                                           ph_func = weighted_homology, l_rate=0.5, eta=4.0) )   
  
  save2RData(t3orus470_list1_31to100_wvr_aggrs2_time)
  save2RData(t3orus470_list1_31to100_wvrH2_aggrs2)
  
}

#-------------------------------------
#460点3次元トーラスで2次ベッチ数の成功率を求めてみる----
#r = 2, R1 = 8, R2 = 4
#全データセット数30、サブサンプル数5
#eta=4.0, ランドマーク点50%
t3orus460_list1_1to30_wvr_aggrs2_time<-system.time(
  t3orus460_list1_1to30_wvrH2_aggrs2<-calc_distance_change_betti_paral(X = t3orus460_list1[1:30], maxdim = 3, maxscale = 9, samples = 5, 
                                                                       ph_func = weighted_homology, l_rate=0.5, eta=4.0) )   

{
  t3orus460_list1_31to100_wvr_aggrs2_time<-system.time(
    t3orus460_list1_31to100_wvrH2_aggrs2<-calc_distance_change_betti_paral(X = t3orus460_list1[31:100], maxdim = 3, maxscale = 9, samples = 5, 
                                                                           ph_func = weighted_homology, l_rate=0.5, eta=4.0) )   
  
  save2RData(t3orus460_list1_31to100_wvr_aggrs2_time)
  save2RData(t3orus460_list1_31to100_wvrH2_aggrs2)
  
}
