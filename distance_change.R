#距離関数を変化させてPHを計算する
#一律に距離を縮めるか、密度の薄いところだけ他の点との距離を縮めるか

torus1<-torusUnif(300, 1, 2.5)
figurePlot3d(torus1)

trs1_pd<-ripsDiag(X = torus1, maxdimension = 2, maxscale = 3, printProgress = T)
trs1_pl<-calc_landscape(diag = trs1_pd, maxscale = 3)

#---------------------------------------------
#全点の近傍10点の距離を調べる

vics_dist<-t(apply(as.matrix(dist(torus1)), 1, function(k){
  
  vic_d<-sort(k) %>% .[2:(10+1)]
  
  return(vic_d)
  
}))

#---------------------------------------------
#距離関数から一律で一定値減らす
trs1_dist<-as.matrix(dist(torus1))

#一律減少0.5
trs1_dist3<-trs1_dist-0.5
trs1_dist3[trs1_dist3 < 0]<-0

#一律減少0.4
trs1_dist3B<-trs1_dist-0.4
trs1_dist3B[trs1_dist3B < 0]<-0

#---------------------------------------------
#距離行列を使ってフィルトレーション計算

#オリジナル
trs1_filt<-ripsFiltration(X = trs1_dist, maxdimension = 2, maxscale = 4, dist = "arbitrary", library = "Dionysus",
                          printProgress = T)

trs1_filt_pd<-filtrationDiag(filtration = trs1_filt, maxdimension = 2, library = "Dionysus", printProgress = T)

trs1_filt_pl<-calc_landscape(diag = trs1_filt_pd, maxscale = 3)

#最近傍点との距離が閾値以上の点に関する距離だけ減らした
trs1_filt2<-ripsFiltration(X = trs1_dist2, maxdimension = 2, maxscale = 3, dist = "arbitrary", library = "Dionysus", 
                           printProgress = T)

trs1_filt2_pd<-filtrationDiag(filtration = trs1_filt2, maxdimension = 2, library = "Dionysus", printProgress = T)

trs1_filt2_pl<-calc_landscape(trs1_filt2_pd, maxscale = 3)

#一律に減らした場合
#一律減少0.5
trs1_filt3<-ripsFiltration(X = trs1_dist3, maxdimension = 2, maxscale = 3, dist = "arbitrary", library = "Dionysus", 
                           printProgress = T)

trs1_filt3_pd<-filtrationDiag(filtration = trs1_filt3, maxdimension = 2, library = "Dionysus", printProgress = T)

trs1_filt3_pl<-calc_landscape(trs1_filt3_pd, maxscale = 3)

#一律減少0.4
trs1_filt3B<-ripsFiltration(X = trs1_dist3B, maxdimension = 2, maxscale = 3, dist = "arbitrary", library = "Dionysus", 
                           printProgress = T)

trs1_filt3B_pd<-filtrationDiag(filtration = trs1_filt3B, maxdimension = 2, library = "Dionysus", printProgress = T)

trs1_filt3B_pl<-calc_landscape(trs1_filt3B_pd, maxscale = 3)

#---------------------------------------------
#一部の点だけ距離を縮める
l<-0.4

trs1_dist2<-trs1_dist

diag(trs1_dist2)<-Inf
trs1_dist2_mins<-apply(trs1_dist2, 1, min)
diag(trs1_dist2)<-0

mins_idx<-which(trs1_dist2_mins > l)

trs1_dist2[mins_idx, ]<-trs1_dist2[mins_idx, ]-l
trs1_dist2[-mins_idx, mins_idx]<-trs1_dist2[-mins_idx, mins_idx]-l
diag(trs1_dist2)<-0

#-------------------------------------------------
#閾値以下に存在する点を減らしてみる
trs1_cell<-cell_set2(torus1, 0.4)
trs1_cnct<-connect2(1, trs1_cell, all = 1:nrow(torus1))
trs1_red<-reduce_points(torus1, trs1_cnct)

trs1_red_dist<-as.matrix(dist(trs1_red))

trs1_red_dist2<-trs1_red_dist - 0.4
trs1_red_dist2[trs1_red_dist2 < 0]<-0
diag(trs1_red_dist2)<-0

trs1_red_filt<-ripsFiltration(X = trs1_red_dist2, maxdimension = 2, maxscale = 3, dist = "arbitrary", library = "Dionysus", 
                           printProgress = T)

trs1r_filt_pd<-filtrationDiag(filtration = trs1_red_filt, maxdimension = 2, library = "Dionysus", printProgress = T)

trs1r_filt_pl<-calc_landscape(trs1r_filt_pd, maxscale = 3)

#---------------------------------------------
##セルに含まれるインデックスをトーラス全体のインデックスから消そうとした
cell_idx<-1:length(trs1_cell)
for (n in cell_idx) {
  
  if(length(trs1_cell[[n]])>1  && any(trs1_cell[[n]][-1] %in% cell_idx)){
    
    cell_idx<-cell_idx[ -(which(cell_idx == trs1_cell[[n]][-1])) ]
    
  }
  
  debugText(n)
  debugText(cell_idx)
  
}

#-----------------------------------------------------------------
#セルの平均に最も近い点を残したのち、距離関数を変更
#その後フィルトレーション計算

trs1_cnct2<-cell_cnct(1, trs1_cell)
#trs1_cnct2A<-connect2(1, cell_p = trs1_cnct2, all = 1:length(trs1_cnct2))
trs1_red2<-reduce_points(torus1, trs1_cnct2)

trs1_red2_dist<-as.matrix(dist(trs1_red2))

trs1_red2_dist2<-trs1_red2_dist - 0.4
diag(trs1_red2_dist2)<-0

trs1_red2_filt<-ripsFiltration(X = trs1_red2_dist2, maxdimension = 2, maxscale = 3, dist = "arbitrary", library = "Dionysus", 
                              printProgress = T)

trs1r2_filt_pd<-filtrationDiag(filtration = trs1_red2_filt, maxdimension = 2, library = "Dionysus", printProgress = T)

trs1r2_filt_pl<-calc_landscape(trs1r2_filt_pd, maxscale = 3)

#reduce_point関数で残った点だけ距離を減らす
trs_remn_idx<-1:nrow(torus1) %>% .[-trs1_red2[[2]]]

trs1_dist4<-trs1_dist
trs1_dist4[trs_remn_idx, ]<-trs1_dist4[trs_remn_idx, ]-0.4
trs1_dist4[-trs_remn_idx, trs_remn_idx]<-trs1_dist2[-trs_remn_idx, trs_remn_idx]-0.4
trs1_dist4[trs1_dist4 < 0]<-0

trs1_filt4<-ripsFiltration(X = trs1_dist4, maxdimension = 2, maxscale = 3, dist = "arbitrary", library = "Dionysus", 
                               printProgress = T)

trs1_filt4_pd<-filtrationDiag(filtration = trs1_filt4, maxdimension = 2, library = "Dionysus", printProgress = T)

trs1_filt4_pl<-calc_landscape(trs1_filt4_pd, maxscale = 3)

#---------------------------------------------
#試しにマイナスになった距離を絶対値にしてみる

#一律減少
trs1_dist5<-trs1_dist-0.5
trs1_dist5[trs1_dist3 < 0]<-abs(trs1_dist5[trs1_dist3 < 0])
diag(trs1_dist5)<-0

trs1_filt5<-ripsFiltration(X = trs1_dist5, maxdimension = 2, maxscale = 3, dist = "arbitrary", library = "Dionysus", 
                           printProgress = T)

trs1_filt5_pd<-filtrationDiag(filtration = trs1_filt4, maxdimension = 2, library = "Dionysus", printProgress = T)

trs1_filt5_pl<-calc_landscape(trs1_filt5_pd, maxscale = 3)

#reduce_point関数で残った点だけ距離を減らす
trs_remn_idx<-1:nrow(torus1) %>% .[-trs1_red2[[2]]]

trs1_dist6<-trs1_dist
trs1_dist6[trs_remn_idx, ]<-trs1_dist6[trs_remn_idx, ]-0.4
trs1_dist6[-trs_remn_idx, trs_remn_idx]<-trs1_dist6[-trs_remn_idx, trs_remn_idx]-0.4
trs1_dist6[trs1_dist4 < 0]<-abs(trs1_dist6[trs1_dist6 < 0])
diag(trs1_dist6)<-0

trs1_filt6<-ripsFiltration(X = trs1_dist6, maxdimension = 2, maxscale = 3, dist = "arbitrary", library = "Dionysus", 
                           printProgress = T)

trs1_filt6_pd<-filtrationDiag(filtration = trs1_filt6, maxdimension = 2, library = "Dionysus", printProgress = T)

trs1_filt6_pl<-calc_landscape(trs1_filt6_pd, maxscale = 3)

#-------------------------------------------------------------------------------
#reduce_point関数で残った点だけ距離を減らす
#距離の閾値をいろいろと変更してみる

#閾値0.4で点数削減、残った点から距離行列で0.4減少
l<-0.4
trs1_cell2<-cell_set2(torus1, l)
trs1_cnct2<-connect2(1, trs1_cell2, all = 1:nrow(torus1))
trs1_red2<-reduce_points(torus1, trs1_cnct2)

trs_remn_idx2<-1:nrow(torus1) %>% .[-trs1_red2[[2]]]

trs1_dist7<-trs1_dist
trs1_dist7[trs_remn_idx2, ]<-trs1_dist7[trs_remn_idx2, ]-l
trs1_dist7[-trs_remn_idx2, trs_remn_idx2]<-trs1_dist7[-trs_remn_idx2, trs_remn_idx2]-l
trs1_dist7[trs1_dist7 < 0]<-0

trs1_filt7<-ripsFiltration(X = trs1_dist7, maxdimension = 2, maxscale = 3, dist = "arbitrary", library = "Dionysus", 
                           printProgress = T)

trs1_filt7_pd<-filtrationDiag(filtration = trs1_filt7, maxdimension = 2, library = "Dionysus", printProgress = T)

trs1_filt7_pl<-calc_landscape(trs1_filt7_pd, maxscale = 3)

##プロット
figurePlot3d(torus1)
spheres3d(torus1[trs_remn_idx2, ], radius = 0.4, col="pink")
rgl.snapshot("./data/torus_sphe3.png")   

#点の減らし方を変える
trs1_cnct2B<-cell_cnct(1, trs1_cell2)
trs1_red2B<-reduce_points(torus1, trs1_cnct2B)

trs_remn_idx2B<-1:nrow(torus1) %>% .[-trs1_red2B[[2]]]

trs1_dist7B<-trs1_dist
trs1_dist7B[trs_remn_idx2B, ]<-trs1_dist7[trs_remn_idx2B, ]-l
trs1_dist7B[-trs_remn_idx2B, trs_remn_idx2B]<-trs1_dist7B[-trs_remn_idx2B, trs_remn_idx2B]-l
trs1_dist7B[trs1_dist7B < 0]<-0

trs1_filt7B<-ripsFiltration(X = trs1_dist7B, maxdimension = 2, maxscale = 3, dist = "arbitrary", library = "Dionysus", 
                           printProgress = T)

trs1_filt7B_pd<-filtrationDiag(filtration = trs1_filt7B, maxdimension = 2, library = "Dionysus", printProgress = T)

trs1_filt7B_pl<-calc_landscape(trs1_filt7B_pd, maxscale = 3)

#閾値0.5で点数削減、残った点から距離行列で0.5減少

l<-0.5
trs1_cell3<-cell_set2(torus1, l)
trs1_cnct3<-connect2(1, trs1_cell3, all = 1:nrow(torus1))
trs1_red3<-reduce_points(torus1, trs1_cnct3)

trs_remn_idx3<-1:nrow(torus1) %>% .[-trs1_red3[[2]]]

trs1_dist8<-trs1_dist
trs1_dist8[trs_remn_idx3, ]<-trs1_dist8[trs_remn_idx3, ]-l
trs1_dist8[-trs_remn_idx3, trs_remn_idx3]<-trs1_dist8[-trs_remn_idx3, trs_remn_idx3]-l
trs1_dist8[trs1_dist8 < 0]<-0

trs1_filt8<-ripsFiltration(X = trs1_dist8, maxdimension = 2, maxscale = 3, dist = "arbitrary", library = "Dionysus", 
                           printProgress = T)

trs1_filt8_pd<-filtrationDiag(filtration = trs1_filt8, maxdimension = 2, library = "Dionysus", printProgress = T)

trs1_filt8_pl<-calc_landscape(trs1_filt8_pd, maxscale = 3)

#閾値0.4で点数削減、残った点から距離行列で0.5減少
l<-0.4
th<-quantile_threshold(0.8, torus1)
trs1_cell3<-cell_set2(torus1, 0.5)
trs1_cnct3<-connect2(1, trs1_cell3, all = 1:nrow(torus1))
trs1_red3<-reduce_points(torus1, trs1_cnct3)

trs_remn_idx3<-1:nrow(torus1) %>% .[-trs1_red3[[2]]]

trs1_dist9<-trs1_dist
trs1_dist9[trs_remn_idx3, ]<-trs1_dist9[trs_remn_idx3, ]-l
trs1_dist9[-trs_remn_idx3, trs_remn_idx3]<-trs1_dist9[-trs_remn_idx3, trs_remn_idx3]-l
trs1_dist9[trs1_dist9 < 0]<-0

trs1_filt9<-ripsFiltration(X = trs1_dist9, maxdimension = 2, maxscale = 3, dist = "arbitrary", library = "Dionysus", 
                           printProgress = T)

trs1_filt9_pd<-filtrationDiag(filtration = trs1_filt9, maxdimension = 2, library = "Dionysus", printProgress = T)

trs1_filt9_pl<-calc_landscape(trs1_filt9_pd, maxscale = 3)


#サブサンプルでもできるか確認
trs1_sub<-torus1[sample(1:nrow(torus1), 0.8*nrow(torus1)), ]
th2<-quantile_threshold(x = 0.8, X = trs1_sub)

trs1_sub_dist<-dist(trs1_sub) %>% as.matrix()

trs1_sub_cell<-cell_set2(trs1_sub, th2)
trs1_sub_cnct<-connect2(1, trs1_sub_cell, all = 1:nrow(trs1_sub))
trs1_sub_red<-reduce_points(trs1_sub, trs1_sub_cnct)

trs1_sub_remn_idx<-1:nrow(trs1_sub) %>% .[-trs1_sub_red[[2]]]

trs1_sub_dist2<-trs1_sub_dist
trs1_sub_dist2[trs1_sub_remn_idx, ]<-trs1_sub_dist2[trs1_sub_remn_idx, ]-th2
trs1_sub_dist2[-trs1_sub_remn_idx, trs1_sub_remn_idx]<-trs1_sub_dist2[-trs1_sub_remn_idx, trs1_sub_remn_idx]-th2
trs1_sub_dist2[trs1_sub_dist2 < 0]<-0

trs1_sub_filt2<-ripsFiltration(X = trs1_sub_dist2, maxdimension = 2, maxscale = 3, dist = "arbitrary", library = "Dionysus", 
                           printProgress = T)

trs1_sub_filt2_pd<-filtrationDiag(filtration = trs1_sub_filt2, maxdimension = 2, library = "Dionysus", printProgress = T)

trs1_sub_filt2_pl<-calc_landscape(trs1_sub_filt2_pd, maxscale = 3)

##サブサンプルで距離行列変化なし
trs1_sub_filt<-ripsFiltration(X = trs1_sub_dist, maxdimension = 2, maxscale = 3, dist = "arbitrary", library = "Dionysus", 
                               printProgress = T)

trs1_sub_filt_pd<-filtrationDiag(filtration = trs1_sub_filt, maxdimension = 2, library = "Dionysus", printProgress = T)

trs1_sub_filt_pl<-calc_landscape(trs1_sub_filt_pd, maxscale = 3)
