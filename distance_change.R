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

trs1_dist3<-trs1_dist-0.5
trs1_dist3[trs1_dist3 < 0]<-0

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
trs1_filt3<-ripsFiltration(X = trs1_dist3, maxdimension = 2, maxscale = 3, dist = "arbitrary", library = "Dionysus", 
                           printProgress = T)

trs1_filt3_pd<-filtrationDiag(filtration = trs1_filt2, maxdimension = 2, library = "Dionysus", printProgress = T)

trs1_filt3_pl<-calc_landscape(trs1_filt3_pd, maxscale = 3)

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

cell_idx<-1:length(trs1_cell)
for (n in cell_idx) {
  
  if(length(trs1_cell[[n]])>1  && any(trs1_cell[[n]][-1]) %in% cell_idx){
    
    cell_idx<-cell_idx[ -(which(cell_idx == trs1_cell[[n]][-1])) ]
    
  }
  
  debugText(n)
  debugText(cell_idx)
  
}

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