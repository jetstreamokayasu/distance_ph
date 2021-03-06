#距離行列を正規化(最大値1、最小値0)してPHを計算してみる----
#trs300_1_10_distを使う

#オリジナルのPD
trs300_1_10_pd_time<-system.time(trs300_1_10_pd<-calculate_homology(mat = trs300_1_10_dist, dim = 2, threshold = 3, format = "distmat"))
trs300_1_10_pdB<-pd_conv_stats2tda(trs300_1_10_pd)
trs300_1_10_pl<-calc_landscape(diag = trs300_1_10_pd, maxscale = 3)
plot_landscape(trs300_1_120_pl, 2, ylim = c(0, 0.3))

#距離行列を[0, 1]に正規化。PH計算
trs300_1_10_dist5<-trs300_1_10_dist/max(trs300_1_10_dist)
trs300_1_10_pd2<-calculate_homology(mat = trs300_1_10_dist5, dim = 2, threshold = 3, format = "distmat")
trs300_1_10_pl2<-calc_landscape(diag = trs300_1_10_pd2, maxscale = 1)

plot_landscape(trs300_1_10_pl2, 2, xlim = c(0, 0.5), ylim = c(0, 0.025))

#------------------------
#正規化後の距離行列において、ランドマーク点の距離を変化させる----
#1-exp(r_ij^2)。multiresolution PHを参考に
#インデックスとしてland10F使用
trs300_1_10_dist5B<-trs300_1_10_dist5
trs300_1_10_dist5B[land10F[1], ]<-1-exp(-(trs300_1_10_dist5[land10F[1], ]^2))

trs300_1_10_dist5_odr<-order(trs300_1_10_dist5[land10F[1], ])

for (i in 1:length(land10F)) {
  
  trs300_1_10_dist5B[land10F[i], ]<-1-exp(-(trs300_1_10_dist5[land10F[i], ]^2))
  trs300_1_10_dist5B[, land10F[i]]<-1-exp(-(trs300_1_10_dist5[, land10F[i]]^2))
  
}

trs300_1_10_dist5B[trs300_1_10_dist5B < 0]<-0
trs300_1_10_pd3_time<-system.time( trs300_1_10_pd3<-calculate_homology(mat = trs300_1_10_dist5B, dim = 2, threshold = 1, format = "distmat") )
trs300_1_10_pl3<-calc_landscape(diag = trs300_1_10_pd3, maxscale = 1)
plot_landscape(land = trs300_1_10_pl3, dim = 2, xlim = c(0, 0.5), ylim = c(0, 0.02))

#----------------------------------------------------------------
#正規化後の距離行列において、ランドマーク点の距離を変化させる----
#1-exp(r_ij^2)。multiresolution PHを参考に
#インデックスとしてland10G使用
trs300_1_10_dist5C<-trs300_1_10_dist5
land10G<-landmark_points(X = torus300_colle_set[[1]][[10]][["noizyX"]], n_land = 30)

for (i in 1:length(land10G)) {
  
  trs300_1_10_dist5C[land10G[i], ]<-1-exp(-(trs300_1_10_dist5[land10G[i], ]^2))
  trs300_1_10_dist5C[, land10G[i]]<-1-exp(-(trs300_1_10_dist5[, land10G[i]]^2))
  
}

trs300_1_10_dist5C[trs300_1_10_dist5C < 0]<-0
trs300_1_10_pd4<-calculate_homology(mat = trs300_1_10_dist5C, dim = 2, threshold = 1, format = "distmat")
trs300_1_10_pl4<-calcLandscape(diag = trs300_1_10_pd4, maxscale = 1)
plot_landscape(trs300_1_10_pl4, dim = 2, xlim = c(0, 0.4), ylim = c(0, 0.02))

#--------------------------------------
#正規化後の距離行列において、ランドマーク点の距離を変化させる----
#2回目
#1-exp(r_ij^2/0.4)。multiresolution PHを参考に
#インデックスとしてland10H使用
trs300_1_10_dist5D<-trs300_1_10_dist5
land10H<-landmark_points(X = torus300_colle_set[[1]][[10]][["noizyX"]], n_land = 30)

for (i in 1:length(land10H)) {
  
  trs300_1_10_dist5D[land10H[i], ]<-1-exp(-(trs300_1_10_dist5[land10H[i], ]^2)/0.3)
  trs300_1_10_dist5D[, land10H[i]]<-1-exp(-(trs300_1_10_dist5[, land10H[i]]^2)/0.3)
  
}

trs300_1_10_dist5D[trs300_1_10_dist5D < 0]<-0
trs300_1_10_pd5_time<-system.time(trs300_1_10_pd5<-calculate_homology(mat = trs300_1_10_dist5D, dim = 2, threshold = 1, format = "distmat"))
trs300_1_10_pl5<-calc_landscape(diag = trs300_1_10_pd5, maxscale = 1)
plot_landscape(trs300_1_10_pl5, 2, xlim = c(0, 0.5), ylim = c(0, 0.025))

plot_landscape(trs300_1_10_pl5, 1, ylim = c(0, 0.3))


#-----------------------------------------------------------------
#正規化後の距離行列において、ランドマーク点の距離を変化させる-----
#正規化後の距離を2乗する
#インデックスとしてland10G使用
trs300_1_10_dist5E<-trs300_1_10_dist/max(trs300_1_10_dist) 
trs300_1_10_dist5E<-trs300_1_10_dist5E^2

trs300_1_10_pd6_time<-system.time(trs300_1_10_pd6<-calculate_homology(mat = trs300_1_10_dist5E, dim = 2, threshold = 1, format = "distmat"))
trs300_1_10_pl6<-calc_landscape(diag = trs300_1_10_pd6, maxscale = 1)
plot_landscape(trs300_1_10_pl6, 2, ylim = c(0, 0.01))
plot_landscape(trs300_1_10_pl6, 1, ylim = c(0, 0.1))



#------------------------------------------------
#スタンフォード・バニーでポリゴン描画の参考に----
bny <- vcgPlyRead("./data/bun_zipper.ply")



#-------------------------------------
#500点3次元トーラスで同様に試す-------

t3_land1<-landmark_points(X = t3orus3, n_land = nrow(t3orus3)*0.5)

t3orus3_dist<-dist(t3orus3) %>% as.matrix()

t3orus3_distB<-t3orus3_dist/max(t3orus3_dist)

for (i in 1:length(t3_land1)) {
  
  t3orus3_distB[t3_land1[i], ]<-1-exp(-(t3orus3_distB[t3_land1[i], ]^2))
  t3orus3_distB[, t3_land1[i]]<-1-exp(-(t3orus3_distB[, t3_land1[i]]^2))
  
}

t3orus3_dpd8<-calculate_homology(mat = t3orus3_distB, dim = 3, threshold = 1, format = "distmat")
t3orus3_dpl8<-calcLandscape(diag = t3orus3_dpd8, maxscale = 1)

plot(seq(0, 1, length=1000), t3orus3_dpl8[["3-land"]], type = "l", col=4, xlim = c(0, 0.1))
abline(h=t3orus3_dpl8[["thresh"]]*(2*pi)/surface_nshpere(3))

plot(seq(0, 1, length=1000), t3orus3_dpl8[["2-land"]], type = "l", col=3, xlim = c(0, 0.1))
abline(h=t3orus3_dpl8[["thresh"]]/2)


#500点3次元トーラスで同様に試す
#2回目
t3_land2<-landmark_points(X = t3orus3, n_land = nrow(t3orus3)*0.5)
t3orus3_distC<-t3orus3_dist/max(t3orus3_dist)

for (i in 1:length(t3_land2)) {
  
  t3orus3_distC[t3_land2[i], ]<-1-exp(-(t3orus3_distC[t3_land2[i], ]^2)/0.4)
  t3orus3_distC[, t3_land2[i]]<-1-exp(-(t3orus3_distC[, t3_land2[i]]^2)/0.4)
  
}

t3orus3_dpd9<-calculate_homology(mat = t3orus3_distC, dim = 3, threshold = 1, format = "distmat")
t3orus3_dpl9<-calcLandscape(diag = t3orus3_dpd9, maxscale = 1)

plot(t3orus3_dpl9[["tseq"]], t3orus3_dpl9[["3-land"]], type = "l", col=4, xlim = c(0, 0.2))
abline(h=t3orus3_dpl9[["thresh"]]*(2*pi)/surface_nshpere(3))

plot(t3orus3_dpl9[["tseq"]], t3orus3_dpl9[["2-land"]], type = "l", col=3, xlim = c(0, 0.18))
abline(h=t3orus3_dpl9[["thresh"]]/2)

#正規化した距離行列
t3orus3_distD<-t3orus3_dist/max(t3orus3_dist)



#--------------------------------------------
#500点3次元トーラスで同様に試す
#3回目

#500点3次元トーラス、ランドマーク点、距離行列
t3orus4<-x3Dtorus_unif(n = 500, r = 2, R1 = 8, R2 = 4)
t4_land1<-landmark_points(X = t3orus4, n_land = nrow(t3orus4)*0.5)
t3orus4_dist<-dist(t3orus4) %>% as.matrix()

#距離行列操作なし
t3orus4_time0<-system.time(t3orus4_dpd0<-calculate_homology(mat = t3orus4_dist, dim = 3, format = "distmat"))
t3orus4_dpl0<-calcLandscape(diag = t3orus4_dpd0, maxscale = 9)
plot_landscape(land = t3orus4_dpl0, dim = 3, xlim = c(0, 9))
plot_landscape(land = t3orus4_dpl0, dim = 2, xlim = c(0, 9), ylim = c(0, 2.0))
plot_landscape(land = t3orus4_dpl0, dim = 1, xlim = c(0, 9))
calc.landscape.peak(X = t3orus4_dpl0[["1-land"]], dimension = 1, thresh = t3orus4_dpl0[["thresh"]], tseq = t3orus4_dpl0[["tseq"]], show = T)

#500点3次元トーラスで同様に試す
#3回目
t3orus4_distA<-t3orus4_dist/max(t3orus4_dist)

for (i in t4_land1) {
  
  t3orus4_distA[i, ]<-1-exp(-(t3orus4_dist[i, ]/10)^2)
  t3orus4_distA[, i]<-1-exp(-(t3orus4_dist[, i]/10)^2)
  
}

t3orus4_distA[t3orus4_distA < 0]<-0

t3orus4_time1<-system.time(t3orus4_dpd1<-calculate_homology(mat = t3orus4_distA, dim = 3, threshold = 1, format = "distmat"))
t3orus4_dpl1<-calcLandscape(diag = t3orus4_dpd1, maxscale = 1)

plot(t3orus4_dpl1[["tseq"]], t3orus4_dpl1[["3-land"]], type = "l", col=4, xlim = c(0, 0.6), ylim = c(0, 0.03))
abline(h=t3orus4_dpl1[["thresh"]]*(2*pi)/surface_nshpere(3))

plot(t3orus4_dpl1[["tseq"]], t3orus4_dpl1[["2-land"]], type = "l", col=3, xlim = c(0, 0.6))
abline(h=t3orus4_dpl1[["thresh"]]/2)

#500点3次元トーラスで同様に試す
#4回目

t3orus4_dist<-dist(t3orus4) %>% as.matrix()
t3orus4_distB<-t3orus4_dist/max(t3orus4_dist)

for (i in t4_land1) {
  
  debugText(i)
  
  t3orus4_distB[i, ]<-1-exp(-(t3orus4_dist[i, ]/5)^2)
  t3orus4_distB[, i]<-1-exp(-(t3orus4_dist[, i]/5)^2)
  
}

t3orus4_distB[t3orus4_distB < 0]<-0

t3orus4_dpd2<-calculate_homology(mat = t3orus4_distB, dim = 3, threshold = 1, format = "distmat")
t3orus4_dpl2<-calcLandscape(diag = t3orus4_dpd2, maxscale = 1)

plot(t3orus4_dpl2[["tseq"]], t3orus4_dpl2[["3-land"]], type = "l", col=4, ylim = c(0, 0.1))
abline(h=t3orus4_dpl2[["thresh"]]*(2*pi)/surface_nshpere(3))

plot(t3orus4_dpl1[["tseq"]], t3orus4_dpl1[["2-land"]], type = "l", col=3, ylim = c(0, 0.03))
abline(h=t3orus4_dpl1[["thresh"]]/2)
#---------------
#500点3次元トーラスで試す。1-exp(r_ij^2)を元の距離で代入5回目-----
#1-exp(r_ij^2)を元の距離で代入
#5回目

t3orus4_dist<-dist(t3orus4) %>% as.matrix()
t3orus4_distC<-t3orus4_dist/max(t3orus4_dist)

for (i in t4_land1) {
  
  t3orus4_distC[i, ]<-1-exp(-(t3orus4_dist[i, ]/3)^2)
  t3orus4_distC[, i]<-1-exp(-(t3orus4_dist[, i]/3)^2)
  
}

t3orus4_distC[t3orus4_distC < 0]<-0

t3orus4_time3<-system.time(t3orus4_dpd3<-calculate_homology(mat = t3orus4_distC, dim = 3, threshold = 1, format = "distmat"))
t3orus4_dpl3<-calcLandscape(diag = t3orus4_dpd3, maxscale = 1)

plot_landscape(t3orus4_dpl3, 3, xlim = c(0, 1), ylim = c(0, 0.02))

plot_landscape(t3orus4_dpl3, 2, xlim = c(0, 1), ylim = c(0, 0.1))

#-------------------
#500点3次元トーラスで試す。1-exp(r_ij^2)を元の距離で代入6回目-----


t3orus4_dist<-dist(t3orus4) %>% as.matrix()
t3orus4_distD<-t3orus4_dist/max(t3orus4_dist)

for (i in t4_land1) {
  
  t3orus4_distD[i, ]<-1-exp(-(t3orus4_dist[i, ]/4)^2)
  t3orus4_distD[, i]<-1-exp(-(t3orus4_dist[, i]/4)^2)
  
}

t3orus4_distD[t3orus4_distD < 0]<-0

t3orus_time6<-system.time(t3orus4_dpd6<-calculate_homology(mat = t3orus4_distD, dim = 3, threshold = 1, format = "distmat"))
t3orus4_dpl6<-calc_landscape(diag = t3orus4_dpd6, maxscale = 1)

plot_landscape(land = t3orus4_dpl6, dim = 3, ylim = c(0, 0.05))
plot_landscape(land = t3orus4_dpl6, dim = 2, ylim = c(0, 0.1))
calc.landscape.peak(X = t3orus4_dpl6[["2-land"]], dimension = 2, thresh = t3orus4_dpl6[["thresh"]]/2, tseq = t3orus4_dpl6[["tseq"]], show = T)

#------------------
#500点3次元トーラスで試す。1-exp(r_ij^2)を元の距離で代入7回目------
#1-exp(r_ij^2)を元の距離で代入
#7回目
#ランドマーク点を逆にする

t4_land1B<-1:nrow(t3orus4) %>% .[-t4_land1]

t3orus4_dist<-dist(t3orus4) %>% as.matrix()
t3orus4_distE<-t3orus4_dist/max(t3orus4_dist)

for (i in t4_land1B) {
  
  t3orus4_distE[i, ]<-1-exp(-(t3orus4_dist[i, ]/4)^2)
  t3orus4_distE[, i]<-1-exp(-(t3orus4_dist[, i]/4)^2)
  
}

t3orus4_distE[t3orus4_distE < 0]<-0

t3orus_time7<-system.time(t3orus4_dpd7<-calculate_homology(mat = t3orus4_distE, dim = 3, threshold = 1, format = "distmat"))
t3orus4_dpl7<-calc_landscape(diag = t3orus4_dpd7, maxscale = 1)
plot_landscape(land = t3orus4_dpl7, dim = 3, ylim = c(0, 0.05))
plot_landscape(land = t3orus4_dpl7, dim = 2, ylim = c(0, 0.1))
calc.landscape.peak(X = t3orus4_dpl7[["3-land"]], dimension = 3, thresh = t3orus4_dpl7[["thresh"]]*(2*pi)/surface_nshpere(3), tseq = t3orus4_dpl7[["tseq"]], show = T)

#-------------------------------------
#500点3次元トーラスで試す。1-exp(r_ij^2)を元の距離で代入8回目-------
#1-exp(r_ij^2)を元の距離で代入
#8回目

t3orus4_dist<-dist(t3orus4) %>% as.matrix()
t3orus4_distG<-dist_fri_change(t3orus4_dist, t4_land1, eta=1)

t3orus_time8<-system.time(t3orus4_dpd8<-calculate_homology(mat = t3orus4_distG, dim = 3, threshold = 1, format = "distmat"))
t3orus4_dpl8A<-calc_landscape(diag = t3orus4_dpd8A, maxscale = 1)
t3orus4_dpd8A<-t3orus4_dpd8  
#ランドスケープ計算で値が小さくなりすぎたので10倍した
t3orus4_dpd8A[, 3]<-t3orus4_dpd8A[, 3]*10
t3orus4_dpd8A[, 2]<-t3orus4_dpd8A[, 2]*10
plot_landscape(t3orus4_dpl8A, 3, ylim = c(0, 0.01), xlim = c(0, 0.1))
  
#-------------------------------
#500点3次元トーラスt3orus4で試す
#正規化後のPDを求める
t3orus4_time0A<-system.time(t3orus4_dpd0A<-calculate_homology(mat = t3orus4_dist/max(t3orus4_dist), dim = 3, format = "distmat"))
t3orus4_dpl0A<-calc_landscape(diag = t3orus4_dpd0A, maxscale = 1)
plot_landscape(land = t3orus4_dpl0A, dim = 3, ylim = c(0, 0.05))
plot_landscape(land = t3orus4_dpl0A, dim = 2, ylim = c(0, 0.1))

#----------------------------------------------
#アニュラスデータでMPHを試す------
anu_land1<-landmark_points(X = anu, n_land = 10)

anu_distB<-anu_dist/max(anu_dist)

for (i in anu_land1) {
  
  anu_distB[i, ]<-1-exp(-(anu_dist[i, ]^2)/5)
  anu_distB[, i]<-1-exp(-(anu_dist[, i]^2)/5)
  
}

anu_pd<-calculate_homology(mat = anu_dist, dim = 1, threshold = 2, format = "distmat")
anu_pl<-calcLandscape(diag = anu_pd, maxscale = 2)

anu_mpd1<-calculate_homology(mat = anu_distB, dim = 1, threshold = 1, format = "distmat")
anu_mpl1<-calcLandscape(diag = anu_mpd1, maxscale = 1)
plot(anu_mpl1[["tseq"]], anu_mpl1[["1-land"]], type = "l", col=2)
abline(h=anu_mpl1[["thresh"]])



