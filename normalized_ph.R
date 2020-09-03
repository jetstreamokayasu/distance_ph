#距離行列を正規化(最大値1、最小値0)してPHを計算してみる
#trs300_1_10_distを使う

#オリジナルのPD
trs300_1_10_pd<-calculate_homology(mat = trs300_1_10_dist, dim = 2, threshold = 3, format = "distmat")
trs300_1_10_pdB<-pd_conv_stats2tda(trs300_1_10_pd)

#距離行列を[0, 1]に正規化。PH計算
trs300_1_10_dist5<-trs300_1_10_dist/max(trs300_1_10_dist)
trs300_1_10_pd2<-calculate_homology(mat = trs300_1_10_dist5, dim = 2, threshold = 3, format = "distmat")

#正規化後の距離行列において、ランドマーク点の距離を変化させる
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
trs300_1_10_pd3<-calculate_homology(mat = trs300_1_10_dist5B, dim = 2, threshold = 1, format = "distmat")
trs300_1_10_pl3<-calcLandscape(diag = trs300_1_10_pd3, maxscale = 1)

#-------------------------------------
#正規化後の距離行列において、ランドマーク点の距離を変化させる
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
plot(seq(0, 1, length=1000), trs300_1_10_pl4[["2-land"]], type = "l", col=3, xlim = c(0, 0.4))
abline(h=trs300_1_10_pl4[["thresh"]]/2)

#-----------------------------------------
#multiresolution PHの概念図
#torus300_colle_set[[1]][[10]][["noizyX"]]の1番目の点を使う
#land10Gの18番目のインデックス

trs300_1_10_arnd1<-torus300_colle_set[[1]][[10]][["noizyX"]][-1, ] - torus300_colle_set[[1]][[10]][["noizyX"]][1, ]
trs300_1_10_arnd1<-trs300_1_10_arnd1 * ((trs300_1_10_dist5C[1, -1]*max(trs300_1_10_dist))/trs300_1_10_dist[1, -1]) + torus300_colle_set[[1]][[10]][["noizyX"]][1, ]

#------------------------------
#スタンフォード・バニーでポリゴン描画の参考に
bny <- vcgPlyRead("./data/bun_zipper.ply")

#-------------------------------------
#500点3次元トーラスで同様に試す

t3_land1<-landmark_points(X = t3orus3, n_land = nrow(t3orus3)*0.5)

t3orus3_dist<-dist(t3orus3) %>% as.matrix()

t3orus3_distB<-t3orus3_dist/max(t3orus3_dist)

for (i in 1:length(t3_land1)) {
  
  t3orus3_distB[t3_land1[i], ]<-1-exp(-(t3orus3_distB[t3_land1[i], ]^2))
  t3orus3_distB[, t3_land1[i]]<-1-exp(-(t3orus3_distB[, t3_land1[i]]^2))
  
}

t3orus3_dpd8<-calculate_homology(mat = t3orus3_distB, dim = 3, threshold = 1, format = "distmat")
t3orus3_dpl8<-calcLandscape(diag = t3orus3_dpd8, maxscale = 1)

plot(seq(0, 1, length=1000), t3orus3_dpl8[["3-land"]], type = "l", col=3)
abline(h=t3orus3_dpl8[["thresh"]]*(2*pi)/surface_nshpere(3))
