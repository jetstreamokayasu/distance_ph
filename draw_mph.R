#MPHのイメージ図をプロットする

#----------------------------------------------
#任意のデータ点を中心とした座標ベクトル表現を試す
#アニュラスデータで

anu<-anulusUnif(100)
anu_dist<-dist(anu) %>% as.matrix()
anu_1<-anu[-1, 1] - anu[1, 1]
anu_2<-anu[-1, 2] - anu[1, 2]

#点1から見た他の点の分布
anu_1A<-(1-exp(-(anu_dist[1, -1]^2)/5)) * anu_1/max(anu_dist) + anu[1, 1]
anu_2A<-(1-exp(-(anu_dist[1, -1]^2)/5)) * anu_2/max(anu_dist) + anu[1, 2]

#点1のポリゴンを形成する点群
anu_1B<-exp(-(anu_dist[1, -1]^2)/5) * anu_1/max(anu_dist) + anu[1, 1]
anu_2B<-exp(-(anu_dist[1, -1]^2)/5) * anu_2/max(anu_dist) + anu[1, 2]

anu_1a<-anu_1/max(anu_dist) + anu[1, 1]
anu_2a<-anu_2/max(anu_dist) + anu[1, 2]

plot(anu)
plot(anu_1a, anu_2a)
points(anu_1A, anu_2A, col=3, pch=16)
points(anu_1B, anu_2B, col=3, pch=16)
points(anu[1, 1], anu[1, 2], pch=16, col=2)
polygon(anu_1B, anu_2B, col="pink")

#-----------------------------------------
#multiresolution PHの概念図
#torus300_colle_set[[1]][[10]][["noizyX"]]の1番目の点を使う
#land10Gの18番目のインデックス

trs300_1_10_arnd1<-torus300_colle_set[[1]][[10]][["noizyX"]][-1, ] - torus300_colle_set[[1]][[10]][["noizyX"]][1, ]
trs300_1_10_arnd1<-trs300_1_10_arnd1 * ((trs300_1_10_dist5C[1, -1]*max(trs300_1_10_dist))/trs300_1_10_dist[1, -1]) + torus300_colle_set[[1]][[10]][["noizyX"]][1, ]

trs300_1_10_arnd1B<-torus300_colle_set[[1]][[10]][["noizyX"]][2:300, ] - torus300_colle_set[[1]][[10]][["noizyX"]][1, ]
trs300_1_10_arnd1B<-trs300_1_10_arnd1B/trs300_1_10_dist[1, -1] + torus300_colle_set[[1]][[10]][["noizyX"]][1, ]
#trs300_1_10_arnd1B<-trs300_1_10_arnd1B + torus300_colle_set[[1]][[10]][["noizyX"]][1, ]

trs300_1_10_arnd1C<-torus300_colle_set[[1]][[10]][["noizyX"]][2, ] - torus300_colle_set[[1]][[10]][["noizyX"]][1, ]
trs300_1_10_arnd1C<- trs300_1_10_arnd1C/trs300_1_10_dist[1, 2] + torus300_colle_set[[1]][[10]][["noizyX"]][1, ]

#---------------------------------------------------------
#距離行列変化後、変化した点の他の点からの相対位置を見る

anu2<-anulusUnif(30)

a2_land<-landmark_points(anu2, n_land = 5)

anu2_dist<-dist(anu2) %>% as.matrix()

#距離を正規化([0, 1]化)しプロット。点9を中心とする
#a2_land[5]=9

#点9からの正規化された方向ベクトル
anu2x<-(anu2[-9, 1] - anu2[9, 1])/max(anu2_dist)
anu2y<-(anu2[-9, 2] - anu2[9, 2])/max(anu2_dist)
plot(anu2x+anu2[9, 1], anu2y+anu2[9, 2])
text(x=anu2x+anu2[9, 1], y=anu2y+anu2[9, 2], (1:30)[-9])
points(x = anu2[9, 1], y = anu2[9, 2], pch=15, col=2)

#点9から見た他の点の分布
#sqrt(anu2x^2 + anu2y^2)で割って単位ベクトル化している
anu2x_A<-(1-exp(-(anu2_dist[9, -9]/3)^2)) * anu2x/sqrt(anu2x^2 + anu2y^2) + anu2[9, 1]
anu2y_A<-(1-exp(-(anu2_dist[9, -9]/3)^2)) * anu2y/sqrt(anu2x^2 + anu2y^2) + anu2[9, 2]
points(anu2x_A, anu2y_A, pch=16, col=4)

a1_arnd9_idx<-(1:30)[-9]

#正規化後の距離と、距離変更後の差
anu2_dist_fr<-anu2_dist[9, -9]/max(anu2_dist) - (1-exp(-(anu2_dist[9, -9]/3)^2))

#点9以外の点から正規化後の距離と距離変更後の差を半径とした円を描く
for (i in 1:length(anu2x)) {
  
  plot_circle(x = anu2x[i]+anu2[9, 1], y = anu2y[i]+anu2[9, 2], r = anu2_dist_fr[i])
  
}

# > anu2_dist[9, -9]/max(anu2_dist)
# 1          2          3          4          5          6          7          8         10 
# 0.45623637 0.40734400 0.38708479 0.46694201 0.26735361 0.26846401 0.23324507 0.26099422 0.06389799 
# 11         12         13         14         15         16         17         18         19 
# 0.14809908 0.14315630 0.14697505 0.28427487 0.29937331 0.30369842 0.39616945 0.49901599 0.67475049 
# 20         21         22         23         24         25         26         27         28 
# 0.86749682 0.86029357 0.59707500 0.71533483 0.64196659 0.59626718 0.56938644 0.62148013 0.48493167 
# 29         30 
# 0.62754936 0.56812733 

#-------------------------------------------
#MHPの適用後の計算を見る
x1<-c(1.2, 0)
x2<-c(0, 0.9)
x3<-c(-1.1, 0)
x4<-c(0, -1.3)
x5<-c(0.5, 0.4)
plot_circle(0, 0, 1)
plot(c(1.2, 0, -1.1, 0, 0.5), c(0, 0.9, 0, -1.3, 0.4), xlim = c(-1.5, 1.5), ylim = c(-1.5, 1.5))
text(c(1.2, 0, -1.1, 0, 0.5), c(0, 0.9, 0, -1.3, 0.4), labels = 1:5)
d_t<-cbind(c(1.2, 0, -1.1, 0, 0.5), c(0, 0.9, 0, -1.3, 0.4))
d_dist<-dist(d_t) %>% as.matrix()
d_dist_sorted<-sort(dist(d_t))

# > d_dist
# 1         2        3        4         5
# 1 0.0000000 1.5000000 2.300000 1.769181 0.8062258
# 2 1.5000000 0.0000000 1.421267 2.200000 0.7071068
# 3 2.3000000 1.4212670 0.000000 1.702939 1.6492423
# 4 1.7691806 2.2000000 1.702939 0.000000 1.7720045
# 5 0.8062258 0.7071068 1.649242 1.772005 0.0000000

d_t_pd<-calculate_homology(mat = d_t, dim = 1)
# >d_t_pd
# dimension    birth     death
# [1,]         0 0.000000 0.7071068
# [2,]         0 0.000000 0.8062258
# [3,]         0 0.000000 1.4212670
# [4,]         0 0.000000 1.7029386
# [5,]         1 1.769181 1.7720045

d_t_pd2<-calculate_homology(mat = d_dist, dim = 1, format = "distmat")

lines(c(x2[1], x5[1]), c(x2[2], x5[2]))
lines(c(x1[1], x5[1]), c(x1[2], x5[2]))
lines(c(x2[1], x3[1]), c(x2[2], x3[2]))
lines(c(x1[1], x2[1]), c(x1[2], x2[2]))
lines(c(x3[1], x5[1]), c(x3[2], x5[2]))
lines(c(x3[1], x4[1]), c(x3[2], x4[2]))
lines(c(x1[1], x4[1]), c(x1[2], x4[2]))#生成
lines(c(x4[1], x5[1]), c(x4[2], x5[2]))#消滅

#距離の正規化
d_t_x<-(d_t[-5, 1] - x5[1])/d_dist_sorted[10]
d_t_y<-(d_t[-5, 2] - x5[2])/d_dist_sorted[10]
plot(d_t_x+x5[1], d_t_y+x5[2], xlim = c(-1, 1), ylim = c(-1, 1))
points(x5[1], x5[2], pch=16, col=2)
text(c(d_t_x+x5[1], x5[1]), c(d_t_y+x5[2], x5[2]), labels = 1:5)

#FRI適用後の点５を中心とした分布
eta<-1.7#ハイパラ
d_t_xA<-(1-exp(-(d_dist[5, -5]/eta)^2)) * d_t_x/sqrt(d_t_x^2 + d_t_y^2) + x5[1]
d_t_yA<-(1-exp(-(d_dist[5, -5]/eta)^2)) * d_t_y/sqrt(d_t_x^2 + d_t_y^2) + x5[2]
points(d_t_xA, d_t_yA, col=4, pch=16)

d_distA<-d_dist/max(d_dist)
d_distA[5,]<-(1-exp(-(d_dist[5, ]/eta)^2))
d_distA[,5]<-(1-exp(-(d_dist[, 5]/eta)^2))
d_distA[4,]<-(1-exp(-(d_dist[4, ]/eta)^2))
d_distA[,4]<-(1-exp(-(d_dist[, 4]/eta)^2))
d_dist_normed_sorted<-sort(d_dist/max(d_dist))
d_distA_sorted<-sort(d_distA) %>% unique()
d_t_pd3<-calculate_homology(mat = d_dist/max(d_dist), dim = 1, format = "distmat")
d_t_pdA1<-calculate_homology(mat = d_distA, dim = 1, format = "distmat")

# > d_distA
# 1         2         3         4         5
# 1 0.0000000 0.6521739 1.0000000 0.6614370 0.2014147
# 2 0.6521739 0.0000000 0.6179422 0.8126432 0.1588711
# 3 1.0000000 0.6179422 0.0000000 0.6333913 0.6098315
# 4 0.6614370 0.8126432 0.6333913 0.0000000 0.6626064
# 5 0.2014147 0.1588711 0.6098315 0.6626064 0.0000000

lines(c(d_t_x[2]+x5[1], x5[1]), c(d_t_y[2]+x5[2], x5[2]))
lines(c(d_t_x[1]+x5[1], x5[1]), c(d_t_y[1]+x5[2], x5[2]))
lines(c(d_t_x[3]+x5[1], x5[1]), c(d_t_y[3]+x5[2], x5[2]))
lines(c(d_t_x[2]+x5[1], d_t_x[3]+x5[1]), c(d_t_y[2]+x5[2], d_t_y[3]+x5[2]))
lines(c(d_t_x[3]+x5[1], d_t_x[4]+x5[1]), c(d_t_y[3]+x5[2], d_t_y[4]+x5[2]))
lines(c(d_t_x[1]+x5[1], d_t_x[2]+x5[1]), c(d_t_y[1]+x5[2], d_t_y[2]+x5[2]))
lines(c(d_t_x[1]+x5[1], d_t_x[4]+x5[1]), c(d_t_y[1]+x5[2], d_t_y[4]+x5[2]))#生成
lines(c(d_t_x[4]+x5[1], x5[1]), c(d_t_y[4]+x5[2], x5[2]))#消滅
lines(c(d_t_x[2]+x5[1], d_t_x[4]+x5[1]), c(d_t_y[2]+x5[2], d_t_y[4]+x5[2]))
lines(c(d_t_x[1]+x5[1], d_t_x[3]+x5[1]), c(d_t_y[1]+x5[2], d_t_y[3]+x5[2]))

d_t_pd4<-ripsDiag(X = cbind(c(d_t_x+x5[1], x5[1]), c(d_t_y+x5[2], x5[2])), maxdimension = 1, maxscale = 1)

for (i in 1:length(d_t_x)) {
  
  plot_circle(x = d_t_x[i]+x5[1], y = d_t_y[i]+x5[2], r = d_dist[i, 5]/max(d_dist) - d_distA[i, 5])
  
}

for (i in 1:3) {
  
  plot_circle(x = d_t_x[i]+x5[1], y = d_t_y[i]+x5[2], r = d_dist[i, 4]/max(d_dist) - d_distA[i, 4], col = rgb(1, 0, 0, 0.6))
  
}

plot_circle(x = x5[1], y = x5[2], r = d_dist[5, 4]/max(d_dist) - d_distA[5, 4], col = rgb(1, 0, 0, 0.6))

