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
d_t<-cbind(c(1.2, 0, -1.1, 0, 0.5), c(0, 0.9, 0, -1.3, 0.4))
plot(d_t, xlim = c(-1.5, 1.5), ylim = c(-1.5, 1.5))
text(c(1.2, 0, -1.1, 0, 0.5), c(0, 0.9, 0, -1.3, 0.4), labels = 1:5)

d_dist<-dist(d_t) %>% as.matrix()
d_dist_sorted<-sort(dist(d_t)) %>% unique()

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
#点5中心
d_t_x<-(d_t[-5, 1] - x5[1])/d_dist_sorted[10]
d_t_y<-(d_t[-5, 2] - x5[2])/d_dist_sorted[10]
plot(d_t_x+x5[1], d_t_y+x5[2], xlim = c(-1, 1), ylim = c(-1, 1))
points(x5[1], x5[2], pch=16, col=2)
text(c(d_t_x+x5[1], x5[1]), c(d_t_y+x5[2], x5[2]), labels = 1:5)

#距離の正規化
#点4中心
d_t_x2<-(d_t[-4, 1] - x4[1])/d_dist_sorted[10]
d_t_y2<-(d_t[-4, 2] - x4[2])/d_dist_sorted[10]
plot(d_t_x2+x4[1], d_t_y2+x4[2], xlim = c(-1, 1), ylim = c(0, -2))
points(x4[1], x4[2], pch=16, col=2)
text(c(d_t_x2+x4[1], x4[1]), c(d_t_y2+x4[2], x4[2]), labels = c(1, 2, 3, 5, 4))

#FRI適用後の点５を中心とした分布
eta<-1.7#ハイパラ
d_t_xA2<-(1-exp(-(d_dist[4, -4]/eta)^2)) * d_t_x2/sqrt(d_t_x2^2 + d_t_y2^2) + x4[1]
d_t_yA2<-(1-exp(-(d_dist[4, -4]/eta)^2)) * d_t_y2/sqrt(d_t_x2^2 + d_t_y2^2) + x4[2]
points(d_t_xA2, d_t_yA2, col=4, pch=16)

d_distA<-d_dist/max(d_dist)
d_distA[4,]<-(1-exp(-(d_dist[4, ]/eta)^2))
d_distA[,4]<-(1-exp(-(d_dist[, 4]/eta)^2))
d_distA[5,]<-(1-exp(-(d_dist[5, ]/eta)^2))
d_distA[,5]<-(1-exp(-(d_dist[, 5]/eta)^2))
d_distA[3,]<-(1-exp(-(d_dist[3, ]/eta)^2))
d_distA[,3]<-(1-exp(-(d_dist[, 3]/eta)^2))
d_dist_normed_sorted<-sort(d_dist/max(d_dist))
d_distA_sorted<-sort(d_distA) %>% unique()
d_t_pd3<-calculate_homology(mat = d_dist/max(d_dist), dim = 1, format = "distmat")
d_t_pdA1<-calculate_homology(mat = d_distA, dim = 1, format = "distmat")
calcper(d_t_pdA1, 1)

# > d_dist/max(d_dist)
# 1         2         3         4         5
# 1 0.0000000 0.6521739 1.0000000 0.7692090 0.3505329
# 2 0.6521739 0.0000000 0.6179422 0.9565217 0.3074377
# 3 1.0000000 0.6179422 0.0000000 0.7404081 0.7170618
# 4 0.7692090 0.9565217 0.7404081 0.0000000 0.7704367
# 5 0.3505329 0.3074377 0.7170618 0.7704367 0.0000000

# > calcper(diag = d_t_pd3, 1)
# death 
# 0.001227788 

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

#--------------------------------
#点4だけFRI適用
#d_t_x2は1,2,3,5点の点4からの方向ベクトル
lines(c(d_t_x2[2]+x4[1], d_t_x2[4]+x4[1]), c(d_t_y2[2]+x4[2], d_t_y2[4]+x4[2]))#2-5
lines(c(d_t_x2[1]+x4[1], d_t_x2[4]+x4[1]), c(d_t_y2[1]+x4[2], d_t_y2[4]+x4[2]))#1-5
lines(c(d_t_x2[2]+x4[1], d_t_x2[3]+x4[1]), c(d_t_y2[2]+x4[2], d_t_y2[3]+x4[2]))#2-3
lines(c(d_t_x2[3]+x4[1], x4[1]), c(d_t_y2[3]+x4[2], x4[2]))#3-4
lines(c(d_t_x2[1]+x4[1], d_t_x2[2]+x4[1]), c(d_t_y2[1]+x4[2], d_t_y2[2]+x4[2]))#1-2
lines(c(d_t_x2[1]+x4[1], x4[1]), c(d_t_y2[1]+x4[2], x4[2]))#1-4#生成
lines(c(x4[1], d_t_x2[4]+x4[1]), c(x4[2], d_t_y2[4]+x4[2]))#4-5
lines(c(d_t_x2[3]+x4[1], d_t_x2[4]+x4[1]), c(d_t_y2[3]+x4[2], d_t_y2[4]+x4[2]))#3-5#消滅


lines(c(d_t_x[1]+x5[1], x5[1]), c(d_t_y[1]+x5[2], x5[2]))
lines(c(d_t_x[3]+x5[1], x5[1]), c(d_t_y[3]+x5[2], x5[2]))
lines(c(d_t_x[2]+x5[1], d_t_x[3]+x5[1]), c(d_t_y[2]+x5[2], d_t_y[3]+x5[2]))
lines(c(d_t_x[3]+x5[1], d_t_x[4]+x5[1]), c(d_t_y[3]+x5[2], d_t_y[4]+x5[2]))
lines(c(d_t_x[1]+x5[1], d_t_x[2]+x5[1]), c(d_t_y[1]+x5[2], d_t_y[2]+x5[2]))
lines(c(d_t_x[1]+x5[1], d_t_x[4]+x5[1]), c(d_t_y[1]+x5[2], d_t_y[4]+x5[2]))#生成
lines(c(d_t_x[4]+x5[1], x5[1]), c(d_t_y[4]+x5[2], x5[2]))#消滅
lines(c(d_t_x[2]+x5[1], d_t_x[4]+x5[1]), c(d_t_y[2]+x5[2], d_t_y[4]+x5[2]))
lines(c(d_t_x[1]+x5[1], d_t_x[3]+x5[1]), c(d_t_y[1]+x5[2], d_t_y[3]+x5[2]))


#--------------------------------------
#FRI適用後の幾何的解釈
#点4の周辺に点を散らす
delta<-c(pi/3, pi/2, 2*pi/3, 5*pi/4, 7*pi/4)
zeta1<-rnorm(n = 5, sd = 0.05)
zeta2<-rnorm(n = 5, sd = 0.05)

points(0.2*cos(delta)+x4[1]+zeta1, 0.2*sin(delta)+x4[2]+zeta2)
test_data2<-cbind(0.2*cos(delta)+x4[1]+zeta1, 0.2*sin(delta)+x4[2]+zeta2) %>% rbind(d_t, .)
plot(test_data2, xlim = c(-1.5, 1.5), ylim = c(-1.5, 1.5))
pointLabel(x = test_data2, labels = as.character(1:10))

test_data2_dist<-dist(test_data2) %>% as.matrix()
test_data2_dist_sorted<-sort(test_data2_dist) %>% unique()

#FRI適用前PH計算
test_data2_pd1<-calculate_homology(mat = test_data2, dim = 1)
calcper(test_data2_pd1, 1)#＝0.134317

#距離を昇順に並べ、相当するインデックスを求める
for (i in 2:length(test_data2_dist_sorted)) {
  
  cat("i=", i, "\n")
  which(test_data2_dist==test_data2_dist_sorted[i], arr.ind = T) %>% print()
  
}

#フィルトレーション順に線を結ぶ
lines(c(test_data2[7, 1], test_data2[8, 1]), c(test_data2[7, 2], test_data2[8, 2]))#7-8
lines(c(test_data2[6, 1], test_data2[7, 1]), c(test_data2[6, 2], test_data2[7, 2]))#6-7
lines(c(test_data2[4, 1], test_data2[9, 1]), c(test_data2[4, 2], test_data2[9, 2]))#4-9
lines(c(test_data2[4, 1], test_data2[7, 1]), c(test_data2[4, 2], test_data2[7, 2]))#4-7
lines(c(test_data2[4, 1], test_data2[8, 1]), c(test_data2[4, 2], test_data2[8, 2]))#4-8
lines(c(test_data2[4, 1], test_data2[6, 1]), c(test_data2[4, 2], test_data2[6, 2]))#4-6
lines(c(test_data2[6, 1], test_data2[8, 1]), c(test_data2[6, 2], test_data2[8, 2]))#6-8
lines(c(test_data2[8, 1], test_data2[9, 1]), c(test_data2[8, 2], test_data2[9, 2]))#8-9
lines(c(test_data2[4, 1], test_data2[10, 1]), c(test_data2[4, 2], test_data2[10, 2]))#4-10
lines(c(test_data2[7, 1], test_data2[9, 1]), c(test_data2[7, 2], test_data2[9, 2]))#7-9
lines(c(test_data2[6, 1], test_data2[10, 1]), c(test_data2[6, 2], test_data2[10, 2]))#6-10
lines(c(test_data2[9, 1], test_data2[10, 1]), c(test_data2[9, 2], test_data2[10, 2]))#9-10
lines(c(test_data2[7, 1], test_data2[10, 1]), c(test_data2[7, 2], test_data2[10, 2]))#7-10
lines(c(test_data2[6, 1], test_data2[9, 1]), c(test_data2[6, 2], test_data2[9, 2]))#6-9
lines(c(test_data2[8, 1], test_data2[10, 1]), c(test_data2[8, 2], test_data2[10, 2]))#8-10
lines(c(test_data2[2, 1], test_data2[5, 1]), c(test_data2[2, 2], test_data2[5, 2]))#2-5
lines(c(test_data2[1, 1], test_data2[5, 1]), c(test_data2[1, 2], test_data2[5, 2]))#1-5
lines(c(test_data2[2, 1], test_data2[3, 1]), c(test_data2[2, 2], test_data2[3, 2]))#2-3
lines(c(test_data2[3, 1], test_data2[8, 1]), c(test_data2[3, 2], test_data2[8, 2]))#3-8
lines(c(test_data2[1, 1], test_data2[2, 1]), c(test_data2[1, 2], test_data2[2, 2]))#1-2
lines(c(test_data2[1, 1], test_data2[6, 1]), c(test_data2[1, 2], test_data2[6, 2]))#1-6#生成
lines(c(test_data2[5, 1], test_data2[6, 1]), c(test_data2[5, 2], test_data2[6, 2]))#5-6
lines(c(test_data2[3, 1], test_data2[7, 1]), c(test_data2[3, 2], test_data2[7, 2]))#3-7
lines(c(test_data2[5, 1], test_data2[7, 1]), c(test_data2[5, 2], test_data2[7, 2]))#5-7
lines(c(test_data2[5, 1], test_data2[8, 1]), c(test_data2[5, 2], test_data2[8, 2]))#5-8
lines(c(test_data2[1, 1], test_data2[7, 1]), c(test_data2[1, 2], test_data2[7, 2]))#1-7
lines(c(test_data2[3, 1], test_data2[5, 1]), c(test_data2[3, 2], test_data2[5, 2]))#3-5#消滅
lines(c(test_data2[3, 1], test_data2[6, 1]), c(test_data2[3, 2], test_data2[6, 2]))#3-6

#FRIを点４に適用
test_data2_dist_fri<-test_data2_dist/max(test_data2_dist)
test_data2_dist_fri[4, ]<-1-exp(-(test_data2_dist[4, ]/1.7)^2)
test_data2_dist_fri[, 4]<-1-exp(-(test_data2_dist[, 4]/1.7)^2)

for (i in test_data2_land1) {
  
  test_data2_dist_fri[i, ]<-1-exp(-(test_data2_dist[i, ]/1.7)^2)
  test_data2_dist_fri[, i]<-1-exp(-(test_data2_dist[, i]/1.7)^2)
  
}

test_data2_normed_pd2<-calculate_homology(mat = test_data2_dist/max(test_data2_dist), dim = 1, format = "distmat")
test_data2_normed_dist<-test_data2_dist/max(test_data2_dist)
calcper(test_data2_normed_pd2, 1)#=0.05683944
test_data2_fri_pd1<-calculate_homology(mat = test_data2_dist_fri, dim = 1, format = "distmat")
calcper(test_data2_fri_pd1, 1)

test_data2_land1<-landmark_points(X = test_data2, n_land = 3)

test_data2_dist_fri_sorted<-sort(test_data2_dist_fri) %>% unique()

#距離を昇順に並べ、相当するインデックスを求める
for (i in 2:length(test_data2_dist_fri_sorted)) {
  
  cat("i=", i-1, "\n")
  which(test_data2_dist_fri==test_data2_dist_fri_sorted[i], arr.ind = T) %>% print()
  
}

#フィルトレーション順に線を結ぶ
#FRI後
lines(c(test_data2[4, 1], test_data2[10, 1]), c(test_data2[4, 2], test_data2[10, 2]))#4-10
lines(c(test_data2[6, 1], test_data2[10, 1]), c(test_data2[6, 2], test_data2[10, 2]))#6-10
lines(c(test_data2[9, 1], test_data2[10, 1]), c(test_data2[9, 2], test_data2[10, 2]))#9-10
lines(c(test_data2[7, 1], test_data2[10, 1]), c(test_data2[7, 2], test_data2[10, 2]))#7-10
lines(c(test_data2[7, 1], test_data2[8, 1]), c(test_data2[7, 2], test_data2[8, 2]))#7-8
lines(c(test_data2[6, 1], test_data2[7, 1]), c(test_data2[6, 2], test_data2[7, 2]))#6-7
lines(c(test_data2[4, 1], test_data2[9, 1]), c(test_data2[4, 2], test_data2[9, 2]))#4-9
lines(c(test_data2[4, 1], test_data2[7, 1]), c(test_data2[4, 2], test_data2[7, 2]))#4-7
lines(c(test_data2[8, 1], test_data2[10, 1]), c(test_data2[8, 2], test_data2[10, 2]))#8-10
lines(c(test_data2[4, 1], test_data2[8, 1]), c(test_data2[4, 2], test_data2[8, 2]))#4-8
lines(c(test_data2[4, 1], test_data2[6, 1]), c(test_data2[4, 2], test_data2[6, 2]))#4-6
lines(c(test_data2[6, 1], test_data2[8, 1]), c(test_data2[6, 2], test_data2[8, 2]))#6-8
lines(c(test_data2[8, 1], test_data2[9, 1]), c(test_data2[8, 2], test_data2[9, 2]))#8-9
lines(c(test_data2[7, 1], test_data2[9, 1]), c(test_data2[7, 2], test_data2[9, 2]))#7-9
lines(c(test_data2[2, 1], test_data2[5, 1]), c(test_data2[2, 2], test_data2[5, 2]))#2-5
lines(c(test_data2[6, 1], test_data2[9, 1]), c(test_data2[6, 2], test_data2[9, 2]))#6-9
lines(c(test_data2[1, 1], test_data2[5, 1]), c(test_data2[1, 2], test_data2[5, 2]))#1-5
lines(c(test_data2[2, 1], test_data2[3, 1]), c(test_data2[2, 2], test_data2[3, 2]))#2-3
lines(c(test_data2[1, 1], test_data2[2, 1]), c(test_data2[1, 2], test_data2[2, 2]))#1-2
lines(c(test_data2[1, 1], test_data2[6, 1]), c(test_data2[1, 2], test_data2[6, 2]))#1-6
lines(c(test_data2[1, 1], test_data2[7, 1]), c(test_data2[1, 2], test_data2[7, 2]))#1-7
lines(c(test_data2[3, 1], test_data2[8, 1]), c(test_data2[3, 2], test_data2[8, 2]))#3-8#生成
lines(c(test_data2[1, 1], test_data2[10, 1]), c(test_data2[1, 2], test_data2[10, 2]))#1-10
lines(c(test_data2[1, 1], test_data2[8, 1]), c(test_data2[1, 2], test_data2[8, 2]))#1-8
lines(c(test_data2[5, 1], test_data2[6, 1]), c(test_data2[5, 2], test_data2[6, 2]))#5-6
lines(c(test_data2[1, 1], test_data2[4, 1]), c(test_data2[1, 2], test_data2[4, 2]))#1-4
lines(c(test_data2[3, 1], test_data2[7, 1]), c(test_data2[3, 2], test_data2[7, 2]))#3-7
lines(c(test_data2[5, 1], test_data2[7, 1]), c(test_data2[5, 2], test_data2[7, 2]))#5-7
lines(c(test_data2[5, 1], test_data2[8, 1]), c(test_data2[5, 2], test_data2[8, 2]))#5-8
lines(c(test_data2[3, 1], test_data2[5, 1]), c(test_data2[3, 2], test_data2[5, 2]))#3-5#消滅
lines(c(test_data2[3, 1], test_data2[6, 1]), c(test_data2[3, 2], test_data2[6, 2]))#3-6
lines(c(test_data2[3, 1], test_data2[4, 1]), c(test_data2[3, 2], test_data2[4, 2]))#3-4

#--------------------------------------
#距離を正規化後のプロット
#点10中心
library(maptools)

test_data2_x<-(test_data2[-10, 1] - test_data2[10, 1])/max(test_data2_dist) + test_data2[10, 1] 
test_data2_x<-c(test_data2_x, test_data2[10, 1])

test_data2_y<-(test_data2[-10, 2] - test_data2[10, 2])/max(test_data2_dist) + test_data2[10, 2]
test_data2_y<-c(test_data2_y, test_data2[10, 2])

#距離を正規化後のプロット実行
plot(test_data2_x, test_data2_y, xlim = c(-0.5, 1), ylim = c(-1.5, 0), pch=16)
pointLabel(test_data2_x, test_data2_y, labels = as.character(1:10))
points(test_data2_x[test_data2_land1], test_data2_y[test_data2_land1], pch=16, col=2)

#正規化された距離行列
#下三角行列化
test_data2_dist_normed<-test_data2_dist/max(test_data2_dist)
test_data2_dist_normed_low<-test_data2_dist_normed
test_data2_dist_normed_low[upper.tri(test_data2_dist_normed_low)]<-0

#正規化後の距離をソート
test_data2_dist_normed_sorted<-sort(test_data2_dist_normed) %>% unique()

#結合順に線分を図示
#FRI前
#birth=22
#death=28
idx_mat<-which(test_data2_dist_normed_low > 0 & test_data2_dist_normed_low <= test_data2_dist_normed_sorted[28], arr.ind = T)
#結ばれる線の描画
title(main = paste("r =", round(test_data2_dist_normed_sorted[28], 5)), cex.main=2)
trush<-lapply(1:nrow(idx_mat), function(i){
  
  lines(c(test_data2_x[idx_mat[i,1]], test_data2_x[idx_mat[i,2]]), c(test_data2_y[idx_mat[i,1]], test_data2_y[idx_mat[i,2]]))
  
})

#FRI後
#birth=23
#death=31
idx_mat<-which(test_data2_dist_fri_low > 0 & test_data2_dist_fri_low <= test_data2_dist_fri_sorted[31], arr.ind = T)
#結ばれる線の描画
title(main = paste("r =", round(test_data2_dist_fri_sorted[31], 5)), cex.main=2)#タイトル
trush<-lapply(1:nrow(idx_mat), function(i){
  
  lines(c(test_data2_x[idx_mat[i,1]], test_data2_x[idx_mat[i,2]]), c(test_data2_y[idx_mat[i,1]], test_data2_y[idx_mat[i,2]]))
  
})

#------------------------------------------
#フィルトレーションのアニメーション
library(animation)
library(jpeg)
library(tcltk)

#FRI適用前
saveGIF({
  
  for (k in 2:28) {
    
    cat("i=", k, "\n")
    plot(test_data2_x, test_data2_y, xlim = c(-0.5, 1), ylim = c(-1.5, 0), main = paste0("r=", test_data2_dist_sorted[k]/max(test_data2_dist)))
    text(test_data2_x, test_data2_y, labels = 1:10)
    
    re<-lapply(2:k, function(i){
      
      idx<-which(test_data2_dist==test_data2_dist_sorted[i], arr.ind = T) 
      lines(c(test_data2_x[idx[1,1]], test_data2_x[idx[1,2]]), c(test_data2_y[idx[1,1]], test_data2_y[idx[1,2]]))
      
    })
    
  }
  
}, interbal=1.0, movie.name = "filt2.gif")

#FRI適用後
saveGIF({
  
  for (k in 2:31) {
    
    cat("i=", k, "\n")
    plot(test_data2_x[-test_data2_land1], test_data2_y[-test_data2_land1], xlim = c(-0.5, 1), ylim = c(-1.5, 0), main = paste0("r=", test_data2_dist_fri_sorted[k]))
    points(test_data2_x[test_data2_land1], test_data2_y[test_data2_land1], pch=16, col=2)
    text(test_data2_x, test_data2_y, labels = 1:10)
    
    re<-lapply(2:k, function(i){
      
      idx<-which(test_data2_dist_fri==test_data2_dist_fri_sorted[i], arr.ind = T) 
      lines(c(test_data2_x[idx[1,1]], test_data2_x[idx[1,2]]), c(test_data2_y[idx[1,1]], test_data2_y[idx[1,2]]))
      
    })
    
  }
  
}, interbal=1.0, movie.name = "filt_fri.gif")

#--------------------------------------------
#FRI適用前後比較GIF
oldpar <- par(no.readonly = TRUE) 

#FRI適用後の下三角距離行列
test_data2_dist_fri_low<-test_data2_dist_fri
test_data2_dist_fri_low[upper.tri(test_data2_dist_fri_low)]<-0

plot_filt<-function(){
  
  #フィルトレーション用の円の半径増大ベクトル
  filt_line<-seq(0.004, 0.75, by=0.004)
  
  for (r in filt_line){
    
    cat("r=", r, "\n")
    #プロット画面分割
    par(mfrow=c(1,2)) 
    
    #FRI前描画
    plot(test_data2_x, test_data2_y, xlim = c(-0.5, 1), ylim = c(-1.5, 0), main = paste0("r=", r))
    text(test_data2_x, test_data2_y, labels = as.character(1:10))
    
    #閾値以下の要素のインデックスの行列
    idx_mat<-which(test_data2_dist_normed_low > 0 & test_data2_dist_normed_low <= r, arr.ind = T)
    
    if(length(idx_mat) != 0){
      #結ばれる線の描画
      trush<-lapply(1:nrow(idx_mat), function(i){
        
        lines(c(test_data2_x[idx_mat[i,1]], test_data2_x[idx_mat[i,2]]), c(test_data2_y[idx_mat[i,1]], test_data2_y[idx_mat[i,2]]))
        
      })
    
    }
    
    #FRI後描画
    plot(test_data2_x[-test_data2_land1], test_data2_y[-test_data2_land1], xlim = c(-0.5, 1), ylim = c(-1.5, 0), main = paste0("r=", r))
    points(test_data2_x[test_data2_land1], test_data2_y[test_data2_land1], pch=16, col=2)
    text(test_data2_x, test_data2_y, labels = as.character(1:10))
    
    #閾値以下の要素のインデックスの行列
    idx_mat<-which(test_data2_dist_fri_low > 0 & test_data2_dist_fri_low <= r, arr.ind = T)
    
    if(length(idx_mat) != 0){
      #結ばれる線の描画
      trush<-lapply(1:nrow(idx_mat), function(i){
        
        lines(c(test_data2_x[idx_mat[i,1]], test_data2_x[idx_mat[i,2]]), c(test_data2_y[idx_mat[i,1]], test_data2_y[idx_mat[i,2]]))
        
      })
    
    }
    
  }
  
}

animation::saveGIF(plot_filt(), interbal=0.02, movie.name = "filt_comp2.gif", ani.width=1000, ani.height=500, movietype="gif")

#saveHTMLを試す

animation::saveHTML(plot_filt(), img.name = "filt_comp3", interval=0.4, htmlfile = "filt_comp3.html", ani.width=1000, ani.height=500, imgdir="data")

#ffmpegによる動画化を試す
ff_pass<-animation::ani.options(ffmpeg="D:/okayasu/D_download/ffmpeg-4.3.1-2020-10-01-essentials_build/bin/ffmpeg.exe")

saveVideo(expr = plot_filt(), video.name = "filt_comp4.mp4", img.name = "filt_comp_four", interval=0.2, ani.width=1000, ani.height=500)


#---------------------------------------------
#1-exp(-(d_ij/eta)^2)を図示

plot(seq(0, 1, length=100), 1-exp(-(seq(0, 1, length=100)/2)^2), type="l")
title("1-exp(-(d_ij/eta)^2)")

trs300_1_10_dist5_srt<-sort(trs300_1_10_dist5[land10F[1], ])
plot(trs300_1_10_dist[land10F[1], ], 1-exp(-(trs300_1_10_dist5[land10F[1], ])^2), ylim = c(0, 1))
points(trs300_1_10_dist[land10F[1], ], trs300_1_10_dist5[land10F[1], ])
points(trs300_1_10_dist[land10F[1], ], trs300_1_10_dist5[land10F[1], ]^2)

plot(t3orus4_dist[t4_land1[1], ], t3orus4_distD[t4_land1[1], ])
points(t3orus4_dist[t4_land1[1], ], t3orus4_dist[t4_land1[1], ]/max(t3orus4_dist))

plot(t3orus4_dist[t4_land1[1], ], 1-exp(-(t3orus4_dist[t4_land1[1], ]/max(t3orus4_dist))^2), ylim = c(0, 1))
