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