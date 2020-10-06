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
