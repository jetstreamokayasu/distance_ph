#距離の分布をプロットする
plot(sort(t3orus4_dist[t4_land1[1], ]), 1-exp(-(sort(t3orus4_dist[t4_land1[1], ]/10)^2)))
hist(t3orus4_dist[t4_land1[1], ])

plot(sort(t3orus4_dist[t4_land1, ]), 1-exp(-(sort(t3orus4_dist[t4_land1, ])^2/100)))
hist(t3orus4_dist[t4_land1, ], breaks = seq(0, 28, 0.5))

d_hist1<-hist(t3orus4_dist[t4_land1, ])
