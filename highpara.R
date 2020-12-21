#ハイパラ設定法を試す
#d_t_instを使う

plot(d_t_inst$data, xlim = c(-1.5, 1.5), ylim = c(-1.5, 1.5), pch = 16)
text(d_t_inst$data, labels = 1:5, pos = 3)

#c_eta<-sqrt(2)*d_t_inst$distmat[4, 5]
c_eta<-d_t_inst$distmat[4, 5]/sqrt(log(10))

x_range<-seq(0, 10, length=100)
plot(seq(0, 10, length=100), seq(0, 10, length=100)*(1-exp(-(seq(0, 10, length=100)/2)^2)), type = "l")
points(sqrt(3/2)*2, (sqrt(3/2)*2)*(1-exp(-3/2)), col=2)

plot(x_range, 1-exp(-(x_range/2)^2), type = "l")
points(2*sqrt(2*log(10)), 1-exp(-(2*sqrt(2*log(10))/2)^2), col=2, pch=16)

x_rng2<-seq(0, 3, length=100)
plot(x_rng2, x_rng2*(1-exp(-(x_rng2/c_eta)^2)), type = "l")
points(d_t_inst$distmat[4, 5], d_t_inst$distmat[4, 5]*(1-exp(-(d_t_inst$distmat[4, 5]/c_eta)^2)), col=2, pch=16)
abline(0, 1)

points(d_t_inst$distmat[4, ], d_t_inst$distmat[4, ]*(1-exp(-(d_t_inst$distmat[4, ]/c_eta)^2)), col=3, pch=17)
points(d_t_inst$distmat[3, ], d_t_inst$distmat[3, ]*(1-exp(-(d_t_inst$distmat[3, ]/c_eta)^2)), col=4, pch=17)
points(d_t_inst$distmat[5, ], d_t_inst$distmat[5, ]*(1-exp(-(d_t_inst$distmat[5, ]/c_eta)^2)), col=5, pch=17)


#d_t_inst$create_changed_distmat(l_rate = 0, eta = 1)

d_t_inst$alt_distmat[[13]]$distmat<-d_t_inst$distmat

for (i in c(1, 4)) {
  
  d_t_inst$alt_distmat[[13]]$distmat[i, ]<-d_t_inst$distmat[i, ]*(1-exp(-(d_t_inst$distmat[i, ]/c_eta)^2))
  d_t_inst$alt_distmat[[13]]$distmat[, i]<-d_t_inst$distmat[, i]*(1-exp(-(d_t_inst$distmat[, i]/c_eta)^2))
  
}

#d_t_inst$alt_distmat[[13]]$calc_pd(maxdim = 1, maxscale = 3)

#----------------------
#ハイパラ設定法テスト----

trs300_1_82_inst<-TDAdataset$new(torus300_colle_set[[1]][[82]][["noizyX"]])
#trs300_1_82_inst$calc_pd(maxdim = 2, maxscale = 3)
#trs300_1_82_inst$create_changed_distmat(l_rate = 0, eta = 1)

#trs300_1_82_instの下三角行列
trs300_1_82_dist_cp<-trs300_1_82_inst$distmat
trs300_1_82_dist_cp[upper.tri(trs300_1_82_dist_cp)]<-0

#生成点の集合
birth_p<-which(trs300_1_82_inst$get_pd()[,1] == 2) %>% 
  sapply(., function(i)which(trs300_1_82_dist_cp == trs300_1_82_inst$get_pd()[i,2], arr.ind = T)) #%>% 
  #as.vector() %>% unique()

#重複要素
dupl_p<-duplicated(as.vector(birth_p)) %>% birth_p[.]

dim2deathes<-which(trs300_1_82_inst$get_pd()[,1] == 2) %>% map_dbl(~{trs300_1_82_inst$get_pd()[.,3]})

#2次元の穴の生成・消滅が入っている行
H2_idx<-which(trs300_1_82_inst$get_pd()[,1] == 2)

#距離行列変化

trs300_1_82_inst$alt_distmat[[1]]$distmat<-trs300_1_82_inst$distmat

for (i in 1:ncol(birth_p)) {
  
  c_eta2<-trs300_1_82_inst$get_pd()[H2_idx[i], 3]/sqrt(log(10))
  
  trs300_1_82_inst$alt_distmat[[1]]$distmat[birth_p[1,i],]<-
    trs300_1_82_inst$distmat[birth_p[1,i],]*( 1-exp( -(trs300_1_82_inst$distmat[birth_p[1,i],]/c_eta2)^2 ) )
  
  trs300_1_82_inst$alt_distmat[[1]]$distmat[, birth_p[1,i]]<-
    trs300_1_82_inst$distmat[, birth_p[1,i]]*( 1-exp( -(trs300_1_82_inst$distmat[, birth_p[1,i]]/c_eta2)^2 ) )
  
  trs300_1_82_inst$alt_distmat[[1]]$distmat[birth_p[2,i],]<-
    trs300_1_82_inst$distmat[birth_p[2,i],]*( 1-exp( -(trs300_1_82_inst$distmat[birth_p[2,i],]/c_eta2)^2 ) )
  
  trs300_1_82_inst$alt_distmat[[1]]$distmat[, birth_p[2,i]]<-
    trs300_1_82_inst$distmat[, birth_p[2,i]]*( 1-exp( -(trs300_1_82_inst$distmat[, birth_p[2,i]]/c_eta2)^2 ) )
  
  
}

trs300_1_82_inst$alt_distmat[[1]]$calc_pd(maxdim = 2, maxscale = 3)

#------------------------
#3次元トーラスでも試す
#特に2次元の穴
#400点3次元トーラスt3orus4_list3_15

t3orus4_list3_15$create_changed_distmat(l_rate = 0, eta = 1)

#t3orus4_list3_15の下三角行列
t3orus4_list3_15_dist_cp<-t3orus4_list3_15$distmat
t3orus4_list3_15_dist_cp[upper.tri(t3orus4_list3_15_dist_cp)]<-0

#生成点の集合
birth_p2<-which(t3orus4_list3_15$get_pd()[,1] == 2) %>% 
  sapply(., function(i)which(t3orus4_list3_15_dist_cp == t3orus4_list3_15$get_pd()[i,2], arr.ind = T)) 


# #重複要素
# dupl_p<-duplicated(as.vector(birth_p)) %>% birth_p[.]
# 
# dim2deathes<-which(trs300_1_82_inst$get_pd()[,1] == 2) %>% map_dbl(~{trs300_1_82_inst$get_pd()[.,3]})

#2次元の穴の生成・消滅が入っている行
H2_cols<-which(t3orus4_list3_15$get_pd()[,1] == 2)

#距離行列変化

t3orus4_list3_15$alt_distmat[[3]]$distmat<-t3orus4_list3_15$distmat

for (i in 1:ncol(birth_p2)) {
  
  c_eta2<-t3orus4_list3_15$get_pd()[H2_cols[i], 3]/sqrt(log(10))
  
  t3orus4_list3_15$alt_distmat[[3]]$distmat[birth_p2[1,i],]<-
    t3orus4_list3_15$distmat[birth_p2[1,i],]*( 1-exp( -(t3orus4_list3_15$distmat[birth_p2[1,i],]/c_eta2)^2 ) )
  
  t3orus4_list3_15$alt_distmat[[3]]$distmat[, birth_p2[1,i]]<-
    t3orus4_list3_15$distmat[, birth_p2[1,i]]*( 1-exp( -(t3orus4_list3_15$distmat[, birth_p2[1,i]]/c_eta2)^2 ) )
  
  t3orus4_list3_15$alt_distmat[[3]]$distmat[birth_p2[2,i],]<-
    t3orus4_list3_15$distmat[birth_p2[2,i],]*( 1-exp( -(t3orus4_list3_15$distmat[birth_p2[2,i],]/c_eta2)^2 ) )
  
  t3orus4_list3_15$alt_distmat[[3]]$distmat[, birth_p2[2,i]]<-
    t3orus4_list3_15$distmat[, birth_p2[2,i]]*( 1-exp( -(t3orus4_list3_15$distmat[, birth_p2[2,i]]/c_eta2)^2 ) )
  
  
}

t3orus4_list3_15$alt_distmat[[3]]$calc_pd(maxdim = 3, maxscale = 9)

#--------------------------------
#穴のないデータに対して試す

xx_t<-runif(n = 400, min = 0, max = 2)
yy_t<-runif(n = 400, min = 0, max = 2)

plot(xx_t, yy_t)

plane_inst<-TDAdataset$new(cbind(xx_t, yy_t))
#plane_inst$input_pd(pd = calculate_homology(mat = plane_inst$data, dim = 1))
plane_inst$calc_pd(maxdim = 1, maxscale = 2)

#t3orus4_list3_15の下三角行列
plane_inst_dist_cp<-plane_inst$distmat
plane_inst_dist_cp[upper.tri(plane_inst_dist_cp)]<-0

#生成点の集合
birth_p3<-which(plane_inst$get_pd()[,1] == 1) %>% 
  sapply(., function(i)which(plane_inst_dist_cp == plane_inst$get_pd()[i,2], arr.ind = T)) 


#1次元の穴の生成・消滅が入っている行
H1_cols<-which(plane_inst$get_pd()[,1] == 1)

#距離行列変化

plane_inst$create_changed_distmat(l_rate = 0, eta = 1)

plane_inst$alt_distmat[[1]]$distmat<-plane_inst$distmat

for (i in 1:ncol(birth_p3)) {
  
  c_eta2<-plane_inst$get_pd()[H1_cols[i], 3]/sqrt(log(10))
  
  plane_inst$alt_distmat[[1]]$distmat[birth_p3[1,i],]<-
    plane_inst$distmat[birth_p3[1,i],]*( 1-exp( -(plane_inst$distmat[birth_p3[1,i],]/c_eta2)^2 ) )
  
  plane_inst$alt_distmat[[1]]$distmat[, birth_p3[1,i]]<-
    plane_inst$distmat[, birth_p3[1,i]]*( 1-exp( -(plane_inst$distmat[, birth_p3[1,i]]/c_eta2)^2 ) )
  
  plane_inst$alt_distmat[[1]]$distmat[birth_p3[2,i],]<-
    plane_inst$distmat[birth_p3[2,i],]*( 1-exp( -(plane_inst$distmat[birth_p3[2,i],]/c_eta2)^2 ) )
  
  plane_inst$alt_distmat[[1]]$distmat[, birth_p3[2,i]]<-
    plane_inst$distmat[, birth_p3[2,i]]*( 1-exp( -(plane_inst$distmat[, birth_p3[2,i]]/c_eta2)^2 ) )
  
  
}

plane_inst$alt_distmat[[1]]$calc_pd(maxdim = 1, maxscale = 2)
