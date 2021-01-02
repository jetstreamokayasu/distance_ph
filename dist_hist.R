#データ点間の距離をヒストグラムで出す

#-----------------------------
#2次元トーラスtrs300_1_10。l_rate=0.8, eta=3.0-----------
trs300_1_10$distmat %>% as.dist() %>% hist(breaks = seq(0, 7, by=0.1))
trs300_1_10$data %>% dist() %>% hist()

trs300_1_10$create_subsample(sub_size = trs300_1_10$n_points*0.8, n_subs = 10)

trs300_1_10$subsamples[[1]]$change_dist(l_rate = 0.8, eta = 3.0)
trs300_1_10$subsamples[[1]]$data %>% dist() %>% hist(breaks = seq(0, 7, by=0.1), main="T2 original")
trs300_1_10$subsamples[[1]]$distmat %>% as.dist() %>% hist(breaks = seq(0, 7, by=0.1), main="T2 rate=0.8 eta =3.0")

trs300_1_10$subsamples[[2]]$change_dist(l_rate = 0.8, eta = 3.0)
trs300_1_10$subsamples[[2]]$data %>% dist() %>% hist(breaks = seq(0, 7, by=0.1))
trs300_1_10$subsamples[[2]]$distmat %>% as.dist() %>% hist(breaks = seq(0, 7, by=0.1))

#-----------------------------------
#3次元トーラスt3orus4_list3_81。l_rate=0.5, eta=4.0 or eta=6.5------------

#３次ベッチ数に関して
t3orus4_list3_81_inst<-TDAdataset$new(t3orus4_list3[[81]][["noizyX"]])

t3orus4_list3_81_inst$create_subsample(sub_size = t3orus4_list3_81_inst$n_points*0.8, n_subs = 10)

#t3orus4_list3_81_inst$subsamples[[1]]$distmat<-t3orus4_list3_81_inst$subsamples[[1]]$data %>% dist() %>% as.matrix()
#t3orus4_list3_81_inst$subsamples[[1]]$change_dist(l_rate = 0.5, eta = 6.5)
t3orus4_list3_81_inst$subsamples[[1]]$data %>% dist() %>% hist(breaks = seq(0, 28, by=0.5), main="T^3 original")
t3orus4_list3_81_inst$subsamples[[1]]$distmat %>% as.dist() %>% hist(breaks = seq(0, 28, by=0.5), main="T^3 l_rate=0.5 eta=6.5")

# t3orus4_list3_81_inst$subsamples[[1]]$alt_distmat[[1]]$distmat<-t3orus4_list3_81_inst$subsamples[[1]]$data %>% dist() %>% as.matrix()
# t3orus4_list3_81_inst$subsamples[[1]]$alt_distmat[[1]]$change_dist(l_rate = 0.5, eta = 4.0)
t3orus4_list3_81_inst$subsamples[[1]]$alt_distmat[[1]]$distmat %>% as.dist() %>% hist(breaks = seq(0, 28, by=0.5), main="T^3 l_rate=0.5 eta=4.0")

#t3orus4_list3_81_inst$subsamples[[2]]$distmat<-t3orus4_list3_81_inst$subsamples[[2]]$data %>% dist() %>% as.matrix()
#t3orus4_list3_81_inst$subsamples[[2]]$change_dist(l_rate = 0.5, eta = 6.5)
t3orus4_list3_81_inst$subsamples[[2]]$data %>% dist() %>% hist(breaks = seq(0, 28, by=0.5))
t3orus4_list3_81_inst$subsamples[[2]]$distmat %>% as.dist() %>% hist(breaks = seq(0, 28, by=0.5))

#t3orus4_list3_81_inst$subsamples[[3]]$change_dist(l_rate = 0.5, eta = 4.0)
t3orus4_list3_81_inst$subsamples[[3]]$data %>% dist() %>% hist(breaks = seq(0, 28, by=0.5))
t3orus4_list3_81_inst$subsamples[[3]]$distmat %>% as.dist() %>% hist(breaks = seq(0, 28, by=0.5))

#t3orus4_list3_81_inst$subsamples[[4]]$change_dist(l_rate = 0.5, eta = 4.0)
t3orus4_list3_81_inst$subsamples[[4]]$data %>% dist() %>% hist(breaks = seq(0, 28, by=0.5))
t3orus4_list3_81_inst$subsamples[[4]]$distmat %>% as.dist() %>% hist(breaks = seq(0, 28, by=0.5))

