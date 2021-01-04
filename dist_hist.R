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
t3orus4_list3_81_ori_hist<-t3orus4_list3_81_inst$subsamples[[1]]$data %>% dist() %>% hist(breaks = seq(0, 28, by=0.5), main="T^3 original")
t3orus4_list3_81_inst$subsamples[[1]]$distmat %>% as.dist() %>% hist(breaks = seq(0, 28, by=0.5), main="T^3 l_rate=0.5 eta=6.5")

#-------------------------------------------------
#素の400点3次元トーラスのPDを計算
t3orus4_list3_81_sub1<-TDAdataset$new(t3orus4_list3_81_inst$subsamples[[1]]$data)
t3orus4_list3_81_sub1$calc_pd(maxdim = 3, maxscale = 9)

#3次元のパーシステントホモロジー群を抽出
t3orus4_list3_81_sub1_pdH3<-t3orus4_list3_81_sub1$get_pd()[t3orus4_list3_81_sub1$get_pd()[,1]==3, ]

#どのセルに含まれているかを判別
cell<-seq(0, 28, by=0.5)

#生成時刻の距離が含まれているセル
birth_cell<-map_lgl(seq_along(cell), ~{(t3orus4_list3_81_sub1_pdH3[2] > cell[.]) & (t3orus4_list3_81_sub1_pdH3[2] <= cell[. + 1])}) %>% which()

#消滅時刻の距離が含まれているセル
death_cell<-map_lgl(seq_along(cell), ~{(t3orus4_list3_81_sub1_pdH3[3] > cell[.]) & (t3orus4_list3_81_sub1_pdH3[3] <= cell[. + 1])}) %>% which()

cell_col<-rep(NA, length = length(t3orus4_list3_81_ori_hist$counts))
cell_col[birth_cell]<-"red"
cell_col[death_cell]<-"blue"

t3orus4_list3_81_inst$subsamples[[1]]$data %>% dist() %>% hist(breaks = seq(0, 28, by=0.5), main="T^3 original")

t3orus4_list3_81_sub1$distmat %>% as.dist() %>% hist(breaks = seq(0, 28, by=0.5), main="T^3 original", col = cell_col)

#距離がetaと等しい
abline(v = t3orus4_list3_81_inst$subsamples[[1]]$get_param()$eta, col = "green3")

#距離が変曲点
abline(v = t3orus4_list3_81_inst$subsamples[[1]]$get_param()$eta * sqrt(3/2), col = "deeppink")

#距離が90%点
abline(v = t3orus4_list3_81_inst$subsamples[[1]]$get_param()$eta * sqrt(log(10)), col = "darkviolet")

#--------------------------------------
#l_rate = 0.5, eta = 6.5で変化後の400点3次元トーラス

t3orus4_list3_81_inst$subsamples[[1]]$calc_pd(maxdim = 3, maxscale = 9)

#3次元のパーシステントホモロジー群を抽出
t3orus4_list3_81_alt_sub1_pdH3<-which(t3orus4_list3_81_inst$subsamples[[1]]$get_pd()[, 1] == 3) %>% 
  t3orus4_list3_81_inst$subsamples[[1]]$get_pd()[., ]

#生成時刻の距離が含まれているセル
b_cell2<-map_lgl(seq_along(cell), ~{any(t3orus4_list3_81_alt_sub1_pdH3[, 2] > cell[.]) & any(t3orus4_list3_81_alt_sub1_pdH3[, 2] <= cell[. + 1])}) %>% which()

#消滅時刻の距離が含まれているセル
d_cell2<-map_lgl(seq_along(cell), ~{any(t3orus4_list3_81_alt_sub1_pdH3[, 3] > cell[.]) & any(t3orus4_list3_81_alt_sub1_pdH3[, 3] <= cell[. + 1])}) %>% which()

#変化後の距離のヒストグラム
#カラーコード#00a0e9=シアン、#e4007f=マゼンダ、4D＝アルファ値30%
cell_col2b<-t3orus4_list3_81_sub1_altdist_hist$counts %>% length() %>% rep(NA, length = .)
cell_col2b[b_cell2]<-"#e4007f4d"

cell_col2d<-t3orus4_list3_81_sub1_altdist_hist$counts %>% length() %>% rep(NA, length = .)
cell_col2d[d_cell2]<-"#00a0e94d"

t3orus4_list3_81_sub1_altdist_hist<-t3orus4_list3_81_inst$subsamples[[1]]$distmat %>% 
  as.dist() %>% hist(breaks = seq(0, 28, by=0.5), main="T^3 l_rate=0.5 eta=6.5", col =cell_col2b)

t3orus4_list3_81_inst$subsamples[[1]]$distmat %>% as.dist() %>% hist(breaks = seq(0, 28, by=0.5), col = cell_col2d, add = T)

#-----------------------------
#サブサンプル1でもう一度l_rate=0.5 eta=4.0で距離行列変化---------
# t3orus4_list3_81_inst$subsamples[[1]]$alt_distmat[[1]]$distmat<-t3orus4_list3_81_inst$subsamples[[1]]$data %>% dist() %>% as.matrix()
# t3orus4_list3_81_inst$subsamples[[1]]$alt_distmat[[1]]$change_dist(l_rate = 0.5, eta = 4.0)
t3orus4_list3_81_inst$subsamples[[1]]$alt_distmat[[1]]$distmat %>% as.dist() %>% hist(breaks = seq(0, 28, by=0.5), main="T^3 l_rate=0.5 eta=4.0")

#-------------------------------
#別のサブサンプルでヒストグラム-----
#l_rate = 0.5, eta = 6.5で変化後の400点3次元トーラス
#t3orus4_list3_81_inst$subsamples[[2]]$distmat<-t3orus4_list3_81_inst$subsamples[[2]]$data %>% dist() %>% as.matrix()
#t3orus4_list3_81_inst$subsamples[[2]]$change_dist(l_rate = 0.5, eta = 6.5)
t3orus4_list3_81_inst$subsamples[[2]]$data %>% dist() %>% hist(breaks = seq(0, 28, by=0.5))
t3orus4_list3_81_inst$subsamples[[2]]$distmat %>% as.dist() %>% hist(breaks = seq(0, 28, by=0.5))

t3orus4_list3_81_inst$subsamples[[2]]$calc_pd(maxdim = 3, maxscale = 9)

#------------------------
#変化前----------
t3orus4_list3_81_sub2<-TDAdataset$new(t3orus4_list3_81_inst$subsamples[[2]]$data)
t3orus4_list3_81_sub2$calc_pd(maxdim = 3, maxscale = 9)

#3次のパーシステントホモロジー群を抽出
t3orus4_list3_81_sub2_pdH3<-which(t3orus4_list3_81_sub2$get_pd()[, 1] == 3) %>% 
  t3orus4_list3_81_sub2$get_pd()[., ]

#生成時刻の距離が含まれているセル
b_cell3<-map_lgl(seq_along(cell), ~{any(t3orus4_list3_81_sub2_pdH3[, 2] > cell[.]) & any(t3orus4_list3_81_sub2_pdH3[, 2] <= cell[. + 1])}) %>% which()

#消滅時刻の距離が含まれているセル
d_cell3<-map_lgl(seq_along(cell), ~{any(t3orus4_list3_81_sub2_pdH3[, 3] > cell[.]) & any(t3orus4_list3_81_sub2_pdH3[, 3] <= cell[. + 1])}) %>% which()

#変化前の距離のヒストグラム
#カラーコード#00a0e9=シアン、#e4007f=マゼンダ、4D＝アルファ値30%
cell_col3b<-t3orus4_list3_81_sub2_dist_hist$counts %>% length() %>% rep(NA, length = .)
cell_col3b[b_cell3]<-"#e4007f4d"

cell_col3d<-t3orus4_list3_81_sub2_dist_hist$counts %>% length() %>% rep(NA, length = .)
cell_col3d[d_cell3]<-"#00a0e94d"

t3orus4_list3_81_sub2_dist_hist<-t3orus4_list3_81_sub2$distmat %>% 
  as.dist() %>% hist(breaks = seq(0, 28, by=0.5), main="T^3 l_rate=0.5 eta=6.5", col = cell_col3b)

t3orus4_list3_81_sub2$distmat %>% as.dist() %>% hist(breaks = seq(0, 28, by=0.5), col = cell_col3d, add = T)

#生成時刻と消滅時刻をセットで垂直線をプロット
for (i in seq_len(nrow(t3orus4_list3_81_sub2_pdH3))) {
  abline(v = t3orus4_list3_81_sub2_pdH3[i, 2], col = rainbow(nrow(t3orus4_list3_81_sub2_pdH3))[i] )
  abline(v = t3orus4_list3_81_sub2_pdH3[i, 3], col = rainbow(nrow(t3orus4_list3_81_sub2_pdH3))[i] )
}

# #3次のパーシステントホモロジー群を抽出
# t3orus4_list3_81_altsub2_pdH3<-which(t3orus4_list3_81_inst$subsamples[[2]]$get_pd()[, 1] == 3) %>% 
#   t3orus4_list3_81_inst$subsamples[[2]]$get_pd()[., ]
# 
# #生成時刻の距離が含まれているセル
# b_cell3<-map_lgl(seq_along(cell), ~{any(t3orus4_list3_81_altsub2_pdH3[, 2] > cell[.]) & any(t3orus4_list3_81_altsub2_pdH3[, 2] <= cell[. + 1])}) %>% which()
# 
# #消滅時刻の距離が含まれているセル
# d_cell3<-map_lgl(seq_along(cell), ~{any(t3orus4_list3_81_altsub2_pdH3[, 3] > cell[.]) & any(t3orus4_list3_81_altsub2_pdH3[, 3] <= cell[. + 1])}) %>% which()
# 
# #変化後の距離のヒストグラム
# #カラーコード#00a0e9=シアン、#e4007f=マゼンダ、4D＝アルファ値30%
# cell_col2b<-t3orus4_list3_81_sub1_altdist_hist$counts %>% length() %>% rep(NA, length = .)
# cell_col2b[b_cell2]<-"#e4007f4d"
# 
# cell_col2d<-t3orus4_list3_81_sub1_altdist_hist$counts %>% length() %>% rep(NA, length = .)
# cell_col2d[d_cell2]<-"#00a0e94d"
# 
# t3orus4_list3_81_sub1_altdist_hist<-t3orus4_list3_81_inst$subsamples[[1]]$distmat %>% 
#   as.dist() %>% hist(breaks = seq(0, 28, by=0.5), main="T^3 l_rate=0.5 eta=6.5", col = cell_col2b)
# 
# t3orus4_list3_81_inst$subsamples[[1]]$distmat %>% as.dist() %>% hist(breaks = seq(0, 28, by=0.5), col = cell_col2d, add = T)
# 
# l_idx<-t3orus4_list3_81_inst$subsamples[[2]]$get_param()$l_idx %>% as.integer()
# plot(x = t3orus4_list3_81_sub2$distmat[l_idx[1:3], ], y = t3orus4_list3_81_inst$subsamples[[2]]$distmat[l_idx[1:3], ], col = 2)
# points(t3orus4_list3_81_sub2$distmat[l_idx[1:3], ], t3orus4_list3_81_sub2$distmat[l_idx[1:3], ])
# points(6.5, 6.5*( 1-exp(-1) ), col = 4, pch = 17, cex = 1.5)

#-------------------------------
#別のサブサンプルでヒストグラム-----
#距離変化はl_rate = 0.5, eta = 4.0

#t3orus4_list3_81_inst$subsamples[[3]]$change_dist(l_rate = 0.5, eta = 4.0)
t3orus4_list3_81_inst$subsamples[[3]]$data %>% dist() %>% hist(breaks = seq(0, 28, by=0.5))
t3orus4_list3_81_inst$subsamples[[3]]$distmat %>% as.dist() %>% hist(breaks = seq(0, 28, by=0.5))

#t3orus4_list3_81_inst$subsamples[[4]]$change_dist(l_rate = 0.5, eta = 4.0)
t3orus4_list3_81_inst$subsamples[[4]]$data %>% dist() %>% hist(breaks = seq(0, 28, by=0.5))
t3orus4_list3_81_inst$subsamples[[4]]$distmat %>% as.dist() %>% hist(breaks = seq(0, 28, by=0.5))

