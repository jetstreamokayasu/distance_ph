#----------------------------------
#色を塗ったヒストグラムを描画する関数を試す-----
t3orus4_list3_81_sub1_hist<-
  colored_birth_death_cell_hist(data = t3orus4_list3_81_sub1$data, pd = t3orus4_list3_81_sub1$get_pd(), 
                                dim = 3, breaks = seq(0, 28, by=0.5), m_title = "3-d torus original", eta_line = T, eta = 6.5, pd_line = T, 
                                inflec_line = T, tile_line = T)

which(t3orus4_list3_81_sub1$get_pd()[, 1] ==3) %>% t3orus4_list3_81_sub1$get_pd()[., ] %>% as.matrix() %>% t()

#距離操作後の3次元トーラス
t3orus4_list3_81_sub1_altdist_hist2<-
  colored_birth_death_cell_hist(data = t3orus4_list3_81_inst$subsamples[[1]]$distmat, pd = t3orus4_list3_81_inst$subsamples[[1]]$get_pd(), 
                                dim = 3, breaks = seq(0, 28, by=0.5), distmat = T, 
                                m_title = expression(paste("3d-torus", ~~epsilon==0.5, ~~eta==6.5)), eta = 6.5*(1-exp(-1)), pd_line = T, 
                                inflec_line = T, tile_line = T)

#距離操作前の3次元トーラスt3orus4_list3_81_sub2
t3orus4_list3_81_sub2_hist<-
  colored_birth_death_cell_hist(data = t3orus4_list3_81_sub2$data, 
                                pd = t3orus4_list3_81_sub2$get_pd(), dim = 3, 
                                breaks = seq(0, 28, by=0.5), m_title = "original 3d torus", eta = 6.5, pd_line = F, distmat = F)

par(new=T)
plot_per_barc(pd = t3orus4_list3_81_sub2$get_pd(), dim = 3, xlim = c(5.0, 7.0), col = "red")
abline(v = 6.5, col = "green3")

#距離操作前の3次元トーラスt3orus4_list3_81_sub3
t3orus4_list3_81_sub3<-TDAdataset$new(t3orus4_list3_81_inst$subsamples[[3]]$data)
t3orus4_list3_81_sub3$calc_pd(maxdim = 3, maxscale = 9)

t3orus4_list3_81_sub3_hist<-
  colored_birth_death_cell_hist(data = t3orus4_list3_81_sub3$data, pd = t3orus4_list3_81_sub3$get_pd(), dim = 3, 
                                breaks = seq(0, 28, by=0.5), m_title = "original 3d torus", eta = 6.5, pd_line = F)
par(new=T)
plot_per_barc(pd = t3orus4_list3_81_sub3$get_pd(), dim = 3, xlim = c(0, 28), col = "red")

seq(0, 30, length = 100) %>% plot(., .*(1-exp(-(./6.5)^2)), type = "l", xlim = c(0, 10))
pers_tst<-which(t3orus4_list3_81_sub3$get_pd()[, 1]==3) %>% t3orus4_list3_81_sub3$get_pd()[., 2:3] #%>% 
  #points(., .*(1-exp(-(./(6.5))^2)), col = 2:3, pch = 16)
pers_tst2<-pers_tst*(1-exp(-(pers_tst/(6.5))^2))

#距離操作前の3次元トーラスt3orus4_list3_81_sub4
t3orus4_list3_81_sub4<-TDAdataset$new(t3orus4_list3_81_inst$subsamples[[4]]$data)
t3orus4_list3_81_sub4$calc_pd(maxdim = 3, maxscale = 9)

t3orus4_list3_81_sub4_hist<-
  colored_birth_death_cell_hist(data = t3orus4_list3_81_sub4$data, pd = t3orus4_list3_81_sub4$get_pd(), dim = 3, 
                                breaks = seq(0, 28, by=0.5), m_title = "original 3d torus", eta = 6.5, pd_line = T)

#サブサンプル前の元データt3orus4_list3_81のヒストグラム
t3orus4_list3_81_inst$calc_pd(maxdim = 3, maxscale = 9)

t3orus4_list3_81_hist<-
  colored_birth_death_cell_hist(data = t3orus4_list3_81_inst$data, pd = t3orus4_list3_81_inst$get_pd(), dim = 3, 
                                                     breaks = seq(0, 28, by=0.5), m_title = "original 3d torus", eta = 6.5, pd_line = T)

#距離操作前の2次元トーラスの距離ヒストグラム
trs300_1_10_hist<-
  colored_birth_death_cell_hist(data = trs300_1_10$data, pd = trs300_1_10$get_pd(), dim = 2, 
                                breaks = seq(0, 7, by=0.1), m_title = "original 2d-torus", eta = 3.0, pd_line = F)

trs300_1_10_pdH2<-trs300_1_10$get_pd()[, 1] %>% equals(2) %>% which() %>% trs300_1_10$get_pd()[., ]

#セルの着色がおかしいので確認
cell_t2<-seq(0, 7, by=0.1)

b_cell4<-map_lgl(seq_along(cell_t2[-1]), function(i){some(trs300_1_10_pdH2[, 2], ~{(.x > cell_t2[i]) & (.x <= cell_t2[i+1])})}) %>% which()

b_cell4_col<-cell_t2 %>% length() %>% rep(NA, length = .)
b_cell4_col[b_cell4]<-"#e4007f4d"

trs300_1_10$data %>% dist() %>% hist(breaks = cell_t2, col = b_cell4_col)

for (i in seq_len(nrow(trs300_1_10_pdH2))) {
  abline(v = trs300_1_10_pdH2[i, 2], col = rainbow(nrow(trs300_1_10_pdH2))[i])
}

(trs300_1_10$get_pd()[, 1]==2) %>% trs300_1_10$get_pd()[., ] %>%  as_diag() %>% plot(barcode = T)
abline(v = 3.0, col = "green3")
points(x = c(2.0, 2.0))

#パーシステントバーコード上にetaの垂直線を描いてみる
plot(trs300_1_10_pdH2[, 2:3], xlim = c(min(trs300_1_10_pdH2[, 2]), max(trs300_1_10_pdH2[, 3])), ylim = c(0, nrow(trs300_1_10_pdH2)+1), type = "n")

for (j in seq_len(nrow(trs300_1_10_pdH2))) {
  draw_line(x = c(trs300_1_10_pdH2[j, 2], j), y = c(trs300_1_10_pdH2[j, 3], j), col = 4, lwd = 2)
}

plot.diagram(trs300_1_10$get_pd(), barcode = T, dimension = 2)

trs300_1_10_hist<-
  colored_birth_death_cell_hist(data = trs300_1_10$data, pd = trs300_1_10$get_pd(), dim = 2, 
                                breaks = seq(0, 7, by=0.1), m_title = "original 2d-torus", eta = 3.0, pd_line = F)

par(new=T)
plot_per_barc(pd = trs300_1_10$get_pd(), dim = 2, xlim = c(0, 7), col = "red")
abline(v = 3.0, col = "green3")

#------------------------------
#ヒストグラムとパーシステントバーコードを重ねてみる------

#サブサンプルt3orus4_list3_81_sub1の2次パーシステンスを見る
t3orus4_list3_81_sub1_hist_H2<-
  colored_birth_death_cell_hist(data = t3orus4_list3_81_sub1$data, pd = t3orus4_list3_81_sub1$get_pd(), barcode = T,
                                dim = 2, breaks = seq(0, 28, by=0.5), m_title = "3-d torus original", eta_line = T, eta = 4.5, 
                                inflec_line = T, tile_line = T)
par(new=T)
plot_per_barc(pd = t3orus4_list3_81_sub1$get_pd(), dim = 2, xlim = c(0, 28), col = "red")
plot_per_barc(pd = t3orus4_list3_81_sub1$get_pd(), dim = 2)

#------------------------
#t3orus4_list3_81----
t3orus4_list3_81_hist_H2<-
  colored_birth_death_cell_hist(data = t3orus4_list3_81_inst$data, pd = t3orus4_list3_81_inst$get_pd(), dim = 2, 
                                breaks = seq(0, 28, by=0.5), m_title = "original 3d-torus", eta = 6.5, barcode = T, 
                                inflec_line = T, tile_line = T)
par(new = T)
plot_per_barc(pd = t3orus4_list3_81_inst$get_pd(), dim = 2, xlim = c(0, 28), col = "red")

#-------------------------------
#t3orus4_list3_47
t3orus4_list3_47_inst<-TDAdataset$new(t3orus4_list3[[47]][["noizyX"]])
t3orus4_list3_47_inst$calc_pd(maxdim = 3, maxscale = 9)

t3orus4_list3_47_hist_H3<-
  colored_birth_death_cell_hist(data = t3orus4_list3_47_inst, dim = 3, breaks = seq(0, 28, by=0.5), 
                                m_title = "original 3d-torus", eta = 6.5, barcode = T)

plot_per_barc(pd = t3orus4_list3_47_inst$get_pd(), dim = 3)
abline(v = 6.5)

t3orus4_list3_47_hist_H2<-
  colored_birth_death_cell_hist(data = t3orus4_list3_47_inst, dim = 2, breaks = seq(0, 28, by=0.5), 
                                m_title = "original 3d-torus", eta = 4.5, barcode = T)

#---------------------------
#3次元トーラスt3orus4_list3の20~24セット目を使ってヒストグラムを見る---

t3orus4_list3_20to24_list<-lapply(t3orus4_list3[20:24], function(X){
  
  X_inst<-TDAdataset$new(X$noizyX)
  X_inst$calc_pd(maxdim = 3, maxscale = 9)
  
  return(X_inst)
  
})

#------------------
#2次元トーラスtrs300_1_18_inst-----
trs300_1_18_inst<-TDAdataset$new(torus300_colle_set[[1]][[10]][["noizyX"]])
trs300_1_18_inst$calc_pd(maxdim = 2, maxscale = 3)

trs300_1_18_inst_hist_H2<-
  colored_birth_death_cell_hist(data = trs300_1_18_inst, dim = 2, breaks = seq(0, 7, by=0.1), 
                                m_title = "original 3d-torus", eta = 3.0, barcode = T)

plot_per_barc(pd = trs300_1_18_inst$get_pd(), dim = 2)

tst_pd<-trs300_1_18_inst$get_pd()[, 1] %>% equals(2) %>% trs300_1_18_inst$get_pd()[., ]
tst_pd[, 1]<-6

plot_per_barc(pd = tst_pd)
