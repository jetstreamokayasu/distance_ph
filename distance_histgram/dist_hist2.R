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

plot_per_barc(trs300_1_10$get_pd(), xlim = c(0, 3.5))
abline(v = 3.0, col = "orange")

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
#t3orus4_list3_47----
t3orus4_list3_47_inst<-TDAdataset$new(t3orus4_list3[[47]][["noizyX"]])
t3orus4_list3_47_inst$calc_pd(maxdim = 3, maxscale = 9)

t3orus4_list3_47_hist_H3<-
  colored_birth_death_cell_hist(data = t3orus4_list3_47_inst, dim = 3, breaks = seq(0, 28, by=0.5), 
                                m_title = "original 3d-torus", eta = 6.5, barcode = T)

plot_per_barc(pd = t3orus4_list3_47_inst$get_pd(), dim = 3)
plot_per_barc(pd = t3orus4_list3_47_inst$get_pd())
abline(v = 6.5, col = "orange")
plot_per_barc(pd = t3orus4_list3_47_inst$get_pd(), dim = 2)
abline(v = 4.0, col = "blueviolet")

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

for (pd in t3orus4_list3_20to24_list) {
  plot_per_barc(pd$get_pd())
  abline(v = 6.5, col = "violet")#H3
  abline(v = 4.0, col = "orange")#H2
}

for (pd in t3orus4_list3_20to24_list) {
  plot_per_barc(pd$get_pd(), dim = 2)
  abline(v = 4.0, col = "orange")
}

for (pd in t3orus4_list3_20to24_list) {
  plot_per_barc(pd$get_pd(), dim = 3, lwd = 4)
  abline(v = 6.5, col = "violet")
}

#------------------
#2次元トーラスtrs300_1_18_inst-----
trs300_1_18_inst<-TDAdataset$new(torus300_colle_set[[1]][[10]][["noizyX"]])
trs300_1_18_inst$calc_pd(maxdim = 2, maxscale = 3)

trs300_1_18_inst_hist_H2<-
  colored_birth_death_cell_hist(data = trs300_1_18_inst, dim = 2, breaks = seq(0, 7, by=0.1), 
                                m_title = "original 3d-torus", eta = 3.0, barcode = T)

plot_per_barc(pd = trs300_1_18_inst$get_pd(), dim = 2)

plot_per_barc(pd = trs300_1_18_inst$get_pd(), xlim = c(0, 3.5))
abline(v= 3.0, col ="orange")

#--------------------------------------
#plot_per_barcで、パーシステンスの次元が3を超えても動作するか確認-----
tst_pd<-trs300_1_18_inst$get_pd()[, 1] %>% equals(2) %>% trs300_1_18_inst$get_pd()[., ]
tst_pd[, 1]<-6
plot_per_barc(tst_pd)

tst_pd2<-trs300_1_18_inst$get_pd()[, 1] %>% equals(1) %>% trs300_1_18_inst$get_pd()[., ]

plot_per_barc(pd = rbind(tst_pd2, tst_pd))

#---------------------------------------------------
#発生時刻と消滅時刻の中点の中央値を距離減衰度etaとする距離操作-------
#eta以上の距離に対しては変化を施さない
#trs300_1_10で試す

trs300_1_10_pdH2<-trs300_1_10$get_pd()[trs300_1_10$get_pd()[,1]==2, ]
trs300_1_10_pdH2_mid<-apply(trs300_1_10_pdH2, 1, function(x){(x[2]+x[3])/2})

trs300_1_10_pdH2_mid_median<-median(trs300_1_10_pdH2_mid)
trs300_1_10_altdist<-trs300_1_10$distmat

trs300_1_10_altdist[trs300_1_10_altdist <= trs300_1_10_pdH2_mid_median]<-
  trs300_1_10_altdist[trs300_1_10_altdist <= trs300_1_10_pdH2_mid_median]*( 1-exp(-(trs300_1_10_altdist[trs300_1_10_altdist <= trs300_1_10_pdH2_mid_median]/trs300_1_10_pdH2_mid_median)^2) )

trs300_1_10_altdist_inst<-DistmatPD$new(trs300_1_10_altdist)

trs300_1_10_altdist2<-mid_median_attenu(pd = trs300_1_10$get_pd(), dim = 2, distmat = trs300_1_10$distmat)

#発生時刻と消滅時刻の中点の中央値を距離減衰度etaとする距離操作-------
#eta以上の距離に対しては変化を施さない
#3次元トーラスt3orus4_list3_20to24_listで試す
#3次ベッチ数について

#t3orus4_list3_20
t3rs4_lst3_20_pdH3<-t3orus4_list3_20to24_list[[1]]$get_pd()[, 1] %>% equals(3) %>% t3orus4_list3_20to24_list[[1]]$get_pd()[., ]

t3rs4_lst3_20_pdH3_mid_med<-apply(t3rs4_lst3_20_pdH3, 1, function(x){(x[2]+x[3])/2}) %>% median()

t3rs4_lst3_20_altdist<-t3orus4_list3_20to24_list[[1]]$distmat

t3rs4_lst3_20_altdist[t3rs4_lst3_20_altdist <= t3rs4_lst3_20_pdH3_mid_med]<-
  t3rs4_lst3_20_altdist[t3rs4_lst3_20_altdist <= t3rs4_lst3_20_pdH3_mid_med]*( 1-exp(-(t3rs4_lst3_20_altdist[t3rs4_lst3_20_altdist <= t3rs4_lst3_20_pdH3_mid_med]/t3rs4_lst3_20_pdH3_mid_med)^2) )

t3rs4_lst3_20_altdist_inst<-DistmatPD$new(t3rs4_lst3_20_altdist)
t3rs4_lst3_20_altdist_inst$calc_pd(maxdim = 3, maxscale = 9)

t3rs4_lst3_20_altdist2<-mid_median_attenu(pd = t3orus4_list3_20to24_list[[1]]$get_pd(), dim = 3, distmat = t3orus4_list3_20to24_list[[1]]$distmat)

t3rs4_lst3_20_altdist2_inst<-DistmatPD$new(t3rs4_lst3_20_altdist2$altdist)
t3rs4_lst3_20_altdist2_inst$calc_pd(maxdim = 3, maxscale = 9)

#t3orus4_list3_20
#平均値でやってみる
#うまくいかない
t3rs4_lst3_20_altdist_mean<-mid_median_attenu(pd = t3orus4_list3_20to24_list[[1]]$get_pd(), dim = 3, distmat = t3orus4_list3_20to24_list[[1]]$distmat, type = "mean")

t3rs4_lst3_20_altdist_mean_inst<-DistmatPD$new(t3rs4_lst3_20_altdist_mean$altdist)
t3rs4_lst3_20_altdist_mean_inst$calc_pd(maxdim = 3, maxscale = 9)

#t3orus4_list3_21
#これはうまくいく
t3rs4_lst3_21_altdist<-mid_median_attenu(pd = t3orus4_list3_20to24_list[[2]]$get_pd(), dim = 3, distmat = t3orus4_list3_20to24_list[[2]]$distmat)

t3rs4_lst3_21_altdist_inst<-DistmatPD$new(t3rs4_lst3_21_altdist$altdist)
t3rs4_lst3_21_altdist_inst$calc_pd(maxdim = 3, maxscale = 9)

#t3orus4_list3_22
#うまくいく
t3rs4_lst3_22_altdist<-mid_median_attenu(pd = t3orus4_list3_20to24_list[[3]]$get_pd(), dim = 3, distmat = t3orus4_list3_20to24_list[[3]]$distmat)

t3rs4_lst3_22_altdist_inst<-DistmatPD$new(t3rs4_lst3_22_altdist$altdist)
t3rs4_lst3_22_altdist_inst$calc_pd(maxdim = 3, maxscale = 9)

#t3orus4_list3_23
#うまくいかない
t3rs4_lst3_23_altdist<-mid_median_attenu(pd = t3orus4_list3_20to24_list[[4]]$get_pd(), dim = 3, distmat = t3orus4_list3_20to24_list[[4]]$distmat)

t3rs4_lst3_23_altdist_inst<-DistmatPD$new(t3rs4_lst3_23_altdist$altdist)
t3rs4_lst3_23_altdist_inst$calc_pd(maxdim = 3, maxscale = 9)

#t3orus4_list3_24
#うまくいく
t3rs4_lst3_24_altdist<-mid_median_attenu(pd = t3orus4_list3_20to24_list[[5]]$get_pd(), dim = 3, distmat = t3orus4_list3_20to24_list[[5]]$distmat)

t3rs4_lst3_24_altdist_inst<-DistmatPD$new(t3rs4_lst3_24_altdist$altdist)
t3rs4_lst3_24_altdist_inst$calc_pd(maxdim = 3, maxscale = 9)


#変化したデータ点数をカウント
t3rs4_lst3_20_dist_low<-t3orus4_list3_20to24_list[[1]]$distmat
t3rs4_lst3_20_dist_low[upper.tri(t3rs4_lst3_20_dist_low)]<-0
mdfy_idx<-which(t3rs4_lst3_20_dist_low <= t3rs4_lst3_20_pdH3_mid_med, arr.ind = T)
mdfy_idx<-c(mdfy_idx[, 1], mdfy_idx[, 2]) %>% unique()

#修論距離操作と比較
t3orus4_list3_20to24_list[[1]]$create_changed_distmat(l_rate = 0.5, eta = 6.5)

t3orus4_list3_20to24_list[[1]]$alt_distmat[[1]]$calc_pd(maxdim = 3, maxscale = 9)

#減衰度を中点の中央値にしてみる
t3orus4_list3_20to24_list[[1]]$create_changed_distmat(l_rate = 0.5, eta = t3rs4_lst3_20_altdist2$median)

t3orus4_list3_20to24_list[[1]]$alt_distmat[[2]]$calc_pd(maxdim = 3, maxscale = 9)

#全体に適用
t3rs4_lst3_20_altdist3<-t3orus4_list3_20to24_list[[1]]$distmat
t3rs4_lst3_20_altdist3<-t3rs4_lst3_20_altdist3*( 1-exp( -((t3rs4_lst3_20_altdist3/t3rs4_lst3_20_pdH3_mid_med)^2) ) )

t3rs4_lst3_20_altdist3_inst<-DistmatPD$new(t3rs4_lst3_20_altdist3)
t3rs4_lst3_20_altdist3_inst$calc_pd(maxdim = 3, maxscale = 9)

#発生時刻と消滅時刻の中点の中央値を距離減衰度etaとする距離操作-------
#eta以上の距離に対しては変化を施さない
#3次元トーラスt3orus4_list3_20to24_listで試す
#2次ベッチ数について

#t3orus4_list3_20
#うまくいかない
t3rs4_lst3_20_altdist_H2<-mid_median_attenu(pd = t3orus4_list3_20to24_list[[1]]$get_pd(), dim = 2, distmat = t3orus4_list3_20to24_list[[1]]$distmat)

t3rs4_lst3_20_altdist_H2_inst<-DistmatPD$new(t3rs4_lst3_20_altdist_H2$altdist)
t3rs4_lst3_20_altdist_H2_inst$calc_pd(maxdim = 3, maxscale = 9)

#t3orus4_list3_21
#うまくいく
t3rs4_lst3_21_altdist_H2<-mid_median_attenu(pd = t3orus4_list3_20to24_list[[2]]$get_pd(), dim = 2, distmat = t3orus4_list3_20to24_list[[2]]$distmat)

t3rs4_lst3_21_altdist_H2_inst<-DistmatPD$new(t3rs4_lst3_21_altdist_H2$altdist)
t3rs4_lst3_21_altdist_H2_inst$calc_pd(maxdim = 3, maxscale = 9)

#t3orus4_list3_22
t3rs4_lst3_22_altdist_H2<-mid_median_attenu(pd = t3orus4_list3_20to24_list[[3]]$get_pd(), dim = 2, distmat = t3orus4_list3_20to24_list[[3]]$distmat)

t3rs4_lst3_22_altdist_H2_inst<-DistmatPD$new(t3rs4_lst3_22_altdist_H2$altdist)
t3rs4_lst3_22_altdist_H2_inst$calc_pd(maxdim = 3, maxscale = 9)

#-------------------------------------
#2次元トーラスで中点の中央値を距離減衰度etaとするfiltlation速度変化---------
#バーコードにeta垂直線を追加した図も描く
#trs300_1_10, torus300_colle_set[[1]][10:15]を使う

trs300_lst3_10to15_list<-lapply(torus300_colle_set[[1]][10:15], function(X){
  
  X_inst<-TDAdataset$new(X$noizyX)
  X_inst$calc_pd(maxdim = 2, maxscale = 3)
  
  return(X_inst)
  
})

for (pd in trs300_lst3_10to15_list) {
  plot_per_barc(pd$get_pd(), xlim = c(0, 3.5))
  abline(v = 3.0, col = "orange")
}

for (pd in trs300_lst3_10to15_list) {
  plot_per_barc(pd$get_pd(), xlim = c(0, 3.5), dim = 2)
  abline(v = 3.0, col = "orange")
}

trs300_lst3_10to15_velo_mdfys<-lapply(trs300_lst3_10to15_list, function(Y){
  
  alt_dist<-mid_median_attenu(pd = Y$get_pd(), dim = 2, distmat = Y$distmat)
  alt_dist_inst<-DistmatPD$new(alt_dist$altdist)
  alt_dist_inst$calc_pd(maxdim = 2, maxscale = 3)
  
  return(alt_dist_inst)
  
})
  


#日誌用グラフ作成----------
#3次元トーラスのフィルトレーション速度変化に関するグラフ
for (i in 1:4) {
  t3orus4_list3_20to24_list[[i]]$calc_pl()
}

for (i in 1:4) {#オリジナルのPL
  png(paste0("./pics/T3_pl_H3_origi_0", i, ".png"))
  t3orus4_list3_20to24_list[[i]]$plot_pl(dim = 3, ylim = c(0, 1.2))
  dev.off()
}

for (i in 1:3) {
  X<-get(paste0("t3rs4_lst3_2", i, "_altdist_inst"))
  X$calc_pl()
}

for (i in 1:3) {#filtlation速度変化後のPL
  png(paste0("./pics/T3_pl_H3_velo_mdfy_0", i+1, ".png"))
  X<-get(paste0("t3rs4_lst3_2", i, "_altdist_inst"))
  X$plot_pl(dim = 3, ylim = c(0, 1.2))
  dev.off()
}

for (i in 1:3) {#filtlation速度変化後のPD
  X<-get(paste0("t3rs4_lst3_2", i, "_altdist_inst"))
  X$plot_pd()
  ggsave(paste0("./pics/T3_pd_H3_velo_mdfy_0", i+1, ".png"), width = 6.7, height = 6.97)
}


#2次元トーラスのフィルトレーション速度変化に関するグラフ
for (i in seq_along(trs300_lst3_10to15_velo_mdfys)) {
  trs300_lst3_10to15_velo_mdfys[[i]]$calc_pl()
}

#filtlation速度変化前のPD
for (i in 1:4) {
  trs300_lst3_10to15_list[[i]]$plot_pd()
  ggsave(paste0("./pics/T2_pd_orig_0", i, ".png"), width = 6.7, height = 6.97)
  
}

#filtlation速度変化前のPL
for (i in 1:4) {
  trs300_lst3_10to15_list[[i]]$calc_pl()
}

for (i in 1:4) {
  png(paste0("./pics/T2_pl_H2_orig_0", i, ".png"))
  trs300_lst3_10to15_list[[i]]$plot_pl(dim = 2, ylim = c(0, 0.5))
  dev.off()  
}

#filtlation速度変化後のPD
for (i in 1:4) {
  trs300_lst3_10to15_velo_mdfys[[i]]$plot_pd()
  ggsave(paste0("./pics/T2_pd_velo_mdfy_0", i, ".png"), width = 6.7, height = 6.97)
  
}

#filtlation速度変化後のPL
for (i in 1:4) {
  png(paste0("./pics/T2_pl_H2_velo_mdfy_0", i, ".png"))
  trs300_lst3_10to15_velo_mdfys[[i]]$plot_pl(dim = 2, ylim = c(0, 0.5))
  dev.off()  
}
