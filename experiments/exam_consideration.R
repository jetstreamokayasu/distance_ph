#--------------------------------------
#PDの代表例を出力する----------------

#密度の高い例として500点トーラスPDを計算(していない)

#-----------------------------------------
#2次元トーラス------------------
#CTIC2019手法で失敗したデータ
ctic_fail<-which(trs300_colle1_ctic_aggr[[2]] < 0.5 | trs300_colle1_ctic_aggr[[2]] >= 1.5) 

#補間手法で成功したデータ
inter_suc<-range_index(trs300_list1to5_inted_aggr[[1]][[2]], 0.5, 1.5)

#exp距離変化手法で成功したデータ
wvr_suc<-range_index(trs300_colle1_wvr_aggr[[2]], 0.5, 1.5)


ctic_inter_wvr<-intersect(ctic_fail, inter_suc) %>% intersect(wvr_suc)

#exp距離変化手法で成功したデータのPD
trs350_col1_2_inst<-TDAdataset$new(torus350_colle_set1[[2]])
trs350_col1_5_inst<-TDAdataset$new(torus350_colle_set1[[5]])

trs300_col1_3_inst<-TDAdataset$new(torus300_colle_set[[1]][[3]][["noizyX"]])
trs300_col1_40_inst<-TDAdataset$new(torus300_colle_set[[1]][[40]][["noizyX"]])

#素のPD
# trs350_col1_2_inst$calc_pd(maxdim = 2, maxscale = 3)
# trs350_col1_5_inst$calc_pd(maxdim = 2, maxscale = 3)

trs300_col1_3_inst$calc_pd(maxdim = 2, maxscale = 3)

#PDのプロット
par(cex.lab = 1.5, mgp = c(2.5, 1, 0))

pdf("./pics/trs300_col1_3_pd.pdf", width = 8, height = 8)
par(cex.lab = 1.5, mgp = c(2.5, 1, 0))
trs300_col1_3_inst$plot_diag()
dev.off()

# trs350_col1_2_pd<-trs350_col1_2_inst$get_pd() %>% pd_conv_stats2tda()
# plot(trs350_col1_2_pd)

#graphicでPDを書き直す
abline(v=0)
plot(trs300_col1_3_inst$get_pd()[, 2:3], bty="l")
abline(a = 0, b = 1)

pdf("./pics/trs300_col1_3_pd2.pdf", width = 8, height = 8)
plot_diag(trs300_col1_3_inst$get_pd(), lim = c(0, 3.0))
dev.off()

#exp距離変化適用
trs300_col1_3_inst$create_changed_distmat(l_rate = 0.8, eta = 3.0)
trs300_col1_3_inst$alt_distmat[[1]]$calc_pd(maxdim = 2, maxscale = 3)

pdf("./pics/trs300_col1_3_altdist1_pd.pdf", width = 8, height = 8)
par(cex.lab = 1.5, mgp = c(2.5, 1, 0))
trs300_col1_3_inst$alt_distmat[[1]]$plot_diag()
dev.off()

pdf("./pics/trs300_col1_3_altdist1_pd2.pdf", width = 8, height = 8)
plot_diag(trs300_col1_3_inst$alt_distmat[[1]]$get_pd(), lim = c(0, 3.0))
dev.off()


trs300_col1_40_inst$create_changed_distmat(l_rate = 0.8, eta = 3.0)
trs300_col1_40_inst$alt_distmat[[1]]$calc_pd(maxdim = 2, maxscale = 3)

trs300_col1_40_inst$alt_distmat[[1]]$get_pd() %>% pd_conv_stats2tda() %>% plot()

#補間手法で成功したデータのPD
# trs350_colB_inted1_2<-TDAdataset$new(torus350_colle_setB_inted[[1]][[2]])
# trs350_colB_inted1_2$calc_pd(maxdim = 2, maxscale = 3)
# trs350_colB_inted1_2$get_pd() %>% pd_conv_stats2tda() %>%  plot()

trs300_colB_inted1_3<-TDAdataset$new(torus300_colle_setB_inted[[1]][[3]])
trs300_colB_inted1_3$calc_pd(maxdim = 2, maxscale = 3)

pdf("./pics/trs300_colB_inted1_3_pd.pdf", width = 8, height = 8)
par(cex.lab = 1.5, mgp = c(2.5, 1, 0))
trs300_colB_inted1_3$plot_diag()
dev.off()

#-------------------------------
#3次元トーラス------
#CTIC2019手法で失敗したデータ
t3H2_ctic_fail<-which(t3orus450_list1_1to30_normal_aggrs[[2]] < 2.5 | t3orus450_list1_1to30_normal_aggrs[[2]] >= 3.5)

t3H3_ctic_fail<-which(t3orus450_list1_1to30_normal_aggrs[[3]] < 0.5 | t3orus450_list1_1to30_normal_aggrs[[3]] >= 1.5) 

#exp距離変化手法で成功したデータ
t3H2_wvr_suc1<-range_index(t3orus450_list1_1to30_wvr_aggrs1[[2]], 2.5, 3.5)
t3H2_wvr_suc2<-range_index(t3orus450_list1_1to30_wvrH2_aggrs2[[2]], 2.5, 3.5)


t3H3_wvr_suc1<-range_index(t3orus450_list1_1to30_wvr_aggrs1[[3]], 0.5, 1.5)
t3H3_wvr_suc2<-range_index(t3orus450_list1_1to30_wvrH2_aggrs2[[3]], 0.5, 1.5)

#CTIC2019手法で2次/3次ベッチ数ともに不成功
#かつexpを掛ける距離変化手法でeta=4.0で2次ベッチ数が、eta=6.5で3次ベッチ数が成功したデータ
t3H2H3_ctic_wvr<-intersect(t3H2_ctic_fail, t3H3_ctic_fail) %>% intersect(t3H2_wvr_suc2) %>% intersect(t3H3_wvr_suc1)

t3rs450_lst1_4_inst<-TDAdataset$new(t3orus450_list1[[4]])
t3rs450_lst1_4_inst$create_changed_distmat(l_rate = 0.5, eta = 4.0)
t3rs450_lst1_4_inst$create_changed_distmat(l_rate = 0.5, eta = 6.5)

t3rs450_lst1_4_inst$calc_pd(maxdim = 3, maxscale = 9)
t3rs450_lst1_4_inst$alt_distmat[[1]]$calc_pd(maxdim = 3, maxscale = 9)
t3rs450_lst1_4_inst$alt_distmat[[2]]$calc_pd(maxdim = 3, maxscale = 9)

pdf("./pics/t3rs450_lst1_4_pd.pdf", width = 8, height = 8)
par(cex.lab = 1.5, mgp = c(2.5, 1, 0), cex = 1)
t3rs450_lst1_4_inst$plot_diag()
dev.off()

pdf("./pics/t3rs450_lst1_4_pd2.pdf", width = 8, height = 8)
plot_diag(t3rs450_lst1_4_inst$get_pd(), lim = c(0, 8.1))
dev.off()


pdf("./pics/t3rs450_lst1_4_pdH2.pdf", width = 8, height = 8)
par(cex.lab = 1.5, mgp = c(2.5, 1, 0), cex = 1)
t3rs450_lst1_4_inst$alt_distmat[[1]]$plot_diag()
dev.off()

pdf("./pics/t3rs450_lst1_4_pdH2_2.pdf", width = 8, height = 8)
plot_diag(t3rs450_lst1_4_inst$alt_distmat[[1]]$get_pd(), lim = c(0, 8.1))
dev.off()


pdf("./pics/t3rs450_lst1_4_pdH3.pdf", width = 8, height = 8)
par(cex.lab = 1.5, mgp = c(2.5, 1, 0), cex = 1)
t3rs450_lst1_4_inst$alt_distmat[[2]]$plot_diag()
dev.off()

pdf("./pics/t3rs450_lst1_4_pdH3_2.pdf", width = 8, height = 8)
plot_diag(t3rs450_lst1_4_inst$alt_distmat[[2]]$get_pd(), lim = c(0, 8.1))
dev.off()

#-----------------------------------------
#楕円体------------------
#CTIC2019手法で失敗したデータ
ellip_ctic_fail<-which(ellip150_list2to5_aggr[[1]][[2]] < 0.5 | ellip150_list2to5_aggr[[1]][[2]] >= 1.5) 

#補間手法で成功したデータ
ellip_inter_suc<-range_index(ellip150_list2to5_inted_aggr[[1]][[2]], 0.5, 1.5)

#exp距離変化手法で成功したデータ
ellip_wvr_suc<-range_index(ellip150_list2to5_wvr_aggr[[1]][[2]], 0.5, 1.5)

ellip_ctic_inter_wvr<-intersect(ellip_ctic_fail, ellip_inter_suc) %>% intersect(ellip_wvr_suc)

elp150_lst2_5_inst<-TDAdataset$new(ellip150_list2to5[[1]][[5]])
elp150_lst2_5_inst$calc_pd(maxdim = 2, maxscale = 3)

elp150_lst2_5_diag<-elp150_lst2_5_inst$get_pd() %>% pd_conv_stats2tda()
attr(elp150_lst2_5_diag, "scale")<-c(0.0, 2.0)
plot(elp150_lst2_5_diag)
legend("bottomright", legend = c("dim0", "dim1", "dim2", "dim3")[1:3], 
       col=c(1, 2, 4, 3)[1:3], pch=c(16, 2, 23, 3)[1:3], cex=1.5)

pdf("./pics/elp150_lst2_5_pd.pdf", width = 8, height = 8)
par(cex.lab = 1.5, mgp = c(2.5, 1, 0), cex = 1)
elp150_lst2_5_inst$plot_diag()
dev.off()

pdf("./pics/elp150_lst2_5_pd2.pdf", width = 8, height = 8)
plot_diag(elp150_lst2_5_inst$get_pd(), lim = c(0, 2.0))
dev.off()

elp150_lst2_5_inst$create_changed_distmat(l_rate=0.45, eta=3.7)
elp150_lst2_5_inst$alt_distmat[[1]]$calc_pd(maxdim = 2, maxscale = 3)

elp150_lst2_5_altdist_diag<-elp150_lst2_5_inst$alt_distmat[[1]]$get_pd() %>% pd_conv_stats2tda()
attr(elp150_lst2_5_altdist_diag, "scale")<-c(0.0, 2.0)
plot(elp150_lst2_5_altdist_diag)

pdf("./pics/elp150_lst2_5_altdist_pd.pdf", width = 8, height = 8)
par(cex.lab = 1.5, mgp = c(2.5, 1, 0), cex = 1)
elp150_lst2_5_inst$alt_distmat[[1]]$plot_diag()
dev.off()

pdf("./pics/elp150_lst2_5_altdist_pd2.pdf", width = 8, height = 8)
plot_diag(elp150_lst2_5_inst$alt_distmat[[1]]$get_pd(), lim = c(0, 2.0))
dev.off()

elp150_lst2_5_inted<-TDAdataset$new(ellip150_list2to5_inted[[1]][[5]])
elp150_lst2_5_inted$calc_pd(maxdim = 2, maxscale = 3)

pdf("./pics/elp150_lst2_5_inted_pd.pdf", width = 8, height = 8)
par(cex.lab = 1.5, mgp = c(2.5, 1, 0), cex = 1)
elp150_lst2_5_inted$plot_diag()
dev.off()

#------------------
#4次元直方体--------
#250点

#ctic手法で失敗
cube_ctic_fail<-which((cube4d_250_lst1_1to20_aggr[[4]] < 0.5) | (cube4d_250_lst1_1to20_aggr[[4]] >= 1.5))

#結合時刻早期化手法で成功
cube_wvr_suc<-range_index(cube4d_250_lst1_1to20_wvr_aggr2[[4]], min = 0.5, max = 1.5)

cube4d250_8_inst<-TDAdataset$new(cube4d_250_lst1[[8]])

cube4d250_8_inst$create_changed_distmat(l_rate = 0.5, eta = 1.4)

{
  cube4d250_8_inst$calc_pd(maxdim = 4, maxscale = 2)
  cube4d250_8_inst$alt_distmat[[1]]$calc_pd(maxdim = 4, maxscale = 2)
}


pdf("./pics/cube4d250_8_pd.pdf", width = 8, height = 8)
plot_diag(cube4d250_8_inst$get_pd(), lim = c(0, 1.2))
dev.off()

pdf("./pics/cube4d250_8_altdist_pd.pdf", width = 8, height = 8)
plot_diag(cube4d250_8_inst$alt_distmat[[1]]$get_pd(), lim = c(0, 1.2))
dev.off()
