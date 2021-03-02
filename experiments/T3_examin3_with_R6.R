#R6クラス試し

trs300_1_10<-TDAdataset$new(torus300_colle_set[[1]][[10]][["noizyX"]])

temp_dist<-landmark_points(X = trs300_1_10_dist, n_land = nrow(trs300_1_10_dist)*0.1, d_mat = T) %>% 
  dist_wvr_change(X_dist = trs300_1_10_dist, lands = ., eta = 3)

trs300_1_10_dist_cls<-DistmatPD$new(distmat = trs300_1_10_dist)

anu_dist_cls<-DistmatPD$new(distmat = anu_dist)

trs300_1_10_subs<-trs300_1_10$subsamples

tmp<-map(trs300_1_10$subsamples, ~{.$distmat_c$calc_pd(maxdim=2, maxscale=3)})

tmp2<-map_dbl(trs300_1_10$subsamples, ~{.$distmat_c$get_thresh(1)}) %>% max()


test_mat<-matrix(0, 2, 2)

names(test_mat)<-"test"

attr(test_mat, "distmat")<-"changed"

#----------------------
#smooth_landscape_method_paralの改良試し
trs300_1_10_inst<-TDAdataset$new(torus300_colle_set[[1]][[10]][["noizyX"]])

trs300_1_10_test_aggr<-smoothed_landscape_homology(X = seephacm:::bootstrapper(X = torus300_colle_set[[1]][[10]][["noizyX"]], size = 240, samples = 2), maxdim = 2, maxscale = 3)

t_v<-numeric(3)
names(t_v)<-paste0("H", 1:3)
t_mat<-matrix(0, 1, 4)
colnames(t_mat)<-c(paste0("H", 1:3), "time")

t1<-Sys.time()
t2<-Sys.time()

#--------------------------
#Rファイルを読み込めるか試し

rfile<-scan(file = "./pack.R", what = "")
some(rfile, ~{str_detect(string = ., pattern = "usephacm")}) %>% any()

#seephacmパッケージから関数が用いられているファイルを探す

seephacm_files<-map(list.files(pattern = ".R"), ~{scan(file = ., what = "")}) %>% 
  map_lgl(., ~{some(., ~str_detect(string = ., pattern = "seephacm"))}) %>% which() %>% list.files(pattern = ".R")[.]

seephacm_files2<-map(list.files(pattern = ".R"), ~{read_file(file = .)}) %>% 
  map_lgl(., ~{str_detect(string = ., pattern = "seephacm")}) %>% which() %>% list.files(pattern = ".R")[.]

seephacm_files<-map(list.files(pattern = ".R"), ~{read_file(file = .)}) %>% 
  map_lgl(., ~str_detect(string = ., pattern = "usephacm")) %>% which() %>% list.files(pattern = ".R")[.]

missing_writed_files<-map(list.files(pattern = ".R"), ~{read_file(file = .)}) %>% 
  map_lgl(., ~str_detect(string = ., pattern = "missing")) %>% which() %>% list.files(pattern = ".R")[.]



#----------------------------

torus1<-torusUnif(500, 1, 2.5)
plot3d(torus1)
aspect3d("iso")

torus1_inst<-TDAdataset$new(torus1)

torus1_inst$plot_data()

torus1_inst$calc_pd(maxdim = 2, maxscale = 3)
torus1_inst$plot_pd()
torus1_inst$plot_bar()
torus1_inst$plot_pl()
torus1_inst$pl_peak_count()

torus1_inst$plot_diag()

#---------------------------
#立方体表面上一様分布関数テスト

cuboid1<-xRect_unif(n = 500, sides = 1)

cuboid2<-xRect_unif(n = 500, sides = 1:3)

cuboid3<-xRect_unif(n = 500, sides = 1, d = 3)

cuboid4<-xRect_unif(n = 500, sides = 1:3, d = 3)

cuboid5<-xRect_unif(n = 500, sides = 1, d = 4)

cuboid6<-xRect_unif(n = 300, sides = c(1, 1, 5), d = 3)
cuboid6_inst<-TDAdataset$new(cuboid6$rect)
calculate_homology(cuboid6$rect, dim = 2) %>% cuboid6_inst$input_pd()

cuboid7<-xRect_unif(n = 200, sides = c(1, 1, 8), d = 3)

cuboid8<-xRect_unif(n = 500, sides = 2:5, d = 4) 
cuboid8_inst<-TDAdataset$new(cuboid8$rect)
calculate_homology(cuboid8$rect, dim = 3) %>% cuboid8_inst$input_pd()

rect1<-xRect_unif(n = 100, sides = 1, d = 2)

#----------------------------------
#
domain
plot(domain, domain*(1-exp(-(domain/3.0)^2)))
points(domain, domain*(1-exp(-(domain/3.0)^2)))

domain[domain < 3] %>% plot(., .*(1-exp(-(./3.0)^2)), xlim = c(0, 6.5), ylim = c(0, 6.5))
points(domain[domain >= 4], domain[domain >= 4])

domain3<-which(domain >= 3 & domain < 4) %>% domain[.]
points(1/(1+exp(-3*domain3)))

plot(domain3, 1/(1+exp(-3*(domain3-min(domain3)))))
sigm_curve<-sigmoid(domain3, 7, min(domain3))*2+mph_exp(d = max(domain[domain < 3]), eta = 3.0)-0.5
points(domain3, sigm_curve)


slope1<-(4.0-mph_exp(d = max(domain[domain < 3]), eta = 3.0))
line1<-seq(3.0, 4.0, length = 20)
points(line1, slp_seg[1]*line1+slp_seg[2])
#mph_exp<-. %>% multiply_by(., (1-exp(-(./3.0)^2)))

draw_line(x = c(3.0, mph_exp(d = max(domain[domain < 3]), eta = 3.0)), y = c(4, 4))

slp_seg<-solve(matrix(c(3, 4, 1, 1), 2, 2), matrix(c(mph_exp(3.0, 3.0), 4.0)) )

#--------------------------------
#2次元トーラスでexpフィルト時間変化及び直線変化--------

trs300_3_82_inst<-TDAdataset$new(torus300_colle_setB[[3]][[82]]) 
trs300_3_82_inst$calc_pd(maxdim = 2, maxscale = 3)
trs300_3_82_altdist<-mid_mean_attenu_slope(pd = trs300_3_82_inst$get_pd(), dim = 2, distmat = trs300_3_82_inst$distmat)
trs300_3_82_altdist_inst<-DistmatPD$new(trs300_3_82_altdist$altdist)
trs300_3_82_altdist_inst$calc_pd(maxdim = 2, maxscale = 3)

tst_mat<-matrix(domain, 10, 10)

tst_mat[tst_mat <= 3.0] %<>%  mph_exp(3.0)
tst_mat[tst_mat > 3.0 & tst_mat <= 4.0] %<>% multiply_by(slp_seg[1]) %>% add(slp_seg[2])

#--------------------------------
#3次元トーラスでexpフィルト時間変化及び直線変化--------

t3rs450_2_11_inst<-TDAdataset$new(t3orus450_list2[[11]])
t3rs450_2_11_inst$calc_pd(maxdim = 3, maxscale = 9)

t3rs450_2_11_inst$create_changed_distmat(l_rate = 0, eta = 6.5)
t3rs450_2_11_inst$alt_distmat[[1]]$distmat<-
  mid_mean_attenu_slope(pd = t3rs450_2_11_inst$get_pd(), dim = 3, distmat = t3rs450_2_11_inst$distmat)$altdist

t3rs450_2_11_inst$alt_distmat[[1]]$calc_pd(maxdim = 3, maxscale = 9)

#発生時刻と消滅時刻の中点の平均値を距離減衰度etaとする距離操作-------
#eta以上の距離に対しては変化を施さない
#3次元トーラスで試す
#3次ベッチ数について
t3rs450_2_11_altdist<-mid_median_attenu(pd = t3rs450_2_11_inst$get_pd(), dim = 3, distmat = t3rs450_2_11_inst$distmat, type = "mean")
t3rs450_2_11_altdist_inst<-DistmatPD$new(t3rs450_2_11_altdist$altdist)
t3rs450_2_11_altdist_inst$calc_pd(maxdim = 3, maxscale = 9)
