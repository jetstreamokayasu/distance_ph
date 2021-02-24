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
