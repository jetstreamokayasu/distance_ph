#R6クラス試し

trs300_1_10<-TDAdataset$new(torus300_colle_set[[1]][[10]][["noizyX"]])


temp_dist<-landmark_points(X = trs300_1_10_dist, n_land = nrow(trs300_1_10_dist)*0.1, d_mat = T) %>% 
  dist_wvr_change(X_dist = trs300_1_10_dist, lands = ., eta = 3)

trs300_1_10_dist_cls<-DistmatPD$new(distmat = trs300_1_10_dist)

anu_dist_cls<-DistmatPD$new(distmat = anu_dist)

trs300_1_10_subs<-trs300_1_10$subsamples

tmp<-map(trs300_1_10$subsamples, ~{.$distmat_c$calc_pd(maxdim=2, maxscale=3)})

tmp2<-map_dbl(trs300_1_10$subsamples, ~{.$distmat_c$get_thresh(1)}) %>% max()
