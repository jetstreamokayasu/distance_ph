#R6クラス試し

trs300_1_10<-TDAdataset$new(torus300_colle_set[[1]][[10]][["noizyX"]])


temp_dist<-landmark_points(X = trs300_1_10_dist, n_land = nrow(trs300_1_10_dist)*0.1, d_mat = T) %>% 
  dist_wvr_change(X_dist = trs300_1_10_dist, lands = ., eta = 3)

trs300_1_10_dist_cls<-DistmatPD$new(distmat = trs300_1_10_dist)

anu_dist_cls<-DistmatPD$new(distmat = anu_dist)
