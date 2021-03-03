#ハイパラ設定法を試す2

#-------------------
#データ点のインデックス、消滅時刻、パーシステンスのtibbleを作る-----
p_set1<-c()
for (j in 1:ncol(birth_p)) {
  
  pers<-trs300_1_82_inst$get_pd()[H2_idx[j], 3] - trs300_1_82_inst$get_pd()[H2_idx[j], 2]
  
  if(birth_p[1, j] %in% p_set1[, 1]){
    
    if(pers > p_set1[p_set1[, 1]==birth_p[1, j], 3]){

      death<-trs300_1_82_inst$get_pd()[H2_idx[j], 3]
    
      p_set1[p_set1[, 1]==birth_p[1, j], ]<-c(birth_p[1, j], death, pers)
    
    }
      
  }else{
    
    death<-trs300_1_82_inst$get_pd()[H2_idx[j], 3]
    
    p_set1<-rbind(p_set1, c(birth_p[1, j], death, pers))
    
  }
  
  if(birth_p[2, j] %in% p_set1[, 1]){
    
    if(pers > p_set1[p_set1[, 1]==birth_p[2, j], 3]){
    
      death<-trs300_1_82_inst$get_pd()[H2_idx[j], 3]
      
      p_set1[p_set1[, 1]==birth_p[2, j], ]<-c(birth_p[2, j], death, pers)
      
  }
    
  }else{
    
    death<-trs300_1_82_inst$get_pd()[H2_idx[j], 3]
    
    p_set1<-rbind(p_set1, c(birth_p[2, j], death, pers))
    
  }
  
}

colnames(p_set1)<-c("p_idx", "death", "persistence")

p_set1 %<>% as_tibble() 

#---------------------
#行列操作--------------

trs300_1_82_inst$create_changed_distmat(l_rate = 0, eta = 1)

trs300_1_82_inst$alt_distmat[[2]]$distmat<-trs300_1_82_inst$distmat

for (i in 1:nrow(p_set1)) {
  
  c_eta2<-p_set1$death[i]/sqrt(log(10))
  
  trs300_1_82_inst$alt_distmat[[2]]$distmat[p_set1$p_idx[i], ]<-
    trs300_1_82_inst$distmat[p_set1$p_idx[i], ]*( 1-exp( -(trs300_1_82_inst$distmat[p_set1$p_idx[i], ]/c_eta2)^2 ) )
  
  trs300_1_82_inst$alt_distmat[[2]]$distmat[, p_set1$p_idx[i]]<-
    trs300_1_82_inst$distmat[, p_set1$p_idx[i]]*( 1-exp( -(trs300_1_82_inst$distmat[, p_set1$p_idx[i]]/c_eta2)^2 ) )
  
  
}


#-------------------------------------

dist_test<-dist_element_replace1(pd = trs300_1_82_inst$get_pd(), dim = 2, distmat = trs300_1_82_inst$distmat)

dist_test2<-dist_element_replace_nodupl(pd = trs300_1_82_inst$get_pd(), dim = 2, distmat = trs300_1_82_inst$distmat)

#------------------------------
#素のpd計算結果を用いた距離操作試し----
#torus300_colle_set使用

trs300_1_83_inst<-TDAdataset$new(torus300_colle_set[[1]][[83]][["noizyX"]])
trs300_1_76_inst<-TDAdataset$new(torus300_colle_set[[1]][[76]][["noizyX"]])

#重複あり距離操作
trs300_1_76_inst$create_changed_distmat(l_rate = 0, eta = 1)
trs300_1_76_inst$alt_distmat[[1]]$distmat<-dist_element_replace1(pd = trs300_1_76_inst$get_pd(), dim = 2, distmat = trs300_1_76_inst$distmat)

trs300_1_76_inst$alt_distmat[[2]]$distmat<-dist_element_replace_nodupl(pd = trs300_1_76_inst$get_pd(), dim = 2, distmat = trs300_1_76_inst$distmat)

birth_p4<-which(trs300_1_76_inst$get_pd()[, 1]==2) %>% 
  sapply(., function(i)which(trs300_1_76_inst$distmat == trs300_1_76_inst$get_pd()[i, 2], arr.ind = T))

#------------------------------
ball1<-xBall_unif(200, r = 1)

ball1_inst<-TDAdataset$new(ball1)

#ball1_inst$create_changed_distmat(l_rate = 0, eta = 1)
ball1_inst$alt_distmat[[1]]$distmat<-dist_element_replace1(pd = ball1_inst$get_pd(), dim = 2, distmat = ball1_inst$distmat)

ball1_inst$alt_distmat[[2]]$distmat<-dist_element_replace_nodupl(pd = ball1_inst$get_pd(), dim = 2, distmat = ball1_inst$distmat)

ball2<-xBall_unif(n = 500)

ball2_inst<-TDAdataset$new(ball2)

ball2_inst$alt_distmat[[1]]$distmat<-dist_element_replace1(pd = ball2_inst$get_pd(), dim = 2, distmat = ball2_inst$distmat)

ball2_inst$alt_distmat[[2]]$distmat<-dist_element_replace_nodupl(pd = ball2_inst$get_pd(), dim = 1, distmat = ball2_inst$distmat)

ball3<-xBall_unif(n = 800)
ball3_inst<-TDAdataset$new(ball3)

#3次元トーラス
t3orus4_list3_15$alt_distmat[[4]]$distmat<-dist_element_replace_nodupl(pd = t3orus4_list3_15$get_pd(), dim = 3, distmat = t3orus4_list3_15$distmat)


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

t3rs450_2_11_altdist_birth_ave<-mid_median_attenu(pd = t3rs450_2_11_inst$get_pd(), dim = 3, distmat = t3rs450_2_11_inst$distmat, type = "birth")

t3rs450_2_11_altdist_bave<-DistmatPD$new(t3rs450_2_11_altdist_birth_ave$altdist)
t3rs450_2_11_altdist_bave$calc_pd(maxdim = 3, maxscale = 9)

t3rs450_2_11_altdist_slope1<-mid_mean_attenu_slope(pd = t3rs450_2_11_inst$get_pd(), dim = 3, distmat = t3rs450_2_11_inst$distmat, type = "birth")

t3rs450_2_11_altdist_slp1<-DistmatPD$new(t3rs450_2_11_altdist_slope1$altdist)
t3rs450_2_11_altdist_slp1$calc_pd(maxdim = 3, maxscale = 9)
 