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
