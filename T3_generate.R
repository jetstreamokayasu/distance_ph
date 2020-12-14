#3次元トーラスを生成するための角度の分布生成関数のテスト

#変数の組み合わせの総当たりで最小値を求めてみる
thph<-expand.grid(seq(0, 2*pi, length=100), seq(0, 2*pi, length=100))

t3_rads<-apply(thph, 1, function(i){
  
  g0<-rad_distribute(r = 2, R1=8, R2=4, i[1], i[2])
  
  return(g0)
  
})

#3次元トーラスの極座標の角度の確率密度関数
fr<-function(rad, r, R1, R2){
  
  theta<-rad[1]
  phi<-rad[2]
  
  g<-1/R1*(1 + (r/R2)*sin(theta))*(1/R2 + (1/R1)*sin(phi)*(1 + (r/R2)*sin(theta)))
  
  return(g)
  
}

#3次元トーラスの極座標の角度の確率密度関数の偏微分値を返す関数
grr<-function(rad, r, R1, R2){
  
  theta<-rad[1]
  phi<-rad[2]
  
  f_t<-(r/(R1*R2)) * cos(theta)*( 2*cos(phi)*( (1/R1)+(r/R1*R2)*sin(theta) ) + 1/R2 )
  f_p<-cos(phi) * ( (1/R1) + (r/(R1*R2))*sin(theta) )
  
  return(c(f_t, f_p))
  
}

#関数frの最小値を求める
opt_fr<-optim(par = c(0, 0), fn = fr, r=2, R1=8, R2=4)



t3_rads2<-apply(thph, 1, function(i){
  
  g0<-rad_distribute(r=1, R1=5, R2=2, i[1], i[2])
  
  return(g0)
  
})

optim(par = c(0, 0), fn = fr, r=1, R1=5, R2=2)

# > max(t3_rads2)
# [1] 0.2399748
# > min(t3_rads2)
# [1] 0.04000504

t3orus5<-x3Dtorus_unif(n = 500, r = 1, R1 = 5, R2 = 2)

t3orus5_cls<-TDAdataset$new(t3orus5)

t3orus5_cls$calc_pd(maxdim = 3, maxscale = 6)
#calculate_homology(mat = t3orus5_cls$data, dim = 3) %>% t3orus5_cls$input_pd()

#------------------
#計算時間調べ
t3orus_tst<-x3Dtorus_unif(n = 500, r = 2, R1 = 8, R2 = 4)
t3orus_tst_inst<-TDAdataset$new(t3orus_tst)

t3orus4_list3B_7_inst<-TDAdataset$new(t3orus4_list3B[[7]])
{
start_ph<-Sys.time()
t3orus4_list3B_7_pd<-calculate_homology(mat = t3orus4_list3B[[7]], dim = 3, threshold = 9)
end_ph<-Sys.time()
}

{
  start_ph2<-Sys.time()
  t3orus4_list3B_7_subs_pd<-calculate_homology(mat = t3orus4_list3B_7_inst$subsamples[[1]]$data, dim = 3, threshold = 9)
  end_ph2<-Sys.time()
}

t3ours4_list2_2_sub_normal_time2<-system.time( t3ours4_list2_2_sub_normal_pd2<-calculate_homology(mat = t3ours4_list2_2_sub_dist, dim = 3, threshold = 9, format = "distmat") )

t3ours6<-x3Dtorus_unif(n = 600, r = 2, R1 = 8, R2 = 4)
t3ours6_inst<-TDAdataset$new(t3ours6)
t3ours6_inst$calc_pd(maxdim = 3, maxscale = 9)
