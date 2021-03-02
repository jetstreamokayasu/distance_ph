#n次元直方体のベッチ数推定実験

#----------------------------
#３次元直方体
cuboid3d_1<-xRect_unif(100, sides = c(1, 1, 3))

cuboid3d_2<-xRect_unif(50, sides = c(1, 1, 1))

cuboid3d_2$rect[, 2] %<>% add(3) 
cuboid3d_1and2<-rbind(cuboid3d_1$rect, cuboid3d_2$rect)

cuboid3d_1and2_inst<-TDAdataset$new(cuboid3d_1and2)

{pd_start<-Sys.time()
calculate_homology(mat = cuboid3d_1and2, dim = 2) %>% cuboid3d_1and2_inst$input_pd()
pd_end<-Sys.time()}

#-------------------------------
#7次元直方体(８次元空間に埋め込まれた)---------

cuboid7d_1<-xRect_unif(n = 200, sides = c(rep(1, length = 7), 8), d = 8)

cuboid7d_1_inst<-TDAdataset$new(cuboid7d_1$rect)

{
pd_start<-Sys.time()
calculate_homology(mat = cuboid7d_1$rect, dim = 7) %>% cuboid7d_1_inst$input_pd()
pd_end<-Sys.time()
}

#-------------------------------
#5次元直方体(6次元空間に埋め込まれている)---------

cuboid5d_1<-xRect_unif(n = 100, sides = c(rep(1, length = 5), 5), d = 6)

cuboid5d_1_inst<-TDAdataset$new(cuboid5d_1$rect)

{
  pd_start<-Sys.time()
  calculate_homology(mat = cuboid5d_1$rect, dim = 5) %>% cuboid5d_1_inst$input_pd()
  pd_end<-Sys.time()
}

#-------------------------------
#4次元直方体(5次元空間に埋め込まれている)---------

#４次元直方体1つ目
cuboid4d_1<-xRect_unif(n = 100, sides = c(rep(1, length = 4), 5), d = 5)

cuboid4d_1_inst<-TDAdataset$new(cuboid4d_1$rect)


{
  pd_start<-Sys.time()
  calculate_homology(mat = cuboid4d_1$rect, dim = 4) %>% cuboid4d_1_inst$input_pd()
  pd_end<-Sys.time()
}

#４次元直方体2つ目
cuboid4d_2<-xRect_unif(n = 200, sides = c(rep(1, length = 4), 5), d = 5)

cuboid4d_2_inst<-TDAdataset$new(cuboid4d_2)

{ 
  pd_start<-Sys.time()
  calculate_homology(mat = cuboid4d_2, dim = 4) %>% cuboid4d_2_inst$input_pd()
  pd_end<-Sys.time()
}

#４次元直方体3つ目
cuboid4d_3<-xRect_unif(n = 150, sides = c(rep(1, length = 4), 5), d = 5)

cuboid4d_3_inst<-TDAdataset$new(cuboid4d_3)
cuboid4d_3_inst$calc_pd(maxdim = 4, maxscale = 3)

#4次元直方体3つ目
cuboid4d_4<-xRect_unif(n = 180, sides = c(rep(1, length = 4), 5), d = 5)

cuboid4d_4_inst<-TDAdataset$new(cuboid4d_4)
cuboid4d_4_inst$calc_pd(maxdim = 4, maxscale = 2)