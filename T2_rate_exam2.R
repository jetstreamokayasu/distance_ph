#2次元トーラスの超低密度領域のCTIC2019手法、補間手法、距離操作手法の比較

#-----------------
#200点トーラス------------

trs200_list1<-lapply(1:100, function(i)torusUnif(n = 200, 1, 2.5))

trs200_list1_inted<-lapply(trs200_list1, function(X)interpo3d:::voronoi_interpo(X, 10) %>% rbind(X, .))

trs200_list1_inted1<-TDAdataset$new(trs200_list1_inted[[1]])
trs200_list1_1<-TDAdataset$new(trs200_list1[[1]])
trs200_list1_1$create_changed_distmat(l_rate = 0.8, eta = 3)

trs200_list1_inted_aggr1<-smooth_landscape_method(X = trs200_list1_inted, maxdim = 2, maxscale = 3, samples = 10)

trs200_list1_wvr_aggr1<-calc_distance_change_betti(X = trs200_list1, maxdim = 2, maxscale = 3, samples = 10, 
                                                         ph_func = weighted_homology, l_rate=0.8, eta=3)
#------------------------
#210点トーラス------------

trs210_list1<-lapply(1:100, function(i)torusUnif(n = 210, 1, 2.5))

trs210_list1_inted<-lapply(trs210_list1, function(X){interpo3d:::voronoi_interpo(X, 10) %>% rbind(X, .)})

trs210_list1_inted_aggr1<-smooth_landscape_method(X = trs210_list1_inted, maxdim = 2, maxscale = 3, samples = 10)

trs210_list1_wvr_time1<-system.time(
  trs210_list1_wvr_aggr1<-calc_distance_change_betti(X = trs210_list1, maxdim = 2, maxscale = 3, samples = 10, 
                                                         ph_func = weighted_homology, l_rate=0.8, eta=3) )

#------------------------
#220点トーラス------------

trs220_list1<-lapply(1:100, function(i)torusUnif(n = 220, 1, 2.5))

trs220_list1_inted<-lapply(trs220_list1, function(X){interpo3d:::voronoi_interpo(X, 10) %>% rbind(X, .)})

trs220_list1_inted_time1<-system.time(
trs220_list1_inted_aggr1<-smooth_landscape_method(X = trs220_list1_inted, maxdim = 2, maxscale = 3, samples = 10) 
)

trs220_list1_wvr_time1<-system.time(
  trs220_list1_wvr_aggr1<-calc_distance_change_betti(X = trs220_list1, maxdim = 2, maxscale = 3, samples = 10, 
                                                           ph_func = weighted_homology, l_rate=0.8, eta=3) )


#-----------------
#250点トーラス-----
trs250_list1<-lapply(1:100, function(i)torusUnif(n = 250, 1, 2.5))

trs250_list1_inted<-lapply(trs250_list1, function(X)interpo3d:::voronoi_interpo(X, 10) %>% rbind(X, .))



trs250_list1_inted1<-TDAdataset$new(trs250_list1_inted[[1]])
