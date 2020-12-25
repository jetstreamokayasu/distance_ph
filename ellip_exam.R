#楕円データのPH計算

#楕円体1回目。300点。a = 2.5, b = 1, c = 1
ellip1<-xEllip_unif(n = 300, a = 2.5, b = 1, c = 1)

ellip_time1<-system.time(ellip1_pd1<-calculate_homology(mat = ellip1, dim = 2))
ellip1_pl1<-calcLandscape(diag = ellip1_pd1, maxscale = 2)

#楕円体2回目。100点。a = 2.5, b = 1, c = 1
ellip2<-xEllip_unif(n = 100, a = 2.5, b = 1, c = 1)
ellip2_time1<-system.time(ellip2_pd1<-calculate_homology(mat = ellip2, dim = 2, threshold = 3))
ellip2_pl1<-calcLandscape(diag = ellip2_pd1, maxscale = 3)

#楕円体3回目。50点。a = 2.5, b = 1, c = 1
ellip3<-xEllip_unif(n = 50, a = 2.5, b = 1, c = 1)
ellip3_time1<-system.time(ellip3_pd1<-calculate_homology(mat = ellip3, dim = 2, threshold = 3))
ellip3_pl1<-calcLandscape(diag = ellip3_pd1, maxscale = 3)

#楕円体4回目。80点。a = 5, b = 1, c = 1
ellip4<-xEllip_unif(n = 80, a = 5, b = 1, c = 1)
ellip4_time1<-system.time(ellip4_pd1<-calculate_homology(mat = ellip4, dim = 2))
ellip4_pl1<-calcLandscape(diag = ellip4_pd1, maxscale = 2)
plot_landscape(land = ellip4_pl1, dim = 2, xlim = c(0, 2), ylim = c(0, 0.5))

#ellip4を正規化してPH計算
#ellip4_dist1が正規化後距離行列
ellip4_normed_time1<-system.time( ellip4_normed_pd1<-calculate_homology(mat = ellip4_dist1, dim = 2, format = "distmat") )
ellip4_normed_pl1<-calc_landscape(diag = ellip4_normed_pd1, maxscale = 1)
plot_landscape(land = ellip4_normed_pl1, dim = 2, xlim = c(0, 0.8), ylim = c(0, 0.04))

#-----------------------
#ellip4にMPH適用--------
#1回目
#ランドマーク点、距離行列
e4_land1<-landmark_points(X = ellip4, n_land = 15)
ellip4_dist0<-dist(ellip4) %>% as.matrix()

ellip4_dist1<-ellip4_dist0/max(ellip4_dist0)

for (i in e4_land1) {
  
  ellip4_dist1[i, ]<-1-exp(-(ellip4_dist0[i, ])^2)
  ellip4_dist1[, i]<-1-exp(-(ellip4_dist0[, i])^2)
  
}

ellip4_time2<-system.time(ellip4_pd2<-calculate_homology(mat = ellip4_dist1, dim = 2, format = "distmat"))
ellip4_pl2<-calcLandscape(diag = ellip4_pd2)

plot(ellip4_pl2[["tseq"]], ellip4_pl2[["2-land"]], xlim = c(0, 0.3), type="l", col=3)
abline(h=ellip4_pl2[["thresh"]]/2)

#ellip4にMPH適用
#2回目

ellip4_dist2<-ellip4_dist0/max(ellip4_dist0)

for (i in e4_land1) {
  
  ellip4_dist2[i, ]<-1-exp(-(ellip4_dist0[i, ]/5)^2)
  ellip4_dist2[, i]<-1-exp(-(ellip4_dist0[, i]/5)^2)
  
}

ellip4_time3<-system.time(ellip4_pd3<-calculate_homology(mat = ellip4_dist2, dim = 2, format = "distmat"))
ellip4_pl3<-calcLandscape(diag = ellip4_pd3)

plot(ellip4_pl3[["tseq"]], ellip4_pl3[["2-land"]], xlim = c(0, 0.3), ylim = c(0, 0.012), type="l", col=3)
abline(h=ellip4_pl3[["thresh"]]/2)

#ellip4にMPH適用
#3回目
#ランドマーク点変更

e4_land2<-landmark_points(X = ellip4, n_land = 30)

ellip4_dist3<-ellip4_dist0/max(ellip4_dist0)

for (i in e4_land2) {
  
  ellip4_dist3[i, ]<-1-exp(-(ellip4_dist0[i, ])^2)
  ellip4_dist3[, i]<-1-exp(-(ellip4_dist0[, i])^2)
  
}

ellip4_time4<-system.time(ellip4_pd4<-calculate_homology(mat = ellip4_dist3, dim = 2, format = "distmat"))
ellip4_pl4<-calcLandscape(diag = ellip4_pd4)

plot(ellip4_pl4[["tseq"]], ellip4_pl4[["2-land"]], xlim = c(0, 0.3), ylim = c(0, 0.014), type="l", col=3)
abline(h=ellip4_pl4[["thresh"]]/2)

#ellip4にMPH適用
#4回目
#ランドマーク点変更

e4_land3<-landmark_points(X = ellip4, n_land = 40)

ellip4_dist4<-ellip4_dist0/max(ellip4_dist0)

for (i in e4_land3) {
  
  ellip4_dist4[i, ]<-1-exp(-(ellip4_dist0[i, ])^2)
  ellip4_dist4[, i]<-1-exp(-(ellip4_dist0[, i])^2)
  
}

ellip4_time5<-system.time(ellip4_pd5<-calculate_homology(mat = ellip4_dist4, dim = 2, format = "distmat"))
ellip4_pl5<-calcLandscape(diag = ellip4_pd5)

plot(ellip4_pl5[["tseq"]], ellip4_pl5[["2-land"]], xlim = c(0, 1), ylim = c(0, 0.05), type="l", col=3)
abline(h=ellip4_pl5[["thresh"]]/2)

#ellip4にMPH適用
#5回目

ellip4_pd6<-multiresolut_homology(X = ellip4, maxdim = 2, l_rate = 0.4, a = 3)
ellip4_pl6<-calcLandscape(diag = ellip4_pd6[["pd"]], maxscale = 1)

#ellip4にMPH適用
#6回目

ellip4_dist5<-ellip4_dist0/max(ellip4_dist0)

for (i in e4_land3) {
  
  ellip4_dist5[i, ]<-1-exp(-(ellip4_dist0[i, ]/4.5)^2)
  ellip4_dist5[, i]<-1-exp(-(ellip4_dist0[, i]/4.5)^2)
  
}

ellip4_pd6<-calculate_homology(mat = ellip4_dist5, dim = 2, threshold = 1, format = "distmat")
ellip4_pl6<-calcLandscape(diag = ellip4_pd6, maxscale = 1)

plot(ellip4_pl6[["tseq"]], ellip4_pl6[["2-land"]], xlim = c(0, 0.5), ylim = c(0, 0.02), type="l", col=3)
abline(h=ellip4_pl6[["thresh"]]/2)

calc.landscape.peak(X = ellip4_pl6[["2-land"]], dimension = 2, thresh = ellip4_pl6[["thresh"]]/2, tseq = ellip4_pl6[["tseq"]], show = T)

#ellip4にMPH適用
#7回目
#ランドマーク点変更

e4_land4<-landmark_points(X = ellip4, n_land = 45)


ellip4_dist6<-ellip4_dist0/max(ellip4_dist0)

for (i in e4_land4) {
  
  ellip4_dist6[i, ]<-1-exp(-(ellip4_dist0[i, ]/2)^2)
  ellip4_dist6[, i]<-1-exp(-(ellip4_dist0[, i]/2)^2)
  
}

ellip4_pd7<-calculate_homology(mat = ellip4_dist6, dim = 2, threshold = 1, format = "distmat")
ellip4_pl7<-calcLandscape(diag = ellip4_pd7, maxscale = 1)
ellip4_pl7B<-calc_landscape(diag = ellip4_pd7, maxscale = 1)

# plot(ellip4_pl7[["tseq"]], ellip4_pl7[["2-land"]], xlim = c(0, 0.8), ylim = c(0, 0.04), type="l", col=3)
# abline(h=ellip4_pl7[["thresh"]]/2)

plot_landscape(land = ellip4_pl7, dim = 2, xlim = c(0, 0.8), ylim = c(0, 0.04))

calc.landscape.peak(X = ellip4_pl7[["2-land"]], dimension = 2, thresh = ellip4_pl7[["thresh"]]/2, tseq = ellip4_pl7[["tseq"]], show = T)

plot(sort(ellip4_dist0[e4_land1, ]), 1-exp(-(sort(ellip4_dist0[e4_land1, ])/3)^2))

#--------------------
#ellip4にWVRを適用---

ellip4_inst<-TDAdataset$new(ellip4)
ellip4_inst$calc_pd(maxdim = 2, maxscale = 3)

ellip4_inst$create_changed_distmat(l_rate = 0.5, eta = 3)

plot(ellip4_inst$distmat[52, ], ellip4_inst$distmat[52, ])
points(ellip4_inst$distmat[52, ], ellip4_inst$alt_distmat[[1]]$distmat[52,], col=2)

ellip4_inst$alt_distmat[[1]]$calc_pd(maxdim = 2, maxscale = 3)

ellip4_inst$create_changed_distmat(l_rate = 0.8, eta = 3)
ellip4_inst$alt_distmat[[2]]$calc_pd(maxdim = 2, maxscale = 3)

ellip4_inst$create_changed_distmat(l_rate = 0.5, eta = 2)
ellip4_inst$alt_distmat[[3]]$calc_pd(maxdim = 2, maxscale = 3)

ellip4_inst$create_changed_distmat(l_rate = 0.6, eta = 2)
ellip4_inst$alt_distmat[[4]]$calc_pd(maxdim = 2, maxscale = 3)

ellip4_inst$create_changed_distmat(l_rate = 0.6, eta = 3)
ellip4_inst$alt_distmat[[5]]$calc_pd(maxdim = 2, maxscale = 3)

ellip4_inst$create_subsample(sub_size = ellip4_inst$n_points*0.8, n_subs = 10)

ellip4_inst$subsamples[[1]]$create_changed_distmat(l_rate = 0.5, eta = 3.8)
ellip4_inst$subsamples[[1]]$alt_distmat[[8]]$calc_pd(maxdim = 2, maxscale = 3)
ellip4_inst$subsamples[[1]]$alt_distmat[[8]]$calc_pl()

for (i in 1:10) {
  
  ellip4_inst$subsamples[[i]]$distmat<-dist(ellip4_inst$subsamples[[i]]$data) %>% as.matrix()
  ellip4_inst$subsamples[[i]]$change_dist(l_rate = 0.5, eta = 4)
  ellip4_inst$subsamples[[i]]$calc_pd(maxdim = 2, maxscale = 3)
  ellip4_inst$subsamples[[i]]$calc_pl(plot = F)
  ellip4_inst$subsamples[[i]]$pl_peak_count(show = F)
  print(ellip4_inst$subsamples[[i]]$peaks)
}

ellip4_sub_H2<-map_dbl(ellip4_inst$subsamples, ~{.$peaks[2]})

#----------------
#楕円体の密度が高い場合をやってみる
#ellip5, 200点。パラメータはellip4と同じ
ellip5<-xEllip_unif(n = 200, a = 5, b = 1, c = 1)

ellip5_inst<-TDAdataset$new(ellip5)
ellip5_inst$calc_pd(maxdim = 2, maxscale = 3)

ellip5_inst2<-TDAdataset$new(ellip5)

#---------------------------------------
#楕円体の成功率を求める
#ellip4, 80点。a = 5, b = 1, c = 1
ellip80_list1<-map(1:100, ~{xEllip_unif(n = 80, a = 5, b = 1, c = 1)})

#CTIC2019手法
ellip80_aggr1<-smooth_landscape_method(X = ellip80_list1, maxdim = 2, maxscale = 3, samples = 10)

ellip80_wvr_aggr1<-calc_distance_change_betti(X = ellip80_list1, maxdim = 2, maxscale = 3, samples = 10, ph_func =  weighted_homology, l_rate=0.5, eta=4.0)



#-----------------------
#グリッドサーチのようにランドマーク点割合とハイパラetaを試す------------

lrate_set6<-seq(0.1, 0.8, by=0.05)
eta_set6<-seq(1.0, 7.0, by=0.2)

para_set6<-expand.grid(lrate_set6, eta_set6)
colnames(para_set6)<-c("l_rate", "eta")

ellip80_sub_gsH2_time<-system.time( 
  
  ellip80_sub_gsH2<-lapply(ellip4_inst$subsamples, function(S){
    
    sub_gs<-lapply(1:nrow(para_set6), function(p){
      
      wpd<-weighted_homology(X = S$data, maxdim = 2, maxscale = 3, l_rate = para_set6$l_rate[p], eta = para_set6$eta[p])
      
      cat("para_set=", i, "\n")
      cat("rate=", para_set6$l_rate[p], "\n")
      cat("eta=", para_set6$eta[p], "\n")
      
      return(wpd)
      
    })
    
    return(sub_gs)
    
  })
)

ellip80_sub_gsH2_pls<-lapply(ellip80_sub_gsH2, function(X){
  
  sub_pls<-lapply(X, function(Y){calc_landscape(diag = Y[["pd"]], maxscale = 3, plot = F)})
  
})

#サブサンプルのPLのH2局所最大値をまとめる
ellip80_sub_gsH2_pls_peaks_H2<-lapply(t3ours4_list3_5_sub_gsH2_pls, function(X){
  
  sub_pls<-sapply(X, function(Y){calc.landscape.peak(X = Y[["2-land"]], dimension = 2, thresh = Y[["thresh"]]*(2*pi)/surface_nshpere(2), tseq = Y[["tseq"]], show = F)})
  
})

#サブサンプル1~10のPLのH2局所最大値の平均
t3ours4_list3_5_sub_gsH2_peaks_H2_ave<-sapply(1:length(t3ours4_list3_5_sub_gsH2_peaks_H2[[1]]), function(i){
  mpeaks<-sapply(t3ours4_list3_5_sub_gsH2_peaks_H2, function(PL){PL[[i]]}) %>% mean()
})