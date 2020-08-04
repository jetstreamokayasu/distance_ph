#点数削減手法を用いた距離行列の変更の有効性を確かめる
#torus300_colle_set

trs300_coll1_aggr<-distance_change_method(X = torus300_colle_set[[1]], maxdim = 2, maxscale = 3, samples = 10)

#--------------------------------------------------------
#ランドマーク点の距離を変化させたときのベッチ数推定実験
#torus300_colle_set
trs300_coll1_aggr2<-maxmin_distance_change_method(X = torus300_colle_set[[1]], maxdim = 2, maxscale = 3, samples = 10)


#-------------------------------------------------------
#素の状態でのベッチ数推定実験
#サブサンプル85%
trs300_coll1_ori_time<-system.time(trs300_coll1_ori_aggr<-proposedMethodOnly(X = torus300_colle_set[[1]], maxdim = 2, maxscale = 3, samples = 10, const.size = 300*0.85))

#サブサンプル85%, スプライン曲線spar = seq(0,0.5,0.05)
trs300_coll1_ori2_time<-system.time(trs300_coll1_ori_aggr2<-proposedMethodOnly(X = torus300_colle_set[[1]], maxdim = 2, maxscale = 3, samples = 10, const.size = 300*0.85))

#--------------------------------------
#TDAstat(ripserを使ったパッケージ)テスト
#ポイントクラウドデータ
trs300_1_10_subs_2_time2<-system.time(trs300_1_10_subs_2_pd2<-calculate_homology(mat = trs300_1_10_subs2[[2]], dim = 2))
trs300_1_10_subs2_2_time2B<-system.time(trs300_1_10_subs_2_pd2B<-calculate_homology(mat = trs300_1_10_subs2[[2]], dim = 2, threshold = 3, format = "cloud"))
  
#距離行列
trs300_1_10_subs2_2_time3<-system.time(trs300_1_10_subs2_2_pd3<-dist(trs300_1_10_subs2[[2]]) %>% as.matrix() %>% calculate_homology(mat = ., dim = 2, threshold = 3, format = "distmat"))
plot_persist(trs300_1_10_subs2_2_pd3)

#TDAstatからPLを求められるか？
#リストにすればできる
trs300_1_10_subs2_2_pl3<-calc_landscape(diag = list(trs300_1_10_subs2_2_pd3), maxscale = 3)

#距離行列を変化させて計算できるか？
trs300_1_10_vdmean_time<-system.time(trs300_1_10_vdmean_pd3<-calculate_homology(mat = trs300_1_10_vdmean_dist2, dim = 2, threshold = 3, format = "distmat"))
trs300_1_10_vdmean_pl3<-calc_landscape(list(trs300_1_10_vdmean_pd3), maxscale = 3)
trs300_1_10_vdmean_pl3B<-landscape(Diag = trs300_1_10_vdmean_pd3, dimension = 2)

tseq <-seq(min(trs300_1_10_vdmean_pd3[, 2:3]), max(trs300_1_10_vdmean_pd3[, 2:3]), length = 500)
plot(tseq, trs300_1_10_vdmean_pl3B, type = "l", col = 3)

#--------------------------------------------------------
#ランドマーク点の距離を変化させたときのベッチ数推定実験
#torus300_colle_set
#TDAstat(ripser)を使う
#85%サブサンプル
trs300_coll1_aggr3_time<-system.time(trs300_coll1_aggr3<-maxmin_distance_change_method(X = torus300_colle_set[[1]][1:10], maxdim = 2, maxscale = 3, samples = 10, const.size = 300*0.85))

#90%サブサンプル
trs300_coll1_aggr4_time<-system.time(trs300_coll1_aggr4<-maxmin_distance_change_method(X = torus300_colle_set[[1]][1:10], maxdim = 2, maxscale = 3, samples = 10, const.size = 300*0.9))

#80%サブサンプル
trs300_coll1_aggr5_time<-system.time(trs300_coll1_aggr5<-maxmin_distance_change_method(X = torus300_colle_set[[1]][1:10], maxdim = 2, maxscale = 3, samples = 10))

#85%サブサンプル、ランドマーク点15%、近傍点7
trs300_coll1_aggr6_time<-system.time(trs300_coll1_aggr6<-maxmin_distance_change_method(X = torus300_colle_set[[1]][1:10], maxdim = 2, maxscale = 3, samples = 10, const.size = 300*0.85, n_vic = 7))

#85%サブサンプル、ランドマーク点20%、近傍点10
trs300_coll1_aggr7_time<-system.time(trs300_coll1_aggr7<-maxmin_distance_change_method(X = torus300_colle_set[[1]][1:10], maxdim = 2, maxscale = 3, samples = 10, const.size = 300*0.85, l_rate = 0.2, n_vic = 10))

#85%サブサンプル、ランドマーク点20%、近傍点7
trs300_coll1_aggr8_time<-system.time(trs300_coll1_aggr8<-maxmin_distance_change_method(X = torus300_colle_set[[1]][1:10], maxdim = 2, maxscale = 3, samples = 10, const.size = 300*0.85, l_rate = 0.2, n_vic = 7))

#85%サブサンプル、ランドマーク点20%、近傍点8
trs300_coll1_aggr9_time<-system.time(trs300_coll1_aggr9<-maxmin_distance_change_method(X = torus300_colle_set[[1]][1:10], maxdim = 2, maxscale = 3, samples = 10, const.size = 300*0.85, l_rate = 0.2, n_vic = 8))

#85%サブサンプル、ランドマーク点20%、近傍点7
#結果のバランスとしては最も良い？
trs300_coll1_aggr8B_time<-system.time(trs300_coll1_aggr8B<-maxmin_distance_change_method(X = torus300_colle_set[[1]], maxdim = 2, maxscale = 3, samples = 10, const.size = 300*0.85, l_rate = 0.2, n_vic = 7))

#85%サブサンプル、ランドマーク点20%、近傍点7、スプライン曲線spar = seq(0,0.5,0.05)
trs300_coll1_aggr8C_time<-system.time(trs300_coll1_aggr8C<-maxmin_distance_change_method(X = torus300_colle_set[[1]][1:10], maxdim = 2, maxscale = 3, samples = 10, const.size = 300*0.85, l_rate = 0.2, n_vic = 7, spar = seq(0,0.5,0.05)))

#80%サブサンプル, ランドマーク点15%、近傍点10, スプライン曲線spar = seq(0,0.5,0.05)
trs300_coll1_aggr10_time<-system.time(trs300_coll1_aggr10<-maxmin_distance_change_method(X = torus300_colle_set[[1]][1:10], maxdim = 2, maxscale = 3, samples = 10, l_rate = 0.15, n_vic = 10, spar = seq(0,0.5,0.05)))

#80%サブサンプル, ランドマーク点15%、近傍点10, スプライン曲線spar = seq(0,0.8,0.08)
trs300_coll1_aggr11_time<-system.time(trs300_coll1_aggr11<-maxmin_distance_change_method(X = torus300_colle_set[[1]][1:10], maxdim = 2, maxscale = 3, samples = 10, l_rate = 0.15, n_vic = 10, spar = seq(0,0.8,0.08)))

#---------------------------------------------------
#複数のデータセットで推定

##300点トーラス補間後1~3セット目を推定
#80%サブサンプル, ランドマーク点15%、近傍点10, スプライン曲線spar = seq(0,0.8,0.08)
{
torus300_colle13_dc_aggrs<-lapply(1:3, function(k){
  
  cat("list", k, "calc\n")
  time<-system.time(aggr<-maxmin_distance_change_method(torus300_colle_set[[k]], maxdim = 2, maxscale = 3, samples = 10, spar = seq(0,0.8,0.08)))
  return(append(aggr, list(time=time)))
  
})
save2Rdata(torus300_colle13_dc_aggrs)
}

##300点トーラス補間後1~3セット目を推定
#80%サブサンプル, ランドマーク点15%、近傍点10, スプライン曲線spar = seq(0,0.5,0.05)
{
  torus300_colle13_dc_aggrs2<-lapply(1:3, function(k){
    
    cat("list", k, "calc\n")
    time<-system.time(aggr<-maxmin_distance_change_method(torus300_colle_set[[k]], maxdim = 2, maxscale = 3, samples = 10, spar = seq(0,0.5,0.05)))
    return(append(aggr, list(time=time)))
    
  })
  save2Rdata(torus300_colle13_dc_aggrs2)
}

##300点トーラス補間後1~3セット目を推定
#80%サブサンプル, ランドマーク点15%、近傍点10, スプライン曲線spar = seq(0, 1, 0.1)
#密度推定の値だったのを平均値にした
{
  torus300_colle1to3_dc_aggrs3<-lapply(1:3, function(k){
    
    cat("list", k, "calc\n")
    time<-system.time(aggr<-maxmin_distance_change_method(torus300_colle_set[[k]], maxdim = 2, maxscale = 3, samples = 10, spar = seq(0,1,0.1)))
    return(append(aggr, list(time=time)))
    
  })
  save2Rdata(torus300_colle1to3_dc_aggrs3)
}


##310点トーラス補間後1~3セット目を推定
#80%サブサンプル, ランドマーク点15%、近傍点10, スプライン曲線spar = seq(0,0.8,0.08)
{
  torus310_colle13_dc_aggrs<-lapply(1:3, function(k){
    
    cat("list", k, "calc\n")
    time<-system.time(aggr<-maxmin_distance_change_method(torus310_colle_set[[k]], maxdim = 2, maxscale = 3, samples = 10, spar = seq(0,0.8,0.08)))
    return(append(aggr, list(time=time)))
    
  })
  save2Rdata(torus310_colle13_dc_aggrs)
}

##320点トーラス補間後1~3セット目を推定
#80%サブサンプル, ランドマーク点15%、近傍点10, スプライン曲線spar = seq(0,0.8,0.08)
{
  torus320_colle13_dc_aggrs<-lapply(1:3, function(k){
    
    cat("list", k, "calc\n")
    time<-system.time(aggr<-maxmin_distance_change_method(torus320_colle_set[[k]], maxdim = 2, maxscale = 3, samples = 10, spar = seq(0,0.8,0.08)))
    return(append(aggr, list(time=time)))
    
  })
  save2Rdata(torus320_colle13_dc_aggrs)
}

##330点トーラス補間後1~3セット目を推定
#80%サブサンプル, ランドマーク点15%、近傍点10, スプライン曲線spar = seq(0,0.8,0.08)
{
  torus330_colle13_dc_aggrs<-lapply(1:3, function(k){
    
    cat("list", k, "calc\n")
    time<-system.time(aggr<-maxmin_distance_change_method(torus330_colle_set[[k]], maxdim = 2, maxscale = 3, samples = 10, spar = seq(0,0.8,0.08)))
    return(append(aggr, list(time=time)))
    
  })
  save2Rdata(torus330_colle13_dc_aggrs)
}

##340点トーラス補間後1~3セット目を推定
#80%サブサンプル, ランドマーク点15%、近傍点10, スプライン曲線spar = seq(0,0.8,0.08)
{
  torus340_colle13_dc_aggrs<-lapply(1:3, function(k){
    
    cat("list", k, "calc\n")
    time<-system.time(aggr<-maxmin_distance_change_method(torus340_colle_set[[k]], maxdim = 2, maxscale = 3, samples = 10, spar = seq(0,0.8,0.08)))
    return(append(aggr, list(time=time)))
    
  })
  save2Rdata(torus340_colle13_dc_aggrs)
}

##350点トーラス補間後1~3セット目を推定
#80%サブサンプル, ランドマーク点15%、近傍点10, スプライン曲線spar = seq(0,0.8,0.08)
{
  torus350_colle13_dc_aggrs<-lapply(1:3, function(k){
    
    cat("list", k, "calc\n")
    time<-system.time(aggr<-maxmin_distance_change_method(torus350_colle_set[[k]], maxdim = 2, maxscale = 3, samples = 10, spar = seq(0,0.8,0.08)))
    return(append(aggr, list(time=time)))
    
  })
  save2Rdata(torus350_colle13_dc_aggrs)
}

#-------------------------------------------------
#TDAstatsを使って距離行列変化なしのベッチ数推定を行う
##300点トーラス補間後1~3セット目を推定
#80%サブサンプル, スプライン曲線spar = seq(0,0.8,0.08)
{
  torus300_colle2to3_aggrs2<-lapply(2:3, function(k){
    
    cat("list", k, "calc\n")
    time<-system.time(aggr<-smooth_landscape_method(torus300_colle_set[[k]], maxdim = 2, maxscale = 3, samples = 10, spar = seq(0,0.8,0.08)))
    return(append(aggr, list(time=time)))
    
  })
  save2Rdata(torus300_colle2to3_aggrs2)
}

trs300_coll1_ori3_time<-system.time(trs300_coll1_ori_aggr3<-smooth_landscape_method(X = torus300_colle_set[[1]], maxdim = 2, maxscale = 3, samples = 10, spar = seq(0, 0.8, 0.08)))

#-----------------------------------
#関数のテスト
m<-smoothed_landscape_homology(X = trs300_1_10_subs2, maxdim = 2, maxscale = 3, spar = seq(0, 0.8, 0.08))
mB<-bootstrap.homology.mk2(X = trs300_1_10_subs2, maxdim = 2, maxscale = 3)

m2<-smoothed_landscape_homology(X = trs300_1_10_subs2, maxdim = 2, maxscale = 3, spar = seq(0, 0.8, 0.08))

#-----------------------------------------
#3Dトーラスの角度の分布を見る
theta<-seq(0, 2*pi, length=100)
phi<-seq(0, 2*pi, length=100)

rad_mat<-c()

for (t in theta) {
  
  for (p in phi) {
    
    rad_mat<-rbind(rad_mat, c(t, p))
    
  }
  
}

dar_dstr<-apply(rad_mat, 1, function(x)rad_distribute(r = 1, R1 = 2.5, R2 = 5, theta = x[1], phi = x[2]))

rad_dstr<-matrix(dar_dstr, 100, 100)


#-------------------------------------------------
#3Dトーラス一様分布
t3orus<-x3Dtorus_unif(1000, 1, 2.5, 5)

trs<-torusUnif(1000, 1, 2.5)

t3orus_pd<-calculate_homology(mat = t3orus, dim = 3, threshold = 6)

t3orus2<-x3Dtorus_unif(n = 500, r = 1, R1 = 5, R2 = 2)

t3orus2_pd<-calculate_homology(mat = t3orus2, dim = 3, threshold = 6)

t3orus2_pl<-landscape(t3orus2_pd, dimension = 1)
t3orus2_pl_dim2<-landscape(t3orus2_pd, dimension = 2)
t3orus2_pl_dim3<-landscape(t3orus2_pd, dimension = 3)
tseq <- seq(min(t3orus2_pd[,2:3]), max(t3orus2_pd[,2:3]), length=500)

#---------------------------------------------------------
#ripserで補間後を計算

#平均よりも遠い点への補間を行ったデータセット
#torus300_incolle_set2
{
  torus300_incolle1to3_aggrs2<-lapply(1:3, function(k){
    
    cat("list", k, "calc\n")
    time<-system.time(aggr<-smooth_landscape_method(torus300_incolle_set2[[k]], maxdim = 2, maxscale = 3, samples = 10))
    return(append(aggr, list(time=time)))
    
  })
  save2Rdata(torus300_incolle1to3_aggrs2)
}

#ICAART2020手法。PCA原点平行移動補間
#torus300_incolle_setについて
{
  torus300_incolle1to3_aggrs3<-lapply(1:3, function(k){
    
    cat("list", k, "calc\n")
    time<-system.time(aggr<-smooth_landscape_method(torus300_incolle_set[[k]], maxdim = 2, maxscale = 3, samples = 10))
    return(append(aggr, list(time=time)))
    
  })
  save2Rdata(torus300_incolle1to3_aggrs3)
}
            