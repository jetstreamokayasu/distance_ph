#補間点数を変更した場合のパーシステントホモロジー計算

trs300_cole1_43<-TDAdataset$new(torus300_colle_set[[1]][[43]][["noizyX"]])

trs300_cole1_43_toadd<-neibor_voronoi_interpol(figure = trs300_cole1_43$data, nvics = 10)

trs300_cole1_43$plot_data()
points3d(trs300_cole1_43_toadd, col = "red")

trs300_cole1_43_inted<-TDAdataset$new(rbind(trs300_cole1_43$data, trs300_cole1_43_toadd))
trs300_cole1_43_inted$calc_pd(maxdim = 2, maxscale = 3)

#-----------------------------------------------
#50個のデータセットを推定、成功率を確かめる------

#--------------------------------------
#補間手法 #torus300_colle_setBのうち50個--------
#全ボロノイ領域の頂点への補間
trs300_col_setB_inted_allvertx<-lapply(torus300_colle_setB, function(Z){lapply(Z, function(X)neibor_voronoi_interpol(X, 10) %>% rbind(X, .))})
save2RData(trs300_col_setB_inted_allvertx)

{trs300_lst1_1to50_inted_all_vertx_time<-system.time(
  trs300_lst1_1to50_inted_all_vertx_aggr<-
    smooth_landscape_method_paral(X = trs300_col_setB_inted_allvertx[[1]][1:50], maxdim = 2, maxscale = 3, samples = 10)
)

save2RData(trs300_lst1_1to50_inted_all_vertx_time)
save2RData(trs300_lst1_1to50_inted_all_vertx_aggr)
}

#--------------------------
#隣接する1つのボロノイ領域に対しても、頂点への補間を行う場合------------
trs300_col_setB_inted_nei1<-lapply(torus300_colle_setB[[1]], function(X){neighbor_voronoi_interpol(figure = X, nvics = 10, neighbor = 1) %>% rbind(X, .)})
save2RData(trs300_col_setB_inted_nei1)

{trs300_col_setB_inted_nei1_time<-system.time(
  trs300_col_setB_inted_nei1_aggr<-
    smooth_landscape_method_paral(X = trs300_col_setB_inted_allvertx[[1]][1:50], maxdim = 2, maxscale = 3, samples = 10)
)
  
  save2RData(trs300_col_setB_inted_nei1_time)
  save2RData(trs300_col_setB_inted_nei1_aggr)
}
