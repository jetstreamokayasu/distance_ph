#300点2次元トーラスの成功率再確認


trs300_colle1_ctic_aggr<-smooth_landscape_method(X = torus300_colle_setB[[1]], maxdim = 2, maxscale = 3, samples = 10)

trs350_colle1_ctic_aggr<-smooth_landscape_method(X = torus350_colle_set1, maxdim = 2, maxscale = 3, samples = 10)

plot3d(torus300_colle_setB[[1]][[2]], size = 4)
aspect3d("iso")
rgl.postscript("trs300.pdf", fmt = "pdf")

trs350_col1_1_pd<-seephacm:::bootstrapper(X = torus350_colle_set1[[1]], size = 350*0.8, samples = 2) %>% 
  bootstrap.homology.mk2(maxdim = 2, maxscale = 3)

trs350_colle1_old_aggr<-proposedMethodOnly(X = torus350_colle_set[[1]], maxdim = 2, maxscale = 3, samples = 10)

old_dense_aggr_idx<-range_index(trs350_colle1_old_aggr[[2]], min = 0.5, max = 1.5)
old_mean_aggr_idx<-range_index(trs350_colle1_old_aggr[[4]], min = 0.5, max = 1.5)

old_aggr_intrsct<-dplyr::intersect(old_dense_aggr_idx, old_mean_aggr_idx)

#--------------------------------------------
#2次元トーラスの成功率再実験------

#------------------------------------
#350点トーラス---------

{
  {#CTIC2019 #100セットリスト2~5つ目
    trs350_list2to5_aggr<-lapply(2:5, function(k){
      
      cat("list", k, "calc\n")
      time<-system.time( aggr<-smooth_landscape_method_paral(X = torus350_colle_setB[[k]], maxdim = 2, maxscale = 3, samples = 10) )
      return(append(aggr, list(time=time)))
      
    })
    save2RData(trs350_list2to5_aggr)
  }
  
  {#補間手法 #100セットリスト1~5つ目
    torus350_colle_setB_inted<-lapply(torus350_colle_setB, function(Z){lapply(Z, function(X)interpo3d:::voronoi_interpo(X, 10) %>% rbind(X, .))})
    save2RData(torus350_colle_setB_inted)
    
    trs350_list1to5_inted_aggr<-lapply(1:5, function(k){
      
      cat("list", k, "calc\n")
      time<-system.time( aggr<-smooth_landscape_method_paral(X = torus350_colle_setB_inted[[k]], maxdim = 2, maxscale = 3, samples = 10) )
      return(append(aggr, list(time=time)))
      
    })
    save2RData(trs350_list1to5_inted_aggr)
  }
}

#------------------------------------
#340点トーラス
torus340_colle_setB<-lapply(torus340_colle_set, function(X)lapply(X, function(Y)Y[["noizyX"]]))

{#100セットリスト1~5つ目
  {#CTIC2019
    trs340_list1to5_aggr<-lapply(1:5, function(k){
      
      cat("list", k, "calc\n")
      time<-system.time( aggr<-smooth_landscape_method_paral(X = torus340_colle_setB[[k]], maxdim = 2, maxscale = 3, samples = 10) )
      return(append(aggr, list(time=time)))
      
    })
    save2RData(trs340_list1to5_aggr)
  }
  
  {#補間手法
    torus340_colle_setB_inted<-lapply(torus340_colle_setB, function(Z){lapply(Z, function(X)interpo3d:::voronoi_interpo(X, 10) %>% rbind(X, .))})
    save2RData(torus340_colle_setB_inted)
    
    trs340_list1to5_inted_aggr<-lapply(1:5, function(k){
      
      cat("list", k, "calc\n")
      time<-system.time( aggr<-smooth_landscape_method_paral(X = torus340_colle_setB_inted[[k]], maxdim = 2, maxscale = 3, samples = 10) )
      return(append(aggr, list(time=time)))
      
    })
    save2RData(trs340_list1to5_inted_aggr)
  }
}

#------------------------------------
#330点トーラス
torus330_colle_setB<-lapply(torus330_colle_set, function(X)lapply(X, function(Y)Y[["noizyX"]]))

{#100セットリスト1~5つ目
  {#CTIC2019
    trs330_list1to5_aggr<-lapply(1:5, function(k){
      
      cat("list", k, "calc\n")
      time<-system.time( aggr<-smooth_landscape_method_paral(X = torus330_colle_setB[[k]], maxdim = 2, maxscale = 3, samples = 10) )
      return(append(aggr, list(time=time)))
      
    })
    save2RData(trs330_list1to5_aggr)
  }
  
  {#補間手法
    torus330_colle_setB_inted<-lapply(torus330_colle_setB, function(Z){lapply(Z, function(X)interpo3d:::voronoi_interpo(X, 10) %>% rbind(X, .))})
    save2RData(torus330_colle_setB_inted)
    
    trs330_list1to5_inted_aggr<-lapply(1:5, function(k){
      
      cat("list", k, "calc\n")
      time<-system.time( aggr<-smooth_landscape_method_paral(X = torus330_colle_setB_inted[[k]], maxdim = 2, maxscale = 3, samples = 10) )
      return(append(aggr, list(time=time)))
      
    })
    save2RData(trs330_list1to5_inted_aggr)
  }
}

#------------------------------------
#320点トーラス
torus320_colle_setB<-lapply(torus320_colle_set, function(X)lapply(X, function(Y)Y[["noizyX"]]))

{#100セットリスト1~5つ目
  {#CTIC2019
    trs320_list1to5_aggr<-lapply(1:5, function(k){
      
      cat("list", k, "calc\n")
      time<-system.time( aggr<-smooth_landscape_method_paral(X = torus320_colle_setB[[k]], maxdim = 2, maxscale = 3, samples = 10) )
      return(append(aggr, list(time=time)))
      
    })
    save2RData(trs320_list1to5_aggr)
  }
  
  {#補間手法
    torus320_colle_setB_inted<-lapply(torus320_colle_setB, function(Z){lapply(Z, function(X)interpo3d:::voronoi_interpo(X, 10) %>% rbind(X, .))})
    save2RData(torus320_colle_setB_inted)
    
    trs320_list1to5_inted_aggr<-lapply(1:5, function(k){
      
      cat("list", k, "calc\n")
      time<-system.time( aggr<-smooth_landscape_method_paral(X = torus320_colle_setB_inted[[k]], maxdim = 2, maxscale = 3, samples = 10) )
      return(append(aggr, list(time=time)))
      
    })
    save2RData(trs320_list1to5_inted_aggr)
  }
}

#------------------------------------
#310点トーラス
torus310_colle_setB<-lapply(torus310_colle_set, function(X)lapply(X, function(Y)Y[["noizyX"]]))

{#100セットリスト1~5つ目
  {#CTIC2019
    trs310_list1to5_aggr<-lapply(1:5, function(k){
      
      cat("list", k, "calc\n")
      time<-system.time( aggr<-smooth_landscape_method_paral(X = torus310_colle_setB[[k]], maxdim = 2, maxscale = 3, samples = 10) )
      return(append(aggr, list(time=time)))
      
    })
    save2RData(trs310_list1to5_aggr)
  }
  
  {#補間手法
    torus310_colle_setB_inted<-lapply(torus310_colle_setB, function(Z){lapply(Z, function(X)interpo3d:::voronoi_interpo(X, 10) %>% rbind(X, .))})
    save2RData(torus310_colle_setB_inted)
    
    trs310_list1to5_inted_aggr<-lapply(1:5, function(k){
      
      cat("list", k, "calc\n")
      time<-system.time( aggr<-smooth_landscape_method_paral(X = torus310_colle_setB_inted[[k]], maxdim = 2, maxscale = 3, samples = 10) )
      return(append(aggr, list(time=time)))
      
    })
    save2RData(trs310_list1to5_inted_aggr)
  }
}

#------------------------------------
#300点トーラス
#torus300_colle_setB<-lapply(torus310_colle_set, function(X)lapply(X, function(Y)Y[["noizyX"]]))

{#100セットリスト1~5つ目
  {#CTIC2019 #100セットリスト2~5つ目
    trs300_list2to5_aggr<-lapply(2:5, function(k){
      
      cat("list", k, "calc\n")
      time<-system.time( aggr<-smooth_landscape_method_paral(X = torus300_colle_setB[[k]], maxdim = 2, maxscale = 3, samples = 10) )
      return(append(aggr, list(time=time)))
      
    })
    save2RData(trs300_list2to5_aggr)
  }
  
  {#補間手法 #100セットリスト2~5つ目
    torus300_colle_setB_inted<-lapply(torus300_colle_setB, function(Z){lapply(Z, function(X)interpo3d:::voronoi_interpo(X, 10) %>% rbind(X, .))})
    save2RData(torus300_colle_setB_inted)
    
    trs300_list1to5_inted_aggr<-lapply(1:5, function(k){
      
      cat("list", k, "calc\n")
      time<-system.time( aggr<-smooth_landscape_method_paral(X = torus300_colle_setB_inted[[k]], maxdim = 2, maxscale = 3, samples = 10) )
      return(append(aggr, list(time=time)))
      
    })
    save2RData(trs300_list1to5_inted_aggr)
  }
}

#--------------------------------------
#補間により増加した点数を調べる
trs300_col_setB_inted_points<-map(torus300_colle_setB_inted, ~map_dbl(., nrow))
unlist(trs300_col_setB_inted_points) %>% subtract(300) %>% mean() #%>% divide_by(300)

