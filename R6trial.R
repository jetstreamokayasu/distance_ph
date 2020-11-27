#R6クラスの実装を試す

library(R6)

#1つのデータについて、変化後の距離行列とサブサンプル集をまとめたクラス
TDAdataset<-
  R6Class(classname = "TDAdataset",
          public = list(
            
            data = NA, #任意の点群データ
            n_points = 0, #データ点数
            distmat_c = NA, #DistmatPDクラスの距離行列
            alt_distmat = list(), #変化後の距離行列リスト。DistmatPDクラス
            subsamples = list(), #サブサンプルのリスト。TDAdatasetクラス
            
            initialize = function(data){#点群データを代入して初期化
              self$data <- data       
              self$distmat_c <- dist(data) %>% as.matrix() %>% DistmatPD$new(distmat = .)
              self$n_points <- nrow(data)
            },
            
            create_changed_distmat = function(l_rate, eta){#変化させた距離行列を作成
              
              temp_dist <- DistmatPD$new(distmat = self$distmat_c$distmat)
              
              temp_dist$change_dist(l_rate = l_rate, eta = eta)
              
              self$alt_distmat <- append(self$alt_distmat, temp_dist)
              
            },
            
            #サブサンプルを作成。TDAdatasetクラス
            #sub_size=サブサンプルの点数、n_subs=サブサンプルの個数
            create_subsample = function(sub_size, n_subs){
              
              self$subsamples <- map(1:n_subs, function(i){
                
                seephacm:::bootstrapper(X = self$data, size = sub_size, 1)[[1]] %>% 
                              TDAdataset$new(data = .)
                
                })  %>% append(self$subsamples, .)
              
            },
            
            #サブサンプルの中で最大の閾値を求める
            max_thresh = function(dim = 1){
              
              if(length(self$subsamples) < 1){stop("subsamples didn't created.")}
              
              map(self$subsamples, ~{
                
                if(length(.$distmat_c$get_pd()) <= 1){stop("pd didn't calculated.")}
                  
                })
              
              max_t <- map_dbl(self$subsamples, ~{.$distmat_c$get_thresh(dim)}) %>% max()
              
              return(max_t)
              
            },
            
            plot_data = function(...){#データのプロット
              
              if(ncol(self$data) <= 2){plot(self$data, ...)}
              else{
                
                plot3d(self$data, ...)
                aspect3d("iso")
                
                }
              
            }
            
            
          )#public閉じ括弧
  
) #TDAdataset Class閉じ括弧

#------------------------------------------
#距離関数にたいしてPH関連関数をまとめた

DistmatPD<-
  R6Class(classname = "DistmatPD",
  
    public = list(
      
      distmat = NA, #距離行列
      l_rate = 0, #ランドマーク点割合
      eta = 1, #1-exp{-(d_ij/eta)^2}のハイパラ
      l_idx = NA, #ランドマーク点のインデックス
      maxdim = NULL, #計算する位相特徴の最大次元
      maxscale = NULL, #フィルトレーションの最大値
      peaks = 0, #平滑化後のPLの局所最大値の数
      
      initialize = function(distmat){self$distmat <- distmat},
      
      change_dist = function(l_rate, eta){
        
        self$l_rate <- l_rate
        self$eta <- eta
        
        self$l_idx <- landmark_points(X = self$distmat, n_land = nrow(self$distmat)*l_rate, d_mat = T)
        self$distmat <- dist_wvr_change(X_dist = self$distmat, lands = self$l_idx, eta = eta)
        
        private$changed = TRUE
        
      },
      
      calc_pd = function(maxdim, maxscale){
        
        self$maxdim <- maxdim
        self$maxscale <- maxscale
        
        self$peaks <- numeric(maxdim)
        names(self$peaks) <- map_chr(1:maxdim, ~{paste0(., "dim")})
        
        private$time<-system.time( private$pd <- calculate_homology(mat = self$distmat, dim = self$maxdim, threshold = self$maxscale, format = "distmat") )
        
      },
      
      plot_pd = function(){
        
        if(length(private$pd)==1){stop("pd didn't calcutated.")}
        
        plot_persist(private$pd)
        
        },
      
      plot_diag = function(){#TDAのdiagramを描画
        
        pd_conv_stats2tda(pd=private$pd) %>% plot()
        legend("bottomright", legend = c("dim0", "dim1", "dim2", "dim3")[1:(max(private$pd[,1])+1)], 
               col=c(1, 2, 4, 3)[1:(max(private$pd[,1])+1)], pch=c(16, 23, 5, 3)[1:(max(private$pd[,1])+1)], cex=1.5)
        
      },
      
      get_pd = function(){return(private$pd)},#PDを呼び出し
      
      calc_pl = function(plot = T){
        private$pl <- calc_landscape(diag = private$pd, maxscale = self$maxscale, plot = plot)
        },
      
      plot_pl = function(dim, xlim = NULL, ylim = NULL){
        if(length(private$pl)==1){self$calc_pl()}
        
        plot_landscape(land = private$pl, dim = dim, xlim = xlim, ylim = ylim)
        
        }, 
      
      get_thresh = function(dim){#次元ごとの閾値を返す
        
        return(seephacm:::persistence_weighted_mean(private$pd)*(2*pi)/surface_nshpere(dim))
        
      },
      
      pl_peak_count = function(dim, show = T){#平滑化したPLの局所最大値を数える
        
        self$peaks[dim] <- calc.landscape.peak(X = private$pl[[paste0(dim, "-land")]], dimension = dim, 
                            thresh = self$get_thresh(dim), tseq = private$pl[["tseq"]], show = show)
        
      },
      
      get_pl = function(){return(private$pl)}, #PLを呼び出し
      
      get_time = function(){return(private$time)}, #PDの計算時間を呼び出し
      
      is_changed = function(){return(private$changed)} #距離行列を変化させたか否かを呼び出す
      
    ),#public閉じ括弧
    
    private = list(
      pd = NA, #TDAstatsのPD
      pl = NA, #パーシステントランドスケープ
      time = 0, #PDの計算時間
      changed = FALSE #距離行列を変化させたか否か
    )
  
)#DistmatPD Class閉じ括弧
