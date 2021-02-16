#R6クラスの実装を試す

library(R6)

#1つのデータについて、変化後の距離行列とサブサンプル集をまとめたクラス
TDAdataset<-
  R6Class(classname = "TDAdataset",
          
          inherit = DistmatPD,
          
          public = list(
            
            data = NA, #任意の点群データ
            n_points = 0, #データ点数
            alt_distmat = list(), #変化後の距離行列リスト。DistmatPDクラス
            subsamples = list(), #サブサンプルのリスト。TDAdatasetクラス
            mat_pds = list(), #alt_distmatの距離行列群から求められたPDのリスト
            mat_pls = list(), #alt_distmatの距離行列群から求められたPLのリスト
            mat_peaks = NA, #alt_distmatの距離行列群から求められたpekaの行列
            sub_pds = list(), #subsampesのサブサンプル群から求められたPDのリスト
            sub_pls = list(), #subsampesのサブサンプル群から求められたPLのリスト
            sub_peaks = NA, #subsampesの距離行列群から求められたpekaの行列
            
            initialize = function(data){#点群データを代入して初期化
              self$data <- data       
              super$initialize( distmat = as.matrix(dist(data)) )
              self$n_points <- nrow(data)
            },
            
            create_changed_distmat = function(l_rate, eta, l_idx){#変化させた距離行列を作成
              
              temp_dist <- DistmatPD$new(distmat = self$distmat)
              
              temp_dist$change_dist(l_rate = l_rate, eta = eta, l_idx = l_idx)
              
              self$alt_distmat <- append(self$alt_distmat, temp_dist)
              
            },
            
            #サブサンプルを作成。TDAdatasetクラス
            #sub_size=サブサンプルの点数、n_subs=サブサンプルの個数
            create_subsample = function(sub_size, n_subs){
              
              self$subsamples <- usephacm:::bootstrapper(X = self$data, size = sub_size, samples = n_subs) %>% 
                lapply(., function(x)TDAdataset$new(data = x)) %>% append(self$subsamples, .)
              
            },
            
            #サブサンプルの中で最大の閾値を求める
            max_thresh = function(dim = 1){
              
              if(length(self$subsamples) < 1){stop("subsamples didn't created.")}
              
              map(self$subsamples, ~{
                
                if(length(.$get_pd()) <= 1){stop("pd didn't calculated.")}
                  
                })
              
              max_t <- map_dbl(self$subsamples, ~{.$get_thresh(dim)}) %>% max()
              
              return(max_t)
              
            },
            
            #alt_distmatの距離行列群から求められたPDをリストにまとめる
            bind_mat_pds = function(){
              
              if(length(self$alt_distmat) < 1){stop("alt_distmat didn't created.")}
              
              self$mat_pds<-lapply(self$alt_distmat, function(X){
                
                if(length(X$get_pd()) <= 1){stop("pd didn't calculated.")}
                
                return(X$get_pd())
                
              })
              
            },
            
            #alt_distmatの距離行列群から求められたPLをリストにまとめる
            bind_mat_pls = function(){
              
              if(length(self$alt_distmat) < 1){stop("alt_distmat didn't created.")}
              
              self$mat_pls<-lapply(self$alt_distmat, function(X){
                
                if(length(X$get_pl()) <= 1){stop("pl didn't calculated.")}
                
                return(X$get_pl())
                
              })
              
            },
            
            #alt_distmatの距離行列群から求められたpeakをリストにまとめる
            bind_mat_peaks = function(){
              
              self$mat_peaks<-lapply(self$alt_distmat, function(X){X$peaks}) %>% do.call(rbind, .) %>% 
                set_rownames(paste0("alt_mat", seq_along(self$alt_distmat)))
              
            },
            
            #subsamplesのサブサンプル群から求められたPDをリストにまとめる
            bind_sub_pds = function(){
              
              if(length(self$subsamples) < 1){stop("subsamples didn't created.")}
              
              self$sub_pds<-lapply(self$subsamples, function(X){
                
                if(length(X$get_pd()) <= 1){stop("pd didn't calculated.")}
                
                return(X$get_pd())
                
              })
              
            },
            
            #subsamplesのサブサンプル群から求められたPLをリストにまとめる
            bind_sub_pls = function(){
              
              if(length(self$subsamples) < 1){stop("subsamples didn't created.")}
              
              self$sub_pls<-lapply(self$subsamples, function(X){
                
                if(length(X$get_pl()) <= 1){stop("pl didn't calculated.")}
                
                return(X$get_pl())
                
              })
              
            },
            
            #subsamplesのサブサンプル群から求められたpeakをリストにまとめる
            bind_sub_peaks = function(){
              
              self$sub_peaks<-lapply(self$subsamples, function(X){X$peaks}) %>% do.call(rbind, .) %>% 
                set_rownames(paste0("sub", seq_along(self$subsamples)))
              
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
#距離関数に対してPH関連関数をまとめた

DistmatPD<-
  R6Class(classname = "DistmatPD",
  
    public = list(
      
      distmat = NA, #距離行列
      maxdim = NA, #計算する位相特徴の最大次元
      maxscale = NA, #フィルトレーションの最大値
      peaks = 0, #平滑化後のPLの局所最大値の数
      
      initialize = function(distmat){self$distmat <- distmat},
      
      change_dist = function(l_rate, eta, l_idx){
        
        private$eta <- eta
        
        if(missing(l_idx)){
          private$l_rate <- l_rate
          private$l_idx <- landmark_points(X = self$distmat, n_land = nrow(self$distmat)*l_rate, d_mat = T)
        }else{
          private$l_idx <- l_idx
          private$l_rate <- length(l_idx)/nrow(self$distmat)
          }
        
        self$distmat <- dist_wvr_change(X_dist = self$distmat, lands = private$l_idx, eta = eta)
        
        
        private$changed = TRUE
        
      },
      
      calc_pd = function(maxdim, maxscale){
        
        self$maxdim <- maxdim
        self$maxscale <- maxscale
        
        private$time<-system.time( private$pd <- calculate_homology(mat = self$distmat, dim = self$maxdim, threshold = self$maxscale, format = "distmat") )
        
      },
      
      plot_pd = function(){
        
        if(length(private$pd) == 1){stop("pd didn't calcutated.")}
        
        plot_persist(private$pd)
        
        },
      
      plot_diag = function(){#TDAのdiagramを描画
        
        as_diag(pd=private$pd) %>% plot()
        legend("bottomright", legend = c("dim0", "dim1", "dim2", "dim3")[1:(max(private$pd[,1])+1)], 
               col=c(1, 2, 4, 3)[1:(max(private$pd[,1])+1)], pch=c(16, 2, 23, 3)[1:(max(private$pd[,1])+1)], cex=1.5)
        
      },
      
      #TDAstatの、TDAの、もしくは自作の後からgraphicで編集可能なパーシステントバーコードを描画
      #gplt=TDAstatの(ggplotで描かれた)バーコードを描画するか否か
      #edit=自作の編集可能なバーコードを描画するか否か
      plot_bar = function(dim, xlim, ylim, col, lwd = 2, ggplot = T, edit = F, ...){
        
        if(length(private$pd) == 1){stop("pd didn't calcutated.")}
        
        if(edit){
          
          if(missing(dim)){dim<-0:self$maxdim}
          
          fill_ifmissing(xlim = c(min(private$pd[, 2]), max(private$pd[, 3])), ylim = c(0, nrow(private$pd)+1), 
                         col = c(1, 2, 4, 3, 5:(5+max(0, max(dim)-3)) )[1:(max(dim)+1)] )
          
          plot_per_barc(pd = private$pd, dim, xlim, ylim, col, lwd = lwd, ...)
          
          }
        
        else if(ggplot){TDAstats::plot_barcode(private$pd)}
        
        else{as_diag(private$pd) %>% plot(barcode=T)}
        
      },
      
      #PDを呼び出し
      #dimで次元指定。missingの場合はすべて返す
      get_pd = function(dim){
        
        if(missing(dim)){return(private$pd)}
        
        else{
          
          assertthat::assert_that(is.numeric(dim))
          
          return(private$pd[(private$pd[,1] %in% dim), ])
          
          }
        
        },
      
      #pdを外部から代入。すでに計算されている場合はstop
      input_pd = function(pd){
        
        if( is.matrix(private$pd) && ("dimension" %in% colnames(private$pd)) ){
          stop("pd is already calculated.")
        }
        
        private$pd<-pd
        self$maxdim<-max(pd[, 1])
        self$maxscale<-round(max(pd[, 3])+0.5)
        
      },
      
      calc_pl = function(plot = T){
        private$pl <- calc_landscape(diag = private$pd, maxscale = self$maxscale, plot = plot)
        },
      
      plot_pl = function(dim, xlim, ylim){
        if(length(private$pl) == 1){self$calc_pl()}
        
        plot_landscape(land = private$pl, dim = dim, xlim = xlim, ylim = ylim)
        
        }, 
      
      get_thresh = function(dim){#次元ごとの閾値を返す
        
        return(usephacm:::persistence_weighted_mean(private$pd)*(2*pi)/usephacm:::surface_nshpere(dim))
        
      },
      
      pl_peak_count = function(show = T){#平滑化したPLの局所最大値を数える
        
        self$peaks <- 
          sapply(1:self$maxdim, function(d){calc.landscape.peak(X = private$pl[[paste0(d, "-land")]], dimension = d, 
                                                                thresh = self$get_thresh(d), tseq = private$pl[["tseq"]], show = show)})
        
        names(self$peaks) <- sapply(1:self$maxdim, function(i){paste0(i, "dim")})
        
      },
      
      get_pl = function(){return(private$pl)}, #PLを呼び出し
      
      input_pl = function(pl){
        
        if( is.list(private$pl) && "tseq" %in% names(private$pl) ){
          stop("pl is already calculated.")
        }
        
        private$pl<-pl
        
      },
      
      #計算されたPDから、各次元ごとにパーシステンスを求める
      calc_persists = function(){
        
        if(length(private$pd) == 1){stop("pd didn't calcutated.")}
        
        private$persists<-lapply(c(0, seq_len(self$maxdim)), function(i){calc_per(private$pd, dim = i)}) %>% 
          set_names(paste0("pers_dim", c(0, seq_len(self$maxdim))))
        
      },
      
      get_persists = function(){return(private$persists)}, #パーシステンスを呼び出し
      
      get_time = function(){return(private$time)}, #PDの計算時間を呼び出し
      
      is_changed = function(){return(private$changed)}, #距離行列を変化させたか否かを呼び出す
      
      #l_rate(ランドマーク点割合), eta, l_idx(ランドマーク点のインデックス)呼び出し。
      #距離行列が変化していない場合はハイパラを呼び出せない
      get_param = function(){
        
        if(!self$is_changed()){stop("Distance matrix didn't changed. Parameters aren't set.")}
        
        return(list(l_rate=private$l_rate, eta=private$eta, l_idx=private$l_idx))
        
        }
      
    ),#public閉じ括弧
    
    private = list(
      pd = NA, #TDAstatsのPD
      pl = NA, #パーシステントランドスケープ
      persists = list(), #パーシステンス
      time = NA, #PDの計算時間
      l_rate = 0, #ランドマーク点割合
      eta = 1, #1-exp{-(d_ij/eta)^2}のハイパラ
      l_idx = NA, #ランドマーク点のインデックス
      changed = FALSE #距離行列を変化させたか否か
    )
  
)#DistmatPD Class閉じ括弧
