#-------------------------------------------
#WVRの適用後の計算を見る----
x1<-c(1.2, 0)
x2<-c(0, 0.9)
x3<-c(-1.1, 0)
x4<-c(0, -1.3)
x5<-c(0.5, 0.4)
d_t<-cbind(c(1.2, 0, -1.1, 0, 0.5), c(0, 0.9, 0, -1.3, 0.4))
plot(d_t, xlim = c(-1.5, 1.5), ylim = c(-1.5, 1.5))
text(c(1.2, 0, -1.1, 0, 0.5), c(0, 0.9, 0, -1.3, 0.4), labels = 1:5, pos=3)

d_t_inst<-TDAdataset$new(d_t)

# > d_t_inst$get_pd()
# dimension    birth     death
# [1,]         0 0.000000 0.7071068
# [2,]         0 0.000000 0.8062258
# [3,]         0 0.000000 1.4212670
# [4,]         0 0.000000 1.7029386
# [5,]         1 1.769181 1.7720045 #パーシステンス0.002823913

# > d_t_inst$distmat
# 1         2        3        4         5
# 1 0.0000000 1.5000000 2.300000 1.769181 0.8062258
# 2 1.5000000 0.0000000 1.421267 2.200000 0.7071068
# 3 2.3000000 1.4212670 0.000000 1.702939 1.6492423
# 4 1.7691806 2.2000000 1.702939 0.000000 1.7720045
# 5 0.8062258 0.7071068 1.649242 1.772005 0.0000000

lines(c(x2[1], x5[1]), c(x2[2], x5[2]))
lines(c(x1[1], x5[1]), c(x1[2], x5[2]))
lines(c(x2[1], x3[1]), c(x2[2], x3[2]))
lines(c(x1[1], x2[1]), c(x1[2], x2[2]))
lines(c(x3[1], x5[1]), c(x3[2], x5[2]))
lines(c(x3[1], x4[1]), c(x3[2], x4[2]))
lines(c(x1[1], x4[1]), c(x1[2], x4[2]))#生成#1.769181
lines(c(x4[1], x5[1]), c(x4[2], x5[2]))#消滅#1.7720045

d_t_inst$create_changed_distmat(l_rate = 0.4, eta = 1)
d_t_inst$create_changed_distmat(l_rate = 0.2, eta = 1)
d_t_inst$create_changed_distmat(l_rate = 0.5, eta = 1)

d_t_inst$alt_distmat[[8]]$calc_pd(maxdim = 1, maxscale = 3)

# > d_t_inst$alt_distmat[[8]]$distmat
# 1         2        3        4         5
# 1 0.000000 1.3419012 2.288404 1.691836 0.3853390
# 2 1.341901 0.0000000 1.232728 2.200000 0.7071068
# 3 2.288404 1.2327282 0.000000 1.609237 1.5405988
# 4 1.691836 2.2000000 1.609237 0.000000 1.7720045
# 5 0.385339 0.7071068 1.540599 1.772005 0.0000000

# > d_t_inst$alt_distmat[[8]]$get_pd()
# dimension    birth     death
# [1,]         0 0.000000 0.3853390
# [2,]         0 0.000000 0.7071068
# [3,]         0 0.000000 1.2327282
# [4,]         0 0.000000 1.6092375
# [5,]         1 1.691836 1.7720045 #パーシステンスは0.08016859

#d_t_instの8個目の変化後距離行列の下三角行列
d_t_altdistmat8_low<-d_t_inst$alt_distmat[[8]]$distmat
d_t_altdistmat8_low[upper.tri(d_t_altdistmat8_low)]<-0

#d_t_instのオリジナル距離行列の下三角行列
d_t_dist_low<-d_t_inst$distmat
d_t_dist_low[upper.tri(d_t_dist_low)]<-0

#d_t_instの8個目の変化後距離行列の値をソート
#[8]生成、[9]消滅
d_t_altdistmat8_sorted<-sort(d_t_altdistmat8_low) %>% unique()

for (i in 1:8) {
  idx<-which(d_t_altdistmat8_low==d_t_altdistmat8_sorted[i], arr.ind = T) 
  draw_line(x = d_t[idx[1], ], y =d_t[idx[2],])
}

d_t8_idxs<-lapply(2:9, function(i)which(d_t_altdistmat8_low==d_t_altdistmat8_sorted[i], arr.ind = T)) %>% do.call(rbind, .)

d_t_inst$create_changed_distmat(eta = 1, l_idx = c(3, 4))
d_t_inst$create_changed_distmat(eta = 1, l_idx = c(1, 2))
d_t_inst$create_changed_distmat(eta = 1, l_idx = c(1, 3, 5))
d_t_inst$create_changed_distmat(eta = 1, l_idx = c(3, 5))

#-------------------------------------
#d_ij*[1-exp(-(d_ij/eta)^2)による距離変化前後のフィルトレーション比較----
library(animation)
library(jpeg)
library(tcltk)

#描画用関数
plot_filt_wvr<-function(){
  
  #フィルトレーション用の円の半径増大ベクトル
  filt_line<-seq(0.2, 2, by=0.03)
  
  for (r in filt_line){
    
    cat("r=", r, "\n")
    #プロット画面分割
    par(mfrow=c(1,2)) 
    
    #変化前描画
    plot(d_t, xlim = c(-1.5, 1.5), ylim = c(-1.5, 1.5), main = paste0("r=", r))
    text(d_t[,1], d_t[,2], labels = as.character(1:5), pos = 3)
    
    #閾値以下の要素のインデックスの行列
    idx_mat<-which(d_t_dist_low > 0 & d_t_dist_low <= r, arr.ind = T)
    
    if(length(idx_mat) != 0){
      #結ばれる線の描画
      trush<-apply(idx_mat, 1, function(p)draw_line(x = d_t[p[1], ], y = d_t[p[2], ]))
      
    }
    
    #変化後描画
    plot(d_t[c(2,4,5), ], xlim = c(-1.5, 1.5), ylim = c(-1.5, 1.5), main = paste0("r=", r))
    points(d_t[c(1,3), ], pch=16, col=2)
    text(d_t[, 1], d_t[, 2], labels = as.character(1:5), pos = 3)
    
    #閾値以下の要素のインデックスの行列
    idx_mat<-which(d_t_altdistmat8_low > 0 & d_t_altdistmat8_low <= r, arr.ind = T)
    
    if(length(idx_mat) != 0){
      #結ばれる線の描画
      trush<-apply(idx_mat, 1, function(p)draw_line(x = d_t[p[1], ], y = d_t[p[2], ]))
      
    }
    
  }
  
}

#ffmpegによる動画化
ff_pass<-animation::ani.options(ffmpeg="D:/okayasu/D_download/ffmpeg-4.3.1-2020-10-01-essentials_build/bin/ffmpeg.exe")

saveVideo(expr = plot_filt_wvr(), video.name = "filt_comp_wvr.mp4", img.name = "filt_comp_wvr", interval=0.3, ani.width=1000, ani.height=500)
