#歩容イメージの分析
#APヘタリア日本
jp_gait1<-load.image("./data/japan_gait_90deg2_00001.png")

#グレースケール変換
jp_gait1_gray<-convert2Gray(jp_gait1)

#ゴマ塩ノイズを載せる
jp_gait1_nois<-noizeSaltPapper(jp_gait1_gray)

#画像サイズ縮小
jp_gait1A<-resize(jp_gait1, round(width(jp_gait1)/3), round(height(jp_gait1)/3))

#トリミング
jp_gait1B<-imsub(jp_gait1, x > 250) %>% imsub(., x < 150) %>%  plot()

#トリミング後、縮小
jp_gait1C<-imsub(jp_gait1, x > 250) %>% imsub(., x < 150) %>% resize(., round(width(.)/2), round(height(.)/2)) %>% plot()

#トリミング、縮小、グレースケール化
jp_gait1C_gray<-convert2Gray(jp_gait1C)

#歩容画像2枚め
jp_gait2<-load.image("D:/okayasu/D_documents/MMD_walk/japan_gait_000deg/japan_gait_0deg_00001.png")

#トリミング、縮小、グレースケール化
jp_gait2_gray<-imsub(jp_gait2, x > 250) %>% imsub(., x < 150) %>% resize(., round(width(.)/2), round(height(.)/2)) %>% convert2Gray(.)

#-----------------------------------------------------
#歩容画像一括読み込み
#0~75度まで
jp_gaits_list<-lapply(seq(0, 75, by=15), function(k){
  
  img<-load.image(paste0("D:/okayasu/D_documents/MMD_walk/japan_gait_", formatC(k, width = 3, flag = "0") , "deg/japan_gait_", k, "deg_00001.png"))
  gray_img<-imsub(img, x > 250) %>% imsub(., x < 150) %>% resize(., round(width(.)/2), round(height(.)/2)) %>% convert2Gray(.)
  
  return(gray_img)
  
})

#歩容画像読み込み
#90度
jp_gait_90deg1<-load.image("D:/okayasu/D_documents/MMD_walk/japan_gait_090deg/japan_gait_90deg2_00001.png") %>%  imsub(., x > 250) %>% imsub(., x < 150) %>% 
                  resize(., round(width(.)/2), round(height(.)/2)) %>% convert2Gray(.)

#歩容画像一括読み込み
#105~345度
jp_gaits_list2<-lapply(seq(105, 120, by=15), function(k){
  #debugText(k)
  img<-load.image(paste0("D:/okayasu/D_documents/MMD_walk/japan_gait_", k, "deg/japan_gait_", k, "deg_00001.png"))
  gray_img<-imsub(img, x > 250) %>% imsub(., x < 150) %>% resize(., round(width(.)/2), round(height(.)/2)) %>% convert2Gray(.)
  
  return(gray_img)
  
})

#歩容画像読み込み
#135度
jp_gait_135deg1<-load.image("D:/okayasu/D_documents/MMD_walk/japan_gait_135deg/test_walk_135deg_00001.png") %>%  imsub(., x > 250) %>% imsub(., x < 150) %>% 
  resize(., round(width(.)/2), round(height(.)/2)) %>% convert2Gray(.)

#歩容画像一括読み込み
#105~345度
jp_gaits_list3<-lapply(seq(150, 165, by=15), function(k){
  debugText(k)
  img<-load.image(paste0("D:/okayasu/D_documents/MMD_walk/japan_gait_", formatC(k, width = 3, flag = "0"), "deg/japan_gait_", k, "deg_00001.png"))
  gray_img<-imsub(img, x > 250) %>% imsub(., x < 150) %>% resize(., round(width(.)/2), round(height(.)/2)) %>% convert2Gray(.)
  
  return(gray_img)
  
})

#歩容画像読み込み
#180度
jp_gait_180deg1<-load.image("D:/okayasu/D_documents/MMD_walk/japan_gait_180deg/test_walk_270deg_00001.png") %>%  imsub(., x > 250) %>% imsub(., x < 150) %>% 
  resize(., round(width(.)/2), round(height(.)/2)) %>% convert2Gray(.)

#歩容画像一括読み込み
#105~345度
jp_gaits_list4<-lapply(seq(195, 345, by=15), function(k){
  debugText(k)
  img<-load.image(paste0("D:/okayasu/D_documents/MMD_walk/japan_gait_", formatC(k, width = 3, flag = "0"), "deg/japan_gait_", k, "deg_00001.png"))
  gray_img<-imsub(img, x > 250) %>% imsub(., x < 150) %>% resize(., round(width(.)/2), round(height(.)/2)) %>% convert2Gray(.)
  
  return(gray_img)
  
})

#画像リスト統合
jp_Gait_list<-c(jp_gaits_list, list(jp_gait_90deg1), jp_gaits_list2, list(jp_gait_135deg1), jp_gaits_list3, list(jp_gait_180deg1), jp_gaits_list4)

for (i in 1:length(jp_Gait_list)) {
  showImage(jp_Gait_list[[i]])
}

#画像を行ベクトルとして並べて行列化
jp_Gait_mat<-do.call(rbind, lapply(jp_Gait_list, as.vector))

jp_Gait_pca<-prcomp(jp_Gait_mat)
plot3d(jp_Gait_pca[["x"]][, 1:3])
texts3d(jp_Gait_pca[["x"]][, 1:3], texts = 1:24)

jp_Gait_pd1<-calculate_homology(jp_Gait_mat, dim = 2)
ahiru_pd1<-calculate_homology(ahiru.mat, dim = 2)

#----------------------------------------------
#5度刻みの歩容データ

#歩容画像一括読み込み

jp_gait2<-load.image("D:/okayasu/D_documents/MMD_walk/japan_gait2/japan_gait_000deg2/japan_gait_000deg2_00001.png")
jp_gait_gray<-imsub(jp_gait2, x > 250) %>% imsub(., x < 150) %>% resize(., round(width(.)/2), round(height(.)/2)) %>% convert2Gray(.) %>% t() %>%  showImage(.)

jp_gaits2_list<-lapply(seq(0, 355,by=5), function(k){
  debugText(k)
  img<-load.image(paste0("D:/okayasu/D_documents/MMD_walk/japan_gait2/japan_gait_", formatC(k, width = 3, flag = "0"), "deg2/japan_gait_", formatC(k, width = 3, flag = "0"), "deg2_00001.png"))
  gray_img<-imsub(img, x > 250) %>% imsub(., x < 150) %>% resize(., round(width(.)/2), round(height(.)/2)) %>% convert2Gray(.)
  
  return(gray_img)
  
})

jp_gait2_mat<-do.call(rbind, lapply(jp_gaits2_list, as.vector))

jp_gait2_pca<-prcomp(x = jp_gait2_mat)
plot3d(jp_gait2_pca[["x"]][, 1:3], col=rainbow(72))
rgl.snapshot("./data/jp_gait2_pca.png")

jp_gait2_pd1<-calculate_homology(jp_gait2_mat, dim = 2)

#プロット
jp_gaits2_list4plot<-lapply(jp_gaits2_list, t)
showImageList(jp_gaits2_list4plot[c(1, 10, 19, 28, 37, 46, 55, 64, 72)])

#-----------------------------------------
#COIL-20データの回転を試してみる
ahiru1<-load.image("D:/okayasu/D_documents/coil-20-proc/coil-20-proc/obj1__0.png")

#画像回転
ahiru_rot30<-imrotate(ahiru1, angle = 30) %>% plot()

#回転後、サイズ縮小
ahiru_rot30A<-resize(ahiru_rot30, round(width(ahiru_rot30)/2), round(height(ahiru_rot30)/2))
plot(ahiru_rot30A)

#画像回転後、元サイズと同じになるようにトリミング
ahiru_rot30B<-imsub(ahiru_rot30, x < (178-25) ) %>% imsub(., x > 25 ) %>% imsub(., y < (178-25) ) %>% imsub(., y > 25 ) %>%  plot()

#90度回転
ahiru_rot90<-imrotate(ahiru1, angle = 90) %>% plot()

#60度回転
ahiru_rot60<-imrotate(ahiru1, angle = 60) %>% plot()
ahiru_rot60B<-imsub(ahiru_rot60, x < (dim(ahiru_rot60)[1]-24) ) %>% imsub(., x > 23 ) %>% imsub(., y < (dim(ahiru_rot60)[2]-24) ) %>% imsub(., y > 23 ) %>%  plot()

#45度回転後、元サイズと同じになるようにトリミング
ahiru_rot45<-imrotate(ahiru1, angle = 45) %>% plot()
ahiru_rot45B<-imsub(ahiru_rot45, x < (181-27) ) %>% imsub(., x > 27 ) %>% imsub(., y < (181-27) ) %>% imsub(., y > 27 ) %>%  plot()

#黒部分を増やして画像を拡大
#ahiru_rot60と同じピクセルサイズになるように
ahiru1B<-cbind(matrix(0, 128, 23), ahiru1[, , 1, 1]) %>% cbind(., matrix(0, 128, 23)) %>% rbind(matrix(0, 23, 174), .) %>% rbind(., matrix(0, 23, 174))

#配列結合テスト
array1<-array(0, dim=c(4, 3, 2))

array2<-array(1, dim=c(4, 3, 2))

#アヒル回転画像のリストを作る
ahiru_rot_list<-lapply(seq(0, 360, by=5), function(k){
  
  a_rot<-imrotate(ahiru1, angle = k)
  return(a_rot)
  
})

#アヒル回転画像のサイズを統一する
ahiru_pix<-sapply(ahiru_rot_list, function(X)dim(X)[1])

ahiru_align_list<-align_img_size(ahiru_rot_list)
ahiru_align_list_4plot<-lapply(ahiru_align_list, t)

#画像を行ベクトルとして並べて行列化
ahiru_rot_mat<-do.call(rbind, lapply(ahiru_align_list, as.vector))
ahiru_rot_pca<-prcomp(ahiru_rot_mat)
plot3d(ahiru_rot_pca[["x"]][, 1:3], col=rainbow(72))
texts3d(ahiru_rot_pca[["x"]][, 1:3], texts = 1:72)
rgl.snapshot("./data/ahiru_rot_pca.png")

ahiru_rot_pd1<-calculate_homology(ahiru_rot_mat, dim = 2)
class(ahiru_rot_pd1)<-NULL
plot(ahiru_rot_pd1[, 2:3], pch=c(20, 2, 3)[ahiru_rot_pd1[, 1]+1], col=c(1, 2, 3)[ahiru_rot_pd1[, 1]+1], xlim = c(0, 55), ylim = c(0, 55))
abline(0, 1)

#----------------------------------------------
showImageList(ahiru.img[c(1, 10, 19, 28, 37, 46, 55, 64, 72)])

#-------------------------------------------------
#アヒル画像一括読み込み
ahiru_list<-lapply(0:71, function(k){
  
  ahiru<-load.image(paste0("D:/okayasu/D_documents/coil-20-proc/coil-20-proc/obj1__", k, ".png"))
  
  return(ahiru)
  
})

#アヒル回転画像リスト作成
ahiru2_rot_list<-lapply(seq(0, 360, by=5), function(k){
  
  a_rot<-imrotate(ahiru_list[[2]], angle = k)
  return(a_rot)
  
})


ahiru2_pix<-align_img_pix(imgs = ahiru2_rot_list, pix = 181)

#アヒル回転画像リスト作成
#全画像に対して
ahiru_rot_list<-lapply(ahiru_list, function(img){
  
  a_list<-lapply(seq(0, 360, by=5), function(k){
    
    a_rot<-imrotate(img, angle = k)
    return(a_rot)
  
  })
  
  return(a_list)

}) %>% lapply(., function(X)align_img_pix(imgs = X, 181))
  
 
#画像を行ベクトルとして並べて行列化
ahiru_rot_mat<-do.call(rbind, lapply(ahiru_align_list, as.vector))  

ahiru_rmat_list<-lapply(ahiru_rot_list, function(X)do.call(rbind, lapply(X, as.vector)))
ahiru_rmat<-do.call(rbind, ahiru_rmat_list)  

#アヒル回転画像のPD計算
ahiru_rot_pd<-calculate_homology(mat = ahiru_rmat[sample(nrow(ahiru_rmat), nrow(ahiru_rmat)*0.5), ], dim = 2)
ahiru_rot_pl<-calcLandscape(diag = ahiru_rot_pd, maxscale = 47)

ahiru_rot_pd2<-calculate_homology(mat = ahiru_rmat[sample(nrow(ahiru_rmat), nrow(ahiru_rmat)*0.3), ], dim = 2, threshold = 50)
ahiru_rot_pl2<-calcLandscape(diag = ahiru_rot_pd2, maxscale = 50)

ahiru_rot_pd2B<-as_diagram(ahiru_rot_pd2) %>% plot()
#--------------------------------------------------------------
#アヒル回転画像をプロット
ahiru_align_list4plot<-lapply(c(1, 10, 19, 28, 37, 46, 55, 64, 72), function(k)t(ahiru_rot_list[[k]][[1]]))
ahiru_rot1_list4plot<-lapply(c(1, 10, 19, 28, 37, 46, 55, 64, 72), function(k)t(ahiru_rot_list[[1]][[k]]))
ahiru_rot10_list4plot<-lapply(c(1, 10, 19, 28, 37, 46, 55, 64, 72), function(k)t(ahiru_rot_list[[10]][[k]]))
ahiru_rot46_list4plot<-lapply(c(1, 10, 19, 28, 37, 46, 55, 64, 72), function(k)t(ahiru_rot_list[[46]][[k]]))
showImageList(ahiru_rot46_list4plot)
