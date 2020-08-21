#画像処理に関する関数群
#画像のサイズを画像リストの中の最大サイズに統一する
#回転後の処理などに使用
align_img_size<-function(imgs){
  
  assertthat::assert_that(is.list(imgs))
  
  max_size<-sapply(imgs, function(X)dim(X)[1]) %>% max()
  
  aligns<-lapply(imgs, function(X){
    
    debugText(dim(X))
    
    if(dim(X)[1] != max_size){
      
      s_diff<-max_size - dim(X)[1]
      
      if(s_diff == 1){
        
        lx<-by<-s_diff
        
        X2<-cbind(matrix(0, dim(X)[1], lx), X[, , 1, 1]) %>% rbind(., matrix(0, by, max_size))
        
        return(X2)
        
      }
      
      else if((s_diff/2) %% 1 != 0){
        
        lx<-(s_diff/2)-0.5
        rx<-(s_diff/2)+0.5
        uy<-(s_diff/2)-0.5
        by<-(s_diff/2)+0.5
        
      }else{lx<-rx<-uy<-by<-(s_diff/2)}
      
      
      X2<-cbind(matrix(0, dim(X)[1], lx), X[, , 1, 1]) %>% cbind(., matrix(0, dim(X)[1], rx)) %>% rbind(matrix(0, uy, max_size), .) %>% rbind(., matrix(0, by, max_size))
      debugText(max_size, dim(X2))
      
      
      return(X2)}else{return(X[, , 1, 1])}
    
  })
  
  return(aligns)
  
}

#-------------------------------------------------
#画像サイズを指定サイズpixに統一する
align_img_pix<-function(imgs, pix){
  
  assertthat::assert_that(is.list(imgs))
  
  aligns<-lapply(imgs, function(X){
    
    debugText(dim(X))
    
    if(dim(X)[1] != pix){
      
      s_diff<-pix - dim(X)[1]
      
      if(s_diff == 1){
        
        lx<-by<-s_diff
        
        X2<-cbind(matrix(0, dim(X)[1], lx), X[, , 1, 1]) %>% rbind(., matrix(0, by, pix))
        
        return(X2)
        
      }
      
      else if((s_diff/2) %% 1 != 0){
        
        lx<-(s_diff/2)-0.5
        rx<-(s_diff/2)+0.5
        uy<-(s_diff/2)-0.5
        by<-(s_diff/2)+0.5
        
      }else{lx<-rx<-uy<-by<-(s_diff/2)}
      
      
      X2<-cbind(matrix(0, dim(X)[1], lx), X[, , 1, 1]) %>% cbind(., matrix(0, dim(X)[1], rx)) %>% rbind(matrix(0, uy, pix), .) %>% rbind(., matrix(0, by, pix))
      debugText(dim(X2))
      
      
      return(X2)}else{return(X[, , 1, 1])}
    
  })
  
  return(aligns)
  
}
