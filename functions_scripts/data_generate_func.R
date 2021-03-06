#データセットを作る関数群

#--------------------------------------
#立方体の各面に一様分布
#立方体全体でみて一様分布にはなっていない
#l=一辺の長さ、fp=一つの面上にある点の個数
cube_xunif<-function(l=1, fp=100){
  
  faces<-lapply(1:6, function(i){
    
    x<-runif(fp, 0, l)
    y<-runif(fp, 0, l)
    
    return(cbind(x, y))
    
  })
  
  bottom<-cbind(faces[[1]], rep(0, fp))
  right<-cbind(rep(l, fp), faces[[2]][, 2], l-faces[[2]][, 1])
  front<-cbind(faces[[3]][,1], rep(0, fp), faces[[3]][,2])
  behind<-cbind(faces[[4]][,1], rep(l, fp), l-faces[[4]][,2])
  left<-cbind(rep(0, fp), faces[[5]][,2], faces[[5]][,1])
  top<-cbind(faces[[6]], rep(l, fp))
  cube<-rbind(bottom, right, front, behind, left, top)
  
  return(cube)
  
}

#-----------------------------------
#N次元直方体・立方体表面に一様分布する点群を作成
#n=データ点数、sides=各辺の長さ、d=直方体の次元
#rep(1, length = 6) %>% divide_by(., sum(.)) %>% accumulate(sum)
xRect_unif<-function(n, sides = rep(1, length = 3), d = 3){
  
  if(length(sides)==1){sides<-rep(sides, length=d)}
  assertthat::assert_that(length(sides)==d)
  
  face<-combn(x = 1:d, m = d-1)
  face_area<-combn(x = sides, m = d-1) %>%  apply(2, prod) %>% rep(each = 2)
  prob<-divide_by(face_area, sum(face_area)) %>% accumulate(sum) %>% c(0, .)
  
  unif_num<-runif(n)
  
  rect<-lapply(1:d, function(i){
    
    n1<-range_index(unif_num, prob[2*i-1], prob[2*i]) %>% length()
    n2<-range_index(unif_num, prob[2*i], prob[2*i+1]) %>% length()
    
    coord_mat1<-matrix(0, nrow = n1, ncol = d)
    coord_mat2<-matrix(0, nrow = n2, ncol = d)
    
    for (j in face[, i]) {
      
      coord_mat1[, j]<-runif(n1, min = 0, max = sides[j])
      coord_mat2[, j]<-runif(n2, min = 0, max = sides[j])
      
    }
    
    remain_axis<-setdiff(1:d, face[, i])
    coord_mat1[, remain_axis]<-rep(0, length = n1)
    coord_mat2[, remain_axis]<-rep(sides[remain_axis], length = n2)
    
    return(rbind(coord_mat1, coord_mat2))
    
  }) %>% do.call(rbind, .)
  
  #各面のデータ点数算出
  #表現方法要修正
  face_point<-lapply(1:d, function(k){
    
    low<-which(rect[, k] == 0) %>% length()
    high<-which(rect[, k] == sides[k]) %>% length()
    
    return(lst(low, high))
    
  }) %>% flatten() %>% as_vector() %>% tibble(axis=paste0(rep(paste0("axis", 1:d), each=2), names(.)), n_point=.)
  
  attr(rect, "face_point")<-face_point
  attr(rect, "face")<-face
  attr(rect, "face_area")<-face_area
  attr(rect, "prob")<-prob
  
  return(rect)
  
}

#-------------------------------------------------
#楕円体状の一様分布(仮)
#ヤコビアンを使っている(一応)
#n=データ点数、a=x軸方向の径、b=y軸方向の径、c=z軸方向の径、

xEllip_unif<-function(n, a, b, c){
  
  p<-runif(n, -1, 1)
  theta<-acos(-p)
  phi<-runif(n, 0, 2*pi)
  
  x<-a*sin(theta)*cos(phi)
  y<-b*sin(theta)*sin(phi)
  z<-c*cos(theta)
  
  return(cbind(x, y, z))
  
}

#----------------------------------------
#半径Rの円内に一様分布する点群を作る関数
#N=データ点数、R=円盤の半径

uniDisMake<-function(N, R){
  
  theta <- runif(N, min=0, max=2*pi)
  r <- sqrt(2*runif(N, min=0, max=0.5*R^2))
  df <- data.frame(x=r*cos(theta), y=r*sin(theta))
  return(df)
  
}

#-----------------------------------------------
#3Dトーラスの角度一様分布
rad_distribute<-function(rad, r, R1, R2){
  
  theta<-rad[1]
  phi<-rad[2]
  
  g<-1/R1*(1 + (r/R2)*sin(theta))*(1/R2 + (1/R1)*sin(phi)*(1 + (r/R2)*sin(theta)))
  
  return(g)
  
}


#-------------------------------------------
#3Dトーラス一様分布を作る。暫定版
#n=データ点数、r, R1, R2=半径
x3Dtorus_unif<-function(n, r, R1, R2){
  
  theta<-c()
  phi<-c()
  
  max_g<-rad_distribute(rad = c(pi/2, pi/2), r, R1, R2)
  
  #関数の最適化により最小値を求める
  #par=初期パラメータ
  min_g<-optim(par = c(0, 0), fn = rad_distribute, r = r, R1 = R1, R2 = R2)$value
  
  while(length(theta) < n){
    
    xvec<-runif(1, 0, 2*pi)
    yvec<-runif(1, 0, 2*pi)
    zvec<-runif(1, min_g, max_g)
    
    fxy<-rad_distribute(rad = c(xvec, yvec), r, R1, R2)
    
    if(zvec <= fxy){
      theta<-c(theta, xvec)
      phi<-c(phi, yvec)
    }
    
  }
  
  delta<-runif(n, 0, 2*pi)
  
  x<-sin(delta)*(R1 + sin(phi)*(R2 + r*sin(theta)))
  y<-cos(delta)*(R1 + sin(phi)*(R2 + r*sin(theta)))
  z<-cos(phi)*(R2 + r*sin(theta))
  w<-r*cos(theta)
  
  out<-cbind(x, y, z, w)
  
  attr(out, "max_g")<-max_g
  attr(out, "min_g")<-min_g
  
  return(out)
  
}


#---------------------------------------
#円を描く関数
#(x,y)を中心とした半径rの円をpolygonで描く
#colで色指定。代入なしの場合は透明な白をpolygonに代入する

plot_circle<-function(x, y, r, col){
  
  if(missing(col)){col=rgb(1, 1, 1, 0)}
  
  theta <- seq(-pi, pi, length=100)
  polygon(x + r*cos(theta), y + r*sin(theta), col=col)
  
  
}

#----------------------------------------------------
#GlobalEnvの全変数(関数含む)を"変数名".RDataにしてvarsディレクトリに保存
save_environment_variale2rdata<-function (){
 
  dir <- match("vars", list.files())
  if (is.na(dir)) {
    dir.create("./vars")
  }
  else {
    if (!(file.info("./vars")$isdir)) {
      dir.create("./vars")
    }
  }
  
  vars<-sapply(ls(pos = .GlobalEnv), function(var){
    
      if( class(get(var, envir = .GlobalEnv)) != "function" ){
        
        save(list = var, file = paste0("./vars/", var, ".RData"))
        
      }
    
    })
  
}

#-----------------------------------------------------
#pathで指定したディレクトリ内に、引数で指定した変数のRDataがあれば
#.GlobalEnvにロードする
#指定した変数が.GlobalEnvにあればロードしない
#overwite=TRUEならば.GlobalEnvに変数が既ににあっても上書きする

search_load<-function(var, path="./data", overwite = F){
  
  elname <- substitute(var) %>% as.character()
  
  if(elname %in% ls(name = ".GlobalEnv")){
    
    if(!overwite){stop(paste0("'", elname, "' is already exit in '.GlobalEnv'." ))}
    
  }
  
  if(file.exists(paste0(path, "/", elname, ".RData"))){

    cat("file exists\n")
    load(paste0(path, "/", elname, ".RData"), envir = .GlobalEnv)

  }else{cat("file doesn't exit\n")}
  
  print(paste0(path, "/", elname, ".RData"))
  
}

#--------------------------------------------
#3次元球体内に一様分布---------
#n=データ点数、r=半径
xBall_unif<-function(n, r = 1){
  
  R<-runif(n = n, min = 0, max = r)
  theta<-runif(n = n, min = -1, max = 1)
  phi<-runif(n = n, min = 0, max = 2*pi)
  
  x<-(R^(1/3)) * sqrt(1 - theta^2) * cos(phi)
  y<-(R^(1/3)) * sqrt(1 - theta^2) * sin(phi)
  z<-(R^(1/3)) * theta
  
  return(cbind(x, y, z))
  
}

#--------------------------------------------
#楕円体表面積近似値算出関数----
#a=x軸方向の径、b=y軸方向の径、c=z軸方向の径
#p = 1.6075 のとき最大誤差は1.061% の近似式
ellip_surface<-function(a, b, c, p = 1.6075){
  
  s<-4*pi * ( ((a*b)^p + (a*c)^p + (b*c)^p)/3 )^(1/p)
  
  return(s)
  
}

#-------------------------------------------
#呼び出し元の関数で、引数に値が渡されていない(missingの)場合に予め定めた値を一括して代入する関数----
#...にmissingの場合に予め定めた値を代入したい変数と、代入したい値をセットで入れる
#例えば、fill_ifmissing(x = 1, y = 2)とすると、fill_missingを呼び出した関数内でx,yがmissingのとき、
#xに1が、yに2が代入される

fill_ifmissing<-function(...){
  
  args_list = as.list(match.call(definition = sys.function(-1), call = sys.call(-1)))
  
  #代入されていない引数に初期値を与える
  elp<-list(...)
  
  elp_names<-names(elp)
  arg_names<-names(formals(fun = sys.function(-1)))
  
  for (elp_n in elp_names) {
    
    #もし引数以外の値が指定されていたらエラーを出して止まる
    if(!(elp_n %in% arg_names)){stop(paste0(elp_n, " isn't the argument of ", args_list[[1]], "."))}
    
    if(is.null(args_list[[elp_n]])){
      
        assign(elp_n, elp[[elp_n]], envir = sys.frame(-1))
      
      }
    
  }
  
}

#---------------------------------------
#変数が.GlobalEnvに存在するか判定-------
isin_environment<-function(val){
  
  val_name<-substitute(val) %>% as.character()
  
  return(val_name %in% ls(envir = .GlobalEnv))
  
}


