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
  
  vars<-sapply(ls(pos = .GlobalEnv), function(var)save(list = var, file = paste0("./vars/", var, ".RData")))
  
}

#-----------------------------------------------------
#pathで指定したディレクトリ内に、引数で指定した変数のRDataがあれば
#.GlobalEnvにロードする

search_load<-function(var, path="./data"){
  
  elname <- substitute(var) %>% as.character()
  
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

