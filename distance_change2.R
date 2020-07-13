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

#--------------------------------------
#TDAstat(ripserをつかったパッケージ)テスト
#ポイントクラウドデータ
trs300_1_10_subs_2_time2<-system.time(trs300_1_10_subs_2_pd2<-calculate_homology(mat = trs300_1_10_subs2[[2]], dim = 2))

#距離行列
trs300_1_10_subs2_2_time3<-system.time(trs300_1_10_subs2_2_pd3<-dist(trs300_1_10_subs2[[2]]) %>% as.matrix() %>% calculate_homology(mat = ., dim = 2, threshold = 3, format = "distmat"))
plot_persist(trs300_1_10_subs2_2_pd3)

#TDAstatからPLを求められるか？
#リストにすればできる
trs300_1_10_subs2_2_pl3<-calc_landscape(diag = list(trs300_1_10_subs2_2_pd3), maxscale = 3)

#------------------------------------
#ripsDiagの中身
function (X, maxdimension, maxscale, dist = "euclidean", 
          library = "GUDHI", location = FALSE, printProgress = FALSE) 
{
  if (!is.numeric(X) && !is.data.frame(X)) {
    stop("X should be a matrix of coordinates")
  }
  if (!is.numeric(maxdimension) || length(maxdimension) != 
      1 || maxdimension < 0) {
    stop("maxdimension should be a nonnegative integer")
  }
  if (!is.numeric(maxscale) || length(maxscale) != 1) {
    stop("maxscale should be a number")
  }
  if (dist != "euclidean" && dist != "arbitrary") {
    stop("dist should be either 'euclidean' or 'arbitrary'")
  }
  if (length(library) == 1) {
    library <- rep(library, 2)
  }
  if (library[1] == "gudhi" || library[1] == "Gudhi") {
    library[1] <- "GUDHI"
  }
  if (library[1] == "dionysus" || library[1] == "DIONYSUS") {
    library[1] <- "Dionysus"
  }
  if (library[1] != "GUDHI" && library[1] != "Dionysus") {
    stop("library for building a filtration should be a string: either 'GUDHI' or 'Dionysus'")
  }
  if (library[2] == "gudhi" || library[2] == "Gudhi") {
    library[2] <- "GUDHI"
  }
  if (library[2] == "dionysus" || library[2] == "DIONYSUS") {
    library[2] <- "Dionysus"
  }
  if (library[2] == "phat" || library[2] == "Phat") {
    library[2] <- "PHAT"
  }
  if (library[2] != "GUDHI" && library[2] != "Dionysus" && 
      library[2] != "PHAT") {
    stop("library for computing persistence diagram should be a string: either 'GUDHI', 'Dionysus', or 'PHAT'")
  }
  if (!is.logical(location)) {
    stop("location should be logical")
  }
  if (!is.logical(printProgress)) {
    stop("printProgress should be logical")
  }
  if (dist == "arbitrary" && library[2] == "GUDHI" && 
      maxdimension > 1) {
    stop("there is a bug for computing homological features of dimension higher than 1 when the distance is arbitrary and library 'GUDHI' is used")
  }
  if (dist == "arbitrary" && library[1] == "GUDHI") {
    library[1] <- "Dionysus"
  }
  X <- as.matrix(X)
  max_num_pairs <- 5000
  if (dist == "euclidean" && library[1] == "Dionysus" && 
      .Machine[["sizeof.pointer"]] != 8) {
    ripsOut <- RipsDiag(X = as.matrix(dist(X)), maxdimension = maxdimension, 
                        maxscale = maxscale, dist = "arbitrary", libraryFiltration = library[1], 
                        libraryDiag = library[2], location = location, printProgress = printProgress)
  }
  else {
    ripsOut <- RipsDiag(X = X, maxdimension = maxdimension, 
                        maxscale = maxscale, dist = dist, libraryFiltration = library[1], 
                        libraryDiag = library[2], location = location, printProgress = printProgress)
  }
  if (location == TRUE) {
    if (dist == "euclidean") {
      BirthLocation <- X[ripsOut[[2]][, 1], ]
      DeathLocation <- X[ripsOut[[2]][, 2], ]
      if (library[2] == "Dionysus") {
        CycleLocation <- lapply(ripsOut[[3]], function(bdy) {
          array(X[bdy, ], dim = c(dim(bdy), NCOL(X)))
        })
      }
    }
    else {
      BirthLocation <- ripsOut[[2]][, 1]
      DeathLocation <- ripsOut[[2]][, 2]
      if (library[2] == "Dionysus") {
        CycleLocation <- ripsOut[[3]]
      }
    }
  }
  Diag <- ripsOut[[1]]
  if (NROW(Diag) > 0) {
    Diag[which(Diag[, 3] == Inf), 3] <- maxscale
  }
  colnames(Diag) <- c("dimension", "Birth", "Death")
  class(Diag) <- "diagram"
  attributes(Diag)[["maxdimension"]] <- max(Diag[, 1])
  attributes(Diag)[["scale"]] <- c(0, maxscale)
  attributes(Diag)[["call"]] <- match.call()
  if (location == FALSE || library[2] == "GUDHI") {
    out <- list(diagram = Diag)
  }
  else if (library[2] == "PHAT") {
    out <- list(diagram = Diag, birthLocation = BirthLocation, 
                deathLocation = DeathLocation)
  }
  else {
    out <- list(diagram = Diag, birthLocation = BirthLocation, 
                deathLocation = DeathLocation, cycleLocation = CycleLocation)
  }
  return(out)
}
<bytecode: 0x0000026f71fa35c0>
  <environment: namespace:TDA>