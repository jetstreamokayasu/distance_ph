#密度が低い場合にスケールを変化させた場合のPHがどうなるかを調べる
half_trs1<-torus1/2

hf_trs1_pd<-calculate_homology(half_trs1, dim = 2, threshold = 3)
hf_trs1_pl<-calcLandscape(diag = hf_trs1_pd, maxscale = 3)
