#技術伝承用一時ファイル


sum(sqrt(unique(runif(20))))



runif(20) %>% unique() %>% sqrt() %>% sum()



candi_vertxs_idx1<-vicinity$res %$% dirsgs %$% (is.element(ind1, voro_idx) & (ind2 != 1) & not(bp1))



candi_vertxs_idx1<-(is.element(vicinity$res$dirsgs$ind1, vicinity$res$dirsgs$voro_idx) & 
                                      (vicinity$res$dirsgs$ind2 != 1) & not(vicinity$res$dirsgs$bp1))



list1<-lapply(1:5, function(x){rep(x, 5)})

list2<-map(1:5, ~{rep(., 5)})


purrr::map(.x, .f, ...)