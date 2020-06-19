#再帰的にcellに入っている点が作るcellを消す
cell_cnct<-function(i, cell){
  
  if(length(cell[[i]]) > 1){
    
    cell<-cell[-cell[[i]][-1]]
    
  }
  
  if(i+1 < length(cell)){cell_cnct(i+1, cell)}
  
  return(cell)
  
}