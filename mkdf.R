mkdf = function(nr, col_names){
    
    x = data.frame(matrix(nrow = nr, ncol = length(col_names)))
    
    names(x) = col_names
    
    return(x)
}