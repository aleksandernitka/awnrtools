dwell.calc.wins = function(gazex, gazey, lb, rb, tb, bb) {
    
    library(dplyr)
    
    d = cbind(gazex, gazey)
    
    w1 = (between(d[1:150,1], lb, rb) & between(d[1:150,2], bb, tb))
    w2 = (between(d[151:300,1], lb, rb) & between(d[151:300,2], bb, tb))
    w3 = (between(d[301:450,1], lb, rb) & between(d[301:450,2], bb, tb))
    w4 = (between(d[451:600,1], lb, rb) & between(d[451:600,2], bb, tb))
    w5 = (between(d[601:750,1], lb, rb) & between(d[601:750,2], bb, tb))
    w6 = (between(d[751:nrow(d),1], lb, rb) & between(d[751:nrow(d),2], bb, tb))
    
    dw1 = sum(w1 == TRUE, na.rm = TRUE)
    dw2 = sum(w2 == TRUE, na.rm = TRUE)
    dw3 = sum(w3 == TRUE, na.rm = TRUE)
    dw4 = sum(w4 == TRUE, na.rm = TRUE)
    dw5 = sum(w5 == TRUE, na.rm = TRUE)
    dw6 = sum(w6 == TRUE, na.rm = TRUE)
    
    sum = sum(dw1,dw2,dw3,dw4,dw5,dw6)
    
    # Old algo
    # ( (xd[s] > lb) & (xd[s] < rb) & (yd[s] > bb) & (yd[s] < tb) )
    
    
    return(list(
        'w1' = dw1,
        'w2' = dw2,
        'w3' = dw3,
        'w4' = dw4,
        'w5' = dw5,
        'w6' = dw6,
        'sum' = sum))
    
    
}

