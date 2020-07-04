epci = function(txt, cf = .95, extended = TRUE){
    
    # Print CI for e2p
    
    library(apaTables)
    
    par = as.numeric(unlist(regmatches(txt, gregexpr("[[:digit:]]+\\.*[[:digit:]]*", txt))))
    
    e = get.ci.partial.eta.squared(F.value = par[3], 
                                   df1 = par[1], 
                                   df2 = par[2], 
                                   conf.level = cf
    )
    
    if (extended == TRUE){
        print(sprintf('F = %f, df1 = %f, df2 = %f', par[3], par[1], par[2]))
    }
    
    print(sprintf('CIη2p = [%.3f, %.3f]', e$LL, e$UL))
}

