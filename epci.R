epci = function(txt, cf = 0.95, fci = FALSE){
    
    # function takes text copied from the manuscript:
    # ie, "F(1,33) = 13.453" and returns n2p CIs 
    # which can be copied back to the manuscript
    
    # addittionally function can return a CI for F value
    
    require(MBESS)
    
    # get values from string
    par = as.numeric(unlist(regmatches(txt, gregexpr("[[:digit:]]+\\.*[[:digit:]]*", txt))))
    
    # Check if enough arguemtns extracted
    if(length(par) > 3 | length(par) < 3){stop('Could not extract parameters from the sting')}
    
    # Calcualte CIs with MBESS, method by Daniel Lakens 
    # https://daniellakens.blogspot.com/2014/06/calculating-confidence-intervals-for.html
    lims = conf.limits.ncf(F.value = par[3], conf.level = cf, df.1 = par[1], df.2 = par[2])
    low = lims$Lower.Limit/(lims$Lower.Limit + par[1] + par[2] + 1)
    upp = lims$Upper.Limit/(lims$Upper.Limit + par[1] + par[2] + 1)
    
    # return CI for F if requested
    if (fci == TRUE){
        print(sprintf('CIF = [%.3f, %.3f], CIη2p = [%.3f, %.3f]', lims$Lower.Limit, lims$Upper.Limit, low, upp))
    } else {
        print(sprintf('CIη2p = [%.3f, %.3f]', low, upp))
    }
    
}