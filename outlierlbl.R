outlierlbl = function(x, g = 2.2, plot = FALSE){
    # follows https://www.youtube.com/watch?v=WSflSmcNRFI
    
    # Outlier labeling rule:
    # David C. Hoaglin, Boris Iglewicz & John W. Tukey (1986) 
    # Performance of Some Resistant Rules for Outlier Labeling, 
    # Journal of the American Statistical Association, 
    # 81:396, 991-999, DOI: 10.1080/01621459.1986.10478363
    
    # g of 2.2 (outlier demarcation criterion) argued in:
    # Hoaglin, D., & Iglewicz, B. (1987). 
    # Fine-Tuning Some Resistant Rules for Outlier Labeling. 
    # Journal of the American Statistical Association, 
    # 82(400), 1147-1149. doi:10.2307/2289392
    
    # Check if distribution is normal
    n = shapiro.test(x)
    if (n$p.value > .05){
        normtxt = sprintf('Shapiro-Wilk normality test was: W = %0.3f, p = %0.3f, DATA is normally distributed.', n$statistic[[1]], n$p.value)
    } else {
        normtxt = sprintf('Shapiro-Wilk normality test was: W = %0.3f, p = %0.3f, DATA is NOT normally distributed.', n$statistic[[1]], n$p.value)
    }
    
    # Outlier labeling
    q = quantile(x)
    
    # Upper = Q3 + (2.2 * (Q3 - Q1))
    upper = q[[4]] + (g * (q[[4]] - q[[2]]))
    # Lower = Q1 - (2.2 * (Q3 - Q1))
    lower = q[[2]] - (g * (q[[4]] - q[[2]]))
    
    # label outliers:
    inds = which( x > upper | x < lower )
    ind_above = which( x > upper )
    ind_below = which( x < lower )
    
    # Check what % of data constitutes outliers, should be less than 5%
    percout = length(inds) / length(x)
    
    if (plot == TRUE){
        hist(x, xlim = c(lower-1, upper+1))
        abline(v = upper, col = 'red')
        abline(v = lower, col = 'red')
        lines(density(x), col = 'green', lwd = 2.5)
    }
    
    # return stuff
    return(list(g = g, # save g that was used
                normality = normtxt, # Results of normality test
                lower = lower, # save lower criterion value
                upper = upper, # save upper criterion value
                outlierN = length(inds), # how many outliers
                percentOutlier = percout, # how many % of data are outliers, shouldn't be more than 5%
                whichvals = inds, # which values are outliers
                whichvals_below = ind_below, # which values are outliers below the criterion
                whichvals_above = ind_above, # which values are outliers above the criterion
                whichvals_logical = x > upper | x < lower, # logical vector of outliers
                whichvals_logical_below = x < lower, # logical vector of outliers below the criterion
                whichvals_logical_above = x > upper # logical vector of outliers above the criterion  
                ))
}
    
    