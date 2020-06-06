tt.os = function(x, mu, N, DV, alpha = .05, type = 'two.sided'){
    
    # This wrapper funnction takes care of:
    # (1) normality check
    # (2) t-test
    # (3) effect size calculation (Cohen's D)
    # (4) power calculation:
    # Arguments:
    # x = DV vector
    # mu = value to run t-test against
    # N = number of sample for each group
    # DV = string name for the DV (eg. "RT", "Dwell Time")
    # alpha = significance criterion
    # type = hypothesis tailing

    
    source('os.cohend.R')
    source('outlierlbl.R')
    library('pwr')
    library('DescTools') # for win
    library('rcompanion') # for wilcoxon effect size r
    
    # Check if data is normally distributed
    x.norm = shapiro.test(x)
    # write 
    if (x.norm$p.value >= .05){
        msg = sprintf("Shapiro-Wilk normality test for the DV of %s was not significant (\textit{p} = %.3f), so then the null hypothesis that the data are normally distributed is not rejected. ", 
                      DV, x.norm$p.value)
    } else {
        msg = sprintf("Shapiro-Wilk normality test for the DV of %s was significant (\textit{W} = %.3f, \textit{p} = %.3f), so the null hypothesis that the data are normally distributed is rejected. ", 
                      DV, x.norm$statistic[[1]], x.norm$p.value)
    }
    
    # Check for outliers
    outl = outlierlbl(x, plot = 0)
    if (outl$outlierN == 0){
        msg = paste(msg, "No outliers were detected. ", sep = '')
    } else {
        # winsorize to remove outliers
        x = Winsorize(x, minval = NULL, maxval = NULL, probs = c(0.05, 0.95),  na.rm = FALSE)
        # re-run normality
        x.norm = shapiro.test(x)
        
        if (x.norm$p.value >= .05){
            msg = paste(msg, sprintf("A total of %i outliers were detected, 0.9 winsorization was applied to the data and Shapiro-Wilk normality test re-run, which was not significant (\textit{p} = %.3f) so the then the null hypothesis that the data are normally distributed is not rejected ", 
                        outl$outlierN, x.norm$p.value), sep = '')
        } else {
            msg = paste(msg, sprintf("A total of %i outliers were detected, 0.9 winsorization was applied to the data and Shapiro-Wilk normality test re-run, which was significant (\textit{W} = %.3f, \textit{p} = %.3f) so the null hypothesis that the data are normally distributed is rejected. ", 
                        outl$outlierN, x.norm$statistic[[1]], x.norm$p.value), sep = '')
        }
        
    }
    
    # run t-test or wilcoxon
    if (x.norm$p.value < 0.05){
        
        # WILCOXON
        
        # One-Sample Wilcoxon Signed Rank Test 
        test = wilcox.test(x, mu = 0, alternative = "two.sided", conf.int = T)
        
        # chng name of test
        test$method = paste('One Sample', test$method, sep = ' ')
        
        # effect size
        r = wilcoxonOneSampleR(x, mu = 0)[[1]]
        # r to d
        d = (2*r) / sqrt(1-r^2)
        # Hodges Correction as per Lakens 2013
        df = N - 1
        d = d * (1 - (3 / (4 * df - 1 ))) 
        # effect size CI
        se = sqrt(1/N + d^2 / (2*N))
        z = qnorm(1-0.05/2)
        
        lCI = d - se * z
        uCI = d + se * z
        
        # power
        # power analysis has been adapted from https://elifesciences.org/articles/02245
        # and https://www.researchgate.net/post/How_to_calculate_the_Statistical_Power_of_a_nonparametric_test_the_probability_of_making_Type_II_error
        # using: https://github.com/loladze/co2/blob/master/pwr.boot.R
        source('pwr.boot.R')
        rep = 1000
        power = pwr.boot(x, alpha = alpha, rep = rep, delta = d, sdp = 0)
        
        # power type
        if (type == 'two.sided'){
            powertype = 'two-tailed'
        } else {
            powertype == 'one-tailed'
        }
        
        # sign for p
        if (round(test$p.value,3) == 0){
            psign = '<'
            p = .001
        } else {
            psign = '='
            p = test$p.value
        }
        
        # write test and effect
        if (test$p.value < 0.05){
            msg = paste(msg, sprintf("The %s for the DV of %s (\textit{M} = %.3f, \textit{Mdn} = %3.f, \textit{SD} = %.3f, \textit{IQR} = %.3f) and $mu$ = %.0f was significant (\textit{V} = %.3f, p %s %.3f, 0.95 CI [%.3f, %.3f]), so the alternative hypothesis (central tendency of %s is not equal to %i) can not be rejected. The effect size (unbiased Hodge’s corrected Cohen's) was \textit{d} = %.3f, 0.95 CI [%.3f, %.3f], \textit{r} = %.3f. ", 
                                     test$method, DV, mean(x, na.rm = 1), median(x, na.rm = 1), sd(x, na.rm = 1), IQR(x, na.rm = 1), mu, test$statistic[[1]], psign, p, test$conf.int[1], test$conf.int[2], DV, mu, abs(d), lCI, uCI, r), sep = '')
            
        } else {
            
            msg = paste(msg, sprintf("The %s for the DV of %s (\textit{M} = %.3f, \textit{Mdn} = %3.f, \textit{SD} = %.3f, \textit{IQR} = %.3f) and $mu$ = %.0f was not significant (\textit{p} = %.3f, 0.95 CI [%.3f, %.3f]. The unbiased Hodge’s corrected \textit{d} = %.3f, CI 0.95 [%.3f, %.3f], \textit{r} = %.3f. The alternative hypothesis (central tendency of %s is not equal to %i) can be rejected. ", 
                                     test$method, DV, mean(x, na.rm = 1), median(x, na.rm = 1), sd(x, na.rm = 1), IQR(x, na.rm = 1), mu, test$p.value, test$conf.int[1], test$conf.int[2], abs(d), lCI, uCI, r, DV, mu), sep = '')
        }
        
        
        # write power
        if (power$power > .8){
            msg = paste(msg, sprintf('The observed power for that effect size was %.3f (\textit{N} = %i, $\alpha$ = %.2f, %s) which is greater than the recommended power of 0.8 and means an around %.0f\\%% probability of encountering a Type-II error. ', 
                                     power$power, power$size, alpha, powertype, (1-power$power)*100), sep = '')
        } else if (power$power == .8){
            msg = paste(msg, sprintf('The observed power for that effect size was %.3f (\textit{N} = %i, $\alpha$ = %.2f, %s) which is equal to the recommended power of 0.8 and means an around %.0f\\%% probability of encountering a Type-II error. ', 
                                     power$power, power$size, alpha.level, powertype, (1-power$power)*100), sep = '')
        } else {
            msg = paste(msg, sprintf('The observed power for that effect size was %.3f (\textit{N} = %i, $\alpha$ = %.2f, %s) which is below the recommended power of 0.8 and means an around %.0f\\%% probability of encountering a Type-II error. ', 
                                     power$power, power$size, alpha, powertype, (1-power$power)*100), sep = '')
        }
        
        
        
    } else {
        
        
        # T-TEST
        
        
        test = t.test(x, mu = mu)
        
        # run effect size 
        cohen = os.cohend(x,mu,N)
        d = cohen$d
        r = cohen$r
        lCI = cohen$lCI # CI
        uCI = cohen$uCI # CI
        
        # run power analysis
        power = pwr.t.test(N, d = d, sig.level = alpha, type = 'one.sample', alternative = type)
        
        # sign for p
        if (round(test$p.value,3) == 0){
            psign = '<'
            p = .001
        } else {
            psign = '='
            p = test$p.value
        }
        
        # power type
        if (type == 'two.sided'){
            powertype = 'two-tailed'
        } else {
            powertype == 'one-tailed'
        }
        
        # write
        if (test$p.value >= .05){
            msg = paste(msg, sprintf("The %s for the DV of %s (\textit{M} = %.3f, \textit{SD} = %.3f) and $mu$ = %.0f was not significant (\textit{p} = %.3f, 0.95 CI [%.3f, %.3f]. The unbiased Hodge’s corrected \textit{d} = %.3f, CI 0.95 [%.3f, %.3f], \textit{r} = %.3f, suggesting that the mean of %s and $mu$ = %.0f differ by a %.1f of \textit{SD} of the data. The alternative hypothesis (true mean is not equal to %i) can be rejected. ", 
                                     test$method, DV, mean(x, na.rm = 1), sd(x, na.rm = 1), mu, test$p.value, test$conf.int[1], test$conf.int[2], abs(d), lCI, uCI, r, DV, mu, d,mu), sep = '')
        } else {
            msg = paste(msg, sprintf("The %s for the DV of %s (\textit{M} = %.3f, \textit{SD} = %.3f) and $mu$ = %.0f was significant (\textit{t}(%i) = %.3f, p %s %.3f, 0.95 CI [%.3f, %.3f]), so the alternative hypothesis (true mean is not equal to %i) can not be rejected. The effect size (unbiased Hodge’s corrected Cohen's) was \textit{d} = %.3f, 0.95 CI [%.3f, %.3f], \textit{r} = %.3f, suggesting that the mean of %s and $mu$ = %.0f differ by a %.1f of \textit{SD} of the data.", 
                                     test$method, DV, mean(x, na.rm = 1), sd(x, na.rm = 1), mu, test$parameter[[1]], test$statistic[[1]], psign, p, test$conf.int[1], test$conf.int[2], mu, abs(d), lCI, uCI, r, DV, mu, d), sep = '')
        }
        
        # write on power
        if (power$power > .8){
            msg = paste(msg, sprintf('The observed power for that effect size was %.3f (\textit{N} = %i, $\alpha$ = %.2f, %s) which is greater than the recommended power of 0.8 and means an around %.0f\\%% probability of encountering a Type-II error. ', 
                                     power$power, power$n, power$sig.level, powertype, (1-power$power)*100), sep = '')
        } else if (power$power == .8){
            msg = paste(msg, sprintf('The observed power for that effect size was %.3f (\textit{N} = %i, $\alpha$ = %.2f, %s) which is equal to the recommended power of 0.8 and means an around %.0f\\%% probability of encountering a Type-II error. ', 
                                     power$power, power$n, power$sig.level, powertype, (1-power$power)*100), sep = '')
        } else {
            msg = paste(msg, sprintf('The observed power for that effect size was %.3f (\textit{N} = %i, $\alpha$ = %.2f, %s) which is below the recommended power of 0.8 and means an around %.0f\\%% probability of encountering a Type-II error. ', 
                                     power$power, power$n, power$sig.level, powertype, (1-power$power)*100), sep = '')
            
            # power to low, calculate N required
            reqN = pwr.t.test(n = NULL, power = .8, d = d,sig.level = alpha, type = 'one.sample', alternative = type)
            msg = paste(msg, sprintf('For a One-sample t-test with \textot{d} = %.3f and required power of 0.8 a sample of \textit{N} = %.1f is required. ',
                                     reqN$d, reqN$n), sep = '')
        }
        
        
        
    }
    
    
    
    # return written stuff
    return(msg)
}
