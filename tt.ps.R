tt.ps = function(x, y, N, DV1, DV2, alpha = .05, type = 'two.sided'){
    
    source('outlierlbl.R')
    library('effsize')
    library('pwr')
    library('rcompanion')
    
    # normality test is for difference between the x and y
    xydiff = x - y
    norm = shapiro.test(xydiff)
    
    if (norm$p.value >= .05){
        msg = sprintf("Shapiro-Wilk normality test for the difference between the paired DV of %s and %s was not significant (\textit{p} = %.3f), so then the null hypothesis that the data are normally distributed is not rejected. ", 
                      DV1, DV2, norm$p.value)
    } else {
        msg = sprintf("Shapiro-Wilk normality test for the the difference between the paired DV of %s and %s was significant (\textit{W} = %.3f, \textit{p} = %.3f), so the null hypothesis that the data are normally distributed is rejected. ", 
                      DV1, DV2, norm$statistic[[1]], norm$p.value)
    }
    
    # Check for outliers
    outl = outlierlbl(xydiff, plot = 0)
    if (outl$outlierN == 0){
        msg = paste(msg, "No outliers were detected. ", sep = '')
    } else {
        msg = paste(msg, sprintf("N = %i OUTLIERS DETECTED. ", outl$outlierN), sep = '' )
        
    }
    
    if (norm$p.value > 0.05){
        
        # T-TEST
        test = t.test(x,y)
        
        # Effect
        effect = cohen.d(x,y)
        d = effect$estimate
        # Hedges correct
        df = N - 1
        d = d * (1 - (3 / (4 * df - 1 ))) 
        r = d/sqrt(d^2 + 4)
        
        # sign for p
        if (round(test$p.value,3) == 0){
            psign = '<'
            p = as.numeric(0.001)
        } else {
            psign = '='
            p = as.numeric(test$p.value)
        }
        
        # power
        # power type
        if (type == 'two.sided'){
            powertype = 'two-tailed'
        } else {
            powertype == 'one-tailed'
        }
        
        power = pwr.t.test(n = N, d = d, sig.level = alpha, type = 'paired')
        
        # write
        if (test$p.value < 0.05){
            msg = paste(msg, sprintf("The %s for the difference between the DV of %s (textit{M} = %.3f, \textit{SD} = %.3f) and %s (\textit{M} = %.3f, \textit{SD} = %.3f) was significant (\textit{t}(%.3f) = %.3f, \textit{p} %s %.3f, 0.95 CI [%.3f, %.3f]. The unbiased Hodge’s corrected \textit{d} = %.3f, CI 0.95 [%.3f, %.3f], \textit{r} = %.3f, suggesting that the mean of %s and the mean of %s differ by a %.1f of \textit{SD} of the data. The alternative hypothesis (true mean of %s is not equal to the mean of %s) can not be rejected. ", 
                                     test$method, DV1, mean(x, na.rm = 1), sd(x, na.rm = 1), DV2, mean(y, na.rm = 1), sd(y, na.rm = 1), 
                                     test$parameter[[1]], abs(test$statistic[[1]]), psign, p,
                                     abs(test$conf.int[1]), abs(test$conf.int[2]), abs(d), effect$conf.int[[1]], effect$conf.int[[2]], 
                                     abs(r), DV1, DV2, abs(d), DV1, DV2), sep = '')
        } else {
            msg = paste(msg, sprintf("The %s for the difference between the DV of %s (textit{M} = %.3f, \textit{SD} = %.3f) and %s (\textit{M} = %.3f, \textit{SD} = %.3f) was not significant (\textit{p} = %.3f, 0.95 CI [%.3f, %.3f]. The unbiased Hodge’s corrected \textit{d} = %.3f, CI 0.95 [%.3f, %.3f], \textit{r} = %.3f, suggesting that the mean of %s and the mean of %s differ by a %.1f of \textit{SD} of the data. The alternative hypothesis (true mean of %s is not equal to the mean of %s) can be rejected. ", 
                                     test$method, DV1, mean(x, na.rm = 1), sd(x, na.rm = 1), DV2, mean(y, na.rm = 1), sd(y, na.rm = 1), test$p.value, abs(test$conf.int[1]), abs(test$conf.int[2]), abs(d), effect$conf.int[[1]], effect$conf.int[[2]], abs(r), DV1, DV2, abs(d), DV1, DV2), sep = '')
        }
        
        # write power
        
        if (power$power > .8){
            msg = paste(msg, sprintf('The observed power for that effect size was %.3f (\textit{N} = %i, $\alpha$ = %.2f, %s) which is greater than the recommended power of 0.8 and means an around %.0f\\%% probability of encountering a Type-II error. ', 
                                     power$power, power$n, alpha, powertype, (1-power$power)*100), sep = '')
        } else if (power$power == .8){
            msg = paste(msg, sprintf('The observed power for that effect size was %.3f (\textit{N} = %i, $\alpha$ = %.2f, %s) which is equal to the recommended power of 0.8 and means an around %.0f\\%% probability of encountering a Type-II error. ', 
                                     power$power, power$n, alpha.level, powertype, (1-power$power)*100), sep = '')
        } else {
            msg = paste(msg, sprintf('The observed power for that effect size was %.3f (\textit{N} = %i, $\alpha$ = %.2f, %s) which is below the recommended power of 0.8 and means an around %.0f\\%% probability of encountering a Type-II error. ', 
                                     power$power, power$n, alpha, powertype, (1-power$power)*100), sep = '')
            
            # power to low, calculate N required
            reqN = pwr.t.test(n = NULL, power = .8, d = d, sig.level = alpha, type = 'paired', alternative = type)
            msg = paste(msg, sprintf('For a %s with \textot{d} = %.3f and required power of 0.8 a sample of \textit{N} = %.1f is required. ',
                                     test$method, reqN$d, reqN$n), sep = '')
        }
        
        
        
    } else {
        
        # WILCOXON
        
        # paired Sample Wilcoxon Signed Rank Test 
        test = wilcox.test(x, y, alternative = "two.sided", conf.int = T)
        
        # chng name of test
        test$method = paste('Paired Sample', test$method, sep = ' ')
        
        # effect size
        # needs different arrangement 
        data = as.data.frame(rbind(cbind(x, 1), cbind(y,2)))
        names(data) = c('vars','group')
        r = wilcoxonPairedR(data$vars, data$group)[[1]]
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
            msg = paste(msg, sprintf("The %s for the for the difference between the DV of %s (\textit{M} = %.3f, \textit{Mdn} = %3.f, \textit{SD} = %.3f, \textit{IQR} = %.3f) and DV of %s (\textit{M} = %.3f, \textit{Mdn} = %3.f, \textit{SD} = %.3f, \textit{IQR} = %.3f) was significant (\textit{V} = %.3f, p %s %.3f, 0.95 CI [%.3f, %.3f]), so the alternative hypothesis (median of %s is not equal to that of %s) can not be rejected. The effect size (unbiased Hodge’s corrected Cohen's) was \textit{d} = %.3f, 0.95 CI [%.3f, %.3f], \textit{r} = %.3f. ", 
                                     test$method, DV1, mean(x, na.rm = 1), median(x, na.rm = 1), sd(x, na.rm = 1), IQR(x, na.rm = 1), 
                                     DV2, mean(y, na.rm = 1), median(y, na.rm = 1), sd(y, na.rm = 1), IQR(y, na.rm = 1),
                                     test$statistic[[1]], psign, p, test$conf.int[1], test$conf.int[2], DV1, DV2, abs(d), lCI, uCI, r), sep = '')
            
        } else {
            
            msg = paste(msg, sprintf("The %s for the for the difference between the DV of %s (\textit{M} = %.3f, \textit{Mdn} = %3.f, \textit{SD} = %.3f, \textit{IQR} = %.3f) and DV of %s (\textit{M} = %.3f, \textit{Mdn} = %3.f, \textit{SD} = %.3f, \textit{IQR} = %.3f) was not significant (\textit{p} = %.3f, 0.95 CI [%.3f, %.3f]. The unbiased Hodge’s corrected \textit{d} = %.3f, CI 0.95 [%.3f, %.3f], \textit{r} = %.3f. The alternative hypothesis (median of %s is not equal to that of %s) can be rejected. ", 
                                     test$method, DV1, mean(x, na.rm = 1), median(x, na.rm = 1), sd(x, na.rm = 1), IQR(x, na.rm = 1), 
                                     DV2, mean(y, na.rm = 1), median(y, na.rm = 1), sd(y, na.rm = 1), IQR(y, na.rm = 1),
                                     test$p.value, test$conf.int[1], test$conf.int[2], abs(d), lCI, uCI, r, DV1, DV2), sep = '')
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
    }
    
    return(msg)
}