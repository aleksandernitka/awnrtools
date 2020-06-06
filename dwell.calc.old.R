dwell.calc = function(dataframe, xdata = "gaze_x", ydata = "gaze_y", lb, rb, tb, bb, start=FALSE, end=FALSE) {
    
    # Author: Aleksander W. Nitka - aleksander.nitka@nottingham.ac.uk
    
    # Calculates total dwell time and dwell scores as a funciton of time
    
    # This function takes the following arguments:
    # dataframe = name of the data frame used, has to be a string in " " or ' ' 
    # xdata     = vector of x dim values for that trial aka locations of gaze in x plane
    # ydata     = vector of y dim values for that trial
    #   lb      = left boundry of the AOI in px
    #   rb      = right boundry
    #   tb      = top boundry
    #   bb      = bottoms boundry
    # Function returns a list of
    #   Total Dwell                 = returns a total dwell value
    #   Dwell over time             = returns a string of values - dwell as a function of time
    #   Probability of Looking at   = returns 1d vector of 1,0 depending whether there was an overlap with AOI for this sample
    # start     = where to begin the dwell calculation in samples
    # end       = If you want to calculate dwell for a subset of time, starting at 0 IN SAMPLES NOT SECONDS
    
    # To Do:
    # function to return both total and ftime into two values
    # return(list(v1=v1,v2=v2))
    
    # Load the data
    xn = paste(dataframe,xdata, sep = "$")
    yn = paste(dataframe,ydata, sep = "$")
    
    xd = eval(parse(text = xn))
    yd = eval(parse(text = yn))
    
    # DEBUG / TESTING DATA
    # xd = c(1,1,1,1,1,1,5,5,5,5,5,5,7,7,7,7,7)
    # yd = c(1,1,1,1,1,1,5,5,5,5,5,5,7,7,7,7,7)
    # lb = 0
    # rb = 6
    # tb = 6
    # bb = 0
    # mode = 'ft'
    # start=FALSE
    # end=FALSE
    # DEBUG END
    
    # Below subsets the vector according to the start/stop numbers
    if (end != FALSE | start != FALSE) {
        
        if (start != FALSE){
            
            xd = xd[start:end]
            yd = yd[start:end]
        }
        
        else {
            
            xd = xd[1:end]
            yd = yd[1:end]
            
        }
        
    }
    
    
    # Create two variables, 'dwelltotal' is a total dwell (sum) and
    # the 'dwellftime' is a vector which calculates a current value of dwell
    # for that sample
    dwelltotal = 0
    dwellftime = c()
    dwellprobt = c()


        for (s in 1:length(xd)) {
            
            # First check if either xd[s] or yd[s] are NA
            if ( is.na(xd[s]) == TRUE ) {
                
                # if values are NA, following from filtering, then don't add to dwell
                # but still append it to dwellftime 
                # report NA to dwellprobt
                dwelltotal = dwelltotal
                dwellftime = append(dwellftime,dwelltotal)
                dwellprobt = append(dwellprobt, NA)
                
            }
            
            else {
                
                if ( (xd[s] > lb) & (xd[s] < rb) & (yd[s] > bb) & (yd[s] < tb) ){
                    dwelltotal = dwelltotal + 1
                    dwellftime = append(dwellftime,dwelltotal)
                    dwellprobt = append(dwellprobt, 1)
                    }
                
                else {
                    dwelltotal = dwelltotal
                    dwellftime = append(dwellftime,dwelltotal)
                    dwellprobt = append(dwellprobt, 0)
                    } 
                
            }
            

            
            
        }
    
return(list('Total Dwell' = dwelltotal, 'Dwell over time' = dwellftime, 'Probability of Looking at' = dwellprobt))
    
    
}

