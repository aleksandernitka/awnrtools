hdf5.extractor = function(files) {
    
    # For easier handling later on I would create lists of imported DFs
    # This can make looping easier in analysis:
    hdf5.import.df  = c()     # Log for all DFs with ET data (events)
    hdf5.import.evt = c()     # Log for all MessageEvent DFs created
    
    library('hdf5r')
    
    # Progress
    pb = txtProgressBar(min = 0, max = length(files), initial = 0, style = 3) 
    
    for (f in 1:length(files)) {
        
        # Progress
        setTxtProgressBar(pb,f)
        
        df = H5File$new(files[f], mode="r")
        # import eyetracker events
        et = df[["data_collection/events/eyetracker/BinocularEyeSampleEvent"]]
        et = et[] 
        # import experiment evnts
        ee = df[["data_collection/events/experiment/MessageEvent"]]
        ee = ee[]
        
        # Save to df
        if (f < 10){
            # Adds leading 0 to the ss id no
            tmp.et.name = sprintf("ss0%i.%s", f, "BinocularEyeSampleEvent")
            tmp.ee.name = sprintf("ss0%i.%s", f, "MessageEvent")
        } else {
            tmp.et.name = sprintf("ss%i.%s", f, "BinocularEyeSampleEvent")
            tmp.ee.name = sprintf("ss%i.%s", f, "MessageEvent")
        }
        
        # save the DF to the GLOBAL ENVI
        assign(tmp.et.name, et, envir = .GlobalEnv)
        assign(tmp.ee.name, ee, envir = .GlobalEnv)
        
        # append to log
        hdf5.import.df  = append(hdf5.import.df, tmp.et.name)
        hdf5.import.evt = append(hdf5.import.evt, tmp.ee.name)
        
        df$close_all()
        
    }
    
    assign("hdf5.import.df", hdf5.import.df, envir = .GlobalEnv)
    assign("hdf5.import.evt", hdf5.import.evt, envir = .GlobalEnv)
    
}