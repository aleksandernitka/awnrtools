getpk = function(this_packages){
    
    this_packages = c('ggplot2')
    newpk = this_packages[!(this_packages %in% installed.packages()[,"Package"])]
    
    if (length(newpk) > 0){
        message(sprintf('Installing packages: %s', newpk))
        install.packages(newpk)
    }
    
    if (length(this_packages) > 1){
        for (i in 1:length(this_packages)){
            #print(this_packages[i])
            library(this_packages[i]) 
            
            eval(parse(text = hdf5.import.df[s]))
        } else {library(parse(text = this_packages))}
    }
}
