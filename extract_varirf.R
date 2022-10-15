# -------------------------------------------------------------------------------
# `extract_varirf()` extracts the impulse reponse vector, along with the upper and 
# lower confidence interval vectors, created by the `irf()` function in the `vars`
# package and puts them into a tidy dataframe that allows for easier 
# impulse-reponse function plotting, particularly with the ggplot2. `extract_varirf()`
# accepts single or multiple 'varirf' list objects created by `irf()`, provided they 
# are created from the same dataset and of the same length. For additional details
# and examples of usage, please consult:
# mentalbreaks.rbind.io/posts/impulse-reponse-plots-with-vars-and-ggplot2
# 
# @anguyen1210
# -------------------------------------------------------------------------------

extract_varirf <- function(...){
    
    varirf_object <- list(...) #list one or more varirf input objects
    
    get_vec_length <- function(list_item){nrow(list_item[[1]][[1]])}
    
    if (!("varirf" %in% mapply(class, varirf_object))){
        stop("this function only accepts 'varirf' class objects")
    }
    
    if (length(unique(mapply(class, varirf_object)))!=1){
        stop("all input items must be 'varirf' class objects")
    }    
    if (length(unique(mapply(get_vec_length, varirf_object)))!=1){
        stop("all irf vectors must have the same length")   
    }  
    
    period <- as.data.frame(0:(nrow(varirf_object[[1]][[1]][[1]])-1)) 
    names(period) <- "period"
    
    for (l in 1:length(varirf_object)){
        for (i in 1:3){
            for (j in 1:dim(varirf_object[[l]][[i]][[1]])[2]){
                for (k in 1:length(varirf_object[[l]][[1]])){
                    temp_colname <- paste(names(varirf_object[[l]][i]), #vector type (irf, lower, or upper)
                                          names(varirf_object[[l]][[i]])[k], #impulse name
                                          colnames(varirf_object[[l]][[i]][[k]])[j], #response name
                                          sep = "_")
                    
                    temp <- as.data.frame(varirf_object[[l]][[i]][[k]][, j]) #extracts the vector
                    
                    names(temp) <- temp_colname #add the column name (vectortype_impulse_reponse)
                    period <- cbind(period, temp) 
                }
                
            }
        }
    }
    names(period) <- tolower(names(period))
    return(period)
}
