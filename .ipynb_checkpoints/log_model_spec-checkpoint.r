
library(tidyverse)
source("conf_mat_logistic.r")

## Input: a logistic model, desiring sensitivity or specitivity 
##        NOTE: since this does not work for a regularized logistic model, there is an option to manually input true and pred vectors
## Output: a cut off number and resulting misclassification rate, sensitivity and specitivity

logistic_model_sens <- function(log_model = NA, sensitivity = 0, specificity = 0, true_y = FALSE, pred_y = FALSE) {

    if(sensitivity < 0 || specificity < 0 || sensitivity > 1 || specificity > 1) stop()

    cutoff <- 0.5
    
    if (!is.na(log_model)) {
        pred_y <- log_model$y
        true_y <- log_model$fitted.value
    } 
    
    tolerance_sens <- 1 / sum(true_y == 1)
    tolerance_spec <- 1 / sum(true_y == 0)
    
    cf_info <- conf_mat_logistic(true_y = true_y, pred_y = pred_y, cutoff = 0.5, get.info = TRUE)
    result <- data.frame(Cutoff = 0.5, Sensitivity = cf_info$sensitivity, Specificity = cf_info$specificity, Accuracy = cf_info$accuracy)
    
    if(sensitivity != 0 && specificity != 0) {
        stop("Please only specify one of the sensitivity and speciticity")
    }
    
    if(sensitivity == 0 && specificity == 0) {
        cat("No input for sensitivity/specifity, return current information. \n")
        return(result)
    }
    
    
    if(sensitivity != 0) { 
        sens_now <- cf_info$sensitivity 
        step <- 0.1
        last_time <- 0  
        dif <- abs(sensitivity - sens_now)
        
        while(dif > tolerance_sens || sensitivity > sens_now) {
            
            if (cutoff < 0 || cutoff > 1) stop
            this_time <- 0 
            if(sensitivity >= sens_now) {
                cutoff <- cutoff - step
                this_time <- 1
            }
            
            if(sensitivity < sens_now) {
                cutoff <- cutoff + step
                this_time <- -1
            }
            
            if (this_time != last_time && last_time != 0) {
                step <- step / 10
            }
            last_time <- this_time            
            cf_info <- conf_mat_logistic(true_y = true_y, pred_y = pred_y, cutoff = cutoff, get.info = TRUE)
            sens_now <- cf_info$sensitivity
            dif <- abs(sensitivity - sens_now)
        }
    }
    
    if(specificity != 0) { 
        spec_now <- cf_info$specificity
        step <- 0.1
        last_time <- 0  
        dif <- abs(specificity - spec_now)
        
        while(dif > tolerance_spec || specificity > spec_now) {
            
            if (cutoff < 0 || cutoff > 1) stop
            this_time <- 0 
            if(specificity >= spec_now) {
                cutoff <- cutoff + step
                this_time <- 1
            }
            
            if(specificity < spec_now) {
                cutoff <- cutoff - step
                this_time <- -1
            }
            
            if (this_time != last_time && last_time != 0) {
                step <- step / 10
            }
            
            last_time <- this_time            
            cf_info <- conf_mat_logistic(true_y = true_y, pred_y = pred_y, cutoff = cutoff, get.info = TRUE)    
            spec_now <- cf_info$specificity
            dif <- abs(specificity - spec_now)
                        cat("acc \n")
        }
    }
    
    result <- data.frame(Cutoff = cutoff, Sensitivity = cf_info$sensitivity, Specificity = cf_info$specificity, Accuracy = cf_info$accuracy)
    return(result)
        
}