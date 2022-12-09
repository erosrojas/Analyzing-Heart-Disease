
library(tidyverse)

## Input: a logistic model, desiring sensitivity or specitivity 
##        NOTE: since this does not work for a regularized logistic model, there is an option to manually input true and pred vectors
## Output: a cut off number and resulting misclassification rate, sensitivity and specitivity

logistic_model_sens <- function(log_model, sensitivity = 0, specificity = 0, true_y = FALSE, pred_y = FALSE, model = TRUE) {
    
    sens_spec <- function(newdata) { 
        TT <- newdata %>%
            filter(y == pred, pred == 1) %>%
            nrow()     
        TF <- newdata %>%
            filter(y == pred, pred == 0) %>%
            nrow()
        FT <- newdata %>%
            filter(y != pred, pred == 1) %>%
            nrow()
        FF <- newdata %>%
            filter(y != pred, pred == 0) %>%
            nrow()
        
        return(c(TT / (TT + FF), TF / (TF + FT)))
        
        }

    if(sensitivity < 0 || specificity < 0 || sensitivity > 1 || specificity > 1) stop()

    cutoff <- 0.5
    
    if (model == TRUE) {
        tolerance_sens <- 1 / sum(model$y == 1)
        tolerance_spec <- 1 / sum(model$y == 0)
        data <- data.frame(y = log_model$y, pred = log_model$fitted.value)
    } else {
        tolerance_sens <- 1 / sum(true_y == 1)
        tolerance_spec <- 1 / sum(true_y == 0)
        data <- data.frame(y = true_y, pred = pred_y)
    }
    
    sen_and_spe <- sens_spec(data %>% mutate(pred = round(pred)))
    result <- data.frame(Cutoff = 0.5, Sensitivity = sen_and_spe[1], Specificity = sen_and_spe[2])
    
    if(sensitivity != 0 && specificity != 0) {
        stop("Please only specify one of the sensitivity and speciticity")
    }
    
    if(sensitivity == 0 && specificity == 0) {
        cat("No input for sensitivity/specifity, return current information. \n")
        return(result)
    }
    

    
    if(sensitivity != 0) { 
        sens_now <- sen_and_spe[1] 
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
            sen_and_spe <- sens_spec(data %>% mutate(pred = round(pred - cutoff + 0.5)))       
            sens_now <- sen_and_spe[1]
            dif <- abs(sensitivity - sens_now)
        }
    }
    
    if(specificity != 0) { 
        spec_now <- sen_and_spe[2] 
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
            sen_and_spe <- sens_spec(data %>% mutate(pred = round(pred - cutoff + 0.5)))       
            spec_now <- sen_and_spe[2]
            dif <- abs(specificity - spec_now)
        }
    }
    
    result <- data.frame(Cutoff = cutoff, Sensitivity = sen_and_spe[1], Specificity = sen_and_spe[2])
    return(result)
        
}