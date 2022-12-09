

# Produces a confusion matrix for a logistic model with cutoff value (default cutoff = 0.5)
# if display is true, it prints results
# Must be given the true y and the predicted y

conf_mat_logistic <- function(log_model = NA, true_y = FALSE, pred_y = FALSE, cutoff = 0.5, get.info = FALSE, display = FALSE) { 
    
    if (!is.na(log_model)) {
        newdata <- tibble(y = log_model$y, pred = round(log_model$fitted.value - cutoff + 0.5))
    } else {
        newdata <- tibble(y = true_y, pred = round(pred_y - cutoff + 0.5))
    }
    
    suppressWarnings({
        TT <- newdata %>%
            filter(y == pred, pred == 1) %>%
            nrow()
        FF <- newdata %>%
            filter(y != pred, pred == 0) %>%
            nrow()
        FT <- newdata %>%
            filter(y != pred, pred == 1) %>%
            nrow()    
        TF <- newdata %>%
            filter(y == pred, pred == 0) %>%
            nrow()
    
        conf_mat <- data.frame(c(TT,FT),c(FF,TF))
        colnames(conf_mat) <- c("Pred True", "Pred False")
        rownames(conf_mat) <- c("Actual True","Actual False")
        conf_mat <- as.matrix(conf_mat)
        
        accuracy <- sum(diag(conf_mat)) / sum(conf_mat)
        sensitivity <- TT / (TT + FF)
        specificity <- TF / (TF + FT)
        
        if (display == TRUE) {
            print(conf_mat)
            cat("\n")
            cat("Accuracy:", accuracy, "\n")
            cat("Sensitivity:", sensitivity, "\n")
            cat("Specificity:", specificity, "\n")
        }
        })
    
        if (get.info == FALSE) return(conf_mat)
            else return(list(conf_mat = conf_mat, accuracy = accuracy, sensitivity = sensitivity, specificity = specificity))
        }