
## X, Y are **dataframes** containing the independent variables and responsible variable, respectively.
## max_var_num is a number that limiting the maximum number of variables can be included in the model
## cutoff is the cutoff value for logistic regression, defult is 0.5. 
##        Decrease cutoff to increase sensitivity, increase cutoff to increase specificity
## It outputs a table with number of variables, selected variables, and mis-classification rate (training)

forward_selection_for_logistic_model <- function(x, y, max_var_num = Inf, cutoff = 0.5) {
    n <- ncol(x)  # n denotes the number of independent variables in the data
    max_var_num <- min(n, max_var_num) # max_var_num denotes the maxinum number of variables can be in a model

    # dat <- cbind(y,x)
    all_vars <- colnames(x) # all-vars denotes the names of all independent variables
    vars_selected <- c() # initialization of selected
    vars_selected_string <- logical(0) # initialization of selected variables, will be updated by the algorithm
    result <- data.frame(num_of_var = logical(0), vars = logical(0), mis_rate = logical(0), 
                         sensitivity = logical(0), specificity = logical(0)) # initialization of the result table
    
    for(j in 1:max_var_num) {
        mis_rate <- 1
        for(i in 1:n) {
            if(!(all_vars[i] %in% vars_selected)) {
                form <- as.formula(paste(colnames(y),"~", paste(vars_selected_string, all_vars[i], sep = "+") ,sep = " ")) # creates formula to be fitted
                model <- glm(formula = form, data = cbind(x,y), family = "binomial") # fit the data with (y,x)
                pred <- round(model$fitted.value + 0.5 - cutoff)
                conf_mat <- table(as.matrix(y), pred)
                # above line creates the confusion matrix. [1,1]: TN, [1,2]:FP, [2,1]: FN, [2,2]: TP, if all predicted value are 0 or 1, only 1 column
                zero_in <- 0 %in% pred
                one_in <- 1 %in% pred
                if (zero_in && one_in) {
                    mis_rate_now <- 1 - sum(diag(conf_mat)) / sum(conf_mat) # calculates the misclassification rate for the trainig set
                } else {
                    if (!one_in) {
                        mis_rate_now <- sum(as.numeric(as.matrix(y))) / nrow(y)
                    }
                    if (!zero_in) {
                        mis_rate_now <- 1 - sum(as.numeric(as.matrix(y))) / nrow(y)
                    }
                }
                if (mis_rate_now < mis_rate) { #update of smallest mis_rate
                    mis_rate <- mis_rate_now
                    if (zero_in && one_in) {
                            sensitivity <- conf_mat[2,2] / sum(conf_mat[2,])
                            specificity <- conf_mat[1,1] / sum(conf_mat[1,])
                        }
                    if (!one_in) {
                        sensitivity <- 0
                        specificity <- 1
                        }
                    if (!zero_in) {
                        sensitivity <- 1
                        specificity <- 0
                        }
                    new_var <- all_vars[i]
                }
            }
        }
        if (length(vars_selected) == 0) { #this if_else expression is to add newly selected variable to vars_selected
            vars_selected_string <- new_var
        } else {
            vars_selected_string <- paste(vars_selected_string, new_var, sep = "+")
        }
        vars_selected <- append(vars_selected, new_var)
        result[j,] <- c(j, vars_selected_string, mis_rate, sensitivity, specificity)
    }
    
    result
}