
## X, Y are **dataframes** containing the independent variables and responsible variable, respectively.
## max_var_num is a number that limiting the maximum number of variables can be included in the model
## It outputs a table with number of variables, selected variables, and mis-classification rate (training)

forward_selection_for_logistic_model <- function(x,y, max_var_num = Inf) {
    n <- ncol(x)
    max_var_num <- min(n, max_var_num)
    y_mat <- as.matrix(y, ncol=1)
    dat <- cbind(y,x)
    all_vars <- colnames(x)
    vars_selected <- logical(0)
    
    result <- data.frame(num_of_var = logical(0), vars = logical(0), mis_rate = logical(0))
    for(j in 1:max_var_num) {
        mis_rate <- logical(0)
    for(i in 1:n) {
        mis_rate_now <- 1
        if(!(all_vars[i] %in% vars_selected)){
        form <- as.formula(paste(colnames(y),"~", paste(vars_selected, all_vars[i], sep = "+") ,sep = " "))
        model <- glm(formula = form, data = dat, family = "binomial")
        tab <- table(y_mat, round(predict(model, data = dat, type = "response")))
        mis_rate_now <- 1 - sum(diag(tab)) / sum(tab)
        }
        mis_rate[i] <- mis_rate_now
    }
        if (length(vars_selected) == 0) {
            vars_selected <- all_vars[which.min(mis_rate)]

        } else {
            vars_selected <- paste(vars_selected, all_vars[which.min(mis_rate)], sep = "+")
        }
        mis_rate <- min(mis_rate)
        result[j,] <- c(j, vars_selected, mis_rate)
    }
    
    result
}