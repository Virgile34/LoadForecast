CV_pred <- function(equation, model = gam, n_block = 10, data = data0) {

    # if needed, create CV_block_list
    if (!exists('CV_block_list') ||
        length(CV_block_list) != n_block ||
        tail(unlist(tail(CV_block_list, n = 1)), n = 1) != nrow(data)
        ) {

        borne_block <- floor(seq(1, nrow(data), length = n_block + 1))
        CV_block_list <<- list()
        for (i in c(2:n_block)){
            CV_block_list[[i-1]] <<- c(borne_block[i-1]:(borne_block[i]-1))
        }
        CV_block_list[[n_block]] <<- c(borne_block[n_block]:borne_block[n_block+1])
    }

    blockRMSE <- function(block) {
        g <- model(as.formula(equation), data = data[-block, ])
        forecast <- predict(g, newdata = data[block, ])
        return(forecast)
    }

    pred <- unlist(lapply(CV_block_list, blockRMSE))
    return(pred)
}