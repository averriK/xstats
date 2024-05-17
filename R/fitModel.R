
#' Title
#'
#' @param .data data.table
#' @param y character
#' @param .newdata vector
#' @param level double.
#' @param regression character
#' @param removeZeroInstances logical
#' @param uniqueResponses logical
#' @param ntree integer
#' @param x character
#' @param n_bootstrap integer Number of bootstrap samples for KNN
#' @param k integer Number of nearest neighbors for KNN

#'
#' @return vector
#' @export fitModel
#'
#' @import data.table
#' @import kknn
#' @import quantregForest
#' @import randomForest
#' @import stats
#'
#' @examples
#'
#'

# ********************************
# Fix level for lm
# remove rf. keep qrf only
# set default level to 0.16,0.50,0.84


fitModel <- function(.data, x=NULL,y,.newdata=NULL,level="mean",regression="qrf",removeZeroInstances=FALSE,uniqueResponses=FALSE,ntree=500,n_bootstrap=100,k=5) {
  on.exit(expr = {    rm(list = ls())  }, add = TRUE)
  .  <- .SD <- NULL
  # Capture the variable arguments as a vector
  stopifnot(y %in% names(.data))
  stopifnot(length(level)==1)
  stopifnot(!is.null(x)||!is.null(.newdata))
  if(!is.null(x)) {
    # .newdata==NULL: build model. Return model. ignore .newdata
    VARS <- x
    XCOLS <- VARS[VARS %in% names(.data)]
  }

  if(!is.null(.newdata)){
    # .newdata!=NULL: build model, predict new data, return response, ignore .x
    VARS <- names(.newdata)
    XCOLS <- VARS[VARS %in% names(.data)]
  }
  # Subset the data to include only valid columns
  # Check and keep only those columns that are present in the data.table

  # COLS <- VARS[VARS %in% names(.data)]

  YCOL <- y
  COLS <- c(XCOLS, YCOL)
  #remmove N/A

  # DATA <- .data[, ..COLS, with = FALSE] |> na.omit() |> unique()
  DATA <- .data[, COLS, with = FALSE] |> na.omit() |> unique()

  # Remove duplicated responses
  if(uniqueResponses){
    idx <- !duplicated(DATA[[YCOL]])
    DATA <- DATA[idx]
  }
  # Remove instances with Zero values
  if(removeZeroInstances){
    idx <- DATA[,apply(.SD, 1, function(x) all(x > 0)),.SDcols = XCOLS]
    DATA <- DATA[idx]
  }

  # Average responses with identical instances
  DATA <- DATA[, .(Y = mean(get(YCOL))), by = mget(XCOLS)]

  if(nrow(DATA) == 0) {
    stop("No valid data found")
  }
  # X <- DATA[, ..XCOLS, with = FALSE]
  X <- DATA[, XCOLS, with = FALSE]
  Y <- DATA$Y

  .model <- switch(regression,
                   "qrf"=quantregForest::quantregForest(x=X,y=Y,nthread=8,keep.inbag = FALSE,ntree=ntree),
                   "rf"=randomForest::randomForest(x=X,y=Y,importance=FALSE,proximity=FALSE,ntree=ntree),
                   "lm"=stats::lm(formula = as.formula(paste("Y~", paste(XCOLS, collapse = "+"))), data = DATA[,mget(XCOLS)]),
                   # "knn" = kknn::train.kknn(formula = as.formula(paste(y, "~", paste(XCOLS, collapse = "+"))), data = DATA, k = k)
                   "knn" = kknn::train.kknn(formula = as.formula(paste("Y~", paste(XCOLS, collapse = "+"))), data = DATA[,mget(XCOLS)], kmax = k)



  )

  if(is.null(.newdata)){
    # .newdata==NULL: build model. Return model
    Yp <- predict(.model,newdata=DATA[,mget(XCOLS)],what=mean)
    RSS <- (Y - Yp) %*% (Y - Yp) |> as.double()
    MSE <- RSS / length(Y) # caret::MSE(Yp,Y)
    RMSE <- sqrt(MSE) # caret::RMSE(Yp,Y)
    muY <- mean(Y)
    TSS <- (Y-muY )%*%(Y-muY )|> as.double()
    R2 <-  1-RSS/TSS #caret::R2(Yp,Y)


    return(list(model=.model,data=DATA, RSS=RSS, MSE=MSE, RMSE=RMSE, R2=R2))
  }

  if(!is.null(.newdata) ){
    # .newdata!=NULL: build model, predict new data, return response
    if(regression=="lm"){
      if(level=="mean"){
        VALUE <- (predict(.model,newdata=.newdata,interval = "prediction",what=0.95)) |> as.data.table()
        VALUE <- VALUE$fit
      } else {
        VALUE <- (predict(.model,newdata=.newdata,interval = "prediction",what=level)) |> as.data.table()
        VALUE <- VALUE$upr
      }

    }

    if (regression == "rf") {
      VALUE <- predict(.model, newdata = .newdata)
    }
    if(regression=="qrf"){
      if(level=="mean"){
        VALUE <- predict(.model,newdata=.newdata, what = mean)
      } else {
        VALUE <- predict(.model,newdata=.newdata, what = level)
      }

    }

    if (regression == "knn") {
      bootstrap_preds <- replicate(n_bootstrap, {
        sample_indices <- sample(1:nrow(DATA), replace = TRUE)
        bootstrap_data <- DATA[sample_indices, ]
        knn_model <- kknn::train.kknn(formula = as.formula(paste(y, "~", paste(XCOLS, collapse = "+"))), data = bootstrap_data, kmax = k)
        predict(knn_model, newdata = .newdata)
      })

      if(level == "mean") {
        VALUE <- rowMeans(bootstrap_preds)
      } else {
        VALUE <- apply(bootstrap_preds, 1, function(x) quantile(x, probs = as.numeric(level)))
      }
    }

    return(VALUE)
  }

}
