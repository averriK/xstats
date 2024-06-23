
#' Title
#'
#' @param .data data.table
#' @param variables character
#' @param response character
#' @param .newdata vector
#' @param level double.
#' @param regression character
#' @param removeZeroInstances logical
#' @param uniqueResponses logical
#' @param cvmax integer
#' @param nbmax integer
#'
#' @return vector
#' @export fitModel
#'
#' @import data.table
#' @import caret
#' @import stats
#'
#' @examples
#'
#'

fitModel <- function(.data, variables=NULL,response,.newdata=NULL,level="mean",regression="qrf",removeZeroInstances=FALSE,uniqueResponses=FALSE,cvmax=10,nbmax=100) {
  on.exit(expr = {    rm(list = ls())  }, add = TRUE)
  .  <- .SD <- rowIndex <- pred <- NULL
  # Capture the variable arguments as a vector
  stopifnot(response %in% names(.data))
  stopifnot(length(level)==1)



  if(!is.null(variables) & is.null(.newdata)) {
    PREDICT <- FALSE
    TRAIN <- TRUE
    # variables <- variables
  }

  if(!is.null(.newdata) & is.null(variables)){
    PREDICT <- TRUE
    TRAIN <- FALSE
    variables <- names(.newdata)
  }

  if(is.null(variables) & is.null(.newdata)){
    # Performance Mode. Taking all variables as independents
    PREDICT <- FALSE
    TRAIN <- TRUE
    variables <- colnames(.data)[!colnames(.data) %in% response]
  }

  if(!is.null(variables) & !is.null(.newdata)){
    # Performance Mode. Taking only variables as independents
    PREDICT <- TRUE
    TRAIN <- FALSE
    .newdata <- .newdata[,mget(variables)]
  }
  if(TRAIN==TRUE){
    trControl <- trainControl(method = "cv", number = cvmax)
    tuneGrid <- switch(
      regression,
      "qrf"=expand.grid(mtry = 2:3),
      "rf"=expand.grid(mtry = 2:3),
      "lm"=NULL,
      "glm"=NULL,
      "glmStepAIC"=NULL,
      "kknn"=expand.grid(kmax = 3:7, distance = 1:2,kernel="optimal")

    )

  } else {
    trControl <- trainControl(
      method = "boot",  # bootstrap resampling
      number = nbmax,  # number of bootstrap samples
      savePredictions = "final"  # save the predictions for each resample
    )
    tuneGrid <- NULL
  }

  # Subset the data to include only valid columns
  # Check and keep only those columns that are present in the data.table

  # X.name <- VARS[VARS %in% names(.data)]

  COLS <- c(variables, response)
  .data <- .data[, COLS, with = FALSE] |> na.omit() |> unique()

  # Remove duplicated responses
  if(uniqueResponses){
    idx <- !duplicated(.data[[response]])
    .data <- .data[idx]
  }
  # Remove instances with Zero values
  if(removeZeroInstances){
    idx <- .data[,apply(.SD, 1, function(x) all(x > 0)),.SDcols = variables]
    .data <- .data[idx]
  }

  # DATA <- DATA[, .(Y = mean(get(response))), by = mget(X.name)]

  if(nrow(.data) == 0) {
    stop("No valid data found")
  }
  X <- .data[, mget(variables)]
  Y <- .data[,get(response)] #.data[[response]]
  .data <- cbind(Y, X)

  .model <- caret::train(
    x=X,y=Y,method=regression,
    trControl = trControl,
    tuneGrid = tuneGrid)


  if(TRAIN==TRUE){
    # Performance
    Yp <- predict(.model,newdata=X) |> as.vector()
    RSS <- (Y - Yp) %*% (Y - Yp) |> as.double()
    MSE <- RSS / length(Y) # caret::MSE(Yp,Y)
    RMSE <- sqrt(MSE) # caret::RMSE(Yp,Y)
    muY <- mean(Y)
    TSS <- (Y-muY )%*%(Y-muY )|> as.double()
    R2 <-  1-RSS/TSS #caret::R2(Yp,Y)
    FIT <- list(model=.model,variables=variables,response=response,data=.data, Yp=Yp,RSS=RSS,MSE=MSE, RMSE=RMSE, R2=R2)


    return(FIT)
  } else {
    # Resampled Predictions
    RPDT <- .model$pred |> as.data.table()
    Yp <- sapply(1:nrow(.newdata), function(i) {
      Yp.samples <- RPDT[rowIndex == i, pred]
      ifelse(level=="mean", mean(Yp.samples), quantile(Yp.samples, probs = level))
    })

    return(Yp)
  }

}

