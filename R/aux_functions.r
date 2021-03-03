rec_a <- function(x){
  case_when(x == "1" ~ 1,
            x == "0" ~ -1,
            TRUE ~ 0)
}

rec_n <- function(x){
  case_when(x %in% c("1", "0") ~ 1,
            TRUE ~ 0)
}

h2o.glrm.ctrl <- function(
                          cols = NULL,
                          model_id = NULL,
                          validation_frame = NULL,
                          ignore_const_cols = TRUE,
                          score_each_iteration = FALSE,
                          representation_name = NULL,
                          loading_name = NULL,
                          transform = c("NONE", "STANDARDIZE","NORMALIZE", "DEMEAN", "DESCALE"),
                          k = 5,
                          loss = c("Quadratic","Absolute", "Huber", "Poisson", "Hinge", "Logistic","Periodic"),
                          loss_by_col = c("Quadratic", "Absolute","Huber", "Poisson", "Hinge", "Logistic", "Periodic", "Categorical", "Ordinal"),
                          loss_by_col_idx = NULL,
                          multi_loss = c("Categorical","Ordinal"),
                          period = 1,
                          regularization_x = c("None","Quadratic", "L2", "L1", "NonNegative", "OneSparse", "UnitOneSparse", "Simplex"),
                          regularization_y = c("None","Quadratic", "L2", "L1", "NonNegative", "OneSparse", "UnitOneSparse", "Simplex"),
                          gamma_x = 0,
                          gamma_y = 0,
                          max_iterations = 10000,
                          max_updates = 2000,
                          init_step_size = 1,
                          min_step_size = 1e-04,
                          seed = -1,
                          init = c("Random", "SVD", "PlusPlus", "User"),
                          svd_method = c("GramSVD", "Power", "Randomized"),
                          user_y = NULL,
                          user_x = NULL,
                          expand_user_y = TRUE,
                          impute_original = FALSE,
                          recover_svd = FALSE,
                          max_runtime_secs = 0,
                          export_checkpoints_dir = NULL,
                          ...){
  parms <- list()
    if (!missing(model_id))
      parms$model_id <- model_id
    if (!missing(validation_frame))
      parms$validation_frame <- validation_frame
    if (!missing(ignore_const_cols))
      parms$ignore_const_cols <- ignore_const_cols
    if (!missing(score_each_iteration))
      parms$score_each_iteration <- score_each_iteration
    if (!missing(representation_name))
      parms$representation_name <- representation_name
    if (!missing(loading_name))
      parms$loading_name <- loading_name
    if (!missing(transform))
      parms$transform <- transform
    if (!missing(k))
      parms$k <- k
    if (!missing(loss))
      parms$loss <- loss
    if (!missing(loss_by_col))
      parms$loss_by_col <- loss_by_col
    if (!missing(loss_by_col_idx))
      parms$loss_by_col_idx <- loss_by_col_idx
    if (!missing(multi_loss))
      parms$multi_loss <- multi_loss
    if (!missing(period))
      parms$period <- period
    if (!missing(regularization_x))
      parms$regularization_x <- regularization_x
    if (!missing(regularization_y))
      parms$regularization_y <- regularization_y
    if (!missing(gamma_x))
      parms$gamma_x <- gamma_x
    if (!missing(gamma_y))
      parms$gamma_y <- gamma_y
    if (!missing(max_iterations))
      parms$max_iterations <- max_iterations
    if (!missing(max_updates))
      parms$max_updates <- max_updates
    if (!missing(init_step_size))
      parms$init_step_size <- init_step_size
    if (!missing(min_step_size))
      parms$min_step_size <- min_step_size
    if (!missing(seed))
      parms$seed <- seed
    if (!missing(init))
      parms$init <- init
    if (!missing(svd_method))
      parms$svd_method <- svd_method
    if (!missing(user_y))
      parms$user_y <- user_y
    if (!missing(user_x))
      parms$user_x <- user_x
    if (!missing(expand_user_y))
      parms$expand_user_y <- expand_user_y
    if (!missing(impute_original))
      parms$impute_original <- impute_original
    if (!missing(recover_svd))
      parms$recover_svd <- recover_svd
    if (!missing(max_runtime_secs))
      parms$max_runtime_secs <- max_runtime_secs
    if (!missing(export_checkpoints_dir))
      parms$export_checkpoints_dir <- export_checkpoints_dir
  return(parms)
}


h2o.init.ctrl <- function(
            enable_assertions = FALSE,
            nthreads = -1,
            max_mem_size = "4G",
            ...){
  list(
    enable_assertions = enable_assertions,
    nthreads = nthreads,
    max_mem_size = max_mem_size)
}

rs <- function(x, l=-2, u=2){
  x <- x-min(x)
  x <- x/max(x)
  x <- x*(u-l)
  x <- x + l
  x
}

getP <- function(x, ....){
  if(!inherits(x, "try-error")){
    b <- coef(x)[2]
    s <- sqrt(diag(vcov(x)))[2]
    p <- pnorm(abs(b/s), lower.tail=FALSE)
    p
  }else{
    NA
  }
}

getCoef <- function(x, ....){
  if(!inherits(x, "try-error")){
    b <- coef(x)[2]
  }else{
    NA
  }
}

##' @method predict logistf
predict.logistf <- function(obj, type="response"){
  ## this version of the predict function for Firth logit
  ## is designed specifically for this application and is 
  ## not a general-use function. 
  type <- match.arg(type)
  plogis(obj$linear.predictors)
}

firth_fun <- function(formula, data, ...){
  logistf(formula, data)
}

