
# generic helper functions ------------------------------------------------

get_data <- function(path) {
  if(!is.null(path)) {
    paste0("../../3_data_encrypted/MESA_2021b/Primary/", path)
  } else {
    stop("Must specify location of CSV directory!")
  }
}

specd <- function(x, k) trimws(format(round(x, k), nsmall=k))

remove_escape_latex <- function(x) {
  enable_special_characters = function(x) {
    gsub("\\\\([&%$#_{}])", "\\1", x, fixed = FALSE, ignore.case = TRUE) 
  }
  enable_backslash = function(x) {
    gsub("\\\\textbackslash([[:space:]])?", "\\\\", x, fixed = FALSE, ignore.case = TRUE)
  }
  enable_tilde = function(x) {
    gsub("\\\\textasciitilde([[:space:]])?", "~", x, fixed = FALSE, ignore.case = TRUE)
  }
  enable_exponents = function(x) {
    gsub("\\\\textasciicircum ", "\\^", x, fixed = FALSE, ignore.case = TRUE)
  }
  
  enable_backslash(enable_special_characters(enable_tilde(enable_exponents(x))))
}


# function to download NHANES data ----------------------------------------

# create function for downloading and merging relevant data files
download_and_merge <-
  function(cycle,
           BPQ,
           RXQ,
           BPX,
           TRIGLY,
           HDL,
           TCHOL,
           SMQ,
           DIQ,
           MCQ,
           BPQ_vars,
           RXQ_vars,
           BPX_vars,
           TRIGLY_vars,
           HDL_vars,
           TCHOL_vars,
           SMQ_vars,
           DIQ_vars,
           MCQ_vars,
           rx_list) {
    
    # load blood pressure and cholesterol questionnaire data
    bpq_data <- nhanes_load_data(
      file_name = BPQ,
      year = cycle, 
      demographics = TRUE,
      recode = TRUE
    )
    
    bpq_data <- select(bpq_data, all_of(BPQ_vars))
    
    # load Rx data
    rxq_data <- nhanes_load_data(
      file_name = RXQ,
      year = cycle, 
      recode = TRUE
    )
    
    rxq_data <- select(rxq_data, all_of(RXQ_vars))
    
    # extract drug list from Rx data
    rxq_data <- mutate(rxq_data, on_statin = RXDDRGID %in% rx_list)
    
    rxq_data <- 
      rxq_data %>%
      group_by(SEQN) %>%
      summarise(on_statin = max(on_statin, na.rm = TRUE), .groups = "drop")
    
    # load blood pressure data 
    bpx_data <- nhanes_load_data(
      file_name = BPX,
      year = cycle, 
      recode = TRUE
    )
    
    bpx_data <- select(bpx_data, all_of(BPX_vars))
    
    
    # load ldl labs 
    trigly_data <- nhanes_load_data(
      file_name = TRIGLY,
      year = cycle, 
      recode = TRUE
    )
    
    trigly_data <- select(trigly_data, all_of(TRIGLY_vars))
    # trigly_data <- mutate(trigly_data, WTSAF2YR = as.numeric(replace(WTSAF2YR, WTSAF2YR == "No Lab Result", NA)))
    
    # load hdl labs 
    hdl_data <- nhanes_load_data(
      file_name = HDL,
      year = cycle, 
      recode = TRUE
    )
    
    hdl_data <- select(hdl_data, any_of(HDL_vars))
    
    # load total cholesterol labs 
    tchol_data <- nhanes_load_data(
      file_name = TCHOL,
      year = cycle, 
      recode = TRUE
    )
    
    tchol_data <- select(tchol_data, all_of(TCHOL_vars))
    
    # load smoking data 
    smq_data <- nhanes_load_data(
      file_name = SMQ,
      year = cycle, 
      recode = TRUE
    )
    
    smq_data <- select(smq_data, all_of(SMQ_vars))
    
    # load smoking data 
    diq_data <- nhanes_load_data(
      file_name = DIQ,
      year = cycle, 
      recode = TRUE
    )
    
    diq_data <- select(diq_data, all_of(DIQ_vars))
    
    # load smoking data 
    mcq_data <- nhanes_load_data(
      file_name = MCQ,
      year = cycle, 
      recode = TRUE
    )
    
    mcq_data <- select(mcq_data, all_of(MCQ_vars))
    
    # merge data sets
    nhanes_data <- list(bpq_data, rxq_data, bpx_data, trigly_data, hdl_data, tchol_data, smq_data, diq_data, mcq_data)
    
    nhanes_combined <- 
      Reduce(function(df1, df2) left_join(df1, df2, by = "SEQN"), nhanes_data)
    
    return(nhanes_combined)
  }


# simulation functions ----------------------------------------------------

generate_snaftm <-
  function(N = 1000,
           P = 1,
           K = 10,
           h0 = function(L0) {
             0.015 * exp(L0)
           },
           c0 = NULL,
           d0 = NULL,
           alpha = function(L, A, k) {
             -2 + 0.75 * L[, 1] + 2.5 * A[, 2]
           },
           beta = function(L, A, T0, k) {
             0.025 * k + L[, 2] - 1 * A[, 2] - 0.10 * (log(T0) - mean(log(T0)))
           },
           gamma = function(L, A, k) {
             exp(-0.5 * A)
           }, 
           Sigma = diag(1, 1, 1),
           Omega = 0.1) {
    
  # initialize matrices
  C <- matrix(rep(0, N * K), nrow = N, ncol = K)
  L <- matrix(rep(0, N * K * P), nrow = N, ncol = K * P)
  A <- matrix(rep(0, N * K), nrow = N, ncol = K)
  D <- matrix(rep(0, N * K), nrow = N, ncol = K)
  Y <- matrix(rep(0, N * K), nrow = N, ncol = K)
  H.psi <- matrix(rep(0, N * K), nrow = N, ncol = K)
  
  # initialize survival times
  T <- rep(K, times = N)
  
  for (k in 1:K) {
    # create history vector
    if ((k + 1) <= K) {
      h <- c(k:1, (k + 1):K)
    } else {
      h <- c(k:1)
    }
    
    hL <- rep(1:P, each = K) + (h - 1) * P
    kL <- 1:P + (k - 1) * P
    
    if (k > 1) {
      # simulate time-varying covariates
      L[, kL] <- rnorm(N, beta(L[, hL], A[, h], T0, k), Omega)
      
    } else {
      # simulate baseline covariates
      L[, kL] <- rmvnorm(N, rep(0, P), Sigma)
      
      # simulate counterfactual time under no treatment
      T0 <- rexp(N, h0(L[k, ]))
    }
    
    # simulate time-varying treatment
    A[, k] <- rbinom(N, 1, plogis(alpha(L[, hL], A[, h], k)))
    
    H.psi[, k] <- rowSums(gamma(L[, hL], A[, h], k)) - (K - k)
      
    # indicate of failure
    Y[, k] <- ifelse(T0 <= H.psi[, k], 1, 0)
    
    # if failed in interval, simulate failure time 
    if (k == 1) {
      Tk <- T0 / gamma(L[, kL], A[, k], k)
      T <- ifelse(T0 <= H.psi[, k], Tk, T)
    } else {
      Tk <- (k - 1) + (T0 - H.psi[, k-1]) / gamma(L[, kL], A[, k], k)
      T <- ifelse(T0 <= H.psi[, k] & Y[, k-1] == 0, Tk, T)
    }
    
    
  }
  
  mat <- cbind(cbind(0, C), cbind(0, D), cbind(0, Y), cbind(L, 0), cbind(A, 0))
  
  vars <- list("C_", "D_", "Y_", paste0("L", 1:P, "_"), "A_")
  matnames <- lapply(vars, function(x) paste0(x, rep(0:K, each = length(x))))
  
  colnames(mat) <- unlist(matnames)
  
  ordernames <- lapply(0:K, function (x) paste0(unlist(vars), as.character(x)))
  mat <- mat[, unlist(ordernames)]
  
  mat <- cbind("id" = 1:N, mat, "T0" = T0, "T" = T)
  return(mat)
}



# custom modelsummary tidiers ---------------------------------------------

tidy.custom <- function (x, conf.int = FALSE, conf.level = 0.95, conf.method = "wald", exponentiate = F, 
                         ...) 
{
  ret <- as_tibble(summary.glm(x)$coefficients, rownames = "term")
  colnames(ret) <- c("term", "estimate", "std.error", "statistic", 
                     "p.value")
  coefs <- tibble::enframe(stats::coef(x), name = "term", 
                           value = "estimate")
  ret <- dplyr::left_join(coefs, ret, by = c("term", "estimate"))
  if (conf.int) {
    if(conf.method == "plik") 
      ci <- suppressMessages(confint(x, ...))
    else
      ci <- suppressMessages(confint.default(x, ...))
    
    if (is.null(dim(ci))) {
      ci <- matrix(ci, nrow = 1)
      rownames(ci) <- names(coef(x))[1]
    }
    
    ci <- as_tibble(ci, rownames = "term")
    names(ci) <- c("term", "conf.low", "conf.high")
    ret <- dplyr::left_join(ret, ci, by = "term")
  }
  if (exponentiate) {
    ret <- mutate_at(ret, vars(estimate), exp)
    if ("conf.low" %in% colnames(ret)) {
      ret <- mutate_at(ret, vars(conf.low, conf.high), exp)
    }
  }
  # ret <- mutate(ret, across(where(is.numeric), round, 2))
  ret
}

glance.custom <- function(x, ...) {
  out <- broom:::as_glance_tibble(null.deviance = x$null.deviance, df.null = x$df.null, 
                                  logLik = as.numeric(stats::logLik(x)), AIC = stats::AIC(x), 
                                  BIC = stats::BIC(x), deviance = stats::deviance(x), 
                                  df.residual = stats::df.residual(x), nobs = stats::nobs(x), 
                                  na_types = "rirrrrii")
  return(out)
}


tidy.snaftm <- function (x, exponentiate = F, ...) {
  if (exponentiate) {
    mutate_at(x, vars(estimate, conf.low, conf.high), exp)
  } else {
    x
  }
}

glance.snaftm <- function(x, ...) {
  out <-
    data.frame(
      nobs = 0
    )
  return(out)
}


# g-methods ---------------------------------------------------------------



# ipcw --------------------------------------------------------------------


ipcw <- function(formula, treatment, numerator = NULL, denominator, id, time, data, weights = NULL) {
  if (is.null(weights)) {
    # treatment models: denominator
    denominator_model_at <- glm(
      formula = denominator,
      family = binomial(link = "logit"),
      data = data[data[[treatment]] == 1, ]
    )
    
    denominator_model_nt <- glm(
      formula = denominator,
      family = binomial(link = "logit"),
      data = data[data[[treatment]] == 0, ]
    )
    
    # treatment models: numerator
    if (!is.null(numerator)) {
      stopifnot(all.vars(numerator)[1] == all.vars(denominator)[1])
      numerator_model_at <- glm(
        formula = numerator,
        family = binomial(link = "logit"),
        data = data[data[[treatment]] == 1, ]
      )
      
      numerator_model_nt <- glm(
        formula = numerator,
        family = binomial(link = "logit"),
        data = data[data[[treatment]] == 0, ]
      )
    }
    
    censor <- all.vars(denominator)[1]
    
    # get predictions
    p_num_at <- 1 - predict(numerator_model_at, newdata = data, type = "response")
    p_num_nt <- 1 - predict(numerator_model_nt, newdata = data, type = "response")
    p_den_at <- 1 - predict(denominator_model_at, newdata = data, type = "response")
    p_den_nt <- 1 - predict(denominator_model_nt, newdata = data, type = "response")
    
    # add to dataset
    data <- dplyr::mutate(
      data, 
      p_num = dplyr::if_else(.data[[treatment]] == 1,  p_num_at, p_num_nt),
      p_den = dplyr::if_else(.data[[treatment]] == 1,  p_den_at, p_den_nt)
    )
    
    # calculate cumulative product
    data <- dplyr::group_by(data, .data[[id]]) 
    
    data <- dplyr::mutate(
      data, 
      p_num = dplyr::if_else(
        .data[[censor]] == 1, 
        1 - p_num * cumprod(dplyr::lag(p_num, 1, default = 1)),
        p_num * cumprod(dplyr::lag(p_num, 1, default = 1))
      ),
      p_den = dplyr::if_else(
        .data[[censor]] == 1, 
        1 - p_den * cumprod(dplyr::lag(p_den, 1, default = 1)),
        p_den * cumprod(dplyr::lag(p_den, 1, default = 1))
      ),
      ipcw = p_num / p_den
    )
    
    data <- dplyr::ungroup(data)
    
  } else {
    denominator_model_at <- NULL
    denominator_model_nt <- NULL
    numerator_model_at <- NULL
    numerator_model_nt <- NULL
    data$ipcw <- weights
  }

  ipcw_model <-
    glm(
      formula = formula,
      family = binomial(link = "logit"),
      weights = ipcw,
      data = data
    )
  
  ret <- list(
    "fit" = ipcw_model,
    "denominator" = list(
      "at" = denominator_model_at,
      "nt" = denominator_model_nt
    ),
    "numerator" = list(
      "at" = numerator_model_at,
      "nt" = numerator_model_nt
    ),
    "ipcw" = data$ipcw
  )
  
  return(ret)
}


# structural nested accelerated failure time models -----------------------

H <- function(psi, data, id, time, eventtime) {
  H.psi <- by(data, data[, id], function(d) {
    x <- d[,!colnames(d) %in% c(id, time, eventtime)]
    tv <- c(d[, time], max(d[, eventtime]))[-1] - d[, time]
    
    if (is.vector(x)) {
      d[, time] + sum(tv * exp(x * psi)) - c(0, cumsum(tv * exp(x * psi)))[1:length(x)]
    } else {
      x <- as.matrix(x)
      d[, time] + sum(tv * exp(x %*% psi)) - c(0, cumsum(tv * exp(x %*% psi)))[1:nrow(x)]
    }
  },
  simplify = FALSE)
  
  H.psi <- unsplit(H.psi, data[, id])
  return(H.psi)
}

K <- function(psi, x1, time, Kmax) {
  ifelse((x1 %*% psi) < 0,
         time + (Kmax - time) * exp(x1 %*% psi),
         Kmax)
}

delta <- function(H.psi, K.psi, event) {
  tol <- 1e-6
  ifelse(event == 0, 0, ifelse(H.psi + tol < K.psi, 1, 0))
}

#' Title
#'
#' @param model 
#' @param tx.formula 
#' @param tx.family 
#' @param cens.formula 
#' @param cens.family 
#' @param comprisk.formula 
#' @param comprisk.family 
#' @param dr.formula 
#' @param dr.family 
#' @param data 
#' @param id 
#' @param time 
#' @param admin.cens.time 
#' @param psi.range 
#' @param psi.start 
#' @param conf.int 
#'
#' @return
#' @export
#'
#' @examples
snaftm <- function(
    model,
    tx.formula,
    tx.family,  
    cens.formula = NULL,
    cens.family = binomial(link = "logit"),
    comprisk.formula = NULL,
    comprisk.family = binomial(link = "logit"),
    dr.formula = NULL,
    dr.family = "coxph",
    data,
    id,
    time, 
    K.max, 
    psi.range = c(-3, 3),
    psi.start = NULL,
    conf.int = FALSE
) {
  
  # check arguments
  
  # remove intercept from formula for rpsm
  model <- update(model, ~ . - 1)
  mf <- model.frame(model, data)
  
  # get survival time and status
  survtime <- mf[, 1][, 1]
  survstat <- mf[, 1][, 2]
  
  # get rpsm model matrix with time and id
  mat <- model.matrix(update(model, paste0("~ . + ", time, " + ", id)), data) 
  mat <- cbind(mat, "survtime" = survtime)
  
  # fit treatment model E[A | L] 
  A.fit <- glm(tx.formula, tx.family, data)
  A.hat <- predict(A.fit, data, type = "response")
  
  # get treatment vector and variable
  A.var <- all.vars(tx.formula)[1]
  A <- mf[[A.var]]
  
  # fit censoring model E[C | A, L]
  if (!is.null(cens.formula)) {
    C.var <- all.vars(cens.formula)[1]
    C.fit <- glm(cens.formula, cens.family, data)
    C.hat <- predict(C.fit, data, type = "response")
  }
  
  # fit competing risk model E[D | A, L]
  if (!is.null(comprisk.formula)) {
    D.var <- all.vars(comprisk.formula)[1]
    D.fit <- glm(comprisk.formula, comprisk.family, data)
    D.hat <- predict(D.fit, data, type = "response")
  }
  
  # set all A to 1 for K
  d <- data
  d[[A.var]] <- 1
  x1 <- model.matrix(model, d)
  
  n <- length(unique(data[[id]]))
  model.dim <- ncol(x1)
  
  # define estimating equation
  eefun <- function(psi) {
    
    # calculate counterfactual quantities H(psi), K(psi), X(psi), and delta(psi)
    H.psi <- H(psi, mat, id, time, "survtime")
    K.psi <- K(psi, x1, data[[time]], K.max)
    delta.psi <- delta(H.psi, K.psi, survstat)
    X.psi <- pmin(H.psi, K.psi)
    
    if (!is.null(dr.formula)) {
      dr.fit <- coxph(dr.formula, data = transform(data, X.psi = X.psi, delta.psi = delta.psi))
      dr.res <- resid(dr.fit)
      delta.psi <- dr.res
    }
    
    smat <- delta.psi * (A - A.hat)
    tmat <- tapply(smat, data[[id]], sum, na.rm = TRUE)
    sval <- sum(tmat, na.rm = TRUE)
    save <- sval / n
    tmat <- tmat - rep(save, n)
    
    # covariance
    sigma <- t(tmat) %*% tmat
    if (sigma == 0) {
      sigma <- 1e-16
    }
    
    return(sval * solve(sigma) * t(sval))
  }
  
  if (model.dim == 1) {
    res <- optimize(eefun, interval = psi.range)
    psi1 <- res$minimum
    objfunc <- as.numeric(res$objective)
  } else {
    res <- optim(psi.values, eefun)
  }
  
  if (conf.int) {
    # Use simple bisection method to find estimates of lower and upper 95% confidence bounds
    increm <- 0.1
    for_conf <- function(x){
      return(eefun(x) - 3.84)
    }
    
    if (objfunc < 3.84) {
      # Find estimate of where sumeef(x) > 3.84
      
      # Lower bound of 95% CI
      psilow <- psi1
      testlow <- objfunc
      countlow <- 0
      while (testlow < 3.84 & countlow < 100){
        psilow <- psilow - increm
        testlow <- eefun(psilow)
        countlow <- countlow + 1
      }
      
      # Upper bound of 95% CI
      psihigh <- psi1
      testhigh <- objfunc
      counthigh <- 0
      while (testhigh < 3.84 & counthigh < 100) {
        psihigh <- psihigh + increm
        testhigh <- eefun(psihigh)
        counthigh <- counthigh + 1
      }
      
      # Better estimate using bisection method
      if ((testhigh > 3.84) & (testlow > 3.84)) {
        
        # Bisection method
        left <- psi1
        fleft <- objfunc - 3.84
        right <- psihigh
        fright <- testhigh - 3.84
        middle <- (left  + right) / 2
        fmiddle <- for_conf(middle)
        count <- 0
        diff <- right - left
        
        while (!(abs(fmiddle) < 0.0001 | diff < 0.0001 | count > 100)) {
          test <- fmiddle * fleft
          if (test < 0){
            right <- middle
            fright <- fmiddle
          } else {
            left <- middle
            fleft <- fmiddle
          }
          middle <- (left + right) / 2
          fmiddle <- for_conf(middle)
          count <- count + 1
          diff <- right - left
        }
        
        psi_high <- middle
        objfunc_high <- fmiddle + 3.84
        
        # lower bound of 95% CI
        left <- psilow
        fleft <- testlow - 3.84
        right <- psi1
        fright <- objfunc - 3.84
        middle <- (left + right) / 2
        fmiddle <- for_conf(middle)
        count <- 0
        diff <- right - left
        
        while(!(abs(fmiddle) < 0.0001 | diff < 0.0001 | count > 100)) {
          test <- fmiddle * fleft
          if (test < 0){
            right <- middle
            fright <- fmiddle
          } else {
            left <- middle
            fleft <- fmiddle
          }
          middle <- (left + right) / 2
          fmiddle <- for_conf(middle)
          diff <- right - left
          count <- count + 1
        }
        psi_low <- middle
        objfunc_low <- fmiddle + 3.84
        psi <- psi1
        
      }
    }
    
    ret <- data.frame(
      term = A.var,
      estimate = psi,
      conf.low = psi_low,
      conf.high = psi_high
    )
    
    return(ret)
  } else {
    return(psi1)
  }
  
}

# ------ G-ESTIMATION OF STRUCTUARL NESTED CUMULATIVE FAILURE TIME MODELS ------
# ---------------------- WITH ADJUSTMENT FOR CONFOUNDERS -----------------------
#
# -------------------------------- BY: Joy Shi ---------------------------------
# -------------------------- LAST MODIFIED: 2021-06-21 -------------------------
#
# NOTES: 
# Please see simulated data as an example of how the data needs to be
# set up for the analysis
# 
# REQUIREMENTS:
# The function relies on packages 'optimx' and 'parallel' (for paralellization
# of the code)
#
# BASED ON THE SNCFTM SAS MACRO BY SALLY PICCIOTTO:
# For more information, refer to
# https://www.hsph.harvard.edu/causal/software/ and 
# https://pubmed.ncbi.nlm.nih.gov/24347749/
#
# ARGUMENTS:
#  - data: name of the data frame containing the variables in the model
#  - id: name of the variable (as a string) corresponding to participant index
#  - time: name of the variable (as a string) corresponding to time index 
#          (minimum must be 1)
#  - x: name of the variable (as a string) corresponding to the treatment
#  - x.modelvars: formula for the model for treatment
#  - x.linkfunction: link function for the treatment model (options are 
#    "identity" for linear regression and "logit" for logistic regression)
#  - y: name of the variable (as a string) corresponding to the outcome
#  - clost: name of the variable (as a string) corresponding to censoring due
#    to lost to follow-up
#  - clost.modelvars: formula for the model for censoring due to lost to 
#    follow-up
#  - cdeath: name of the variable (as a string) corresponding to censoring due
#    to death
#  - death.modelvars: formula for the model for censoring due to death
#  - blipfunction: options are 1 (for 1+[exp(psi*Am)-1]/(k-m)) or 2 
#    (for psi*Am)
#  - start.value: starting value for grid search
#  - grid: set to T to obtain output from the estimating equation across
#    a range of psi value
#  - grid.range: range of psi values to calculate the estimating equation; a
#    single value c is given and the range is (+c, -c)
#  - grid.increment: increments of psi used to calculate the estimating equation
#  - blipupdown: set to T to obtain marginal cumulative risks under the "never
#    treat" and "always treat" regimes by blipping down and blipping up
#  - boot: set to T to obtain 95% CI by bootstrapping
#  - R: number of bootstraps
#  - parallel: set to T to parallelize
#  - seed: seed used for bootstrapping

# Installing and loading required packages
if (!require('parallel')) install.packages('parallel'); library('parallel')
if (!require('optimx')) install.packages('optimx'); library('optimx')

# SNCFTM function 
sncftm <- function(data, id, time, x, x.modelvars, x.linkfunction="identity", y,
                        clost=NULL, clost.modelvars=NULL, cdeath=NULL, cdeath.modelvars=NULL,
                        blipfunction, start.value=0,
                        grid=F, grid.range=1.5, grid.increment=0.01,
                        blipupdown=T, boot=T, R=1000, parallel=T, seed=549274){
  
  # Data Prep and calculations not required for psi 
  if (parallel==T){numCores <- max(detectCores()-1, 1)}
  
  estf.dataprep <- function(data){
    d <- data.frame(data)
    
    # Predicted values from X and censoring models
    if (x.linkfunction=="identity"){x.model <- glm(as.formula(paste(x, "~", paste(x.modelvars)[2], sep="")), data=d)
    } else if (x.linkfunction=="logit"){
      x.model <- glm(as.formula(paste(x, "~", paste(x.modelvars)[2], sep="")), family=binomial, data=d)
    } else{print("Invalid link function for x")}
    d$x.pred <- predict(x.model, newdata=d, type="response")
    if (is.null(clost)==F & is.null(clost.modelvars)==F){
      clost.model <- glm(as.formula(paste(clost, "==0~", paste(clost.modelvars)[2], sep="")), family=binomial(), data=d)
      d$clost.pred <- predict(clost.model, d, type="response")
      d$clost <- d[,clost]
    } else{
      d$clost.pred <- 1
      d$clost <- 0
    }
    if (is.null(cdeath)==F & is.null(cdeath.modelvars)==F){
      cdeath.model <- glm(as.formula(paste(cdeath, "==0~", paste(cdeath.modelvars)[2], sep="")), family=binomial(), data=d)
      d$cdeath.pred <- predict(cdeath.model, d, type="response")
      d$cdeath <- d[,cdeath]
    } else{
      d$cdeath.pred <- 1
      d$cdeath <- 0
    }
    d <- d[order(d[,id], d[,time]),]
    n.followup <- length(unique(d[[time]]))
    
    # Creating dataset restricted to participants who ever had an event
    ever.y.id <- d[which(d[,y]==1),][[id]]
    ever.treat.id <- d[which(d[,x]!=0),][[id]]
    cero <- d[which(d[[id]] %in% ever.y.id),] # Restrict to ever had an event
    cero$ever_treat <- ifelse((cero[[id]] %in% ever.treat.id)==T, 1, 0) # Indicate if ever treated
    cero$count <- ave(rep(1, nrow(cero)), cero[[id]], FUN = sum) # Count for each ID
    
    # Calculating contributions to estimating equation among untreated
    cero.untreated <- cero[which(cero$ever_treat==0),]
    if (nrow(cero.untreated)==0){
      newcov.untreated <- 0
      newu1.untreated <- 0
    } else{
      # Calculating censoring weights
      ## Note:
      ## Censoring weight for time=1 is cumulative product from time=1 to time=k for ID i
      ## Censoring weight for time=2 is cumulative product from time=2 to time=k for ID i
      ## ...etc.
      tpw.untreated <- 1/(cero.untreated[["clost.pred"]]*cero.untreated[["cdeath.pred"]]) # Weight for each time point
      cumw.untreated <- unname(ave(tpw.untreated, cero.untreated[[id]], FUN=prod)) # Cumulative product of weights per ID
      lagtpw.untreated <- suppressWarnings(unname(ave(tpw.untreated, cero.untreated[[id]], FUN=function(j) c(1, j[1:(length(j)-1)]), 1))) # Lagged weights by ID
      cumlagw.untreated <- unname(ave(lagtpw.untreated, cero.untreated[[id]], FUN=cumprod)) # Cumulative product of lagged weights
      w.untreated <- cumw.untreated/cumlagw.untreated
      
      # Contribution to estimating equation from H(psi)
      ## Note:
      ## Only contribution is when Y = 1
      ## Note that if, say, Y = 1 at k = 3, then also contribute to estimating equation at k = 4, 5, K (i.e. until end of follow-up)
      ## Contribution is exp(psi*A)=1 when A=0 (untreated)
      totalfu.untreated <- unname(ave(cero.untreated[[id]], cero.untreated[[id]], FUN=length)) # Duration of follow-up per ID
      tpcontributed.untreated <- n.followup+1-totalfu.untreated # Number of time points contributed to est eq
      hm.untreated <- tpcontributed.untreated*w.untreated # Contribution, weighted by censoring weights
      
      # Multiply H(psi) by X-E[X]
      u0i.untreated <- hm.untreated * (cero.untreated[[x]]-cero.untreated[["x.pred"]])
      
      # Calculating covariance matrix
      newu1.untreated <- aggregate(u0i.untreated, list(cero.untreated[[id]]), FUN=sum)[,2]
      newcov.untreated <- newu1.untreated %*% newu1.untreated
    }
    
    # Calculating contributions to estimating equation among treated
    # Note: only include calculations that aren't dependent on psi here
    cero.treated <- cero[which(cero$ever_treat==1),]
    
    # Calculating censoring weights
    ## Note:
    ## Censoring weight for time=1 is cumulative product from time=1 to time=k for ID i
    ## Censoring weight for time=2 is cumulative product from time=2 to time=k for ID i
    ## ...etc.
    tpw.treated <- 1/(cero.treated[["clost.pred"]]*cero.treated[["cdeath.pred"]]) # Weight for each time point
    cumw.treated <- unname(ave(tpw.treated, cero.treated[[id]], FUN=prod)) # Cumulative product of weights per ID
    lagtpw.treated <- suppressWarnings(unname(ave(tpw.treated, cero.treated[[id]], FUN=function(j) c(1, j[1:(length(j)-1)]), 1))) # Lagged weights by ID
    cumlagw.treated <- unname(ave(lagtpw.treated, cero.treated[[id]], FUN=cumprod)) # Cumulative product of lagged weights
    w.treated <- cumw.treated/cumlagw.treated
    
    # Contribution to estimating equation from H(psi)
    a.treated <- cero.treated[[x]]
    tcount.treated <- as.integer(table(cero.treated[[id]]))
    y.treated <- cero.treated[[y]]
    
    # Return environment
    dataprep.env <- list(
      d = d,
      n.followup = n.followup,
      newcov.untreated = newcov.untreated,
      newu1.untreated = newu1.untreated,
      cero.treated = cero.treated,
      w.treated = w.treated,
      a.treated = a.treated,
      tcount.treated = tcount.treated,
      y.treated = y.treated
    )
    return(dataprep.env)
  }
  dataprep.env <- estf.dataprep(data)
  
  # Calculations for estimating equation dependent on psi
  estf.conf <- function(psi, dataprep.results, blipfunction){
    hm <- rep(0, nrow(dataprep.results$cero.treated))
    last <- 0
    for (i in 1:length(dataprep.results$tcount.treated)){
      count <- dataprep.results$tcount.treated[i]
      start <- last+1
      last <- start+count-1
      atmp <- dataprep.results$a.treated[start:last]
      ytmp <- dataprep.results$y.treated[start:last]
      hm.id <- rep(0, count) 
      for (m in 1:count){
        for (k in count:dataprep.results$n.followup){
          sumblip <- 0
          for (j in m:k){
            if (j <= count){
              if (blipfunction==1){
                numtmp <- (k+1)-j
                denomtmp <- k-j+exp(psi*atmp[j])
              }
              if (blipfunction==2){
                numtmp <- 1
                denomtmp <- exp(psi*atmp[j])
              }
              blip.tmp2 <- log(numtmp)-log(denomtmp)
              sumblip <- sumblip + blip.tmp2
            }
          }
          exp.blip <- exp(sumblip)
          if (k>=count){
            hm.id[m] <- hm.id[m] + exp.blip
          }
        }
      }
      hm[start:last] <- hm.id
    }
    # Multiply Hm with weights and A-E[A]    
    u0i.treated <- hm*dataprep.results$w.treated*(dataprep.results$cero.treated[[x]]-dataprep.results$cero.treated[["x.pred"]])
    
    # Covariance matrix
    newu1.treated <- aggregate(u0i.treated, list(dataprep.results$cero.treated[[id]]), FUN=sum)[,2]
    newcov.treated <- newu1.treated %*% newu1.treated
    
    # Estimating Equation
    newcov <- newcov.treated + dataprep.results$newcov.untreated
    # newcov <- ifelse(newcov==0, 1e-12, newcov)
    newu <- sum(newu1.treated) + sum(dataprep.results$newu1.untreated)
    # return(newu %*% solve(newcov) %*% newu)
    return(as.numeric(newu*newu/newcov))
  }
  
  # Finding minimum of estimating equation
  psi <- NULL
  psi.esteq <- NULL
  psi.converge <- NULL
  estf.results <- suppressWarnings(optimx(start.value, estf.conf, dataprep.results=dataprep.env, blipfunction=blipfunction, method=c("nlminb")))
  if (estf.results$convcode!=0|estf.results$value>0.0001){
    estf.results1 <- suppressWarnings(optimx(start.value, estf.conf, dataprep.results=dataprep.env, blipfunction=blipfunction, method=c("nlm")))
    if (estf.results1$value<estf.results$value){
      psi <- estf.results1$p1
      psi.esteq <- estf.results1$value
      psi.converge <- estf.results1$convcode
    }else{
      psi <- estf.results$p1
      psi.esteq <- estf.results$value
      psi.converge <- estf.results$convcode
    }
  } else{
    psi <- estf.results$p1
    psi.esteq <- estf.results$value
    psi.converge <- estf.results$convcode
  }  
  
  results <- list(psi=psi,
                  psi.esteq=psi.esteq,
                  psi.converge=psi.converge)
  
  #Estimating equation across range of psi values
  if (grid==T){
    if (parallel==T){
      cl <- makeCluster(numCores)
      clusterExport(cl, ls(), envir=environment())
      est.eq.results <- parLapply(cl, seq(-grid.range, grid.range, by=grid.increment), function(i) {estf.conf(i, dataprep.env, blipfunction)})
      stopCluster(cl)
      psi.grid <- data.frame(cbind(psi=seq(-grid.range, grid.range, by=grid.increment), est.eq=do.call(rbind, est.eq.results)))
    }
    if (parallel==F){
      psi.grid <- data.frame(cbind(psi=seq(-grid.range, grid.range, by=grid.increment),
                                   est.eq=sapply(seq(-grid.range, grid.range, by=grid.increment), function(i){estf.conf(i, dataprep.env, blipfunction)})))
    }
    results[["psi.grid"]] <- psi.grid
  }
  
  # Function for blipping down/up
  blipf <- function(dataprep.results, psi.estimate){
    blipdown <- expand.grid(unique(dataprep.results$d[[id]]), unique(dataprep.results$d[[time]]))
    colnames(blipdown) <- c(id, time)
    blipdown <- merge(x=as.data.frame(dataprep.results$d), y=blipdown, by=c(id, time), all=T)
    blipdown$ever_y <- ifelse(blipdown[[id]] %in% unique(blipdown[which(blipdown[[y]]==1),][[id]]), 1, 0)
    blipdown$ever_clost <- ifelse(blipdown[[id]] %in% unique(blipdown[which(blipdown$clost==1),][[id]]), 1, 0)
    blipdown$ever_cdeath <- ifelse(blipdown[[id]] %in% unique(blipdown[which(blipdown$cdeath==1),][[id]]), 1, 0)
    blipdown$ever_cens <- ifelse(blipdown$ever_clost==1|blipdown$ever_cdeath==1, 1, 0)
    blipdown[,y] <- ifelse(is.na(blipdown[[y]]) & blipdown$ever_y==1, 1, blipdown[,y])
    blipdown$ipcw <- ave(1/(blipdown$cdeath.pred*blipdown$clost.pred), blipdown[[id]], FUN=cumprod)
    blipdown$ipcw_max <- ave(blipdown$ipcw, blipdown[[id]], FUN=function(i) max(i, na.rm=T))
    blipdown$ipcw <- ifelse(is.na(blipdown$ipcw), blipdown$ipcw_max, blipdown$ipcw)
    blipdown <- blipdown[!is.na(blipdown[[y]]),]
    blipdown[,x] <- ifelse(is.na(blipdown[[x]]), 0, blipdown[[x]])
    y.0 <- paste(y, 0, sep = ".")
    blipdown[, y.0] <- NA
    
    for (t in unique(dataprep.results$d[[time]])){
      var <- paste("blip0", t, sep="_")
      varcum <- paste("blip0cum", t, sep="_")
      if (blipfunction==1){
        blipdown[,var] <- ifelse(blipdown[,time]>t, 1, ifelse(blipdown$ever_cens==0, (t+1-blipdown[[time]])/(t-blipdown[[time]]+exp(psi.estimate*blipdown[[x]])),0))
      }
      if (blipfunction==2){
        blipdown[,var] <- ifelse(blipdown[,time]>t, 1, ifelse(blipdown$ever_cens==0, 1/exp(psi.estimate*blipdown[,x]), 0))
      }
      blipdown[,varcum] <- ave(blipdown[[var]], blipdown[[id]], FUN=cumprod)
      blipdown[,varcum] <- ifelse(blipdown[[time]]>t, 1, blipdown[[varcum]])
      blipdown[, y.0] <- ifelse(blipdown[[time]]==t, blipdown[[y]]*blipdown[[varcum]], blipdown[[y.0]])
    }
    
    meanY <- data.frame(1:dataprep.results$n.followup)
    colnames(meanY) <- time
    meanY[,y] <- sapply(split(blipdown, blipdown[[time]]), function(i) weighted.mean(i[[y]], i[["ipcw"]]))
    meanY[,y.0] <- sapply(split(blipdown, blipdown[[time]]), function(i) weighted.mean(i[[y.0]], i[["ipcw"]]))
    
    Y0avgs <- rev(meanY[[y.0]])
    
    # Blipping Up
    regime <- rep(1, dataprep.results$n.followup)
    for (k in 1:dataprep.results$n.followup){
      matrix.tmp <- matrix(0, nrow=k, ncol=k)
      km1 <- k-1
      km3 <- k-3
      if (blipfunction==1|blipfunction==2){
        matrix.tmp[1,k] <- exp(psi.estimate*regime[k])
      }
      if (km3>=-1){
        for (c in seq(km3, -1, by=-1)){
          j <- c+2
          jp1 <- j+1
          cp1 <- c+1
          for (rowin in 0:km1){
            m <- rowin+1
            if (rowin<k-j){
              if (blipfunction==1){
                matrix.tmp[m,j] <- matrix.tmp[m,jp1]*((exp(psi.estimate*regime[jp1])-1)/(k-rowin-1-c)+1)
              }
              if (blipfunction==2){
                matrix.tmp[m,j] <- matrix.tmp[m,jp1]*exp(psi.estimate*regime[jp1])
              }
            } 
            if (rowin==(k-j)){
              sumcol <- sum(matrix.tmp[,jp1])
              if (blipfunction==1|blipfunction==2){
                matrix.tmp[m,j] <- (1-sumcol)*exp(psi.estimate*regime[jp1])
              }
            }
          }
        }
      }
      assign(paste("tg", k, sep="_"), matrix.tmp)
    }
    
    EYgs <- rep(0, dataprep.results$n.followup)
    for (matin in 1:dataprep.results$n.followup){
      EYgs[matin] <- Y0avgs[(dataprep.results$n.followup-matin+1):dataprep.results$n.followup] %*% get(paste("tg", matin, sep="_"))[,1]
    }
    meanY[,paste(y, "g", sep=".")] <- EYgs
    return(meanY)
  }
  
  # Checking that we found a minimum that is equal to zero and algorithm converged
  if (blipupdown==T){
    if ((blipfunction==1|blipfunction==2) & (psi.converge!=0|psi.esteq>0.0001)){
      print("Algorithm did not converge, and/or psi estimated on boundary. Cumulative risks under interventions will not be calculated.")
    } else{
      blip.results <- blipf(dataprep.env, psi)
      results[["blip.results"]] <- blip.results
    }
  }
  
  # Bootstrapping
  sncftm.boot <- function(i){
    # Bootstrapping sample
    set.seed(seed.vector[i])
    d.boot <- data.frame(original.id=sample(unique(data[[id]]), replace=T),
                         new.id=1:length(unique(data[[id]])))
    d.boot <- merge(d.boot, data, by.x="original.id", by.y=id, all.x=T)
    colnames(d.boot)[2] <- id
    # Data prep
    dataprep.boot <- estf.dataprep(d.boot)
    # Finding minimum
    psiboot <- NULL
    psiboot.esteq <- NULL
    psiboot.converge <- NULL
    estf.bootresults <- suppressWarnings(optimx(start.value, estf.conf, dataprep.results=dataprep.boot, blipfunction=blipfunction, method=c("nlminb")))
    if (estf.bootresults$convcode!=0|estf.bootresults$value>0.0001){
      estf.bootresults1 <- suppressWarnings(optimx(start.value, estf.conf, dataprep.results=dataprep.boot, blipfunction=blipfunction, method=c("nlm")))
      if (estf.bootresults1$value<estf.bootresults$value){
        psiboot <- estf.bootresults1$p1
        psiboot.esteq <- estf.bootresults1$value
        psiboot.converge <- estf.bootresults1$convcode
      }else{
        psiboot <- estf.bootresults$p1
        psiboot.esteq <- estf.bootresults$value
        psiboot.converge <- estf.bootresults$convcode
      }
    } else{
      psiboot <- estf.bootresults$p1
      psiboot.esteq <- estf.bootresults$value
      psiboot.converge <- estf.bootresults$convcode
    }
    # Blipping down/up
    if (blipupdown==T & psiboot.converge==0 & psiboot.esteq<0.0001){
      blip.results <- blipf(dataprep.boot, psiboot)
      results <- c(psiboot, psiboot.esteq, psiboot.converge, 
                   blip.results[,2], blip.results[,3], blip.results[,4])
      names(results) <- c("psi", "psi.esteq", "psi.converge",
                          paste("Y.t", blip.results[,1], sep=""),
                          paste("Y0.t", blip.results[,1], sep=""),
                          paste("Yg.t", blip.results[,1], sep=""))
      return(results)
    } else{
      results <- c(psi=psiboot, psi.esteq=psiboot.esteq, psi.converge=psiboot.converge)
      return(results)
    }
  }
  if (boot==T){
    set.seed(seed)
    seed.vector <- round(runif(R, min=0, max=1)*10000)
    if (parallel==T){
      cl <- makeCluster(numCores)
      clusterEvalQ(cl, library(optimx))
      clusterExport(cl, ls(), envir=environment())
      boot.results <- parLapply(cl, 1:R, function(i){sncftm.boot(i)})
      stopCluster(cl)
      boot.results <- do.call(rbind, boot.results)
    }
    if (parallel==F){
      boot.results <- lapply(1:R, function(i){sncftm.boot(i)})
      boot.results <- do.call(rbind, boot.results)
    }
    results[["boot.results"]] <- boot.results  
  }
  
  # Returning results
  return(results)
}
sncftm <- function() {
  
}

sncstm <- function() {
  
}


# risk equations ----------------------------------------------------------

#' ACC/AHA 2013 ASCVD risk score
#'
#' Computes 10-year risk for hard ASCVD event (defined as first occurrence of
#' non-fatal myocardial infarction (MI), congestive heart disease (CHD) death,
#' or fatal or nonfatal stroke).
#'
#' @param race patient race (white, aa)
#' @param gender patient gender (male, female)
#' @param age patient age (years)
#' @param totchol Total cholesterol (mg/dL)
#' @param hdl HDL cholesterol (mg/dL)
#' @param sbp Systolic blood pressure (mm Hg)
#' @param bp_med Patient is on a blood pressure medication (1=Yes, 0=No)
#' @param smoker Current smoker (1=Yes, 0=No)
#' @param diabetes Diabetes (1=Yes, 0=No)
#' @param ... Additional predictors can be passed and will be ignored
#'
#'
#' @return Estimated 10-Y Risk for hard ASCVD (percent)
#'
#' @export
#'
#' @examples
#' library(CVrisk)
#' ascvd_10yr_accaha(
#'   race = "aa", gender = "male", age = 55,
#'   totchol = 213, hdl = 50, sbp = 140,
#'   bp_med = 0, smoker = 0, diabetes = 0
#' )
#' @references
#' Goff, David C., et al. "2013 ACC/AHA guideline on the assessment of
#' cardiovascular risk: a report of the American College of
#' Cardiology/American Heart Association Task Force on Practice
#' Guidelines." Journal of the American College of Cardiology 63.25
#' Part B (2014): 2935-2959.
ascvd_10yr_accaha <- function(race = "white", gender = c("male", "female"),
                              age, totchol, hdl, sbp,
                              bp_med, smoker, diabetes, 
                              baseline_survival = c(0.9665, 0.9533, 0.9144, 0.8954),  ...) {
  if (any(!race %in% c("aa", "white") | missing(race))) {
    stop("race must be either 'aa' or 'white'")
  }
  
  if (any(!gender %in% c("male", "female") | missing(gender))) {
    stop("gender must be either 'male' or 'female'")
  }
  
  if (any(!is.numeric(age) | age < 1 | age > 120 | missing(age))) {
    stop("age must be a valid numeric value'")
  }
  
  if (any(!is.numeric(totchol) | totchol < 1 | totchol > 999 | missing(totchol))) {
    stop("totchol must be a valid numeric value'")
  }
  
  
  d <- tribble(
    ~race, ~gender, ~ln_age, ~ln_age_squared, ~ln_totchol, ~ln_age_totchol, ~ln_hdl, ~ln_age_hdl, ~ln_treated_sbp, ~ln_age_treated_sbp, ~ln_untreated_sbp, ~ln_age_untreated_sbp, ~smoker, ~ln_age_smoker, ~diabetes, ~group_mean, ~baseline_survival,
    "white", "female", -29.799, 4.884, 13.54, -3.114, -13.578, 3.149, 2.019, 0, 1.957, 0, 7.574, -1.665, 0.661, -29.18, baseline_survival[1],
    "aa", "female", 17.114, 0, 0.94, 0, -18.92, 4.475, 29.291, -6.432, 27.82, -6.087, 0.691, 0, 0.874, 86.61, baseline_survival[2],
    "white", "male", 12.344, 0, 11.853, -2.664, -7.99, 1.769, 1.797, 0, 1.764, 0, 7.837, -1.795, 0.658, 61.18, baseline_survival[3],
    "aa", "male", 2.469, 0, 0.302, 0, -0.307, 0, 1.916, 0, 1.809, 0, 0.549, 0, 0.645, 19.54, baseline_survival[4]
  )
  
  r <- mapply(
    FUN = function(race, gender)
      which(d$race == race & d$gender == gender),
    race = race,
    gender = gender
  )
  
  pooled_coef <- d[r, ]
  
  sbp_treated <- ifelse(bp_med == 1, sbp, 1)
  sbp_untreated <- ifelse(bp_med == 0, sbp, 1)
  
  indv_sum <- log(age) * pooled_coef$ln_age +
    log(age)^2 * pooled_coef$ln_age_squared +
    log(totchol) * pooled_coef$ln_totchol +
    log(age) * log(totchol) * pooled_coef$ln_age_totchol +
    log(hdl) * pooled_coef$ln_hdl +
    log(age) * log(hdl) * pooled_coef$ln_age_hdl +
    log(sbp_treated) * pooled_coef$ln_treated_sbp +
    log(sbp_treated) * log(age) * pooled_coef$ln_age_treated_sbp +
    log(sbp_untreated) * pooled_coef$ln_untreated_sbp +
    log(sbp_untreated) * log(age) * pooled_coef$ln_age_untreated_sbp +
    smoker * pooled_coef$smoker +
    smoker * log(age) * pooled_coef$ln_age_smoker +
    diabetes * pooled_coef$diabetes
  
  risk_score <- (1 - (pooled_coef$baseline_survival ^ exp(indv_sum - pooled_coef$group_mean)))
  
  return(risk_score)
}

#' Framingham 2008 ASCVD risk score (with lab measurement)
#'
#' Computes 10-year risk for ASCVD event (coronary death, myocardial
#' infarction (MI), coronary insufficiency, angina, ischemic stroke,
#' hemorrhagic stroke, transient ischemic attack, peripheral artery disease,
#' or heart failure).
#'
#' @param gender patient gender (male, female)
#' @param age patient age (years), between 30 and 74
#' @param hdl HDL cholesterol (mg/dL)
#' @param totchol Total cholesterol (mg/dL)
#' @param sbp Systolic blood pressure (mm Hg)
#' @param bp_med Patient is on a blood pressure medication (1=Yes, 0=No)
#' @param smoker Current smoker (1=Yes, 0=No)
#' @param diabetes Diabetes (1=Yes, 0=No)
#' @param ... Additional predictors can be passed and will be ignored
#'
#' @return Estimated 10-Y Risk for hard ASCVD event (percent)
#'
#' @export
#'
#' @examples
#' library(CVrisk)
#' ascvd_10y_frs(
#'   gender = "male", age = 55,
#'   hdl = 50, totchol = 213, sbp = 140,
#'   bp_med = 0, smoker = 0, diabetes = 0
#' )
#'
#' # 16.7
#' @references
#' D’agostino, R.B., Vasan, R.S., Pencina, M.J., Wolf, P.A., Cobain, M.,
#' Massaro, J.M. and Kannel, W.B., 2008. General cardiovascular risk
#' profile for use in primary care: the Framingham Heart Study.
#' Circulation, 117(6), pp.743-753.
ascvd_10yr_frs <- function(gender = c("male", "female"),
                           age, hdl, totchol, sbp,
                           bp_med, smoker, diabetes,
                           baseline_survival = c(0.88936, 0.95012), ...) {
  if (any(!gender %in% c("male", "female") | missing(gender))) {
    stop("gender must be either 'male' or 'female'")
  }
  
  if (any(!is.numeric(age) | missing(age))) {
    stop("age must be a valid numeric value'")
  }
  
  if (any(age < 1 | age > 120)) {
    return(NA)
  }
  
  
  # retrieve model coefficients
  d <- tribble(
    ~gender,
    ~ln_age,
    ~ln_totchol,
    ~ln_hdl,
    ~ln_untreated_sbp,
    ~ln_treated_sbp,
    ~smoker,
    ~diabetes,
    ~group_mean,
    ~baseline_survival,
    "male",
    3.06117,
    1.12370,
    -0.93263,
    1.93303,
    1.99881,
    0.65451,
    0.57367,
    23.9802,
    baseline_survival[1],
    "female",
    2.32888,
    1.20904,
    -0.70833,
    2.76157,
    2.82263,
    0.52873,
    0.69154,
    26.1931,
    baseline_survival[2]
  )
  
  r <- mapply(
    FUN = function(race, gender)
      which(d$gender == gender),
    gender = gender
  )
  
  model_coef <- d[r, ]
  
  sbp_treated <- ifelse(bp_med == 1, sbp, 1)
  sbp_untreated <- ifelse(bp_med == 0, sbp, 1)
  
  indv_sum <- log(age) * model_coef$ln_age +
    log(hdl) * model_coef$ln_hdl +
    log(totchol) * model_coef$ln_totchol +
    log(sbp_treated) * model_coef$ln_treated_sbp +
    log(sbp_untreated) * model_coef$ln_untreated_sbp +
    smoker * model_coef$smoker +
    diabetes * model_coef$diabetes
  
  risk_score <- (1 - (model_coef$baseline_survival ^ exp(indv_sum - model_coef$group_mean)))
  
  return(risk_score)  
}


compute_risk_score <- function(fit, newdata, time) {
  
  log_HR <- predict(fit, newdata = newdata, type = 'lp', reference = 'zero') 
  h0 <- suppressWarnings(basehaz(fit, centered = FALSE))
  time <- max(h0$time[h0$time <= time])
  p <- 1 - exp(-h0$hazard[h0$time == time])^exp(log_HR)
  
  return(p)
}

