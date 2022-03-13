
# generic helper functions ------------------------------------------------

get_data <- function(path) {
  if(!is.null(path)) {
    paste0("../../3_data_encrypted/MESA_2021b/Primary/", path)
  } else {
    stop("Must specify location of CSV directory!")
  }
}

specd <- function(x, k) trimws(format(round(x, k), nsmall=k))


# simulation functions ----------------------------------------------------

generate_snaftm <-
  function(N = 1000,
           P = 1,
           K = 10,
           h0 = function(L0) {
             0.01 * exp(L0)
           },
           c0 = NULL,
           d0 = NULL,
           alpha = function(L, A, k) {
             -4 + 4 * I(L[, 1] > 1) + 2 * A[, 2]
           },
           beta = function(L, A, T0, k) {
             0.05 * k + L[, 2] - A[, 1] - 0.10 * (log(T0) - mean(log(T0)))
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
  Hk <- matrix(rep(0, N * K), nrow = N, ncol = K)
  
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
      L[, kL] <- mvtnorm::rmvnorm(N, rep(0, P), Sigma)
      
      # simulate counterfactual time under no treatment
      T0 <- rexp(N, h0(L[k, ]))
    }
    
    # simulate time-varying treatment
    A[, k] <- rbinom(N, 1, plogis(alpha(L[, hL], A[, h], k)))
    
    Hk[, k] <- rowSums(gamma(L[, hL], A[, h], k)) - (K - k)
      
    # indicate of failure
    Y[, k] <- ifelse(T0 <= Hk[, k], 1, 0)
    
    # if failed in interval, simulate failure time 
    if (k == 1) {
      Tk <- T0 / gamma(L[, kL], A[, k], k)
      T <- ifelse(T0 <= Hk[, k], Tk, T)
    } else {
      Tk <- (k - 1) + (T0 - Hk[, k-1]) / gamma(L[, kL], A[, k], k)
      T <- ifelse(T0 <= Hk[, k] & Y[, k-1] == 0, Tk, T)
    }
    
    
  }
  
  mat <- cbind(cbind(0, C), cbind(0, D), cbind(0, Y), cbind(L, 0), cbind(A, 0))
  
  vars <- list("C", "D", "Y", paste0("L", 1:P, "_"), "A")
  matnames <- lapply(vars, function(x) paste0(x, rep(0:K, each = length(x))))
  
  colnames(mat) <- unlist(matnames)
  
  ordernames <- lapply(0:K, function (x) paste0(unlist(vars), as.character(x)))
  mat <- mat[, unlist(ordernames)]
  
  mat <- cbind("id" = 1:N, mat, "T0" = T0, "T" = T)
  return(mat)
}



# custom modelsummary tidiers ---------------------------------------------

tidy.custom <- function (x, conf.int = FALSE, conf.level = 0.95, conf.method = "wald", exponentiate = TRUE, 
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


tidy.snaftm <- function (x, exponentiate = TRUE, ...) {
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

coxmsm <- function(treatment, msm, numerator = NULL, denominator, id, time, data, center = FALSE) {
  
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
      ipw = p_num / p_den
    )
  
  data <- dplyr::ungroup(data)
  
  if (!is.list(msm)) {
    msm <- list(msm)
  }
  
  msm_model <- lapply(msm, function (x) {
    glm(
      formula = x,
      family = binomial(link = "logit"),
      weights = ipw,
      data = data
    )
  })
  
  ret <- list(
    "msm" = msm_model,
    "denominator" = list(
      "at" = denominator_model_at,
      "nt" = denominator_model_nt
    ),
    "numerator" = list(
      "at" = numerator_model_at,
      "nt" = numerator_model_nt
    ),
    "ipw" = data$ipw
  )
  
  return(ret)
}


# structural nested accelerated failure time models -----------------------

H <- function(psi, data, id, time, eventtime) {
  H.psi <- by(data, data[, id], function(d) {
    x <- d[,!colnames(d) %in% c(id, time, eventtime)]
    tv <- c(d[, time], max(d[, eventtime]))[-1] - d[, time]
    
    if (is.vector(x)) {
      sum(tv * exp(x * psi)) - c(0, cumsum(tv * exp(x * psi)))[1:length(x)]
    } else {
      x <- as.matrix(x)
      sum(tv * exp(x %*% psi)) - c(0, cumsum(tv * exp(x %*% psi)))[1:nrow(x)]
    }
  },
  simplify = FALSE)
  
  H.psi <- unsplit(H.psi, data[, id])
  return(H.psi)
}

K <- function(psi, x1, time, Kmax) {
  ifelse((x1 %*% psi) < 0,
         (Kmax - time) * exp(x1 %*% psi),
         Kmax - time)
}

delta <- function(H.psi, K.psi, event) {
  ifelse(event == 0, 0, ifelse(H.psi < K.psi, 1, 0))
}

snaftm <- function(
  rpsm,
  formula,
  family,
  data,
  id = NULL,
  time = NULL,
  Kmax = 120,
  psi.range = c(-2, 2),
  psi.values = NULL
) {
  
  # extract treatment variable
  A.var <- all.vars(formula)[1]
  
  # remove intercept from formula for rpsm
  rpsm <- update(rpsm, ~ . - 1)
  
  mf <- model.frame(rpsm, data)
  
  # get survival time and status
  survtime <- mf[, 1][, 1]
  survstat <- mf[, 1][, 2]
  
  # get rpsm model matrix with time
  mat <- model.matrix(update(rpsm, paste0("~ . + ", time, " + ", id)), data) 
  mat <- cbind(mat, "survtime" = survtime)
  
  # set all A to 1 for K
  d <- data
  d[[A.var]] <- 1
  x1 <- model.matrix(rpsm, d)
  
  n <- nrow(data)
  rpsm.dim <- ncol(x1)
  
  # get treatment vector
  A <- mf[[A.var]]
  
  # fit treatment model E[A | L] 
  A.fit <- glm(formula, family, data)
  A.hat <- predict(A.fit, data, type = "response")

  # define estimating equation
  eefun <- function(psi) {
  
    H.psi <- H(psi, mat, id, time, "survtime")
    K.psi <- K(psi, x1, data[[time]], Kmax)
    delta.psi <- delta(H.psi, K.psi, survstat)
  
    smat <- delta.psi * (A - A.hat)
    sval <- sum(smat, na.rm = TRUE)
    save <- sval / n
    smat <- smat - rep(save, n)
    
    # covariance
    sigma <- t(smat) %*% smat
    if (sigma == 0) {
      sigma <- 1e-16
    }
    
    return(sval * solve(sigma) * t(sval))
  }
  
  if (rpsm.dim == 1) {
    res <- optimize(eefun, interval = psi.range)
    psi1 <- res$minimum
    objfunc <- as.numeric(res$objective)
  } else {
    res <- optim(psi.values, eefun)
  }
  
  if (rpsm.dim == 1) {
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
    return(res)
  }
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
