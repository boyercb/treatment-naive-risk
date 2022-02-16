
# generic helper functions ------------------------------------------------

get_data <- function(path) {
  if(!is.null(path)) {
    paste0("../../3_data_encrypted/MESA_2021b/Primary/", path)
  } else {
    stop("Must specify location of CSV directory!")
  }
}


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


# g-methods ---------------------------------------------------------------


snaftm <- function() {
  
}

sncftm <- function() {
  
}

sncstm <- function() {
  
}

