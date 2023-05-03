viking <- function(theta1,P1,X,y,hata1,s1,hatb1,Sigma1,n_iter,
                   rho_b=0,rho_a=0,verbose=100,learn_sigma=F,learn_Q=T,K=NULL) {
  n <- dim(X)[1]
  d <- dim(X)[2]
  if (is.null(K))
    K <- diag(d)
  
  theta_arr <- matrix(0,n,d)
  P_arr <- array(0,dim=c(n,d,d))
  q_arr <- matrix(0,n,d)
  hata_arr <- numeric(n)
  s_arr <- numeric(n)
  hatb_arr <- numeric(n)
  Sigma_arr <- numeric(n)
  
  theta <- theta1
  P <- P1
  hata <- hata1
  s <- s1
  hatb <- hatb1
  Sigma <- Sigma1
  
  for (t in 1:n) {
    if (t %% verbose == 0)
      print(t)
    # saving values
    theta_arr[t,] <- K %*% theta
    P_arr[t,,] <- K %*% P %*% t(K)
    hata_arr[t] <- hata
    s_arr[t] <- s
    hatb_arr[t] <- hatb
    Sigma_arr[t] <- Sigma
    q_arr[t,] <- rep(exp(hatb + Sigma/2), d)
    
    hata_new <- hata
    s_new <- s + rho_a
    hatb_new <- hatb
    Sigma_new <- Sigma + rho_b
    for (iter in 1:n_iter) {
      C_inv <- solve(K %*% P %*% t(K) + exp(hatb_new) * diag(d))
      A <- C_inv - exp(hatb_new) * Sigma_new * C_inv %*% C_inv / 2 +
        exp(2*hatb_new) * Sigma_new * C_inv %*% C_inv %*% C_inv
      A_inv <- solve((A+t(A))/2)
      if (max(eigen(A_inv)$values) < 0)
        print(A_inv)
      
      # update of theta and P
      v <- exp(hata_new - s_new/2)
      P_new <- A_inv - tcrossprod(A_inv %*% X[t,]) /
        (crossprod(X[t,], A_inv %*% X[t,])[1] + v)
      theta_new <- K %*% theta + 
        A_inv %*% X[t,] / (crossprod(X[t,], A_inv %*% X[t,])[1] + v) *
        (y[t] - crossprod(K %*% theta,X[t,])[1])
      
      # update of hata and s
      if (learn_sigma) {
        c <- (y[t] - crossprod(theta_new, X[t,])[1])^2 + crossprod(X[t,], P_new %*% X[t,])[1]
        s_new <- ((s + rho_a)^-1 + 0.5 * c * exp(- hata_new + (s - rho_a) / 2))^-1
        s_new <- max(s_new, s-rho_a)
        M <- 100 * rho_a
        diff <- 0.5 * ((s + rho_a)^-1 + 0.5 * c * exp(- hata + s_new / 2 + M))^-1 *
          (c * exp(- hata + s_new / 2) - 1)
        hata_new <- hata + max(min(diff, M), -M)
      }
      
      # update of hatl and Sigma
      if (learn_Q) {
        B <- P_new + tcrossprod(theta_new - K %*% theta)
        
        C_inv <- solve(K %*% P %*% t(K) + exp(hatb) * diag(d))
        g <- sum(diag( C_inv %*% (diag(d) - B%*%C_inv) )) * exp(hatb)
        
        ##### APPROXIMATION AND NO UPPER BOUND
        C_inv <- solve( K %*% P %*% t(K) + diag(d) * exp(hatb))
        H <- sum(diag( C_inv %*% (diag(d) - B%*%C_inv) )) * exp(hatb) +
          2 * sum(diag( C_inv %*% C_inv %*% (B %*% C_inv - diag(d) / 2) )) * exp(2*(hatb))
        ##### TRUE UPPER BOUND
        # H <- d + 2 * sum(diag( B %*% solve(K %*% P %*% t(K)) ))
        ##### OPTIMISTIC UPPER BOUND
        if (T) {
          H <- max(0, H)
          alpha <- sum(diag( C_inv %*% (diag(d) - B %*% C_inv) )) / 6 / 
            sum(diag( C_inv %*% C_inv %*% C_inv %*% (diag(d) / 3 - B %*% C_inv) ))
          beta <- sum(diag( C_inv %*% C_inv %*% (B %*% C_inv - diag(d) / 2) )) /
            sum(diag( C_inv %*% C_inv %*% C_inv %*% (diag(d) / 3 - B %*% C_inv) ))
          if (beta^2 >= 4 * alpha) {
            b1 <- - beta / 2 + sqrt(beta^2 / 4 - alpha)
            if (b1 > 0) {
              b1 <- log(b1)
              C1 <- solve( K %*% P %*% t(K) + diag(d) * exp(b1))
              H <- max(H, sum(diag( C1 %*% (diag(d) - B%*%C1) )) * exp(b1) +
                         2 * sum(diag( C1 %*% C1 %*% (B %*% C1 - diag(d) / 2) )) * exp(2*b1))
            }
            b2 <- - beta / 2 - sqrt(beta^2 / 4 - alpha)
            if (b2 > 0) {
              b2 <- log(b2)
              C2 <- solve( K %*% P %*% t(K) + diag(d) * exp(b2))
              H <- max(H, sum(diag( C2 %*% (diag(d) - B%*%C2) )) * exp(b2) +
                         2 * sum(diag( C2 %*% C2 %*% (B %*% C2 - diag(d) / 2) )) * exp(2*b2))
            }
          }
        }
        
        Sigma_new <- ((Sigma + rho_b)^-1 + H / 2)^-1
        hatb_new <- hatb - Sigma_new * g / 2
      }
    }
    
    P <- P_new
    theta <- theta_new
    hata <- hata_new
    s <- s_new
    hatb <- hatb_new
    Sigma <- Sigma_new
  }
  list(theta_arr=theta_arr, P_arr=P_arr, q_arr=q_arr, hata_arr=hata_arr, s_arr=s_arr,
       hatb_arr=hatb_arr, Sigma_arr=Sigma_arr)
}
