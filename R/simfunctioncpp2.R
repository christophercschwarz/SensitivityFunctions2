simfunctioncpp2 <- function(y,X){
      ny <- colnames(y)
      nx <- colnames(X)
      cov <- cov(cbind(y,X))
      cov <- augmentcpp(cov)

      crm <- as.data.frame(cor(cbind(y,X)))
      vec <- cov[ncol(X)+2,]
      crm[ncol(X)+2,] <- vec[-(ncol(X)+2)]
      crm[,ncol(X)+2] <- vec
      crm[ncol(X)+2,ncol(X)+2] <- 1
      crm <- as.matrix(crm)

      out1 <- vec

      cv_inv <- solve(cov)

      mat <- cv_inv

      for (i in 1:nrow(mat)){
        for (j in 1:ncol(mat)){
          mat[i,j] <- -cv_inv[i,j]/cv_inv[i,i]
        }
      }

      beta <- mat[1,-1]

      rsq <- crm[1,-1] %*% solve(crm[-1,-1]) %*% crm[-1,1]

      out2 <- c(beta,rsq)
      names(out2) <- c(nx,"cf","rsq")

      list("Correlations" = out1, "Coefficients" = out2)
}
