se.fixef <- function (object) {
  fcoef.name <- names(fixef(object))
  corF <- vcov(object)@factors$correlation
  ses <- corF@sd
  names(ses) <- fcoef.name
  ses
}

se.ranef <- function (object) {
  se.bygroup <- ranef(object, condVar = TRUE)
  n.groupings <- length(se.bygroup)
  for (m in 1:n.groupings) {
    vars.m <- attr(se.bygroup[[m]], "postVar")
    K <- dim(vars.m)[1]
    J <- dim(vars.m)[3]
    names.full <- dimnames(se.bygroup[[m]])
    se.bygroup[[m]] <- array(NA, c(J, K))
    for (j in 1:J) {
      se.bygroup[[m]][j, ] <- sqrt(diag(as.matrix(vars.m[,,j])))
    }
    dimnames(se.bygroup[[m]]) <- list(names.full[[1]], names.full[[2]])
  }
  se.bygroup  
}

ranefSE <- function (object) {
  re <- do.call(cbind,ranef(object))
  se <- as.data.frame(do.call(cbind,se.ranef(object)))
  names(se) <- paste0("se.",names(se))
  cbind(re,se)
}