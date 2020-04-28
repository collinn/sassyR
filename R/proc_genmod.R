#' Emulator of SAS proc_genmod output
#'
#' @description This function emulates some of the output returned by proc_genmod
#' in SAS. It is simply a wrapper for a handful of diagnostics, organized in
#' a slightly convenient way. This function does not fit any actual models,
#' and thus must be passed a fitted object of class 'glm'
#'
#' @param fit A fitting 'glm' or 'glm.nb' object
#' @param scale Matching SAS argument, uses either pearson or deviance for
#' estimation of dispersion parameter, takes either "pearson" or "deviance"
#'
#' @examples
#' library(MASS)
#'
#' ## Examples match course notes (4.5-4.7)
#' data(ACHD)
#'
#' fit1 <- glm(Mort ~ Age_Gp, family = poisson, offset = Log_Pop, data = ACHD)
#' fit2 <- glm.nb(Mort ~ Age_Gp + offset(Log_Pop), data = ACHD)
#' fit3 <- glm(cbind(Mort, Pop-Mort) ~ Age_Gp,
#'             family = binomial(link = "log"), data = ACHD)
#' fit4 <- glm(cbind(Mort, Pop-Mort) ~ Age_Gp,
#'             family = binomial(link = "logit"), data = ACHD)
#' fit5 <- glm(Log_Mort ~ Age_Gp, family = gaussian,
#'             offset = Log_Pop, data = ACHD)
#'
#' proc_genmod(fit1)
#' proc_genmod(fit2)
#' proc_genmod(fit3)
#' proc_genmod(fit4)
#' proc_genmod(fit5)
#' @export

proc_genmod <- function(fit, scale = "pearson") {

  if(!("glm" %in% class(fit))) {
    stop("must submit glm to proc_genmod")
  }

  fam <- list("family" = fit$family[[1]],
              "link" = fit$family[[2]])

  if(str_detect(fam[[1]], "Negative Binomial")) {
    fam[[1]] <- "negbinom"
  }

  scale <- match.arg(scale, c("pearson", "deviance"))



  ## Can return model info if necessary

  df.r <- fit$df.residual
  dispersion <- summary(fit)$dispersion

  ## Goodness of fit
  GOF <- matrix(NA, nrow = 8, ncol = 3)
  GOF[1:4, 1] <- df.r

  ## Deviance
  deviance <- fit$deviance
  scale_deviance <- deviance/dispersion
  pearson <- sum(residuals(fit, type = scale)^2)
  scale_pearson <- pearson/dispersion
  GOF[1:4, 2] <- c(deviance, scale_deviance, pearson, scale_pearson)
  GOF[1:4, 3] <- GOF[1:4, 2]/GOF[1:4, 1]

  ## LL and AIC
  GOF[5, 2] <- logLik(fit)
  k <- length(fit$coefficients); n <- length(fit$residuals)
  aic <- 2*k - 2*logLik(fit) %>% as.numeric # Given AIC is incorrect
  GOF[6, 2] <- aic
  GOF[7, 2] <- aic + (2*k^2 + 2*k)/(n - k - 1)
  GOF[8, 2] <- aic - 2*k + 2*log(n)

  rownames(GOF) <- c("Deviance", "Scaled Deviance",
                     "Pearson Chi-Square", "Scaled Perason X2",
                     "Log Likelihood", "AIC", "AICc", "BIC")
  colnames(GOF) <- c("DF", "Value", "Value/DF")

  #### MLE Estimates (pretty gross way to do this)
  conf_int <- suppressMessages(confint(fit) %>% unlist)
  colnames(conf_int) <- paste0("Wald CI ", colnames(conf_int))
  mle_mat <- cbind(summary(fit)$coefficients, conf_int)
  mle_mat <- cbind(mle_mat, mle_mat[, 3]^2)
  colnames(mle_mat)[7] <- "Wald Chi-Square"
  mle_mat <- mle_mat[, c(1, 2, 5, 6, 3, 7, 4)]

  ## add scale term
  if(fam[[1]] == "negbinom") {
    scl <- 1/fit$theta
    scl.se <- 1/fit$SE.theta
  } else {
    scl <- sqrt(dispersion)
    scl.se <- 0
  }
  z <- qnorm(0.975)
  mle_mat <- rbind(mle_mat, c(scl, scl.se, scl - z*scl.se, scl + z*scl.se,
                              NA, NA, NA))
  rownames(mle_mat)[3] <- ifelse(fam[[1]] == "negbinom",
                                 "Dispersion", "Scale")

  output <- list("family" = fam[[1]],
                 "link" = fam[[2]],
                 "goodness_of_fit" = GOF, "mle" = mle_mat)

  ## Assign class for printing (see print.sas_table)
  output <- lapply(output, function(x) {
    class(x) <- c("sas_table", class(x))
    if(is.numeric(x)) x <- round(x, 6)
    x
  })

  return(output)
}





