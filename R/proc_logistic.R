#' ((very) limited) Emulation of SAS proc_logistic
#'
#' @param fit Should be a glm model fit with family binomial("logit")
#'
#' @param scale Matching SAS argument, uses either pearson or deviance for
#' estimation of dispersion parameter, takes either "pearson" or "deviance"
#'
#' @param tables Optional length one character vector to indicate output. Enter as
#' a single string (rather than a vector). Options include any of
#' opt = "waldci covb lackfit", in any order
#'
#' @examples
#' fit <- glm(cbind(Mort, Pop-Mort) ~ Age_Gp,
#'            family = binomial(link = "logit"), data = ACHD)
#'
#' fit <- glm(cbind(MJ, Total-MJ) ~ AL + CG, data = maryjane,
#'            family = binomial)
#' @export

proc_logistic <- function(fit, scale = "pearson", tables = "waldci") {

  if(!("glm" %in% class(fit))) {
    stop("must submit glm to proc_logistic")
  }

  fam <- list("family" = fit$family[[1]],
              "link" = fit$family[[2]])

  if(!str_detect(fam[[1]], "binomial")) {
    stop("proc_logistic requries binomial glm")
  }

  if(!str_detect(fam[[2]], "logit")) {
    stop("proc_logistic requires logit link function")
  }

  ## Handle tables
  tabs <- str_split(tables, " ") %>% unlist %>% str_to_lower
  avail_tabs <- c("covb", "waldci", "lackfit")
  tab_to_add <- sum(tabs %in% avail_tabs)
  tabs <- tabs[tabs %in% avail_tabs]

  ## Assumes 11 components of what was used in class
  output <- vector("list", 7 + tab_to_add)

  ## Response profile
  total_obs <- fit$prior.weights
  percent_obs <- fit$y
  total_success <- (fit$y * fit$prior.weights) %>% sum %>% round
  total_failure <- (sum(total_obs) - total_success)
  i <- 1
  output[[i]] <- matrix(c(1, 0, total_success, total_failure), nrow = 2,
                        dimnames = list(c("1", "2"), c("Surv", "Total Frequency")))
  names(output)[i] <- "Response Profile"
  i <- i + 1
  ## Deviance and Goodness of Fit
  df.r <- fit$df.residual
  deviance <- fit$deviance
  pearson <- sum(residuals(fit, type = scale)^2)
  dev_chisq <- pchisq(deviance/df.r, df = df.r, lower.tail = FALSE)
  pearson_chisq <- pchisq(pearson/df.r, df = df.r, lower.tail = FALSE)
  output[[i]] <- matrix(c(deviance, pearson, df.r, df.r,
                          deviance/df.r, pearson/df.r,
                          dev_chisq, pearson_chisq), ncol = 4,
                        dimnames = list(c("Deviance", "Pearson"),
                                        c("Value", "DF", "Value/DF", "Pr > ChiSq")))
  names(output)[i] <- "Deviance and Goodness of Fit"
  i <- i + 1

  ## Model fit statistics (need intercept only)
  logl <- -2*logLik(fit)[1]
  k <- length(fit$coefficients); n <- length(fit$residuals)
  aic <- 2*k - 2*logLik(fit) %>% as.numeric # Given AIC is incorrect
  intercept_only <- glm(fit$model[[1]] ~ 1, family = "binomial")
  aic_int <- AIC(intercept_only)
  logl_int <- -2*logLik(intercept_only)[1]
  output[[i]] <- matrix(c(aic_int, logl_int, aic, logl), nrow = 2,
                        dimnames = list(c("AIC", "-2 Log L"),
                                        c("Intercept Only", "Intercept and Covariates")))
  names(output)[i] <- "Model Fit Statistics"
  i <- i + 1

  ## Analysis of MLE
  output[[i]] <- coef(summary(fit)) %>% zapsmall
  names(output)[i] <- "Analysis of MLE"
  i <- i + 1

  ## Odds ratio estimate
  z <- qnorm(0.975)
  orest <- apply(coef(summary(fit))[2:nrow(coef(summary(fit))), , drop = FALSE], 1, function(x) {
    vv <- c(x[1], x[1] - z*x[2], x[1] + z*x[2])
  })
  orest <- exp(t(orest))
  colnames(orest) <- c("Point Estimate", "95%L", "95%U")
  output[[i]] <- orest
  names(output)[i] <- "Odds Ratio Estimate"
  i <- i + 1


  ## Association of Predicted Probabilities and Obs Responses

  ##### THESE FUNCTIONS TAKEN FROM OII PACKAGE (which has failed dependency)
  # by Scott Hale and Grant Blank
  # Oxford Internet Institute
  # University of Oxford
  tied.first <- function(tab){
    mult<-function(r,c) {
      lr <- tab[(r.x == r) & (c.x > c)]
      tab[r,c]*sum(lr)
    }

    r.x <- row(tab)
    c.x <- col(tab)

    tmp<-(mapply(mult, r = r.x, c = c.x))
    sum(as.numeric(tmp)) #Make sure we have doubles to avoid integer overflow
  }

  resp <- fit$model[[1]]
  tied_pairs <- tied.first(resp)
  pclist <- ConDisPairs(fit$model[[1]])[3:4]  %>% unlist
  total_pairs <- tied_pairs + sum(pclist)
  per_con <- 100*(pclist[1] / total_pairs)
  per_dis <- 100*(pclist[2] / total_pairs)
  per_tie <- 100*(tied_pairs / total_pairs)
  con_dis_vec <- c("Percent Concordant" = per_con,
                   "Percent Discordant" = per_dis,
                   "Percent Tied" = per_tie,
                   "Pairs" = total_pairs) %>% round(2)

  stat_vec <- c("Somers' D" = SomersDelta(fit$model[[1]]),
                "Gamma" = GoodmanKruskalGamma(fit$model[[1]]),
                "Tau-a" = NA,
                "Cstat (error?)" = Cstat(fit))
  output[[i]] <- c(con_dis_vec, stat_vec) %>% as.matrix
  names(output)[i] <- "Association of Predicted Probabilities and Observed Responses"
  i <- i + 1


  ## Par est w/ wald ci
  if("waldci" %in% tabs) {
    z <- qnorm(0.975)
    orest <- apply(coef(summary(fit)), 1, function(x) {
      vv <- c(x[1], x[1] - z*x[2], x[1] + z*x[2])
    }) %>% t()
    colnames(orest) <- c("Point Estimate", "95%L", "95%U")
    output[[i]] <- orest
    names(output)[i] <- "Parameter Estimates and Walkd Confidence Intervals"
    i <- i + 1
  }

  ## Estimated covariance matrix
  if("covb" %in% tabs) {
    output[[i]] <- vcov(fit)
    names(output)[i] <- "Estimated Covariance Matrix"
    i <- i + 1
  }

  ## Hosmer Lemeshow
  if("lackfit" %in% tabs){
    obsvals <- fit$model[[1]]
    total <- rowSums(obsvals)
    obs_suc <- obsvals[, 1]
    exp_suc <- total * fit$fitted.values
    obs_fail <- obsvals[, 2]
    exp_fail <- total - exp_suc
    hl_mat <- cbind(total, obs_suc, exp_suc, obs_fail, exp_fail)
    output[[i]] <- hl_mat
    names(output)[i] <- "Partition for Hosmer and Lemeshow Test"
  }


  ## Assign class for printing (see print.sas_table)
  output <- lapply(output, function(x) {
    class(x) <- c("sas_table", class(x))
    if(is.numeric(x)) x <- round(x, 6)
    x
  })

  return(output)

}

