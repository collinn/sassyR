% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/proc_logistic.R
\name{proc_logistic}
\alias{proc_logistic}
\title{((very) limited) Emulation of SAS proc_logistic}
\usage{
proc_logistic(fit, scale = "pearson", tables = "waldci")
}
\arguments{
\item{fit}{Should be a glm model fit with family binomial("logit")}

\item{scale}{Matching SAS argument, uses either pearson or deviance for
estimation of dispersion parameter, takes either "pearson" or "deviance"}

\item{tables}{Optional length one character vector to indicate output. Enter as
a single string (rather than a vector). Options include any of
opt = "waldci covb lackfit", in any order}
}
\description{
((very) limited) Emulation of SAS proc_logistic
}
\examples{
fit <- glm(cbind(Mort, Pop-Mort) ~ Age_Gp,
           family = binomial(link = "logit"), data = ACHD)

proc_logistic(fit, tables = "waldci covb lackfit")

fit2 <- glm(cbind(MJ, Total-MJ) ~ AL + CG, data = maryjane,
           family = binomial)

proc_logistic(fit2, tables = "waldci")
}
