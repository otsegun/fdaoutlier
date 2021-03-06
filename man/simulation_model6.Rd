% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/simulation_models.R
\name{simulation_model6}
\alias{simulation_model6}
\title{Convenience function for generating functional data}
\usage{
simulation_model6(
  n = 100,
  p = 50,
  outlier_rate = 0.1,
  mu = 4,
  q = 1.8,
  kprob = 0.5,
  a = 0.25,
  b = 0.75,
  cov_alpha = 1,
  cov_beta = 1,
  cov_nu = 1,
  pi_coeff = 0.02,
  exp_pow = 2,
  exp_coeff = 50,
  deterministic = TRUE,
  seed = NULL,
  plot = F,
  plot_title = "Simulation Model 6",
  title_cex = 1.5,
  show_legend = T,
  ylabel = "",
  xlabel = "gridpoints"
)
}
\arguments{
\item{n}{The number of curves to generate. Set to \eqn{100} by default.}

\item{p}{The number of evaluation points of the curves. Curves are usually generated
over the interval \eqn{[0, 1]}. Set to \eqn{50} by default.}

\item{outlier_rate}{A value between \eqn{[0, 1]} indicating the percentage of outliers.
A value of \eqn{0.06} indicates about \eqn{6\%} of the observations will be outliers
depending on whether the parameter \code{deterministic} is \code{TRUE} or not.
Set to \eqn{0.05} by default.}

\item{mu}{The mean value of the functions in the main and contamination model.
Set to \code{4} by default.}

\item{q}{The constant term \eqn{q} in the contamination model. Set to \eqn{1.8}
by default.}

\item{kprob}{The probability \eqn{P(u = 1)}. Set to \eqn{0.5} by default.}

\item{a, b}{Values specifying the interval of from which \eqn{v} in the
contamination model is drawn. Set to \eqn{0.25} and \eqn{0.75} respectively.}

\item{cov_alpha}{A value indicating the coefficient of the exponential function
of the covariance matrix, i.e., the \eqn{\alpha} in the covariance function.
 Set to \eqn{1} by default.}

\item{cov_beta}{A value indicating the coefficient of the terms inside the exponential
function of the covariance matrix, i.e., the \eqn{\beta} in the covariance function.
Set to \eqn{1} by default.}

\item{cov_nu}{A value indicating the power to which to raise the terms inside the exponential
function of the covariance matrix, i.e., the \eqn{\nu} in the covariance function.
Set to \eqn{1} by default.}

\item{pi_coeff}{The constant \eqn{r} in the contamination model i.e., the coefficient of
of \eqn{pi}. Set to \eqn{0.02} by default.}

\item{exp_pow}{The constant \eqn{w} in the contamination model i.e., the power of the term in
the exponential function of the contamination model. Set to \eqn{2}.}

\item{exp_coeff}{The constant \eqn{z} in the contamination model i.e., the coefficient term in
the exponential function of the contamination model. Set to \eqn{50} by default.}

\item{deterministic}{A logical value. If \code{TRUE}, the function will always return
\code{round(n*outlier_rate)} outliers and consequently the number of outliers is always constant.
If \code{FALSE}, the number of outliers are determined using \code{n} Bernoulli trials with
probability \code{outlier_rate}, and consequently the number of outliers returned is random.
\code{TRUE} by default.}

\item{seed}{A seed to set for reproducibility. \code{NULL} by default in which case a seed
is not set.}

\item{plot}{A logical value indicating whether to plot data.}

\item{plot_title}{Title of plot if \code{plot} is \code{TRUE}}

\item{title_cex}{Numerical value indicating the size of the plot title relative to the device default.
Set to 1.5 by default. Ignored if \code{plot = FALSE}.}

\item{show_legend}{A logical indicating whether to add legend to plot if \code{plot = TRUE}.}

\item{ylabel}{The label of the y-axis. Set to \code{""} by default.}

\item{xlabel}{The label of the x-axis if \code{plot = TRUE}. Set to
\code{"gridpoints"} by default.}
}
\value{
A list containing:
\item{data}{a matrix of size \code{n} by \code{p} containing the simulated data set}
\item{true_outliers}{a vector of integers indicating the row index of the outliers in the
generated data.}
}
\description{
This models generates shape outliers that have a different shape for a portion of the domain.
The main model is of the form: \deqn{X_i(t) = \mu t + e_i(t),} with
contamination model of the form:
\deqn{X_i(t) = \mu t + (-1)^u q + (-1)^{(1-u)}(\frac{1}{\sqrt{r\pi}})\exp(-z(t-v)^w) + e_i(t)}
where: \eqn{t\in [0,1]}, \eqn{e_i(t)} is a Gaussian process with zero mean
and covariance function of the form:  \deqn{\gamma(s,t) = \alpha\exp(-\beta|t-s|^\nu),}
\eqn{u} follows Bernoulli distribution with probability \eqn{P(u = 1) = 0.5};
\eqn{q}, \eqn{r}, \eqn{z} and \eqn{w} are constants, and \eqn{v} follows
a Uniform distribution between an interval \eqn{[a, b]} and \eqn{m} is a constant.
Please see the simulation models vignette with
 \code{vignette("simulation_models", package = "fdaoutlier")} for more details.
}
\examples{
dt <- simulation_model6(n = 50, plot = TRUE)
dim(dt$data)
dt$true_outliers
}
