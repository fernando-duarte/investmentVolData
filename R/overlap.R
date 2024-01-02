#' Transformation of Britten-Jones, Neuberger and Nolte (2011) for
#' regressions with overlapping observations
#'
#' From Britten-Jones, Neuberger and Nolte (2011):
#' Let \eqn{r} denote the \eqn{T \times 1} vector of one period \eqn{\log}
#' returns and \eqn{A} the \eqn{(T-k+1) \times T} matrix that has entries
#' \eqn{a_{i j}=1} if \eqn{i \leq j \leq i+k-1} and 0's otherwise,
#' with \eqn{i=1, \ldots, T-k+1}. Thus, \eqn{A} is the transformation
#' matrix with 1's on the main diagonal and the first \eqn{k-1} right
#' off-diagonals and 0's otherwise. Hence, \eqn{A r} is the
#' \eqn{(T-k+1) \times 1} vector of \eqn{k} period \eqn{\log} returns.
#' \eqn{X} denotes the \eqn{(T-k+1) \times \ell} matrix of
#' explanatory variables (with the first column of \eqn{X} consisting of 1's).
#' We consider the following (predictive) linear regression setup with overlapping returns:
#' \deqn{
#'   Ar = X \beta + u,
#' }
#' in which \eqn{u} denotes the \eqn{(T-k+1) \times 1} error term vector.
#' The OLS parameter estimate of \eqn{\beta} is given by:
#' \deqn{
#'    \hat{\beta}=\left(X^{\prime} X\right)^{-1} X^{\prime} A r .
#' }
#' It can be rewritten as:
#' \deqn{
#'    \hat{\beta}=\left(X^{\prime} X\right)^{-1}\left(A^{\prime} X\right)^{\prime} r,
#' }
#' which shows that the OLS estimate of \eqn{\beta} can be rewritten in terms of
#' the original non-overlapping one period returns.
#'
#' Moreover, \eqn{\hat{\beta}} can be obtained as a standard OLS estimator from an
#' associated transformed regression:
#' \deqn{
#'    r = \tilde{X} \beta + \tilde{u},
#' }
#' in which \eqn{\tilde{X}} is the \eqn{T \times \ell} matrix of transformed
#' explanatory variables given by:
#' \deqn{
#'    \tilde{X} \equiv A^{\prime} X\left(X^{\prime} A A^{\prime} X\right)^{-1} X^{\prime} X,
#' }
#' and \eqn{\tilde{u}} is the \eqn{T \times 1} error term vector of this transformed
#' regression.
#' For example, if \eqn{T = 5, K = 3}:
#' \deqn{
#' A =
#' \begin{bmatrix}
#     1 & 1 & 1 & 0 & 0 \\
#     0 & 1 & 1 & 1 & 0 \\
#     0 & 0 & 1 & 1 & 1
#' \end{bmatrix}
#' }
#' \eqn{Ar} is a vector of multi-period returns. Instead of using
#' \deqn{
#'    A*r = X*b + u,
#'}
#' Britten-Jones, Neuberger and Nolte (2011) advise to use
#' \deqn{
#'    r = XX*bb + uu,
#' }
#' where
#' \deqn{
#'    XX = A'*X*inv(X'*A*A*'X)*X'*X.
#' }
#' Then \eqn{bb} and \eqn{b} are the same estimators, but the autocorrelations in
#' errors due to overlapping observations are muted.
#'
#' `transformX()` computes transformed explanatory variables for regression with
#' overlapping observations to implement the test statistics in
#' Britten-Jones, Neuberger and Nolte (2011).
#'
#' @param X Matrix with explanatory variables
#' @param k Number of periods over which returns are computed for the dependent variable
#' @export transformX
#' @seealso [overlap_matA()]
#' @references [Britten-Jones, Neuberger and Nolte (2011)](https://doi.org/10.1111/j.1468-5957.2011.02244.x)

transformX <- function(X, k){

  T <- nrow(X)
  Xlag <- X[1:(T-k+1),]
  AA <- overlap_matA(T,k)

  ( ( t(AA) %*% Xlag )  %*% solve(t(Xlag) %*% ( AA %*% t(AA) ) %*% Xlag) ) %*% ( t(Xlag) %*% Xlag )
}

#' `overlap_matA()` transformation matrix for regression with overlapping observations
#'
#' @param T Number of observations in the regression
#' @param k Number of periods over which returns are computed for the dependent variable
#' @export overlap_matA
#' @seealso [transformX()]
#' @references [Britten-Jones, Neuberger and Nolte (2011)](https://doi.org/10.1111/j.1468-5957.2011.02244.x)
overlap_matA <- function(T,k){
  tmp <- diag(1, T-k+1,T)
  A   <- diag(1, T-k+1,T)
  for (n in 2:T){
    A[,n] <-  rowSums(tmp[,max(n-k+1,1):n])
  }
  return(A)
}


