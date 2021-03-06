#' Calculates various measures of population and community variability 
#' 
#' Calculates various measures of population and community variability 
#' 
#' @param X A matrix with counts or densities arranged in species by time step
#' @param type If \code{pop}, calculate CVpop2. If \code{com}, calculate CVcom2. If \code{comip},
#' calculate CVcomip2. See the vignette for definitions of these quantities.
#'
#' @return \code{cv2} returns the value of population or community variability.
#' 
#' @author Lei Zhao, \email{lei.zhao@@cau.edu.cn}; Daniel Reuman, \email{reuman@@ku.edu}; Shaopeng Wang, \email{shaopeng.wang@@pku.edu.cn}
#' 
#' @seealso \code{\link{vreq_classic}}, \code{\link{vreq_LdM}}, \code{\link{cv2f}}, \code{browseVignettes("tsvr")}
#' 
#' @references 
#' Zhao et al, (In prep) Decomposition of the variance ratio illuminates timescale-specific
#' population and community variability.
#' 
#' @examples
#' X<-matrix(runif(200,1,100), 10, 20)
#' ans<-cv2(X, type="com")
#' 
#' @export
#' @importFrom stats cov

cv2 <- function(X, type){
  errcheck_data(X,"cv2")
  
  meanSp <- rowMeans(X)
  covSp <- stats::cov(t(X))
  mean.comm <- sum(meanSp)
  
  if(type=="com")
  {     
    var.comm <- sum(covSp)
    cv2 <- var.comm/mean.comm^2
    return(cv2)
  }
  if (type=="comip")
  {               
    var.comip <- sum(diag(covSp))
    cv2<-var.comip/mean.comm^2
    return(cv2)
  }
  if (type=="pop")
  {
    var.pop<-(sum(sqrt(diag(covSp))))^2
    cv2<-var.pop/mean.comm^2
    return(cv2)
  }
  stop("Error in cv2: type must be com, comip, or pop")
}

