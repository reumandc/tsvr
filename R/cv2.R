#' Calculate the variability partitioning 
#' 
#' This function is used to calculate the variability partitioning.
#' 
#' @param X a matrix with counts or densities arranged in species (or other scale) by years.
#' @param type If \code{"pop"} (default), calculate population CV^2. If \code{"com"}, calculate community CV^2.
#'
#' @return \code{cv2} return the value of population or community variability.
#' 
#' @author Lei Zhao, \email{leizhao@@ku.edu}; Daniel Reuman, \email{reuman@@ku.edu}; Shaopeng Wang, \email{shaopeng.wang@idiv.de}
#' @references (Our draft)
#' 
#' @examples
#' X<-matrix(runif(200,1,100), 10, 20)
#' rownames(X)<-letters[1:10]
#' colnames(X)<-1991:2010
#' 
#' ans<-cv2(X, type="com")
#' 
#' @export

cv2 <- function(X, type="pop"){
  
  X<-X[rowSums(X)>0, ]  # delete species whose counts are zeros across all the years
  
  meanSp <- rowMeans(X)
  covSp <- cov(t(X))
  mean.comm <- sum(meanSp)
  
  if(type=="com"){     # Gamma variability 
    var.comm <- sum(covSp)
    cv2 <- var.comm/mean.comm^2
  }else{               # Alpha variability
    var.sp <- sum(diag(covSp))
    cv2<-var.sp/mean.comm^2
    }

  return(cv2)
}

