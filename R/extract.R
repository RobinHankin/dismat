setMethod("[", signature(x="dismat_rd_ci",i="disindex",j="ANY",drop="ANY"),
          function(x,i,j,drop=TRUE){
	      stopifnot(identical(hash(x),hash(i)))
              if(missing(j)){
                  out <- elements(x)[values(i),,drop=drop]
              } else {
                  out <- elements(x)[values(i), j, drop=drop]
              }
              out <- dismat_rd_ci(out)
              if(drop){
                  return(drop(out))
              } else {
                  return(out)
              }
          } )


setMethod("[", signature(x="dismat_rd_ci",i="disord",j="ANY",drop="ANY"),
          function(x,i,j,drop=TRUE){
	  stopifnot(is.logical(i)) # NB:  m <- matrix(1:35,5,7); compare m[c(1,0,0,1,0),] with m[c(T,F,F,T,F),]
	  if(missing(j)){
	    out <- x[which(i), ,drop=drop]  # which(i) is a disindex object
	  } else {
  	    out <- x[which(i),j,drop=drop]
	  }
	  return(out)
	  } )


setMethod("[", signature(x="dismat_rd_ci",i="ANY",j="ANY",drop="ANY"),
          function(x,i,j,drop=TRUE){
              stop("\nfor objects of class 'dismat_rd_ci', the [non-missing, non-empty] row accessor
	      must be of either class 'disindex' or Boolean of class 'disord'; and column accessor
	      must be of class index")
          } )

