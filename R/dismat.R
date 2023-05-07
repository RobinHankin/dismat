setClass("dismat",contains = "VIRTUAL", slots = c(x="matrix",hash="character"))
setMethod("elements","dismat",function(x){x@x})  # no occurences of "@" below this line
setMethod("dim","dismat",function(x){dim(elements(x))})

setClass("dismat_rd_ci",
         slots    = c(x="matrix",rowhash="character"),
         contains = "dismat"
         )

setClass("dismat_ri_cd",
         slots    = c(x="matrix",colhash="character"),
         contains = "dismat"
         )

setClass("dismat_rd_cd",
         slots    = c(x="matrix",rowhash="character",colhash="character"),
         contains = "dismat"
         )

`dismat_rd_ci` <- function(M,hash=NULL){
  if(is.null(hash)){h <- hashcal(M)}
  new("dismat_ri_cd",x=M,hash=h)
}

`dismat_ri_cd` <- function(M){new("dismat_ri_cd",x=M,hash=hashcal(M))}

setMethod("show", "dismat_rd_ci", function(object){disord_rd_ci_show(object)})

`disord_rd_ci_show` <- function(x){
    cat("A dismat_rd_ci object with hash",hash(x), "and elements\n")
    jj <- x@x
    rownames(jj) <- rep("-",nrow(jj))
    print(jj)
    cat("\n(the rows are in some implementation-specific order)\n")
    return(invisible(x))
}

setGeneric("rowSums")
setMethod("rowSums",signature(x="dismat_rd_ci"),function(x, na.rm = FALSE, dims = 1L){
stop("not yet written")
} )

setMethod("apply",signature(X="dismat_rd_ci"),
          function (X, MARGIN, FUN, i_am_sure=FALSE, ..., simplify = TRUE){
              out <- apply(elements(X),MARGIN,FUN,...,simplify=simplify)
              if(MARGIN == 1){
                  return(disord(out,h=hash(X)))
                  } else {
                      if(!i_am_sure){
                          stop("\nArgument 'i_am_sure' is FALSE by default.
			  If you are _sure_ that FUN is disord-invariant, call apply() with argument i_am_sure=TRUE")
                          } else {
                              return(out)
                          }
                  }
          } )





setClass("dismat_symm",
         slots    = c(x="matrix",hash="character"),
         contains = "dismat"
         )
