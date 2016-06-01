#' Initialize the fish class
#'
#' @return no return value
#'
initFishClass <- function(){
  methods::setClass("fishObject", representation(ytop="list", ybtm="list", xpos="list",
                                        col="character", timepoints="numeric",
                                        frac.table="matrix", parents="numeric",
                                        nest.level="numeric", inner.space="list",
                                        outer.space="numeric"))
}

##------------------------------------------------------------------------
#' Validate some key assumptions about the fish object's data
#'
#' @param frac.table A numeric matrix containing tumor fraction estimates for all clones at all timepoints
#' @param parents An integer vector specifying parental relationships between clones
#' @param nest.levels An integer vector specifying how deeply a given clone is nested in the overall hierarchy
#'
#' @return No return value - stops execution with an error if invalid inputs are detected
#'
validateInputs <- function(frac.table, parents, nest.levels){
  clones =  1:dim(frac.table)[1]
  timepts = 1:dim(frac.table)[2]

  ##no cluster can go from present to absent and then back
  for(clone in clones){
    started=FALSE
    ended=FALSE
    for(timept in timepts){
      if(frac.table[clone,timept] > 0){
        if(started & ended){
          stop(paste("Clone",clone,"goes from present to absent (fraction=0) and then back to present. Values for which the cluster was present must be non-zero"))
        }
        started=TRUE
      } else {
        if(started){
          ended=TRUE
        }
      }
    }
  }

  ##clusters of entirely zero get a warning
  if(length(which(rowSums(frac.table) == 0)) > 0){
    print("WARNING: at least one cluster has fraction zero at all timepoints. It will not be displayed")
  }

  ##make sure that each timepoint doesn't sum to more than the parental value at a given nest level (or 100% for level 0)
  for(timept in timepts){
    for(i in unique(nest.levels)){
      neighbors = which(nest.levels==i)
      if(sum(frac.table[neighbors,timept]) > 100){
        stop(paste("clones with same nest level cannot have values that sum to more than 100%: Problem is in clusters ",paste(neighbors,collapse=",")))
      }
    }

    for(i in unique(parents)){
      if(i > 0){
        neighbors = which(parents==i)
        if(sum(frac.table[neighbors,timept]) > frac.table[parents[neighbors[1]],timept]){
          stop(paste("clones with same parent cannot have values that sum to more than the percentage of the parent: Problem is in clusters ",paste(neighbors,collapse=","),"at timepoint",timept))
        }
      }
    }
  }
}


##------------------------------------------------------------------------
#' Given the a list representing the parents of each clone, and the number specifying which clone to test, returns how deeply it is nested
#'
#' @param parents An integer vector specifying parental relationships between clones
#' @param x The integer specifying which subclone to calculate nest level for
#'
#' @return An integer representing how deeply this subclone is nested
#' @seealso getAllNestLevels
#'
getNestLevel <- function(parents,x){
  #sanity checks
  if(x > length(parents)){
    stop(paste("cannot have a parent that does not exist in list. parent =",x,", length(parents) =",length(parents)))
  }
  if(x < 0){
    stop("cannot have a value in parents of less than zero")
  }

  if(parents[x] == 0){
    return(0)
  } else {
    return(getNestLevel(parents,parents[x])+1)
  }
}


##------------------------------------------------------------------------
#' Given the a list representing the parents of each clone, return a vector specifying how deeply each clone is nested
#'
#' @param parents An integer vector specifying parental relationships between clones
#'
#' @return An integer vector representing how deeply each subclone is nested
#' @seealso getNestLevel
#'
getAllNestLevels <- function(parents){
  nest.level=c()
  for(i in 1:length(parents)){
    nest.level=c(nest.level, getNestLevel(parents,i))
  }
  return(nest.level)
}

##------------------------------------------------------------------------
#' Create a fish object after doing some input validation and data munging
#'
#' @param frac.table A numeric matrix containing tumor fraction estimates for all clones at all timepoints
#' @param parents An integer vector specifying parental relationships between clones
#' @param timepoints An numeric vector specifying the timepoints for each column of the matrix
#' @param col A vector of colors to use when plotting each clone
#'
#' @return A fish object with the relevant slots filled
#' @export
#'
#' @examples
#' timepoints=c(0,30,75,150)
#' frac.table = matrix(
#'     c(100, 45, 00, 00,
#'        02, 00, 00, 00,
#'        02, 00, 02, 01,
#'        98, 00, 95, 40),
#'     ncol=length(timepoints))
#' parents = c(0,1,1,3)
#' fish = createFishObject(frac.table,parents,timepoints=timepoints)
#' 
createFishObject <- function(frac.table,parents,timepoints=NULL,col=NULL){

  nest.levels = getAllNestLevels(parents)

  rownames(frac.table)=seq(1:dim(frac.table)[1])

  #default timepoints are just 1,2,...,numColumns
  if(is.null(timepoints)){
    colnames(frac.table)=c(1:ncol(frac.table))
  } else {
    if(!(is.numeric(timepoints) & is.vector(timepoints))){
      stop("ERROR: timepoints must be a numeric vector")
    }
    colnames(frac.table)=timepoints
  }

  #sanity checks on input data
  validateInputs(frac.table, parents, nest.levels)

  #create the object
  fish = new("fishObject", ytop=list(), ybtm=list(), col=c("NULL"),
    timepoints=as.numeric(colnames(frac.table)), frac.table=frac.table,
    parents=parents, nest.level=nest.levels, inner.space=list(), outer.space=c(0))

  #set default colors to start
  fish = setCol(fish,col)

  return(fish)
}

##------------------------------------------------------------------------
## put here solely to prevent R CMD check from giving the following note:
## setCol: no visible binding for global variable ‘frac.table’
globalVariables(c("frac.table"))

#' Attach the colors for plotting to the fish object, ensuring that they are valid. If no color vector is provided, a default color scheme is used.
#'
#' @param fish A fish object with the frac.table slot filled in
#' @param col A vector of colors with the same length as the number of clones (number of rows in the frac table)
#'
#' @return The fish object with the colors stored in the appropriate slot
#' @export
#' @examples
#' \dontrun{
#' setCol(fish)
#'
#' fish = setCol(fish, c("red","yellow","blue","green"))
#' }
#' 
setCol <- function(fish,col=NULL){
  nclones = nrow(fish@frac.table)
  if(!(exists("nclones"))){
      print("WARNING: Could not set colors, as the number of rows in the frac.table slot of the fish object could not be calculated")
      return(fish)
  }
  
  if(is.null(col)){
    ##print("Using default color scheme. Use the setCol() function to change this.")
    ##use default color scheme
    col = rev(c("#00008F", "#0000FF", "#0070FF", "#00DFFF", "#50FFAF", "#BFFF40", "#FFCF00", "#FF6000", "#EF0000", "#888888"))

    ##check length
    if(length(col) < nclones){
      print("WARNING: default color scheme only includes 10 colors, but",nclones,"are needed to color each clone uniquely. Use the setCol() function to add a color scheme")
    } else {
      fish@col=col[1:nclones]
    }
    return(fish)
  }

  ##else colors provided, check them for sanity
  if(length(col) != nrow(frac.table)){
    stop(paste("ERROR: number of colors provided must be equal to the number of clones (",nclones,")",sep=""))
  }
  fish@col=col
  return(fish)
}
