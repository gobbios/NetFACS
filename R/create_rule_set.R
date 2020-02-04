#' Take vector of elements and calculate probabilities of elements and combinations occurring
#' 
#' Function underlying the netfacs() function; takes the observed elements, applies the apriori() function of the arules package, and create dataframe with all observed probabilities
#' 
#' 
#' @param elements list with vectors for all elements observed together at each event
#' @param maxlen maximum size of combinations to be considered
#'
#' @return Function returns a dataframe with observed probabilities for each combination in the dataset
#' 
#' @export


create.rule.set <- function(elements, maxlen){
  library(arules)
  ar <- apriori(elements, parameter = list(supp=1/length(elements), conf=1/length(elements), maxlen=maxlen), control = list(verbose = F))
  
  rs= as(ar, "data.frame") 
  rs$rules = gsub("[^A-Za-z0-9, ]","",rs$rules)
  rs$rules = gsub(" ",",",rs$rules)
  xrs = unlist(rs$rules)
  x.elements = lapply(xrs, function(x){
    unlist(strsplit(x, split = ",", fixed = T))
  })
  x.elements = lapply(x.elements, function(x){sort(x)})
  x.elements = lapply(x.elements, function(x){x[x!='']})
  rs$combination.size = sapply(x.elements,FUN = length)
  rs$combination = sapply(x.elements, function(x){paste(x, collapse = '_')})
  rs = rs[,c(7,2:6)]
  colnames(rs)[2]='observed.probability'
  rs$confidence = NULL
  rs$lift = NULL
  rs = rs[!duplicated(rs$combination),]
  return(rs)
}