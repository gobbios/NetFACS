#' Calculate reciprocity of probabilities that two elements appear together
#' 
#' For all dyadic combinations that ever appear, this function calculates how reciprocal the conditional probabilities (i.e. probability of A given B, and B given A) of the two elements are. Combinations that are highly reciprocal indicate that the two elements always occur together and might represent a fixed combination, while low reciprocity might indicate that one element is an extension of the other. Values approaching -1 indicate that one element is strongly dependent on the other, but this is not reciprocated; values around 0 indicate that neither is conditional on the other; and values approaching 1 indicate that both values are conditional on each other. 
#' If P[A|B] is the larger conditional probability, the reciprocity is calculated as reciprocity = ((P[B|A]/P[A|B]) - (P[A|B] - P[B|A])) * P[A|B].
#' 
#' 
#' @param netfacs.data object resulting from netfacs() function
#'
#' @return Function returns a dataframe that includes all dyadic combinations and their reciprocity values
#' 
#' @export
#'
#' @examples
#' ### how do angry facial expressions differ from non-angry ones?
#' data(emotions_set)
#' angry.face = netfacs(data = emotions_set[[1]],
#'  condition = emotions_set[[2]]$emotion,
#'  test.condition = 'anger',
#'  null.condition = NULL,
#'  duration = NULL, 
#'  ran.trials = 100,
#'  control = NULL,
#'  random.level = NULL,
#'  combination.size = 5)
#'  
#'  netfacs.reciprocity(angry.face)


netfacs.reciprocity <- function(netfacs.data){
  library(arules)

  rs = as(netfacs.data$arules, "data.frame") 
  rs$rules = gsub("[^A-Za-z0-9, ]","",rs$rules)
  rs$rules = gsub(" ",",",rs$rules)
  xrs = unlist(rs$rules)
  x.elements = lapply(xrs, function(x){
    unlist(strsplit(x, split = ",", fixed = T))
  })
  x.elements = lapply(x.elements, function(x){x[x!='']})
  rs$combination.size = sapply(x.elements,FUN = length)
  x.elements = x.elements[rs$combination.size==2]
  rs = rs[rs$combination.size == 2,]
  rs$element1 = sapply(x.elements, function(x){x[1]})
  rs$element2 = sapply(x.elements, function(x){x[2]})
  rs$combination = sapply(x.elements, function(x){paste(sort(x), collapse = '_')})
  reci = rs
  
  reci = lapply(unique(reci$combination), function(x){
    xx = reci[reci$combination == x,]
    xx.reci = xx$confidence
    reci.res = ((min(xx.reci)/max(xx.reci)) - (max(xx.reci)-min(xx.reci))) * max(xx.reci)
    xx.reci = c(combination = x, reciprocity = reci.res, minimum.direction = paste(unique(c(xx$element2[xx$confidence==min(xx.reci)], xx$element1[xx$confidence==min(xx.reci)])), collapse = '|'), minimum.conditional = min(xx.reci), maximum.direction = paste(unique(c(xx$element2[xx$confidence==max(xx.reci)], xx$element1[xx$confidence==max(xx.reci)])), collapse = '|'), maximum.conditional = max(xx.reci), count = reci$count[reci$combination == x][1])
  })
  reci = do.call(rbind, reci)
  reci = data.frame(reci)
  reci$reciprocity = round(as.numeric(as.character(reci$reciprocity)), 2)
  reci$minimum.conditional = round(as.numeric(as.character(reci$minimum.conditional)), 2)
  reci$maximum.conditional = round(as.numeric(as.character(reci$maximum.conditional)), 2)
  reci = reci[order(reci$reciprocity),]

  
  return(reci)
}