#' Tests how much each element increases the specificity of all combinations it is in
#' 
#' The function takes all elements and dyadic combinations of elements in a netfacs object, goes through all combinations these elements are in, and compares the specificity (strength with which the combination identifies the test condition) of all combinations with the element and the same combinations without the element, to test how much specificity the element adds when added to a signal. Only works for netfacs objects based on comparison between conditions.
#' 
#' 
#' @param netfacs.data object resulting from netfacs() function
#'
#' @return Function returns a dataframe that includes all elements and first-order combinations that occur at all, the number of combinations that element is in, and how much adding this element to a combination adds on average to its specificity
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
#'  element.specificity(angry.face)



element.specificity <- function(netfacs.data){
  
  data = netfacs.data$result
  if(!'specificity'%in%colnames(data)){return(print('Results are not part of comparison and do not reveal anything about specificity.'))}
  
  xx = data$combination[data$combination.size %in% c(1,2) & data$observed.probability>0]
  element.specificity = data.frame(element = xx, number.combinations = 0, specificity.increase = 0)
  rownames(element.specificity) = xx
  all.combinations = lapply(data$combination, function(x){unlist(strsplit(x, split = '_', fixed = T))})
  
  ii = lapply(rownames(element.specificity), function(i){
    x.i = unlist(strsplit(as.character(i), split = '_', fixed = T))
    elements.with = data[unlist(lapply(all.combinations, function(z) length(intersect(x.i, unlist(z)))==length(x.i))),]
    combinations.with = all.combinations[unlist(lapply(all.combinations, function(z) length(intersect(x.i, unlist(z)))==length(x.i)))]
    return(length(combinations.with))
  })
  
  element.specificity$number.combinations=unlist(ii)
  
  ii = lapply(rownames(element.specificity), function(i){
    x.i = unlist(strsplit(as.character(i), split = '_', fixed = T))
    elements.with = data[unlist(lapply(all.combinations, function(z) length(intersect(x.i, unlist(z)))==length(x.i))),]
    combinations.with = all.combinations[unlist(lapply(all.combinations, function(z) length(intersect(x.i, unlist(z)))==length(x.i)))]
    specificity.with = elements.with$specificity
    combinations.without = sapply(combinations.with, function(x){
      xx = x[!x%in%x.i]
      if(length(xx)>1){xx=paste(xx, collapse = '_')}
      return(xx)
    })
    specificity.without = data$specificity[data$combination%in%unlist(combinations.without)]
    return(mean(specificity.with) - mean(specificity.without))
  })
  
  element.specificity$specificity.increase=unlist(ii)
  element.specificity = element.specificity[order(-1*element.specificity$specificity.increase),]
  
  
  element.specificity$specificity.increase = round(element.specificity$specificity.increase, 3)
  combinations = grepl(element.specificity$element, pattern = '_', fixed = T)
  
  element.specificity = list(element = element.specificity[!combinations,], dyad = element.specificity[combinations,])
  
  return(element.specificity)
  
}
