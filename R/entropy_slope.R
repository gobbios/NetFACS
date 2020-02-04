#' Creates the entropy on different levels
#' 
#' Establishes the n-th order entropies for the netfacs object. 0-Level entropy is the overall possible information content if all elements were equally distributed; first-order entropy is the possible information content gives the probabilities of each element; second-order entropy is the information content given the one element of a combination is known; etc. Decreasing entropy indicates higher predictability/fewer degrees of freedom.
#' 
#' 
#' @param netfacs.data object resulting from netfacs() function
#' @param entropy.level numeric value, indicating the maximum order up to which entropy should be established
#' @param random if F, the entropy slope for the data is produced; if T, the mean entropy slope of a randomisation of the dataset is also shown for comparison
#'
#' @return Function returns dataframe with different orders and entropy level on this order. Entropy should decrease with increasing order, as fewer degrees of freedom exist (i.e., if only one element is known, predictability for the next element is low; if three elements are known, the next element is usually quite predictable). Expected random entropy is the average entropy of 10 datasets with the same number of events and keeping the number of elements per event the same as in the observed data. 
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
#'  entropy.slope(angry.face, entropy.level = 5)

entropy.slope <- function(netfacs.data, entropy.level = 5, random = F){
  library(arules)
  
  data = as.matrix(netfacs.data$used.data$data)
  data = data[netfacs.data$used.data$condition==netfacs.data$used.parameters$test.condition,]
  
  xx.slope=data.frame(order=0, entropy=0)
  # zero order entropy
  xx=data.frame(letter=1:ncol(data), freq=1/ncol(data))
  yy=lapply(xx$freq, function(x){
    y=x*log2(x)
    return(y)
  })
  xx.slope$entropy[1]=-1*sum(unlist(yy))
  if(isTRUE(random)){xx.slope$expected.entropy = xx.slope$entropy[1]}
  
  xx.s = nth.order.entropy(data = data, level = entropy.level)
  
  
  if(isTRUE(random)){
    expected.entropy = lapply(1:10, function(x){
      out <- vegan::permatfull(data, fixedmar = "rows", shuffle = "samp", times = 1, mtype = "prab")
      ran.data=out$perm[[1]] # select outcome of shuffle
      ran.data[ran.data>1]=1 
      xx.ran = nth.order.entropy(data = ran.data, level = entropy.level) 
      return(xx.ran$entropy)
    })
    expected.entropy = do.call(cbind, expected.entropy)
    xx.s$expected.entropy = rowMeans(expected.entropy)
  }
  
  xx.slope = rbind(xx.slope, xx.s)
  
  return(xx.slope)
}