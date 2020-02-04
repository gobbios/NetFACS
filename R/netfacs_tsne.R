#' Conducts dimension reduction using t-distributed stochastic neighbor embedding and plots conditions
#' 
#' Takes full dataset from netfacs function, conducts tsne using Rtsne package, and plots results
#' 
#' @param netfacs.data object resulting from netfacs() function
#'
#' @return Function returns a ggnet plot where each condition is connected to those elements that occur significantly in this condition, and each element is connected to each condition under which it occurs significantly more than expected
#' 
#' @export
#'
#' @examples
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


netfacs.tsne <- function(netfacs.data){
  library(Rtsne)
  library(ggplot2)
  
  data = netfacs.data$used.data$data
  condition = netfacs.data$used.data$condition
  colors = rainbow(length(unique(condition)))
  names(colors) = unique(condition)
  
  condition = condition[rowSums(data)>0]
  data = data[rowSums(data)>0,]
  
  tsne = Rtsne(data, dims = 2, perplexity=20, verbose=F, max_iter = 1000, check_duplicates = F)
  plot.data = data.frame(tsne$Y)

  p = ggplot(plot.data, aes(x = X1, y = X2, color = condition)) +
    geom_point(aes(color = condition), cex = 5) + xlab('First Dimension') +  ylab('Second Dimension') + ggtitle('tsne')

  res = list(model = tsne, plot = p)
  return(res)
}