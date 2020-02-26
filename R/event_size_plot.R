#' Plots the probability that a combination of a certain size appears
#' 
#' The function takes all combination size in a netfacs object, and plots the distribution of ratios between the observed value and all randomisations
#' 
#' 
#' @param netfacs.data object resulting from netfacs() function
#'
#' @return Function returns a ggplot showing for each combination size by how much the observed probability outperforms the expected probabilities
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
#'  event.size.plot(angry.face)

event.size.plot <- function(netfacs.data){
  
  # set digits printed to 3
  options(digits = 3)
  
  plot.netfacs = netfacs.data$event.size.information$total.event.sizes
  plot.netfacs$probability.increase = plot.netfacs$observed.probability / plot.netfacs$expected.probability
  
  plot.netfacs$probability.increase[is.na(plot.netfacs$probability.increase)] = 0
  plot.netfacs$probability.increase[is.infinite(plot.netfacs$probability.increase)] = 10
  
  if(is.null(netfacs.data$used.parameters$test.condition)){netfacs.data$used.parameters$test.condition='all cases'}
  if(is.null(netfacs.data$used.parameters$null.condition)){netfacs.data$used.parameters$null.condition='random'}
  
  p = ggplot(plot.netfacs, aes(x=as.factor(combination.size), y=probability.increase)) + 
    xlab ('element.size') +
    ylab ('event size probability increase') +
    ggtitle (paste(c('Comparison of event sizes between ', netfacs.data$used.parameters$test.condition, ' and ', netfacs.data$used.parameters$null.condition), collapse = '')) +
    scale_y_continuous(trans='log2', labels=scales::percent_format(accuracy = 1)) +
    geom_boxplot() + geom_hline (yintercept = 1)
  
  return(p)  
}