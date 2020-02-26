#' Plots the probability increase for the basic elements based on the condition
#' 
#' The function takes all single elements in a netfacs object, and plots the distribution of ratios between the observed value and all randomisations
#' 
#' 
#' @param netfacs.data object resulting from netfacs() function
#'
#' @return Function returns a ggplot showing for each element by how much the observed probability outperforms the expected probabilities; if expected probabilities are 0, the probability increase is set to 10; y-axis is log transformed
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
#'  element.plot(angry.face)

element.plot <- function(netfacs.data){
  
  # set digits printed to 3
  options(digits = 3)
  
  # take only the result part of the netfacs object
  plot.netfacs = netfacs.data$result
  
  # reduce to single elements
  plot.netfacs = plot.netfacs[plot.netfacs$combination.size == 1,]
  
  # calculate change in probability
  aa = do.call(rbind, lapply(1:length(plot.netfacs$combination), function(x){
    xx = data.frame(combination = plot.netfacs$combination[x], prob.change = plot.netfacs$observed.probability[x] / netfacs.data$used.data$random.probability[x,])
    return(xx)
  }))
  
  # if the probability increase is NA (usually if the null condition has no occurrences), set probability increase to 10
  if(sum(is.na(aa$prob.change))>0){aa$prob.change[is.na(aa$prob.change)] = 10}
  
  # create title and significance stars
  aa$combination = as.character(aa$combination)
  if(is.null(netfacs.data$used.parameters$test.condition)){netfacs.data$used.parameters$test.condition='all cases'}
  if(is.null(netfacs.data$used.parameters$null.condition)){netfacs.data$used.parameters$null.condition='random'}
  plot.netfacs$star = ''
  plot.netfacs$star[plot.netfacs$pvalue<=0.01 & plot.netfacs$z>0]='*'
  
  # create plot
  p = ggplot(aa, aes(x=combination, y=prob.change)) + 
    xlab ('element') +
    ylab ('probability increase') +
    annotate('text', x = plot.netfacs$combination, y = max(aa$prob.change + 1), label = plot.netfacs$star, size = 8) +
    ggtitle (paste(c('Comparison of ', netfacs.data$used.parameters$test.condition, ' and ', netfacs.data$used.parameters$null.condition), collapse = '')) +
    scale_y_continuous(trans='log2', labels=scales::percent_format(accuracy = 1)) +
    geom_boxplot() + 
    geom_hline (yintercept = 1) + 
    theme_bw()
  
  return(p)  
}