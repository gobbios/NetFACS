#' Prune and plot apriori ruleset
#'
#' Returns rules that satisfy specified 'support' (probability that A and B happen together) and 'confidence' (probability that B occurs if A is present) and plots them
#'
#' @param netfacs.data netfacs data list
#' @param support probability that, in the data, the combination occurs
#' @param confidence probability that, in the data, the second element occurs if the first element or combination is already in place
#'
#' @return Returns the pruned ruleset and plots it using the arulesViz package
#' @export
#'
#' @examples
#' data(netfacstestdata)
#' angry.face = netfacs(data = emotions_set[[1]],
#'  condition = emotions_set[[2]]$emotion,
#'  test.condition = 'anger',
#'  null.condition = NULL,
#'  duration = NULL, 
#'  ran.trials = 100,
#'  control = NULL,
#'  random.level = NULL,
#'  combination.size = 5)
#' arules.explorer(netfacs.data = netfacstestdata)



arules.return <- function(netfacs.data, confidence = 0.8, support = 0.05){
  require(arules)
  require(arulesViz)
  
  data = netfacs.data$arules
  subRules = data[quality(data)$confidence>confidence & quality(data)$support>support]
  subset.rules = which(colSums(is.subset(subRules, subRules)) > 1)
  subRules = subRules[-subset.rules]
  
  rule.list = list(ruleset = subRules, plot = plot(subRules, method = "graph",  engine = "htmlwidget"))
  return(rule.list)

}