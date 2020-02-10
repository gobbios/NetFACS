#' Prune and plot apriori ruleset
#'
#' Returns rules that satisfy specified 'support' (probability that A and B happen together) and 'confidence' (probability that B occurs if A is present) and plots them
#' Rules work by selecting the lowest-order sufficient explanation for the observed data. So, for example, if the combination of A and B explains the occurrence of C, but A alone also explains C, than the rule is A -> C. If A alone does not explain C, and B alone neither, then AB -> C
#'
#' @param netfacs.data netfacs data list
#' @param support probability that, in the data, the combination occurs
#' @param confidence probability that, in the data, the second element occurs if the first element or combination is already in place
#'
#' @return Returns the pruned ruleset and plots it using the arulesViz package
#' 
#' @export
#'
#' @examples
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
#' arules.return(netfacs.data = angry.face, confidence = 0.8, support = 0.1)


arules.return <- function(netfacs.data, confidence = 0.8, support = 0.05){
  require(arules)
  require(arulesViz)
  
  netfacs.data = netfacs.data$arules # extract the arules object from the dataset
  subRules = netfacs.data[quality(netfacs.data)$confidence>confidence & quality(netfacs.data)$support>support] #select those subrules that fulfil the selected criteria for support and confidence
  subset.rules = which(colSums(is.subset(subRules, subRules)) > 1)
  subRules = subRules[-subset.rules]
  
  rule.list = list(ruleset = subRules, plot = plot(subRules, method = "graph",  engine = "htmlwidget")) # plot the rules as a html widget
  return(rule.list)

}