#' Use arulesViz::ruleExplorer on NetFACS data
#'
#' Applies 'shiny' application of arulesViz() package, which allows the user to set the support (probability of elements co-occurring), confidence (likelihood of B occurring when A is present), and lift (increase in likelihood of B if A is known), and extract association rules. Includes fancy plots 
#'
#' @param netfacs.data netfacs data list
#'
#' @return Starts 'shiny' application of arulesViz::ruleExplorer() function)
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



arules.explorer <- function(netfacs.data){
  require(arules)
  ruleExplorer(netfacs.data$arules)
}