#' Creates network objects out of the netfacs data
#' 
#' Takes the results of the nefacs object for combinations of 2 elements and turns them into a network object (igraph or sna/network) that can be used for further plotting and analyses
#' 
#' 
#' @param netfacs.list list of multiple objects resulting from netfacs() function or the multiple.netfacs() function
#' @param package determines which kind of network object the data are turned into; can be either 'igraph' or 'sna'; most plotting and analyses in this package are based on igraph
#' @param link determines how nodes/elements are connected. 'unweighted' gives a 1 to significant connections and 0 to all others; 'weighted' gives the difference between observed and expected probability of co-occurrence; 'raw' just uses the observed probability of co-occurrence; 'SRI' uses the simple ratio index/affinity (probability of co-occurrence/ (probablities of each element and the combination))
#' @param min.count numeric value, suggesting how many times a combination should at least occur to be displayed
#' @param min.prob numeric value, suggesting the probability at which a combination should at least occur to be displayed
#' @param min.specificity numeric value, suggesting the specificity a combination should at least have for the test condition to be displayed
#' @param significance numeric value, determining the p-value below which combinations are considered to be dissimilar enough from the null distribution
#' @param ignore.element vector of elements that will not be considered for the network, e.g. because they are too common or too rare or their interpretation is not relevant here
#'
#' @return Function returns a network object where the nodes are the elements, edges represent their co-occurrence, and the vertex and edge attributes contain all additional information from the netfacs object
#' 
#' @export
#'
#' @examples
#' data(emotions_set)
#' emo.faces = multiple.netfacs(data = emotions_set[[1]],
#'  condition = emotions_set[[2]]$emotion,
#'  duration = NULL, 
#'  ran.trials = 100,
#'  control = NULL,
#'  random.level = NULL,
#'  combination.size = 5)
#'  
#'  emo.nets = multiple.netfacs.network(emo.faces)

multiple.netfacs.network <- function(netfacs.list, package = 'igraph', link = 'unweighted', significance = 0.01, min.count = 1, min.prob = 0, min.specificity = 0, ignore.element = NULL){
  
  multi.net = lapply(netfacs.list, function(x){
    xx = netfacs.network(x, package = package, link = link, significance = significance, min.count = min.count, min.prob = min.prob, min.specificity = min.specificity, ignore.element = ignore.element)
    return(xx)
  })
  names(multi.net) = names(netfacs.list)
  return(multi.net)
}