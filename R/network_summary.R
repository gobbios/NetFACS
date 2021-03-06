#' Returns all kinds of network measures for the netfacs network
#' 
#' Calculates node level centrality measures from the network object
#' 
#' @param netfacs.graph igraph network object resulting from netfacs.network() function
#'
#' @return Function returns a dataframe with the element, its 'strength' (mean probability of co-occurrance), 'eigenvector' centrality (connection to other highly connected elements), 'betweeness' centrality (number of connections running through the element), and a number of other network measures
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
#'  combination.size = 2)
#'  
#'  anger.net = netfacs.network(netfacs.data = angry.face, package = 'igraph', link = 'unweighted', significance = 0.01, min.count = 1, min.prob = 0, min.specificity = 0, ignore.element = NULL)
#'  network.summary(anger.net)


network.summary <- function(netfacs.graph){
  
  # set digits printed to 3
  options(digits = 3)
  
  net.measure=data.frame(element = unique(unlist(vertex.attributes(netfacs.graph)$name)),
                         strength = 0,
                         eigenvector = 0,
                         betweenness = 0,
                         transitivity = 0,
                         hub_score = 0,
                         page_rank = 0,
                         modularity = 0,
                         comm.membership = 0,
                         comm.value = 0
  )
  net.strength = igraph::strength(netfacs.graph, mode = 'total', weights = edge.attributes(netfacs.graph)$weight)
  net.strength = net.strength / ((length(net.strength)-1) * 2)
  net.measure$strength = unlist(lapply(1:nrow(net.measure), function(y){
    xx=net.strength[names(net.strength)==net.measure$element[y]]
    if(length(xx)==1){return(xx)}
    if(length(xx)==0){return(0)}
  }))
  
  net.eigen = igraph::eigen_centrality(netfacs.graph, weights = edge.attributes(netfacs.graph)$weight)$vector
  net.measure$eigenvector = unlist(lapply(1:nrow(net.measure), function(y){
    xx=net.eigen[names(net.eigen)==net.measure$element[y]]
    if(length(xx)==1){return(xx)}
    if(length(xx)==0){return(0)}
  }))
  
  net.btw = igraph::betweenness(netfacs.graph, weights = edge.attributes(netfacs.graph)$weight)
  net.btw = net.btw/((length(net.btw)-1)*2)
  net.measure$betweenness = unlist(lapply(1:nrow(net.measure), function(y){
    xx=net.btw[names(net.btw)==net.measure$element[y]]
    if(length(xx)==1){return(xx)}
    if(length(xx)==0){return(0)}
  }))
  
  if(length(unique(edge.attributes(netfacs.graph)$weight))==1){type='undirected'}
  if(length(unique(edge.attributes(netfacs.graph)$weight))>1){type='weighted'}
  net.trans = igraph::transitivity(netfacs.graph, type = 'local', weights = edge.attributes(netfacs.graph)$weight)
  names(net.trans) = V(netfacs.graph)$name
  net.measure$transitivity = unlist(lapply(1:nrow(net.measure), function(y){
    xx=net.trans[names(net.trans)==net.measure$element[y]]
    if(length(xx)==1){return(xx)}
    if(length(xx)==0){return(0)}
  }))
  
  net.hub = igraph::hub_score(netfacs.graph, weights = edge.attributes(netfacs.graph)$weight)$vector
  net.measure$hub_score = unlist(lapply(1:nrow(net.measure), function(y){
    xx=net.hub[names(net.hub)==net.measure$element[y]]
    if(length(xx)==1){return(xx)}
    if(length(xx)==0){return(0)}
  }))
  
  net.page = igraph::page_rank(netfacs.graph, weights = edge.attributes(netfacs.graph)$weight)$vector
  net.measure$page_rank = unlist(lapply(1:nrow(net.measure), function(y){
    xx=net.page[names(net.page)==net.measure$element[y]]
    if(length(xx)==1){return(xx)}
    if(length(xx)==0){return(0)}
  }))
  
  net.community = igraph::walktrap.community(netfacs.graph)
  net.modularity = net.community$modularity
  net.measure$modularity = unlist(lapply(1:nrow(net.measure), function(y){
    xx=net.modularity[names(net.btw)==net.measure$element[y]]
    if(length(xx)==1){return(xx)}
    if(length(xx)==0){return(0)}
  }))
  net.measure$comm.membership = unlist(lapply(1:nrow(net.measure), function(y){
    xx=net.community$membership[names(net.btw)==net.measure$element[y]]
    if(length(xx)==1){return(xx)}
    if(length(xx)==0){return(0)}
  }))
  net.un = as.undirected(netfacs.graph, mode = 'collapse')
  net.measure$comm.value = igraph::modularity(net.un, membership = net.community$membership)
  
  net.measure[sapply(net.measure, is.numeric)] <- lapply(net.measure[sapply(net.measure, is.numeric)], round, 3)
  
  return(net.measure)
}
