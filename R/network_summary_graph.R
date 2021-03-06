#' Returns all kinds of graph-level network measures for the netfacs network
#' 
#' Calculates graph level summary measures from the network object
#' 
#' @param netfacs.graph igraph network object resulting from netfacs.network() function
#'
#' @return Function returns a dataframe with the number of elements in the graph, the number of connected edges, mean strength of connections, transitivity (mean number of closed triads), diameter (furthest path between two elements), degree centralization, and mean distance between elements
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
#'  network.summary.graph(anger.net)

network.summary.graph <- function(netfacs.net){
  
  # set digits printed to 3
  options(digits = 3)
  
  if(length(unique(edge.attributes(netfacs.net)$weight))==1){type='undirected'}
  if(length(unique(edge.attributes(netfacs.net)$weight))>1){type='weighted'}
  net.from.igraph = data.frame(nr.elements = length(vertex.attributes(netfacs.net)$name),
                               nr.edges = length(edge.attributes(netfacs.net)$weight),
                               density =  edge_density(simplify(netfacs.net), loops=F),
                               transitivity =  transitivity(simplify(netfacs.net), type = type),
                               diameter =  diameter(simplify(netfacs.net), directed = F),
                               degree_centralization =  centr_degree(simplify(netfacs.net))$centralization,
                               mean_distance =  mean_distance(simplify(netfacs.net))
  )
  net.from.igraph[sapply(net.from.igraph, is.numeric)] <- lapply(net.from.igraph[sapply(net.from.igraph, is.numeric)], round, 3)
  return(net.from.igraph)
}

