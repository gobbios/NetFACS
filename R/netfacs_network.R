#' Creates a network object out of the netfacs data
#' 
#' Takes the results of the nefacs object for combinations of 2 elements and turns them into a network object (igraph or sna/network) that can be used for further plotting and analyses
#' 
#' 
#' @param netfacs.data object resulting from netfacs() function
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
#'  edge.attributes(anger.net)

netfacs.network <- function(netfacs.data, package = 'igraph', link = 'unweighted', significance = 0.01, min.count = 1, min.prob = 0, min.specificity = 0, ignore.element = NULL){
  
  ### create dyadic association using dyad.comparison
  compare.mat = netfacs.data$result[netfacs.data$result$combination.size==2,]
  node.weight = netfacs.data$result[netfacs.data$result$combination.size==1,]
  node.weight$pvalue[node.weight$z<0] = 1
  if(is.null(netfacs.data$used.parameters$test.condition)){
    compare.mat$specificity = 1
    compare.mat$probability.increase = 1
  }
  compare.mat = compare.mat[compare.mat$observed.probability >= min.prob & compare.mat$count >= min.count & compare.mat$specificity >= min.specificity & compare.mat$z > 0,]
  compare.mat$probability.increase[is.na(compare.mat$probability.increase)]=10
  compare.mat = compare.mat[complete.cases(compare.mat),]
  compare.mat$element1 = sapply(compare.mat$combination, function(x){
    xx = sort(unlist(strsplit(x, split = '_')))
    return(xx[1])
  })
  compare.mat$element2 = sapply(compare.mat$combination, function(x){
    xx = sort(unlist(strsplit(x, split = '_')))
    return(xx[2])
  })
  
  compare.mat = compare.mat[!(compare.mat$element1%in%ignore.element) & !(compare.mat$element2%in%ignore.element),]
  
  compare.mat = compare.mat[,c('element1', 'element2', 'probability.increase', "observed.probability", "expected.probability", 'z', "pvalue", "specificity", 'count', 'combination')]
  
  suppressMessages(require(igraph))
  suppressMessages(require(sna))
  suppressMessages(require(intergraph))
  
  
  descriptive.graph = graph_from_data_frame(compare.mat, directed = F, vertices = NULL)
  vertex.attributes(descriptive.graph)$element.probability = node.weight$observed.probability[match(vertex.attributes(descriptive.graph)$name, node.weight$combination)]
  vertex.attributes(descriptive.graph)$element.significance = node.weight$pvalue[match(vertex.attributes(descriptive.graph)$name, node.weight$combination)]
  
  if(link == 'unweighted'){
    edge.attributes(descriptive.graph)$unweighted = (edge.attributes(descriptive.graph)$pvalue<=significance & edge.attributes(descriptive.graph)$z>0) 
    descriptive.graph = delete_edges(descriptive.graph, edges=which(!edge.attributes(descriptive.graph)$unweighted))
    edge.attributes(descriptive.graph)$weight = as.numeric(edge.attributes(descriptive.graph)$unweighted)
    edge.attributes(descriptive.graph)$association = edge.attributes(descriptive.graph)$observed.probability - edge.attributes(descriptive.graph)$expected.probability
  }
  if(link == 'weighted'){
    edge.attributes(descriptive.graph)$weight = edge.attributes(descriptive.graph)$observed.probability - edge.attributes(descriptive.graph)$expected.probability 
  }
  if(link == 'raw'){
    edge.attributes(descriptive.graph)$weight = edge.attributes(descriptive.graph)$observed.probability
  }
  if(link == 'SRI'){
    xx.sri = graph.adjacency(netfacs.data$affinity,weighted=TRUE)
    xx.sri <- get.data.frame(xx.sri)
    xx.sri.dyad = unlist(lapply(1:nrow(xx.sri), function(x){paste(sort(xx.sri[x,1:2]), collapse = '_')}))
    edge.attributes(descriptive.graph)$weight = xx.sri$weight[match(edge.attributes(descriptive.graph)$combination, xx.sri.dyad)]
  }
  
  missing.nodes = setdiff(node.weight$combination, V(descriptive.graph)$name)
  missing.nodes = setdiff(missing.nodes, ignore.element)
  descriptive.graph = add_vertices(descriptive.graph, length(missing.nodes), attr = list(name = missing.nodes))
  
  if(package == 'sna'){
    descriptive.graph = intergraph::asNetwork(descriptive.graph)
  }
  
  return(descriptive.graph)
}
