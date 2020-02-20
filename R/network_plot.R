#' Plots a network object
#' 
#' Plots the network created using the netfacs.network() function; for networks with clear clusterin of elements, clusters can get different colours
#' 
#' @param netfacs.graph igraph network object resulting from netfacs.network() function
#' @param title string of the graph's main title
#' @param clusters if TRUE, the igraph::walktrap.community function is used to establish possible clusters in the dataset
#'
#' @return Function returns a ggnet plot of the network, where the size of nodes indicates how often they occur on their own, and edges indicate significant co-occurrance between them
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
#'  anger.plot = network.plot(anger.net, title = 'Angry Faces', clusters = F)
#'  anger.plot + ggtitle('This is a ggplot object and can be manipulated')
#'  
#'  all.face = netfacs(data = emotions_set[[1]],
#'  ran.trials = 100,
#'  combination.size = 2)
#'  all.net = netfacs.network(netfacs.data = all.face, package = 'igraph', link = 'unweighted', significance = 0.01, min.count = 1, min.prob = 0, min.specificity = 0, ignore.element = NULL)
#'  all.plot = network.plot(anger.net, title = 'All Emotions', clusters = F)



network.plot <- function(netfacs.graph, title = 'network', clusters = T){
  require(igraph)
  require(ggplot2)
  require(ggnet)
  require(scales)
  
  net.graph = netfacs.graph
  
  node.label = vertex.attributes(net.graph)$name
  node.size = vertex.attributes(net.graph)$element.significance
  node.size[node.size >0.01] = 2
  node.size[node.size <=0.01] = 3
  node.size[is.na(node.size)] = 1
  edge.weight = edge.attributes(net.graph)$weight
  edge.size = cut(edge.weight, 3)
  edge.size.char = as.character(edge.size)
  edge.size.char[edge.size==levels(edge.size)[1]] = 1
  edge.size.char[edge.size==levels(edge.size)[2]] = 3
  edge.size.char[edge.size==levels(edge.size)[3]] = 5
  edge.size = as.numeric(edge.size.char)
  if(length(unique(edge.size))==1){edge.size = edge.size/edge.size}
  
  if(clusters == F){
    p = ggnet2(net.graph, node.size = node.size * 4, color = 'lightblue', edge.size = edge.size, label = node.label, label.size = node.size*3, label.color = 'black', mode = "kamadakawai")  +
      guides(color = FALSE, size = FALSE) + ggtitle(title) +
      theme(plot.title = element_text(hjust = 0.5))
    
  }
  
  if(clusters == T){
    net.un = net.graph
    net.community = igraph::cluster_fast_greedy(net.un)
    modular = round(igraph::modularity(net.community), 2)
    net.com = data.frame(element = net.community$names, community = net.community$membership)
    color = rainbow(length(unique(net.com$community)))
    
    p = ggnet2(net.graph, node.size = node.size * 4, color = color[net.com$community], edge.size = edge.size, label = node.label, label.size = node.size*3, label.color = 'black', mode = "kamadakawai")  +
      guides(color = FALSE, size = FALSE) + ggtitle(paste(c(title, modular), collapse = ' , modularity = ')) +
      theme(plot.title = element_text(hjust = 0.5))
  }
  return(p)
}