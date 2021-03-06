#' Plots networks for multiple conditions
#' 
#' The function takes multiple network objects and plots them next to each other while keeping the element positions etc constant. Uses igraph.plot function
#' 
#' 
#' @param netfacs.graphs list of network objects resulting from netfacs.network() function or multiple.netfacs.networks() function
#'
#' @return Function returns a igraph.plot connections between nodes in the different networks. Elements that are significantly more likely to occur than expected are large, non-significant elements are small, and absent elements are absent.
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
#'  emo.nets = multiple.netfacs.network(emo.faces, min.count = 5)
#'  multiple.network.plot(emo.nets)

multiple.network.plot <- function(netfacs.graphs){
  require(igraph)
  require(ggplot2)
  require(ggnet)
  require(scales)
  
  all.nodes = sort(unique(unlist(lapply(netfacs.graphs, function(x){
    return(V(x)$name)
  }))))
  
  netfacs.graphs = lapply(netfacs.graphs, function(x){
    missing.nodes = setdiff(all.nodes, V(x)$name)
    x = add_vertices(x, length(missing.nodes), attr = list(name = missing.nodes))
    return(x)
  })
  
  net = graph.empty(n=length(all.nodes), directed=F)
  V(net)$name = all.nodes
  
  for(i in 1:length(netfacs.graphs)){
    net = net %u% netfacs.graphs[[i]]
  }
  l <- layout_in_circle(net, order=V(net))
  
  pdf(NULL)
  dev.control(displaylist="enable")
  plot.new()
  par(mfrow=c(2,ceiling(length(netfacs.graphs)/2)), mar = c(1,1,1,1))
  for(i in 1:length(netfacs.graphs)){
    net.i = graph.empty(n=length(all.nodes), directed=F)
    V(net.i)$name = all.nodes
    net.i = net.i %u% netfacs.graphs[[i]]
    node.size = vertex.attributes(net.i)$element.significance
    node.size[node.size > 0.01] = 10
    node.size[node.size <= 0.01] = 20
    node.size[is.na(node.size)] = 0
    
    edge.weight = edge.attributes(net.i)$weight
    edge.size = cut(edge.weight, 3)
    edge.size.char = as.character(edge.size)
    edge.size.char[edge.size==levels(edge.size)[1]] = 1
    edge.size.char[edge.size==levels(edge.size)[2]] = 3
    edge.size.char[edge.size==levels(edge.size)[3]] = 5
    edge.size = as.numeric(edge.size.char)
    if(length(unique(edge.size))==1){edge.size = edge.size/edge.size}
    
    plot(net.i, vertex.color=colors()[i*3], layout=l, main=names(netfacs.graphs)[i], edge.width = edge.size, vertex.size = node.size)
  }
  p <- recordPlot()
  invisible(dev.off())
  
  return(p)
}
