#' Plots the overlap of multiple conditions as bipartite network
#' 
#' The function takes multiple netfacs objects and plots how different elements connect the conditions
#' 
#' @param netfacs.list list of objects resulting from netfacs() function or multiple.netfacs() function
#'
#' @return Function returns a ggnet plot where each condition is connected to those elements that occur significantly in this condition, and each element is connected to each condition under which it occurs significantly more than expected
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
#'  overlap.network(emo.faces)


overlap.network <- function(netfacs.list, min.prob = 0, min.count = 5, significance = 0.01, ignore.element = NULL){
  require(igraph)
  require(ggplot2)
  require(ggnet)
  require(scales)
  if(is.null(names(netfacs.list))){names(netfacs.list)=1:length(netfacs.list)}
  net.data = lapply(netfacs.list, function(x){x$result[x$result$combination.size == 1 & 
                                                         x$result$pvalue <= significance &
                                                         x$result$z > 0 &
                                                         x$result$count >= min.count &
                                                         !(x$result$combination %in% ignore.element) &
                                                         x$result$observed.probability >= min.prob
                                                       ,]})
  multi.net = lapply(1:length(net.data), function(x){
    xx = data.frame(condition = names(net.data)[x], element = net.data[[x]]$combination)
    return(xx)
  })
  
  multi.net = do.call(rbind, multi.net)
  
  net.graph = graph_from_data_frame(multi.net, directed = F, vertices = NULL)
  V(net.graph)$type <- bipartite_mapping(net.graph)$type
  
  node.color = rep(2, length(vertex.attributes(net.graph)$name))
  node.color[vertex.attributes(net.graph)$name%in%multi.net[,1]] = 3
  node.label = vertex.attributes(net.graph)$name
  node.size = rep(4, length(vertex.attributes(net.graph)$name))
  node.size[vertex.attributes(net.graph)$name%in%multi.net[,1]] = 8
  edge.size = rep(1, gsize(net.graph))
  V(net.graph)$size = node.size+4
  V(net.graph)$color <- ifelse(V(net.graph)$type, "lightblue", "salmon")
  V(net.graph)$shape <- ifelse(V(net.graph)$type, "circle", "square")
  
  
  p = ggnet2(net.graph, node.size = node.size+2, color = V(net.graph)$color, edge.size = edge.size, label = node.label, label.size = node.size, label.color = 'black', node.shape = V(net.graph)$shape)  +
    guides(color = FALSE, size = FALSE, shape = F) + ggtitle('Overlap Network') +
    theme(plot.title = element_text(hjust = 0.5))
  
  return(p)
}