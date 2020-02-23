#' Produce conditional probabilities of dyads of elements, and graph object based on conditional probabilities
#' 
#' For all dyadic combinations that ever appear, this function returns the probability of A occurring (P(A)), the probability of B occurring (P(B)), the probability of A and B occurring simultaneously (P(A+B)), and the probability of A occurring if B is given (P(A|B)). It also creates a graph object that can be plotted
#' 
#' 
#' @param netfacs.data object resulting from netfacs() function
#' @param package should the graph object be created in sna or igraph?
#' @param min.prob minimum conditional probability that should be shown in the graph
#' @param min.count minimum number of times that a combination should occur before being included in the graph
#'
#' @return Function returns a dataframe that includes all dyadic combinations and their observed and conditional probabilities
#' 
#' @export
#'
#' @examples
#' ### how do angry facial expressions differ from non-angry ones?
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
#'  
#'  conditional.net = network.conditional(angry.face, package = 'igraph')


network.conditional <- function(netfacs.data, package = 'igraph', min.prob = 0, min.count = 0, ignore.element = NULL){
  library(arules)
  require(ggplot2)
  require(scales)
  suppressMessages(require(ggnet))
  suppressMessages(require(igraph))
  suppressMessages(require(sna))
  suppressMessages(require(intergraph))
  
  rs = as(netfacs.data$arules, "data.frame") 
  rs$rules = gsub("[^A-Za-z0-9, ]","",rs$rules)
  rs$rules = gsub(" ",",",rs$rules)
  xrs = unlist(rs$rules)
  x.elements = lapply(xrs, function(x){
    xx = unlist(strsplit(x, split = ",", fixed = T))
    return(xx)
  })
  x.elements = lapply(x.elements, function(x){x[x!='']})
  rs$combination.size = sapply(x.elements,FUN = length)
  rs.1 = rs[rs$combination.size == 1,]
  rs.1$rules = gsub(rs.1$rules, pattern = ',', replacement = '', fixed = T)
  x.elements = x.elements[rs$combination.size==2]
  rs = rs[rs$combination.size == 2,]
  rs$element1 = sapply(x.elements, function(x){x[1]})
  rs$element2 = sapply(x.elements, function(x){x[2]})
  rs$combination = sapply(x.elements, function(x){paste(sort(x), collapse = '_')})
  rs = rs[,c('element2', 'element1', 'combination', 'count', 'confidence', 'support')]
  colnames(rs) = c('elementA', 'elementB', 'combination', 'count', 'P(A|B)', 'P(A+B)')
  rs$'P(A)' = unlist(lapply(rs$elementA, function(x){
    xx = rs.1$support[rs.1$rules == x]
  }))
  rs$'P(B)' = unlist(lapply(rs$elementB, function(x){
    xx = rs.1$support[rs.1$rules == x]
  }))
  rs = rs[,c('elementA', 'elementB', 'combination', 'count', 'P(A)', 'P(B)', 'P(A+B)', 'P(A|B)')]
  rs$`P(A)` = round(rs$`P(A)`, 3)
  rs$`P(B)` = round(rs$`P(B)`, 3)
  rs$`P(A+B)` = round(rs$`P(A+B)`, 3)
  rs$`P(A|B)` = round(rs$`P(A|B)`, 3)
  
  compare.mat = rs[rs$`P(A|B)` >= min.prob & rs$`count` >= min.count & !rs$elementA%in%ignore.element & !rs$elementB%in%ignore.element,]
  
  descriptive.graph = graph_from_data_frame(compare.mat, directed = T, vertices = NULL)
  vertex.attributes(descriptive.graph)$element.probability = rs.1$support[match(vertex.attributes(descriptive.graph)$name, rs.1$rules)]
  edge.attributes(descriptive.graph)$weight = edge.attributes(descriptive.graph)$'P(A|B)'
  
  missing.nodes = setdiff(rs.1$rules, V(descriptive.graph)$name)
  descriptive.graph = add_vertices(descriptive.graph, length(missing.nodes), attr = list(name = missing.nodes))
  descriptive.graph = delete_vertices(descriptive.graph, igraph::degree(descriptive.graph)==0)
  
  if(package == 'sna'){
    descriptive.graph = intergraph::asNetwork(descriptive.graph)
  }
  
  net.graph = descriptive.graph
  
  node.label = vertex.attributes(net.graph)$name
  vertex.attributes(net.graph)$node.size = vertex.attributes(net.graph)$element.probability

  p = ggraph(graph = net.graph, layout = 'igraph', algorithm = 'kk') +
    geom_edge_link(mapping = aes (label = weight), label_size = 3,
                   arrow = arrow(type = "closed", angle = 15, length = unit(3,'mm')), 
                   end_cap = circle(2, 'mm'), , 
                   start_cap = circle(2, 'mm'), 
                   colour="grey",
                   label_dodge  = unit(3, "mm"),
                   angle_calc = "along", show.legend = F) +
    geom_node_text(mapping = aes(label = name, size = node.size, fontface = 'bold'), show.legend = F) + scale_size(range = c(3,7), breaks = NULL) +
    theme_graph()
  
  
  
  return(list(conditional.probalities = rs, network.graph = descriptive.graph, plot = p))
}