#' Dimension reduction and unsupervised learning of conditions
#' 
#' Takes full dataset from netfacs function, conducts UMAP (uniform manifold approximation and projection) to reduce dimensions and then k-Means to detect optimal number of categories. Can also test those categories against used categories and can be used to add new cases to see where they fall.
#' 
#' @param netfacs.data object resulting from netfacs() function
#' @param condition vector containing information about the classification of known cases. If NULL, unsupervised learning is produced
#'
#' @return Returns a UMAP object that can be used to predict where new cases cluster. Also produces graph of unsupervised clustering, table of overlap of condition and unsupervised clusters
#' 
#' @export
#'
#' @examples
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
#'  angry.umap = netfacs.clustering(angry.face, condition = angry.face$used.data$condition)
#'  angry.umap$plot
#'  angry.umap$comparison.table


netfacs.clustering <- function(netfacs.data, condition = NULL){
  library(umap)
  library(ggplot2)
  
  data = netfacs.data$used.data$data
  data = data[rowSums(data)>0,]
  
  custom.settings = umap.defaults
  custom.settings$n_epochs = 1000
  
  umaps = umap(data, config = custom.settings)
  plot.data.umap = data.frame(umaps$layout)

  umap.clust = Mclust(as.matrix(plot.data.umap), G=1:40, verbose = F)
  umap.clust.optim = dim(umap.clust$z)[2]
  
  u = ggplot(plot.data.umap, aes(x = X1, y = X2, color = as.factor(umap.clust$classification))) +
    geom_point(aes(color = as.factor(umap.clust$classification)), cex = 5) + xlab('First Dimension') +  ylab('Second Dimension') + ggtitle('umap')
  
  defining.elements = lapply(unique(umap.clust$classification), function(x){
    xx = data.frame(data[umap.clust$classification == x,], check.names = F)
    xx = colnames(xx)[(colSums(xx)/nrow(xx))>0.5]
    xx = data.frame(cluster = x, elements = paste(xx, collapse = ','))
  })
  defining.elements = do.call(rbind, defining.elements)
  
  
  if(is.null(condition)){return(list(plot = u, cluster = umap.clust$classification, umap.data = umaps, optimal.k = umap.clust.optim, defining.elements = defining.elements))}
  
  umap.table = table(umap.clust$classification, condition)
  umap.clusters = umap.clust$classification
  
  umap.table = table(umap.clusters, condition)
  umap.table = umap.table[order(as.character(rownames(umap.table))), order(as.character(colnames(umap.table)))]
  
  return(list(plot = u, cluster = umap.clust$classification, umap.data = umaps, optimal.k = umap.clust.optim, comparison.table = umap.table, defining.elements = defining.elements))
}