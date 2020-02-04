nth.order.entropy <- function(data, level){
  elements = lapply(1:nrow(data), function(x){
    xx = colnames(data)[data[x,]==1]
  })
  ar = apriori(elements, parameter = list(supp=1/nrow(data), conf=1/nrow(data), maxlen=level+1), control = list(verbose = F))
  
  xx.s = lapply(1:level, function(f){
    rs = quiet(data.frame(inspect(ar, ruleSep = '', itemSep = '_', setStart = '', setEnd = '')))
    rs$lhs = as.character(rs$lhs)
    rs$rhs = as.character(rs$rhs)
    rs$size = unlist(lapply(rs$lhs, function(x){length(unlist(strsplit(x, split = '_'))) + 1}))
    rs = rs[rs$size==f | rs$size ==f-1,]
    
    
    xx.f = data.frame(ante = rs$lhs)
    xx.f$ante = as.character(xx.f$ante)
    xx.f$condition = unlist(lapply(1:nrow(rs), function(x){
      ante = unlist(strsplit(rs$lhs[x], split = '_'))
      return(paste(c(ante, rs$rhs[x]), collapse = '_'))
    }))
    xx.f$condition.count = rs$count
    xx.f$condition = as.character(xx.f$condition)
    xx.f$condition.size = unlist(lapply(xx.f$condition, function(x){length(unlist(strsplit(x, split = '_')))}))
    xx.f$ante.count = xx.f$condition.count[match(xx.f$ante, xx.f$condition)]
    xx.f$ante.count[is.na(xx.f$ante.count)]=nrow(data)
    
    xx.f = xx.f[xx.f$condition.size == f,]
    
    xx.f$freq.tot = xx.f$ante.count/sum(xx.f$ante.count)
    xx.f$freq.cond = xx.f$condition.count/xx.f$ante.count
    xx.f$yy = xx.f$freq.tot*log2(xx.f$freq.cond)
    
    return(c(f, -1*sum(xx.f$yy, na.rm=T)))
  })
  xx.s = data.frame(do.call(rbind, xx.s))
  colnames(xx.s) = c('order', 'entropy')
  return(xx.s)
}