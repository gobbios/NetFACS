#' Runs a mixed model with Bernoulli distribution for each of the elements in the dataset, allows to test how multiple predictors and random effects influence the probability that an element occurrs
#' 
#' Very basic Bayesian application, takes the dataset from the netfacs object, you define the formula (' ~ fixed effect + (1|random effect)') and add the predictors as a named list, and it fits a linear mixed model with Bernoulli distribution for binary data using brms package and presents the probabilities of each element under the different fixed effects
#' 
#' @param netfacs.data object resulting from netfacs() function
#' @param test.elements vector of the elements for which the analysis will be carried out; will remove elements for which there is too little variation (present in less than 5% or more than 95% of events)
#' @param formula character vector of fixed and random effects, of the format '~ fixed effect 1 + fixed effect 2 + (1|random effect)'
#' @param predictors named list with the vectors for each predictor specified in the formula (e.g. 'list(fixed effect 1 = data$fixed.effect1)')
#' @param warmup number of warmup iterations for the Bayesian analysis
#' @param iter number of iterations for the Bayesian analysis
#' @param autocorrelation should the Bayesian analysis include an autocorrelation term (only for analyses including a random effect and having time series data)
#'
#' @return Function returns for each element the result object of the brms analysis, the estimates for each level of factors, and a distribution plot for factors
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
#'  ### this takes a while
#'  bayesian.results = element.bayesian(netfacs.data = angry.face, test.elements = c('1', '23', '4', '12'), formula = '~emotion', predictors = list(emotion = emotions_set[[2]]$emotion), warmup = 1000, iter = 2000, autocorrelation = F)



element.bayesian <- function(netfacs.data, test.elements = NULL, formula, predictors, warmup = 1000, iter = 2000, autocorrelation = F){
  
  library(tidyverse)
  library(brms)
  library(ggplot2)
  require(parallel)
  require(doParallel)
  require(pbapply)
  
  # if test elements are not specified, all columns that make up the netfacs dataset are used
  if(is.null(test.elements)){test.elements = colnames(netfacs.data$used.data$data)}
  
  # prepare data and elements
  data = data.frame(netfacs.data$used.data$data[,test.elements], check.names = T)
  data = data[,(colSums(data)/nrow(data))> 0.05] # only use elements for which at least 5% of 
  elements = colnames(data)
  lmm.data = cbind(data, predictors)
  
  # prepare parallel computation
  cl=detectCores()-2
  if(cl<1){cl=1}
  myCluster <- makeCluster(cl, type = "PSOCK") # create cluster for parallelization, using all available cores minus one
  clusterExport(myCluster, c('lmm.data', 'elements', 'formula', 'predictors', 'warmup', 'iter', 'autocorrelation'), envir=environment()) # export the relevant information to each core
  registerDoParallel(myCluster)
  
  # algorithm will go through each element and calculate impact of predictors on probability of presence/absence
  element.list = pblapply(elements, cl=myCluster, function(i){
    library(brms)
    library(ggplot2)
    # create formula for this element
    lmm.data$response = lmm.data[,i]
    xx.formula = as.formula(paste(c('response ', formula), collapse = ''))
    fixed.effects = formula
    fixed.effects = gsub(fixed.effects, pattern = '~', replacement = '', fixed = T)
    fixed.effects = gsub(fixed.effects, pattern = ' ', replacement = '', fixed = T)
    fixed.effects = unlist(strsplit(fixed.effects, split = '(', fixed = T))[1]
    fixed.effects = unlist(strsplit(fixed.effects, split = '+', fixed = T))
    
    # create weakly informative priors, either with or without autocorrelation
    if(isTRUE(autocorrelation)){
      m1priors <- c(
        prior(student_t(3, 0, 2.5), class = "Intercept"),
        prior(student_t(3, 0, 2.5), class = "b"),
        set_prior("beta(2,2)", class = "ar")
      )
      
      lmm.data$subject=as.character(lmm.data$subject)
      lmm.data = lmm.data[order(lmm.data$subject),]
      lmm.data$arr = unlist(lapply(unique(lmm.data$subject), function(x){1:nrow(lmm.data[lmm.data$subject==x,])}))
      
      # analysis using autocorrelation and formula
      xx.model = brm(xx.formula, data = lmm.data, 
                     family = bernoulli(), 
                     prior = m1priors,
                     autocor = cor_ar(~ arr | subject, cov = T),
                     warmup = warmup, iter = iter, 
                     cores = 1, chains = 2, seed = 111, 
                     silent = T, refresh = 0)
    }
    
    # in case autocorrelation is not included
    if(isFALSE(autocorrelation)){
      m1priors <- c(
        prior(student_t(3, 0, 2.5), class = "Intercept"),
        prior(student_t(3, 0, 2.5), class = "b")
      )
      
      xx.model = brm(xx.formula, data = lmm.data, 
                     family = bernoulli(), 
                     prior = m1priors,
                     warmup = warmup, iter = iter, 
                     cores = 1, chains = 2, seed = 111, 
                     silent = T, refresh = 0)
    }
    
    # plot! one plot per fixed effect
    plot.list = lapply(fixed.effects, function(y){
      
      # take model data and expand possible combinations for prediction of posterior distribution
      nd = data.frame(xx.model$data[,2:ncol(xx.model$data)])
      nd = lapply(1:ncol(nd), function(x){unique(as.character(nd[,x]))}) # take one example of each fixed effects level
      names(nd) = colnames(xx.model$data[,2:ncol(xx.model$data)])
      nd <- data.frame(expand.grid(nd)) # create every possible combination of fixed effects
      if(nrow(nd)<1000){nd = nd[rep(seq_len(nrow(nd)), round(5000/nrow(nd),0)), ]} # if there are very few cases, amplify them to 5000 so there are enough cases for predicting variation
      
      nd[] <- lapply(nd, function(x) as.character(x))
      nd = data.frame(nd)
      colnames(nd) = names(xx.model$data)[2:ncol(xx.model$data)]
      
      for(x in 1:ncol(nd)){
        suppressWarnings(if(mean(!is.na(as.numeric(as.character(nd[,x])))) == 1){
          nd[,x] = as.numeric(nd[,x])
        })
        suppressWarnings(if(mean(is.na(as.numeric(as.character(nd[,x])))) == 1){
          nd[,x] = as.factor(nd[,x])
        })
      } 
      
      # first model in case fixed effect is continuous
      if(!is.numeric(nd[,y])){
        if(length(fixed.effects)>1){  
          un.fixed = fixed.effects[fixed.effects!=y]
          for(j in un.fixed){
            if(is.factor(nd[,j])){nd[,j]=names(which(table(lmm.data[,j])==max(table(lmm.data[,j]))))}
            if(is.numeric(nd[,j])){nd[,j]=mean(nd[,j])}
          }
        }
        
        f <- predict(xx.model, newdata = nd) # predict posterior values
        xx.post = data.frame(f, nd)
        
        
        xx.post$plot = xx.post[,y]
        
        # create density plots
        density.plot = ggplot(data=xx.post, aes(x=Estimate, group=plot, fill=plot)) + 
          geom_density(aes(x=Estimate, y = ..scaled..), adjust=1, alpha=.4, trim = F) + xlim(-0.1,max(xx.post$Estimate)*1.2) +
          ggtitle(i) + xlab('Estimated Probability') + ylab('Scaled') +labs(fill = y)
        
        xx.estimates = aggregate(xx.post$Estimate, by = list(xx.post$plot), quantile, c(0.025, 0.5, 0.975))
        xx.estimates$x = round(xx.estimates$x, 3)
        
      }
      
      # model in case fixed effect is continuous
      if(is.numeric(nd[,y])){
        
        # prepare data
        nd = xx.model$data[,2:ncol(xx.model$data)]
        nd = lapply(1:ncol(nd), function(x){unique(as.character(nd[,x]))})
        names(nd) = colnames(xx.model$data[,2:ncol(xx.model$data)])
        nd <- data.frame(expand.grid(nd))
        if(nrow(nd)<1000){nd = nd[rep(seq_len(nrow(nd)), round(5000/nrow(nd),0)), ]}
        
        nd[] <- lapply(nd, function(x) as.character(x))
        for(x in 1:ncol(nd)){
          suppressWarnings(if(mean(!is.na(as.numeric(nd[,x]))) == 1){
            nd[,x] = as.numeric(nd[,x])
          })
          suppressWarnings(if(mean(is.na(as.numeric(nd[,x]))) == 1){
            nd[,x] = as.factor(nd[,x])
          })
        } 
        
        f <- predict(xx.model, newdata = nd) # predict posterior distributions
        xx.post = data.frame(f, nd)
        
        
        xx.post$plot = xx.post[,y]
        xx.post$control = NA
        if(length(fixed.effects)>1){  
          un.fixed = fixed.effects[fixed.effects!=y]
          if(length(un.fixed)==1){xx.post$control = xx.post[,un.fixed]}
          if(length(un.fixed)>1){xx.post$control = apply(xx.post[,un.fixed], 1, paste, collapse = '_')}
        }
        
        # plot posterior density
        density.plot = ggplot(data=xx.post, aes(x=plot, y = Estimate, color = control)) + 
          geom_line(aes(x = plot, y=Estimate)) + 
          geom_point(aes(x = plot, y=Estimate)) +
          ggtitle(i) + xlab(y) + ylab('Probability')
        
        xx.estimates = aggregate(xx.post$Estimate, by = list(round(xx.post[,y],0)), quantile, c(0.025, 0.5, 0.975))
        xx.estimates$x = round(xx.estimates$x, 3)
        xx.estimates = data.frame(cbind(xx.estimates[,1:ncol(xx.estimates)-1], xx.estimates$x))
      }
      
      
      cc = list(density.plot, xx.estimates)
      names(cc) = c('plot', 'estimates')
      
      return(cc)
    })
    
    plot.list[[length(plot.list)+1]]=xx.model
    names(plot.list) = c(fixed.effects, 'model')
    
    return(plot.list)
  })
  stopCluster(myCluster)
  names(element.list) = elements
  return(element.list)
}
