## Rewrite Meta-analysis as function

require(reshape2)
require(metafor)
require(tidyverse)


### Function to determine the number of pairwise comparisons available for the studies
compare.eval <- function(meta.data, comparison, site){
  site <- enquo(site)
  comparison <- enquo(comparison)
  ex1 <- meta.data %>% group_by(!!site) %>% summarize(compare.true=length(unique(!!comparison))) %>% filter(compare.true==2) %>% data.frame(.)
  return(nrow(ex1))
}

## Function to extract and join datasets that are both raw and statistics to be joined for meta analysis
meta.eval <- function(meta.data, compare, ids , stats){
  
  ## Identify the closures to prepare for programming  
  comparison <- enquo(compare)
  site <- enquo(ids)
  stat <- enquo(stats)
  
  ## Load in other function for multiDcast
  dcastMult <- function(data, formula, value.var = "value", 
                        funs = list("min" = min, "max" = max)) {
    require(reshape2)
    if (is.null(names(funs)) | any(names(funs) == "")) stop("funs must be named")
    Form <- formula(formula)
    LHS <- as.character(Form[[2]])
    if (length(LHS) > 1) LHS <- LHS[-1]
    temp <- lapply(seq_along(funs), function(Z) {
      T1 <- dcast(data, Form, value.var = value.var, 
                  fun.aggregate=match.fun(funs[[Z]]), fill = 0)
      Names <- !names(T1) %in% LHS
      names(T1)[Names] <- paste(names(T1)[Names], names(funs)[[Z]], sep = "_")
      T1
    })
    Reduce(function(x, y) merge(x, y), temp)
  }
  
  #### Begin Function for comparing datasets
  
  ## function for se
  se <- function(x) { sd(x)/sqrt(length(x))}
  
  ## Identify studies where the comparison is possible
  ex1 <- meta.data %>% group_by(!!site) %>% summarize(compare.true=length(unique(!!comparison))) %>% filter(compare.true==2) %>% data.frame(.)
  
  ## Specify forumla for the comparisons
  #f1 <- reformulate(termlabels = x, response = y)
  f1 <- as.formula(paste(substitute(ids), "~", substitute(compare)))
  
  ## Get summary statistics for raw datasets
  meta.temp1 <- meta.data %>%  filter(!!site %in% ex1[,1]) %>% filter(!!stat =="count") %>% 
    dcastMult(f1, funs=list("mean"=mean,"se"=se, "n"=length), value.var = "Value")  ## calculate mean, se, and length (N)
  meta.temp1[is.na(meta.temp1)] <- 1 ## replace NA values with 1 
  
  ## Get replicate value (N) when one is reported
  raw.replicate <- meta.data %>%  filter(!!site %in% ex1[,1]) %>% filter(!!stat =="count") %>% 
    dcastMult(f1, funs=list("mean"=mean), value.var = "replicate") 
  names(raw.replicate) <- gsub("mean", "n", names(raw.replicate)) ## replace mean lable with N
  missing.n <- na.omit(raw.replicate)## remove instances when no replicate is reported
  
  ## Add replicates to calculated data, unless replicate was not recorded - Then use the calculated replicate value  
  meta.temp1[meta.temp1[,"UniqueSite"] %in% missing.n[,"UniqueSite"],6:7] <- 
    raw.replicate[meta.temp1[,"UniqueSite"] %in% missing.n[,"UniqueSite"],2:3]
  
  ## Get statistic data from manuscripts in format to join
  #f2 <- reformulate(termlabels = c(x,z), response = y)
  f2 <- as.formula(paste(substitute(ids), "~", substitute(compare),"*",substitute(stats)))
  
  meta.temp2 <- meta.data %>%  filter(!!site %in% ex1[,1]) %>% filter(!!stat =="mean" | !!stat =="se") %>% 
    dcastMult(f2, funs=list("mean"=mean), value.var = "Value") ## extracted mean and SE
  
  ## Get replicate value (N)
  meta.temp3 <- meta.data %>%  filter(!!site %in% ex1[,1]) %>% filter(!!stat =="mean") %>% dcastMult(f1, funs=list("mean"=mean), value.var = "replicate") 
  names(meta.temp3) <- gsub("mean", "n", names(meta.temp3)) ## replace mean lable with N
  meta.temp2 <- merge(meta.temp2,meta.temp3, by=c("UniqueSite"))
  
  ## Output the two datasets for raw and stat values
  return(list(meta.temp1, meta.temp2))
  
}
