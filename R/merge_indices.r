merge.dictionaries <- function(dictionary.base, dictionary){
  # Returns:
  #  - A new dictionary
  #  - A mapping vector to map elements from the second dictionary to the base dictionary

  dictionary.global <- dictionary.base
  last.pos <- length(dictionary.global)
  mapping <- vector()
  for(i in 1:length(dictionary)){
    dupl <- FALSE
    
    # search a similar motif in the dictionary
    for(j in 1:length(dictionary.base)){
      if(vcount(dictionary[[i]]) != vcount(dictionary.base[[j]])){
        next
      }
      if(is_isomorphic_to(dictionary[[i]], dictionary.base[[j]], method='vf2')){
        # if already in the base dict, save a pointer to it
        dupl <- TRUE
        mapping[i] <- j
        cat("\n", i, " -> ", j)
        break
      }
    }
    
    # if not found among motifs in the dictionary, give it a new entry
    if(!dupl){
      new.pos <- last.pos + 1
      dictionary.global[[new.pos]] <- dictionary[[i]] # copy motif graph
      mapping[i] <- new.pos
      cat("\nnew ", i, " -> ", new.pos)
      last.pos <- last.pos + 1
    }
  }
  
  return(list(dict = dictionary.global, 
              mapping = mapping))
}