# @title XML to CSV internal conversion function
# @description This function helps to implement XML to CSV conversion
# @param filename name of XML file or vector of XML filenames.
# @param item_nodes list of item nodes defining path, vector of nodes and node types (text/attrs).
# @param collection_nodes list of collection nodes defining path, vector of nodes and vector of node types (text/attrs).
#' @import xml2
#' @import plyr
# @return Returns the dataframe object with rows (observations matching item/collection nodes) and columns (item nodes).


convXMLtoCSV <- function(filename,  item_nodes, collection_nodes){

  nodeExtract <- function(data, extract_formula, path=FALSE,  types){
    if(extract_formula=="primarynode"){node_attr <- xml_attrs(data)}else{
      if(types=="text"){node_attr <- xml_text(xml_find_first(data, extract_formula))}
      if(types=="attrs"){node_attr <- xml_attrs(xml_find_first(data, extract_formula))}
    }
    node_names <- unique(unlist(lapply(node_attr, function(x){names(x)})))
    if(length(node_names)>1){
      node_attr2 <- lapply(node_attr, function(x){
        re_list <- rep(NA, length(node_names))
        for(i in 1:length(node_names)){
          if(node_names[i]%in%names(x)){re_list[i] <- x[names(x)%in%node_names[i]]}
        }
        return(re_list)})
      node_res <- data.frame(matrix(unlist(node_attr2), ncol=length(node_names), byrow=T), stringsAsFactors = FALSE)}else{
        node_res <- node_attr
      }
    if(is.data.frame(node_res)){colnames(node_res) <-
      sapply(1:length(node_res),
             function(x) paste(gsub("[^[:alnum:]]", "", extract_formula),
                               types, x, sep="."))}else{
                                 names(node_res) <- paste(gsub("[^[:alnum:]]", "", extract_formula),
                                                          types, sep=".")}
    return(node_res)}

  collectionExtract <- function(data, extract_formula, path=FALSE,  types){
    if(path==TRUE){node_attr <- xml_attrs(data)}else{
      if(types=="text"){node_attr <- xml_text(xml_find_first(data, extract_formula))}
      if(types=="attrs"){node_attr <- xml_attrs(xml_find_first(data, extract_formula))}
    }
    node_names <- unique(unlist(lapply(node_attr, function(x){names(x)})))
    if(length(node_names)>1){
      node_attr2 <- lapply(node_attr, function(x){
        re_list <- rep(NA, length(node_names))
        for(i in 1:length(node_names)){
          if(node_names[i]%in%names(x)){re_list[i] <- x[names(x)%in%node_names[i]]}
        }
        return(re_list)})
      node_res <- data.frame(matrix(unlist(node_attr2),
                                    ncol=length(node_names), byrow=T),
                             stringsAsFactors = FALSE)}else{
                               node_res <- node_attr
                             }
    if(is.data.frame(node_res)){
      if(dim(node_res)[2]>1){colnames(node_res) <- sapply(1:length(node_res),
                                                          function(x) paste(gsub("[^[:alnum:]]", "",
                                                                                 extract_formula), types, x, sep="."))}}else{
                                                                                   names(node_res) <- paste(gsub("[^[:alnum:]]", "", extract_formula), types, sep=".")}
    return(node_res)}

  fromListtoDataFrame <- function(any_list){
    num_cols <- Reduce("+",
                       lapply(any_list, function(x){d <- dim(x)[2]; d[is.null(d)] <- 1; return(d)}))
    if(is.data.frame(any_list[[1]])){num_rows <- nrow(any_list[[1]])}else{num_rows <- length(any_list[[1]])}

    any_data.frame <- data.frame(matrix(NA, num_rows, num_cols))

    cnames=vector()
    extr_names <- sapply(1:length(any_list),
                         function(x){
                           if(is.data.frame(any_list[[x]])){
                             cnames=c(cnames, colnames(any_list[[x]]))
                           }else{cnames=c(cnames, names(any_list[[x]][1]))}
                         })
    if(any(unlist(lapply(extr_names,function(x)length(x)==0)))){extr_names <- names(any_list)}
    i=1; c=1
    while(i<=length(any_list)){
      d <- any_list[[i]]
      if(is.data.frame(d)){any_data.frame[,c:(c+dim(d)[2])] <- d; c=c+dim(d)[2]}else{
        if(is.list(d)){
          d <- unlist(lapply(d, function(x) if(length(x)==0) return(NA) else(x)))
        }
        any_data.frame[,c] <- d; c=c+1}
      i=i+1
    }
    colnames(any_data.frame) <- unlist(extr_names)
    return(any_data.frame)
  }


  xmldata <- xml_ns_strip(read_xml(filename, as_html = FALSE, options = "NOBLANKS"))


  collection_node_data <- xml_find_all(xmldata, collection_nodes$path)
  collection_list <- list()
  collection_list <- sapply(1:length(collection_nodes$nodes), function(x)
    collection_list[[x]] <- collectionExtract(collection_node_data,
                                              extract_formula=collection_nodes$nodes[x],
                                              types=collection_nodes$types[x]))
  collection_list <- lapply(collection_list, function(x) if(length(x)==0) return(NA)else(x))
  collection_data.frame <- fromListtoDataFrame(collection_list)
  collection_data.frame$level="collection"

  items_node_data <- xml_find_all(xmldata, item_nodes$path)

  if(length(items_node_data)!=0){
    items_list <- lapply(1:length(item_nodes$nodes), function(x)
      nodeExtract(items_node_data,
                  extract_formula=item_nodes$nodes[x],
                  types=item_nodes$types[x],
                  path=item_nodes$path[x]))

    items_data.frame <- fromListtoDataFrame(items_list)

    colnames(items_data.frame)%in%colnames(collection_data.frame)}else{
      items_data.frame=data.frame()
    }

  extracted_data <- rbind.fill(collection_data.frame, items_data.frame)
  extracted_data <- data.frame(filename, extracted_data, stringsAsFactors = FALSE)
  extracted_data$level <- as.character(extracted_data$level)

  if(any(apply(items_data.frame, 2, function(x) any(grepl("^file|^item|^series", x))))){
    extracted_data$level[2:length(extracted_data$level)] <-  items_data.frame[,
                                                                              which.max(apply(items_data.frame, 2, function(x)
                                                                                sum(grepl("^file|^item|^series", x))))]
  }
  return(extracted_data)}
