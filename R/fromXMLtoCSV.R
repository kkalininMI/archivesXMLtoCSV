#' @title XML to CSV conversion function
#' @description The package helps to convert the EAD XML data into the table using XPath expressions. Define item nodes (i.e. files, items etc.) and collection nodes (information about collection) to extract the data.
#' @param filename name of XML file or vector of XML filenames.
#' @param item_nodes list of item nodes defining path, vector of nodes and node types (text/attrs).
#' @param collection_nodes list of collection nodes defining path, vector of nodes and vector of node types (text/attrs).
#' @param excludeFiles vector of filenames excluded from XML to CSV conversion.
#' @param ... auxiliary parameters.
#' @export
#' @return Returns the dataframe object with rows (observations matching item/collection nodes) and columns (item nodes).
#' @examples
#' item_nodes = list(path = "(//c)|(//c01)|(//c02)|(//c03)",
#'                   nodes = c("primarynode", ".//unittitle", "./did//unitdate"),
#'                   types = c("attrs", "text", "text"))
#' # "primarynode" extracts the data from the root nodes.
#'
#' collection_nodes <- list(path="archdesc[@@level='collection']",
#'                         nodes=c(".//unittitle", "./did//unitdate"),
#'                         types=c("text", "text"))
#'
#' filedata <- fromXMLtoCSV(system.file("rusdata.xml", package="archivesXMLtoCSV"),
#'                          item_nodes, collection_nodes)

fromXMLtoCSV <- function(filename, item_nodes, collection_nodes, excludeFiles=NULL, ...){

  if (is.vector(filename) & length(filename)>1){

    counter <- 1
    bigdataRows <- 1
    bigdataCols <- 1

    bigdata<-data.frame(matrix(NA, bigdataRows, bigdataCols))

    for (iter in 1:length(filename)){

      if(!is.null(excludeFiles)){
        if(filename[iter]%in%excludeFiles) next
      }

      print(paste("index: ", iter, "  filename: ", filename[iter], sep=""))
      dat_frag <- convXMLtoCSV(filename[iter], item_nodes, collection_nodes);

      if(iter==1){hnames<-colnames(dat_frag)}

      for(iter2 in 1:dim(dat_frag)[2]){
        if(!names(dat_frag)[iter2]%in%hnames){
          hnames<-c(hnames, names(dat_frag)[iter2])}

        bigdata[counter:((counter+dim(dat_frag)[1])-1),
                which(hnames%in%names(dat_frag)[iter2])] <- dat_frag[,iter2]

      }

      counter=counter+dim(dat_frag)[1]

      gc()
    }

    colnames(bigdata) <- hnames
    extracted_data <- bigdata[!is.na(bigdata[,1]),];bigdata <- bigdata[,!is.na(colnames(bigdata))]
  }else{
    print(paste("index: ", 1, "  filename: ", filename, sep=""))

    extracted_data <- convXMLtoCSV(filename=filename,
                                   item_nodes=item_nodes,
                                   collection_nodes=collection_nodes)
  }
  return(extracted_data)}
