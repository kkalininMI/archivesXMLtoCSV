% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fromXMLtoCSV.R
\name{fromXMLtoCSV}
\alias{fromXMLtoCSV}
\title{XML to CSV conversion function}
\usage{
fromXMLtoCSV(filename, item_nodes, collection_nodes, excludeFiles = NULL,
  ...)
}
\arguments{
\item{filename}{name of XML file or vector of XML filenames.}

\item{item_nodes}{list of item nodes defining path, vector of nodes and node types (text/attrs).}

\item{collection_nodes}{list of collection nodes defining path, vector of nodes and vector of node types (text/attrs).}

\item{excludeFiles}{vector of filenames excluded from XML to CSV conversion.}

\item{...}{auxiliary parameters.}
}
\value{
Returns the dataframe object with rows (observations matching item/collection nodes) and columns (item nodes).
}
\description{
The package helps to convert the EAD XML data into the table using XPath expressions. Define item nodes (i.e. files, items etc.) and collection nodes (information about collection) to extract the data.
}
\examples{
item_nodes = list(path = "(//c)|(//c01)|(//c02)|(//c03)",
                  nodes = c("primarynode", ".//unittitle", "./did//unitdate"),
                  types = c("attrs", "text", "text"))
# "primarynode" extracts the data from the root nodes.

collection_nodes <- list(path="archdesc[@level='collection']",
                        nodes=c(".//unittitle", "./did//unitdate"),
                        types=c("text", "text"))

filedata <- fromXMLtoCSV(system.file("rusdata.xml", package="archivesXMLtoCSV"),
                         item_nodes, collection_nodes)
}
