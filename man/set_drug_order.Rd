% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/drug_order.R
\name{set_drug_order}
\alias{set_drug_order}
\title{Set the order of drug levels in the overextension data.}
\usage{
set_drug_order(n, drug_column, ...)
}
\arguments{
\item{n}{A data frame containing a column named 'drug'.}

\item{drug_column}{The name of the column that contains the drugs, in quotation marks}

\item{...}{list the name of each drug in the order you want them. Each should be in
quotation marks with a comma separating them.}
}
\value{
The input data frame with the 'drug' column converted to an \strong{ordered factor}
with levels set according to the list provided for ...
}
\description{
This function reorders the levels of the 'drug' factor column in a data frame.
You can either provide a specific order for the levels or let the function
use the unique drug names in the order they appear in the data.
}
\examples{

# Example usage:
# ordered_data_drug <- set_drug_order(example_data, "drug", "DMSO", "Taxol", "Colchicine")
# levels(ordered_data_drug$drug)
}
