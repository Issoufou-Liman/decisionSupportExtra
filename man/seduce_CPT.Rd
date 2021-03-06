% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/seduce_CPT.R
\name{seduce_CPT}
\alias{seduce_CPT}
\title{Orchestrate conditional probability table for Bayesain network node based on gross evidence}
\usage{
seduce_CPT(bn, target_child = NULL, target_parents = NULL)
}
\arguments{
\item{bn}{an object of class \code{\link[bnlearn]{bn.fit}} or \code{\link[gRain]{grain-main}}.}

\item{target_child}{character string specifying the node for which
the conditional probability table is requested.}

\item{target_parents}{character vector specifying  the nodes based on which
evidence are to be constructed to query bn at target_child.}
}
\description{
Create conditional probability table for a node based on a set of known nodes
}
\details{
While invalid nodes are silently dropped,
bn is retuned as is, with a warning, when both target_child and target_parents are set to NULL.
}
\examples{
library (gRain)
library(bnlearn)
## setting a bayesian network with gRain
Soil_type <- cptable (~Soil_type, values = c(0.05, 0.55, 0.4),
levels = c('Sandy', 'Loamy', 'Clayey'))
Manure_application <- cptable(~Manure_application, values = c(0.3, 0.7),
levels = c('FALSE', 'TRUE'))
Soil_water_holding_capacity_tmp <- make_gRain_CPT(
 parent_effects = list(c(0, 2.5, 3), c(0, 2)),
 parent_weights = c(2,1),
 b = 3,
 child_prior = c(0.2,0.5,0.3),
 child_states = c('Low', 'Medium', 'High'),
 parent_states = list(c('Sandy', 'Loamy', 'Clayey'), c('FALSE', 'TRUE'))
)
Soil_water_holding_capacity_values <- Soil_water_holding_capacity_tmp$values
Soil_water_holding_capacity_levels <- Soil_water_holding_capacity_tmp$levels
Soil_water_holding_capacity <- cptable (
~Soil_water_holding_capacity|Soil_type:Manure_application,
values = Soil_water_holding_capacity_values,
levels = Soil_water_holding_capacity_levels)
## Compile conditional probability tables
network <- compileCPT(list(Soil_type, Manure_application, Soil_water_holding_capacity))
## Graphical Independence Network ####
network <- grain(network)
## Use grain object (gRain package)
seduce_CPT (network, 'Soil_water_holding_capacity', c( 'Soil_type'))
seduce_CPT (network, 'Soil_water_holding_capacity', c( 'Manure_application'))
seduce_CPT (network, 'Soil_water_holding_capacity', c( 'Soil_type', 'Manure_application'))

## converting the grain bayesian network to bn.fit
network_bn_fit <- as.bn.fit(network)
## Use bn.fit object (bnlearn package)
seduce_CPT (network_bn_fit, 'Soil_water_holding_capacity', c( 'Soil_type'))
seduce_CPT (network_bn_fit, 'Soil_water_holding_capacity', c( 'Manure_application'))
seduce_CPT (network_bn_fit, 'Soil_water_holding_capacity', c( 'Soil_type', 'Manure_application'))
}
\author{
Issoufou Liman
}
