#' Make and format Conditional Probability table (CPT) for use in Bayesian Network
#'
#' Take the required arguments for \code{\link[decisionSupport]{make_CPT}}, compute the nodes states probabilities and format them as ready to use inputs for \code{\link[gRain]{cptable}}.
## #' @param parent_effects see \code{\link[decisionSupport]{make_CPT}} for detail.
## #' @param parent_weights see \code{\link[decisionSupport]{make_CPT}} for details.
## #' @param b see \code{\link[decisionSupport]{make_CPT}} for details.
## #' @param child_prior see \code{\link[decisionSupport]{make_CPT}} for details.
## #' @param ranking_child see \code{\link[decisionSupport]{make_CPT}} for details.
## #' @param child_states see \code{\link[decisionSupport]{make_CPT}} for details.
## #' @param parent_names see \code{\link[decisionSupport]{make_CPT}} for details.
## #' @param parent_states see \code{\link[decisionSupport]{make_CPT}} for details.
#' @inheritParams decisionSupport::make_CPT
#' @param option character string. CPT formatting option; either 'grain' or "bnlearn" (NOT currently implemented).
#' @return A matrix containing the Conditional probabilities.
#' @details \code{\link[decisionSupport]{make_CPT}} does not seems to work well     with simple case (i.e. single parent - single child relationship) which case     does not worth it!
#' @seealso \code{\link[decisionSupport]{make_CPT}}.
#' @references
#' Sjoekvist S & Hansson F, 2013. \emph{Modelling expert judgement into a Bayesian
#' Belief Network - a method for consistent and robust determination of
#' conditional probability tables}. Master's thesis, Faculty of Engineering, Lund
#' University; \url{http://lup.lub.lu.se/luur/download?func=downloadFile&recordOId=3866733&fileOId=3866740}
#'
#' Eike Luedeling and Lutz Goehring (2018). decisionSupport: Quantitative
#' Support of Decision Making under Uncertainty. R package version 1.103.8.
#' \url{https://CRAN.R-project.org/package=decisionSupport}
#' @examples
#' library (gRain)
#' ## Simple nodes specification using gRain package.
#' Soil_type <- cptable (~Soil_type, values = c(0.05, 0.55, 0.4),
#' levels = c('Sandy', 'Loamy', 'Clayey'))
#' Manure_application <- cptable(~Manure_application, values = c(0.3, 0.7),
#' levels = c('FALSE', 'TRUE'))
#' ## Complex nodes specification.
#' Soil_water_holding_capacity_tmp <- make_gRain_CPT(
#'  parent_effects = list(c(0, 2.5, 3), c(0, 2)),
#'  parent_weights = c(2,1),
#'  b = 3,
#'  child_prior = c(0.2,0.5,0.3),
#'  child_states = c('Low', 'Medium', 'High'),
#'  parent_states = list(c('Sandy', 'Loamy', 'Clayey'), c('FALSE', 'TRUE'))
#' )
#' Soil_water_holding_capacity_values <- Soil_water_holding_capacity_tmp$values
#' Soil_water_holding_capacity_levels <- Soil_water_holding_capacity_tmp$levels
#' Soil_water_holding_capacity <- cptable (
#' ~Soil_water_holding_capacity|Soil_type:Manure_application,
#' values = Soil_water_holding_capacity_values,
#' levels = Soil_water_holding_capacity_levels)
#' ## Compile conditional probability tables
#' network <- compileCPT(list(Soil_type, Manure_application, Soil_water_holding_capacity))
#' ## Graphical Independence Network ####
#' network <- grain(network)
#' network
#' plot (network)
#' @importFrom decisionSupport make_CPT
#' @importFrom gRain cptable
#' @export
make_gRain_CPT <- function(parent_effects, parent_weights, b, child_prior,
  ranking_child = NULL, child_states = NULL, parent_names = NULL,
  parent_states = NULL, option = c('grain', 'bnlearn')){
  option <- match.arg(option) # only work for gRain::cptable for now. I will add an option for bnlearn::custom.fit
  # computing the Cconditional probabilities using decisionSupport::makeCPT
  test <- make_CPT(parent_effects=parent_effects, parent_weights=parent_weights, b=b, child_prior=child_prior, ranking_child = ranking_child,
    child_states =child_states, parent_names = parent_names, parent_states = parent_states)
  # create an appropriate size array for gRain or bnlearn CPT
  dim <- c(length(child_states), sapply(parent_states, length))
  ar <- array(data = rep(NA, length(test)),
    dim = dim,
    dimnames = c(list(child_states), parent_states))
  # making the array indexing by extracting the different combinaisons of node states by:
  ## first extracting the combinaisons of parents' states;
  tmp <- as.list(as.data.frame(test$column_legend, stringsAsFactors = FALSE))
  tmp <- lapply(tmp, as.character)
  ## second combining each set of parents states with a child state.
  ar_ids <-lapply(tmp, function(j){
    lapply(rownames(test$CPT), function(i){
      c(i, j)
    })
  })
  ar_ids <- unlist(ar_ids, recursive = FALSE)
  # filling the array
  for(i in 1:length(ar_ids)){
    ar[matrix(ar_ids[[i]], 1)] <- as.numeric(test$CPT)[i]
  }
  if (option == 'grain'){
    ar <- as.numeric(ar)
    ar <- list (values=ar, levels = rownames(test$CPT))
  }
  return(ar)
}
