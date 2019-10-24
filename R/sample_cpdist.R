#' Sampling from posterior distribution of a Bayesian network at a node of interest
#'
#' Take a bayesian network, a node of interst, and a return value type option and returns either the
#' probability/sampled data (depending on the op argument specified) of the different  states of the
#' node of interst given the different conbinaition of its and its parents states.
#'
#' @author Issoufou Liman
#' @param bn an object of class bn.fit.
#' @param node character string, the label of the node which conditional distribution is of interest.
#' @param op a vector of character strings, the type of returned value: either probabilities or raw data sampled from the posterior distribution
#' @param evidence a name value pair:a named character vector which values are node states and names are node names.
#' @param n_generation how far to go in the network topology for building the conditionning specification for the query?
#' @param include_relatives logical Should parents or ancestors, depending on the the argument n_generation, should be included in the query?
#' If TRUE, the default, these will be internally involved in constructing the evidence argument.
#' @param n_try integer \code{\link[bnlearn]{cpquery}} often fails, when the argument evidence involves
#'  deep query, returning NULL. n_try can be used to retry the function call n_try times.
#' @param n_run integer specifying the number of of model run. Default is 1000.
#' @return A matrix containing either the probabilities or raw data sampled from the posterior distribution
#' @details Each row consists in a comditional probability of a state of the node of interest given the combination of the states of the parent nodes.
#' @seealso \code{\link[bnlearn]{cpquery}}
#' @examples
#' library (gRain)
#' library(bnlearn)
#' ## setting a bayesian network with gRain
#' Soil_type <- cptable (~Soil_type, values = c(0.05, 0.55, 0.4),
#' levels = c('Sandy', 'Loamy', 'Clayey'))
#' Manure_application <- cptable(~Manure_application, values = c(0.3, 0.7),
#' levels = c('FALSE', 'TRUE'))
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
#' ## converting the grain bayesian network to bn.fit
#' network <- as.bn.fit(network)
#' sample_cpdist (network, 'Soil_water_holding_capacity')
#' @importFrom bnlearn nodes cpdist as.grain parents ancestors
#' @importFrom gRain nodeStates
#' @export
#' ## Compile conditional probability tables
#' network <- compileCPT(list(Soil_type, Manure_application, Soil_water_holding_capacity))
#' ## Graphical Independence Network ####
#' network <- grain(network)
#' ## converting the grain bayesian network to bn.fit
#' network <- as.bn.fit(network)
#' sample_cpdist (network, 'Soil_water_holding_capacity')
#' @importFrom bnlearn nodes cpdist as.grain parents ancestors
#' @importFrom gRain nodeStates
#' @export
sample_cpdist <- function(bn, node, op = c("sampler", "proba"), evidence = NULL,
                          n_generation = NULL, include_relatives = TRUE, n_try = 10, n_run =1000) {
    op <- match.arg(op)
    bn <- check_bn(bn, include_cpt = TRUE)

    if(!is.null(evidence)){
        evidence <- check_bn_node_states(bn = bn, evidence = evidence)
    }

    if(include_relatives){
        # getting all nodes before the target
        if (is.null(n_generation)) {
            query_list <- parents(x = bn, node = node)
        } else {
            pedigree <- ancestors(x = bn, node = node)
            if(n_generation > length(pedigree)){
                n_generation <- length(pedigree)
            }
            query_list <- pedigree[1:n_generation]
        }
        query_list <- nodeStates(as.grain(bn), query_list)
    }

    if (!is.null(evidence)){
        if(!include_relatives){
            query_list <- nodeStates(as.grain(bn), names(evidence))
        }
        ## all evidence names within parents or ancestor should be replaced by evidence
        query_list[names(query_list) %in% names(evidence)] <- evidence[names(evidence)  %in% names(query_list)]

        # evidence outside of parents or ancestor should be accounted for
        query_list <- c(query_list, evidence[!(names(evidence)  %in% names(query_list))])

    }
    query_list <- expand.grid(query_list, stringsAsFactors = FALSE)
    prior <- query_list

    .sample_cpdist <- function(bn, node, op = c("sampler", "proba"),
                               n_try = 10) {

        query_list <- sapply(1:nrow(query_list), function(i) {
            if(ncol(query_list) == 1){
                evidence <- query_list[i, , drop = FALSE]
            } else {
                evidence <- query_list[i, ]
            }
            evidence <- make_bnlearn_evidence(bn = bn, evidence = evidence)

            n_try_on_failure <- 0

            recursive_try <- function(){
                cmd <- paste0("cpdist", "(", "fitted", "=", "bn", ",", "nodes", "=", "node", ",", "evidence", "=",
                              evidence, ")")
                out <- eval(parse(text = cmd))
                # print(n_try_on_failure)
                if((nrow(out) == 0) & (n_try_on_failure != n_try)){
                    n_try_on_failure <<- n_try_on_failure + 1
                    # print(n_try_on_failure)
                    out <-  recursive_try ()
                }
                return(out)
            }

            out <- recursive_try ()

            if(nrow(out) > 0){
                if (op == "proba") {
                    out <- table(out)
                    somme <- sum(out)
                    if (somme > 0) {
                        out <- out/somme
                        out <- t(as.matrix(out))
                        rownames(out) <- paste(node, "|", evidence)
                    }
                } else {
                    colnames(out) <- paste(node, "|", evidence)
                }
                return(out)
            }
        }, simplify = FALSE)

        clean_up <- !(sapply(query_list, is.null))
        query_list <- query_list[clean_up]
        # prior <- prior[clean_up, ]
        if (op == "proba") {
            query_list <- do.call(rbind, query_list)
        }
        # query_list <- list(prior = prior, posterior = query_list)
        # class(query_list) <- "sample_cpdist"
        return(query_list)
    }

    query_list <- sapply(1:n_run, function(i){
        .sample_cpdist(bn=bn, node=node, op = op, n_try = n_try)
    }, simplify = FALSE)
    query_list <- do.call(rbind, query_list)
    query_list <- list(prior = prior, posterior = query_list)
    class(query_list) <- "sample_cpdist"
    return(query_list)
}
