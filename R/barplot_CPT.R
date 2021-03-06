#' A ggplot wrapper for visualizing Conditional Probability Table (CPT)
#'
#' Take a bayesian network, a node of interest along with other optional ggplot paramaters to produce a barchart corresponding to the CPT of the node of interest given the Bayesian network.
#'
#' @author Issoufou Liman
#' @param bn A grain object from gRain package (see \code{\link[gRain]{grain-main}}) or a \code{\link[bnlearn]{bn.fit}} object from bnlearn package.
#' @param target_node A single character indicating the name of the node which CPT is to be plotted.
#' @param bar_width numeric. The width of the bars in the barplot (see \code{\link[ggplot2]{geom_bar}}).
#' @param ncol_facet A single integer specifying the number of facet columns (see ncol argument in \code{\link[ggplot2]{geom_bar}}).
#' @param n_pages A single integer specifying the number of pages over which the plot is be spread over. This can be particularly useful for lisibility in large CPT plot.
#' @param show_states_only logical. Should both node names and their respective states be shown in the strip labels? If TRUE only the state labels are shown.
#' @param separator character or regular expression. How should node names and their states be separated?
#' @return A single ggplot object (or a list of these, if ncol_facet > 1) such that one could customize it with \code{\link[ggplot2]{theme}} and other ggplot updating utilites.
#' @details It often difficult to know whether something went wrong or not looking at the CPT generated by \code{\link[decisionSupport]{make_CPT}}. \code{\link[decisionSupportExtra]{barplot_CPT}} may help to graphically visualise the generated CPT by \code{\link[decisionSupportExtra]{make_gRain_CPT}} and fix potential mis-specification. This function make visualization and examination of these CPTs easier.
#' @seealso \code{\link[decisionSupportExtra]{make_gRain_CPT}}.
#' @examples
#' library (gRain)
#' library(ggplot2)
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
#' barplot_CPT (network, "Soil_water_holding_capacity")
#' @importFrom methods is
#' @importFrom stats as.formula
#' @importFrom reshape2 melt
#' @import ggplot2
#' @export
barplot_CPT <- function(bn, target_node, bar_width = 0.25, ncol_facet=NULL, n_pages = 1, show_states_only = FALSE, separator = "\n"){
    if(is(bn, "cpt_grain")){
        x <- melt(bn[["cptlist"]][[target_node]])
    } else if (is(bn, "bn.fit")){
        x <- as.data.frame(bn[[target_node]][['prob']])
    }
    if(is.null(ncol_facet)){
        ncol_facet <- (ncol(x)-2)
    }
    my_theme <- theme_minimal() +
        theme(text = element_text(family = 'serif', face = 'plain'),
              panel.spacing=unit(0.075, "lines"),
              panel.border = element_rect(color = "lightgrey", fill = NA, size = 0.5),
              axis.ticks = element_line(colour = 'black', size = 0.05),
              legend.title = element_blank(),
              legend.position="top",
              legend.justification = 'right',
              legend.margin=margin(0, 0, 0, 0),
              legend.box.margin=margin(-22, 0, -10, 0),
              strip.background = element_rect(color = "gray", size = 0.075),
              strip.text = element_text(colour = 'black'),
              plot.subtitle=element_text(size=9.5, face="italic", color="blue")
        )

    fmt_dcimals <- function(decimals=0){
        # return a function responpsible for formatting the
        # axis labels with a given number of decimals
        function(x) as.character(round(x,decimals))
    }

    tmp <- names(x)[2:(ncol(x)-1)]

    facet_formula <- paste(tmp, collapse = '+')
    facet_formula <- paste0(".", '~', facet_formula)
    facet_formula <- as.formula(facet_formula)
    p <- function(data){

        g <- ggplot(data = data, aes_string(x=names(data)[1], y=names(data)[ncol(data)]))+
            geom_bar(stat = 'identity', width = bar_width)+
            coord_flip()+
            scale_y_continuous(labels = fmt_dcimals(2), breaks = scales::pretty_breaks(n=9), expand = c(0, 0.015)) +
            my_theme
        if(ncol(data) > 2){
            title = 'Conditional Probabilities'
            subtitle <- gsub(pattern = '_', replacement = ' ', names(x)[1])
            given_nodes <- gsub(pattern = '_', replacement = ' ', paste(names(x)[2:(ncol(x)-1)], collapse = ' : '))
            subtitle <- paste0(subtitle, ' / ', given_nodes)
            g = g +
                facet_wrap(facet_formula,
                           labeller = function(labs) {
                               if(show_states_only){
                                   label_value(labs, multi_line = FALSE)

                               } else {
                                   label_both(labs, multi_line = FALSE, sep = separator)
                               }
                           },
                           ncol = ncol_facet)
        } else {
            title <- gsub(pattern = '_', replacement = ' ', target_node)
            subtitle <- NULL
        }

        g + labs(title=title, subtitle=subtitle,
                 x='Node States', y='Probabilities')
    }

    if(n_pages > 1){
        pages_range <- ceiling(nrow(x)/n_pages)
        pages_range <- c(seq.int(from = 1, to = nrow(x), by = pages_range), nrow(x))
        pages_range <- c((pages_range[2:(length(pages_range)-1)])-1, pages_range)
        pages_range <- sort(unique(pages_range), decreasing = FALSE)
        pages_range <- matrix(pages_range, ncol = 2, byrow = T)
        pages_range <- as.list(data.frame(t(pages_range)))
        rows2match <- lapply(pages_range, function (i) {
            x[i[1]:i[2], ]
        })
        lapply(rows2match,p)
    } else {
        p(x)
    }

}
