#' An Rgraphviz wrapper for visualizing Bayesian Networks causality.
#'
#' Produce a bayesian network graph indicating the relationship between the nodes
#'
#' @author Issoufou Liman
#' @param bn A grain object from gRain package (see \code{\link[gRain]{grain-main}}) or a \code{\link[bnlearn]{bn.fit}} object from bnlearn package.
#' @inheritParams bnlearn::graphviz.plot
#' @param cex numeric value indicating the scaling factor for text and symbols relative to the default.
#' @param plot_bg Color to be used as plot background.
#' @param highlit_some_nodes,highlit_some_nodes_col vectors of respectively nodes to highlight and highlighting color.
#' @param node_fill,node_lty,node_lwd,node_col,edge_fill,edge_lty,edge_lwd,edge_col node fill, line type, line width and color for nodes and edge. see \code{\link[graphics]{par}}.
#' @param split_labels_accross_lines Should node label be split across multiple lines for better view? If TRUE (default) then node labels are split across line based on line_leng.
#' @param line_leng integer, the number of character to consider per line in the node labels if split_labels_accross_lines = TRUE.
#' @param textCol the color of the text.
#' @param abbreviate should node lables be abbreviated?
#' @param leg_ncol,leg_w,leg_cex,leg_bg,leg_box.col,leg_title.col Number of column (leg_ncol), width (leg_w), cex magnifier (leg_cex), background (leg_bg), box color (leg_box.col), text color (leg_title.col) of the legend to account for abbreviation (if abreviate is set to TRUE).
#' @seealso \code{\link[bnlearn]{graphviz.plot}}.
#' @examples
#' library(Rgraphviz)
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
#' ## plot the grain as graph
#' graphviz_plot_net(network, shape="ellipse", layout = "dot",
#' cex = 0.5, plot_bg = "#DFE3EE", highlit_some_nodes = "Soil_water_holding_capacity",
#' highlit_some_nodes_col = "red", abbreviate = FALSE, node_col = "white", node_lwd = 1)
#'
#' ## converting the grain bayesian network to bn.fit
#' network_bn_fit <- as.bn.fit(network)
#' ## plot the bn.fit as graph
#' graphviz_plot_net(network_bn_fit, shape="ellipse", layout = "dot",
#' cex = 0.5, plot_bg = "#DFE3EE", highlit_some_nodes = "Soil_water_holding_capacity",
#' highlit_some_nodes_col = "red", abbreviate = FALSE, node_col = "white", node_lwd = 1)

#'## extracting the string model
#'string_model <- modelstring(network_bn_fit)
#' ## plot the bn as graph
#' graphviz_plot_net(string_model, shape="ellipse", layout = "dot",
#' cex = 0.5, plot_bg = "#DFE3EE", highlit_some_nodes = "Soil_water_holding_capacity",
#' highlit_some_nodes_col = "red", abbreviate = FALSE, node_col = "white", node_lwd = 1)

#' ## extract the entire Bayesian network as it is
#' extract_bn(network_bn_fit, modelstring(as.bn.fit(network)))
#' @importFrom bnlearn as.bn.fit nodes graphviz.plot model2network
#' @importMethodsFrom bnlearn nodes<-
#' @importFrom graphics strwidth legend
#' @importFrom graph nodeRenderInfo<- edgeRenderInfo<-
#' @importFrom Rgraphviz renderGraph
#' @export graphviz_plot_net
graphviz_plot_net  <- function(bn, shape = "rectangle", layout ="dot", cex = 0.5, plot_bg = "lightgrey", line_leng = 10,
                               node_fill = "white", highlit_some_nodes = NULL, highlit_some_nodes_col='yellow',
                               node_lty=1, node_lwd=2, node_col="white",edge_fill="white",
                               edge_lty=1, edge_lwd=3, edge_col="white", split_labels_accross_lines=TRUE,
                               textCol="darkred", abbreviate=TRUE, leg_ncol=1, leg_w=strwidth("1,000,000"),
                               leg_cex=0.5, leg_bg="lightgrey", leg_box.col="white", leg_title.col = "white"){
  .pardefault <- par(no.readonly = TRUE)
  bn <- check_bn (bn, include_cpt = FALSE)
  nam0 <- nodes(bn)
  nam1 <- gsub("_"," ", nam0)
  if (abbreviate){
    firstup <- function(x) {
      substr(x, 1, 1) <- toupper(substr(x, 1, 1))
      x
    }
    from_first_lo <- function(x) {
      substring(x, 2) <- tolower(substring(x, 2))
      x
    }
    nam1 <- firstup(from_first_lo(nam1))
    leg <- nam1
    nam1 <- abbreviate(nam1)
    leg <- mapply(paste, nam1, leg, MoreArgs=list(sep = " = "))
  }
  if (split_labels_accross_lines){
    split_labels_accross_lines <- function(x) {
      out <- gsub(paste0('(.{1,', line_leng, '})(\\s|$)'), '\\1\n', x)
      substring(out, 1, (nchar(out) - 1))
    }
    nam1 <- split_labels_accross_lines(nam1)
  }

  nodes(bn) <- nam1
  g1 <- graphviz.plot(bn, shape = shape, layout = layout)
  # par(font=font)
  # graph.par(list(nodes=list(textCol=textCol, fontsize=fontsize)))
  nodeRenderInfo(g1) <- list(fill=node_fill, lty=node_lty, lwd=node_lwd, col=node_col)
  # col <- rep (adjustcolor( "green", alpha.f = 0.2), length(leaf.nodes(bn)))
  # names(col) <- leaf.nodes(bn)
  if(!is.null(highlit_some_nodes)){
    if(length(highlit_some_nodes_col) == 1){
      tmp_col <- rep(highlit_some_nodes_col, length(highlit_some_nodes))
    } else {
      tmp_col <- highlit_some_nodes_col
    }
    # tmp_col <- rep (highlit_some_nodes_col, length(highlit_some_nodes))
    names(tmp_col) <- highlit_some_nodes
    # col <- c(col, tmp_col)
  }

  nodeRenderInfo(g1) <- list(textCol=tmp_col)
  edgeRenderInfo(g1) <- list(fill=edge_fill, lty=edge_lty, lwd=edge_lwd, col=edge_col)
  # nodeRenderInfo(g1) <- list(col=col, fill=tmp_col)

  par(cex=cex, bg = plot_bg)
  renderGraph(g1)
  if(abbreviate) {
    legend('topright', legend = leg, text.width = leg_w, cex=leg_cex, ncol = leg_ncol,
           bty="o", box.lwd=1, box.col=leg_box.col, xjust=1, yjust=1, bg=leg_bg,
           title = "Legend", title.col = leg_title.col)
  }
  par(.pardefault)
}
