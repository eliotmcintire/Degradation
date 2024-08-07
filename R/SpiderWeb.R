op <- "~/GitHub/Degradation/output"
if (!require("Require")) install.packages("Require")
Require::Install(c("fmsb"))
Require::Install(c("ggiraphExtra", "ggplot2", "data.table", "stringr", "box"))
box::use(fmsb[...],
         ggplot2[...],
         ggiraphExtra[...],
         data.table[as.data.table,melt,setnames],
         stringr[str_wrap])

txtVEC <- "Indicator of Forest Change"

# https://www.data-to-viz.com/caveat/spider.html
# Possible scenarios….
# 1. Lengthen rotation
# 2. Diversify tree composition at landscape scale
# 3. Increase or decrease AAC,
# 4. Eliminate herbicide use
#
# Indicators for figure
# Amount of conifer,
# amount of old forest,
# wood supply,
# caribou habitat,
# landscape fragmentation,
# carbon storage,
# human footprint/primary forest

Value = c("Amount of Conifer", "Amount of Old Forest", "Wood Supply*", "Caribou Habitat",
          "Intactness", "Carbon Storage", "Primary Forest", "Resilience to Fire")
Scenario = c("Longer Rotation", "More Tree Diversity", "Less Harvest", "Less Herbicide",
             "Clustered Harvest")
val <- list()
val$AC = c(20, -10, 20, -30, 0)
val$AOF = c(20, 0, 20, -10, 0)
val$WS = c(-20, -20, -20, -20, 0)
val$CH = c(30, -10, 20, 0, 10)
val$LF = c(0, 0, 20, 0, 20)
val$CS = c(10, 0, 20, 0, 0)
val$PF = c(-10, 0, 20, -20, 0)
val$RF = c(-10, 20, -10, 20, 0)
vals <- do.call(data.frame, val)
rownames(vals) <- Scenario
colnames(vals) <- Value
cols <- RColorBrewer::brewer.pal(NROW(vals), name = "Set1")
colors_in <- paste0(cols, "66")

valsOrd <- vals[, c(1, 2, 4, 5, 6, 7, 3, 8)]

# min/max
valsWMinMax <- rbind(rep(max(vals),NCOL(vals)), rep(min(vals),NCOL(vals)),
                     valsOrd)
op2 <- par(mar = c(1, 2, 2, 1))
radarchartcirc(valsWMinMax, pcol = cols,  pfcol = c(colors_in), plty = 1,
           axistype = 1, axislabcol = "black",
           caxislabels=c("-30", "-15", "0", "+15", "+30"),
           )
legend(x=-2.4, y=1.3, legend = rownames(valsOrd), bty = "n",
       pch=20 , col=colors_in , text.col = "black", cex=1.2, pt.cex=3)

# ggsave(file.path(op, "SpiderTradeoffs.png"), width = 10, height = 10)


# colors_in <- paste0(cols, "33")
# g1 <- ggradar(
#   valsWGrp,
#   values.radar = c("0", "60", "120"),
#   grid.min = 0, grid.mid = mean(c(0, max(vals))), grid.max = max(vals),
#   # Polygons
#   fill = TRUE,
#   group.line.width = 1,
#   group.point.size = 3,
#   group.colours = colors_in,
#   # Background and grid lines
#   background.circle.colour = "white",
#   gridline.mid.colour = "grey",
#   gridline.max.linetype = 1,
#   gridline.mid.linetype = 1,
#   gridline.min.linetype = 1,
#   legend.position = "bottom"
# )

valsWGrp <- data.frame(group = rownames(vals), vals)
# valsWGrp$group <- paste0("`", valsWGrp$group, "`")
cns <- colnames(valsWGrp)
# colnames(valsWGrp) <- paste0("`", gsub("\\.", " ", cns), "`")
v <- data.table::melt(as.data.table(valsWGrp), id= "group", variable.name = "Indicator")
v[, Indicator := gsub("\\.", " ", Indicator)]
setnames(v, old = c("group", "value"), c("Scenario", txtVEC))

# g <- ggRadar(valsWGrp, aes(group = "group"), rescale = FALSE) +
#   theme_light()
# ggsave(file.path(op, "SpiderTradeoffs2.png"), width = 10, height = 10)
#
# ggplot(v, aes(y = value, colour = group)) + geom_bar()

# ggplot(v, aes(y = value, x = factor(Indicator), fill = group)) +
#   geom_bar(stat = "identity", position=position_dodge()) +
#   # geom_line() +
#   scale_color_brewer(palette="Set3") +
#   scale_fill_brewer(palette = "Set3") #+
#   #facet_grid(cols = vars(group))

ggplot(v, aes(y = get(txtVEC), x = Indicator,
              group = Scenario, color = Scenario, fill = Scenario)) +
  # geom_bar(stat = "identity", position=position_dodge()) +
  geom_line(linewidth = 2) +
  # geom_area(position = "identity", alpha = 0.25, linewidth = 2) +
  # geom_ribbon(aes(ymin = .data[[cccd]], ymax = .data[[ccco]]),
  #             fill="green", alpha=0.25) +
  scale_color_brewer(palette = "Set3") +
  scale_fill_brewer(palette = "Set3") +
  theme_light() +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  ylab(txtVEC)

  # theme(axis.text.x = element_text(size=14, angle=45))

ggsave(file.path(op, "LinePlotTradeoffs.png"), width = 8, height = 4)
