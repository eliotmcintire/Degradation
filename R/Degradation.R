if (!require("Require")) install.packages("Require")
source("R/SpiderWeb.R")
Require::Install(c("ggplot2", "data.table", "gridExtra", "patchwork", "scales", "stringr", "box"))
box::use(data.table[data.table, setnames, set],
         ggplot2[ggplot, aes, geom_line, geom_ribbon, ylim, guides,
                 annotate, labs, scale_color_manual, scale_fill_manual,
                 ggtitle, ggsave],
         patchwork[...],
         scales[...],
         stringr[str_wrap])

op <- "~/GitHub/Degradation/output"
N <- 100
M <- 30 # increase this to get less variation; decrease to get more variation
sdval <- 10
sdRib <- 5 # increase this to get wider confidence intervals on thick lines

theSep <- " "
yrs <- "Years"
scn <- "scenario"
cc <- "Climate Change"
sc <- "Stable Climate"
co <- "Climate Only"
cd <- "plus increasing land use"
ccco <- paste(cc,co, sep = theSep) #"ClimateChangeClimateOnly"
scco <- paste(sc,co, sep = theSep) # StableClimateClimateOnly"
cccd <- paste(cc,cd, sep = theSep) # "ClimateChangeClimateplusincreasinglanduse"
sccd <- paste(sc,cd, sep = theSep) # "StableClimateClimateplusincreasinglanduse"
colors <- c("blue", "red", "orange", "yellow")
sccoCol <- "Historic Natural\nRange of Variation"
cccoCol <- "Forecasted Future\nNatural Range of Variation"
cccdCol <- "Current and Forecasted\nConditions"
sccdCol <- sccd
names(colors) <- c(cccoCol, sccoCol, cccdCol, sccdCol)
LWD <- 1.5

wrapper <- function(x, ...) paste(strwrap(x, ...), collapse = "\n")

degradation <- function(N, M, sc, cc, yrs, scn, base, Years, co, sdval, cd, theSep) {
  df <- data.table(rep(rep(seq(N), M), 2),
                   c(rep(sc, N* M), rep(cc, N*M)))
  setnames(df, new = c(yrs, scn))

  df[get(scn) == sc, base := 100]
  df[get(scn) == cc, base := 100 - Years * 0.4]
  set(df, NULL, co, df$base + rnorm(N*M, sd = sdval))
  set(df, NULL, cd, df$base - df[[yrs]] * 0.3 + rnorm(N*M, sd = sdval))
  set(df, NULL, yrs, df[[yrs]] + 1999)

  set(df, NULL, 'base', NULL)

  df1 <- data.table::melt(df, id.vars = c(yrs, scn), value.name = "VEC")
  dd <- data.table::dcast(df1, formula = get(yrs) ~ get(scn) + variable, drop = T, fun.aggregate = mean)
  setnames(dd, old = "yrs", new = yrs)
  setnames(dd, old = colnames(dd), new = gsub("_", theSep, colnames(dd)))

  dd[]
}

restoration <- function(N, M, sc, cc, yrs, scn, base, Years, co, sdval, cd, theSep,
                        mult = 0.3) {
  df <- data.table(rep(rep(seq(N), M), 2),
                   c(rep(sc, N* M), rep(cc, N*M)))
  setnames(df, new = c(yrs, scn))

  df[get(scn) == sc, base := 100]
  df[get(scn) == cc, base := 100 - Years * 0.4]
  set(df, NULL, co, df$base + rnorm(N*M, sd = sdval))
  x <- 10
  set(df, NULL, cd, df$base - df[[yrs]] * mult + rnorm(N*M, sd = sdval))
  set(df, NULL, yrs, df[[yrs]] + 1999)

  set(df, NULL, 'base', NULL)

  df1 <- data.table::melt(df, id.vars = c(yrs, scn), value.name = "VEC")
  dd <- data.table::dcast(df1, formula = get(yrs) ~ get(scn) + variable, drop = T,
                          fun.aggregate = mean)
  setnames(dd, old = "yrs", new = yrs)
  setnames(dd, old = colnames(dd), new = gsub("_", theSep, colnames(dd)))

  dd[]
}

gg <- function(rr, Years, .data, ccco, cccoCol, LWD, scco, sccoCol, cccd, cccdCol, sdRib, ggplot2, theme_bw, colors) {
  browser()
  p <- ggplot(rr, aes(x = Years))  + #geom_point() +
    geom_line(aes(y = .data[[ccco]], color = cccoCol), lwd = LWD, alpha = 0.7) +
    geom_line(aes(y = .data[[scco]], color = sccoCol), lwd = LWD, alpha = 0.5) +
    geom_line(aes(y = .data[[cccd]], color = cccdCol), lwd = LWD, alpha = 0.7) +
    geom_ribbon(aes(ymin = .data[[scco]]-abs(rnorm(.data[[scco]], sd = sdRib)),
                    ymax = .data[[scco]]+abs(rnorm(.data[[scco]], sd = sdRib)), colour = sccoCol, fill = sccoCol),
                alpha=0.25) +
    geom_ribbon(aes(ymin = .data[[ccco]]-abs(rnorm(.data[[ccco]], sd = sdRib)),
                    ymax = .data[[ccco]]+abs(rnorm(.data[[ccco]], sd = sdRib)), colour = cccoCol, fill = cccoCol),
                alpha=0.25) +
    geom_ribbon(aes(ymin = .data[[cccd]]-abs(rnorm(.data[[cccd]], sd = sdRib)),
                    ymax = .data[[cccd]]+abs(rnorm(.data[[cccd]], sd = sdRib)), colour = cccdCol, fill = cccdCol),
                alpha=0.25) +
    # geom_line(aes(x = Years, y = Stable_Climate_Climate_plus_increasing_land_use), lwd = LWD, col = "red", alpha = 0.5) +
    geom_ribbon(aes(ymin = .data[[cccd]], ymax = .data[[ccco]]),
                fill="green", alpha=0.25) +
    geom_ribbon(aes(ymin = .data[[ccco]], ymax = .data[[scco]]),
                fill="purple", alpha=0.2) +
    ggplot2::theme_bw() + ylim(0, 120)  + guides(fill = "none") +
    labs(y = txtVEC,
         x = "Year",
         color = "Legend", fill = NULL) +
    scale_color_manual(values = rev(colors))# +
    scale_fill_manual(values = rev(colors))
  p
}

ppp <- list()
yrAtChangeStarts <- 20


mult <- 0.3
rr <- restoration(N, M, sc, cc, yrs, scn, base, Years, co, sdval, cd, theSep, mult = mult)
i <- 1
ppp[[i]] <- gg(rr, Years, .data, ccco, cccoCol, LWD, scco, sccoCol, cccd, cccdCol, sdRib, ggplot2, theme_bw, colors)
ppp[[i]] <- ppp[[i]] +
  annotate(x = 2079, y = 83, geom = "text", label = "degradation from global climate policy", angle = c(-4)) +
  annotate(x = 2079, y = 56, geom = "text", label = "degradation from forest land management", angle = c(-9)) +
  # annotate(x = 2010, y = 10, geom = "text", label = LETTERS[i])
  ggtitle(LETTERS[i])
  #ggtitle(wrapper(paste0("Figure 1. Example degradation from climate and forest land management, within historic ",
  #        "Natural Range of Variation and Forecasted Future Natural Range of Variation")))
# print(ppp[[i]])


yrAtChangeStarts <- 30
i <- 2
mult <- c(rep(0.3, yrAtChangeStarts), seq(0.3, 0, length.out = yrAtChangeStarts), rep(0, N - 2*yrAtChangeStarts))
rr <- restoration(N, M, sc, cc, yrs, scn, base, Years, co, sdval, cd, theSep, mult = mult)
ppp[[i]] <- gg(rr, Years, .data, ccco, cccoCol, LWD, scco, sccoCol, cccd, cccdCol, sdRib, ggplot2, theme_bw, colors)
ppp[[i]] <- ppp[[i]] +
  ggtitle(LETTERS[i])
  # annotate(x = 2010, y = 10, geom = "text", label = LETTERS[i])
  # annotate(x = 2079, y = 83, geom = "text", label = "degradation from global climate policy", angle = c(-4)) +
  # annotate(x = 2079, y = 58, geom = "text", label = "degradation from forest land management", angle = c(-12)) +
  # ggtitle("Rapid restoration\n(mitigate degradation)")
print(ppp[[i]])

i <- 3
mult <- c(rep(0.3, yrAtChangeStarts), seq(0.3, 0.1, length.out = yrAtChangeStarts), rep(0.1, N - 2*yrAtChangeStarts))
rr <- restoration(N, M, sc, cc, yrs, scn, base, Years, co, sdval, cd, theSep, mult = mult)
ppp[[i]] <- gg(rr, Years, .data, ccco, cccoCol, LWD, scco, sccoCol, cccd, cccdCol, sdRib, ggplot2, theme_bw, colors)
ppp[[i]] <- ppp[[i]] +
  ggtitle(LETTERS[i])
  # annotate(x = 2010, y = 10, geom = "text", label = LETTERS[i])
  #   annotate(x = 2079, y = 83, geom = "text", label = "degradation from global climate policy", angle = c(-4)) +
  #   annotate(x = 2079, y = 58, geom = "text", label = "degradation from forest land management", angle = c(-12)) +
  # ggtitle("Delayed restoration\n(mitigate degradation)")
print(ppp[[i]])


mult <- c(rep(0, yrAtChangeStarts), seq(0, 0.3, length.out = N - yrAtChangeStarts))
rr <- restoration(N, M, sc, cc, yrs, scn, base, Years, co, sdval, cd, theSep, mult = mult)
i <- 4
ppp[[i]] <- gg(rr, Years, .data, ccco, cccoCol, LWD, scco, sccoCol, cccd, cccdCol, sdRib, ggplot2, theme_bw, colors)
ppp[[i]] <- ppp[[i]] +
  # annotate(x = 2010, y = 10, geom = "text", label = LETTERS[i])
  ggtitle(LETTERS[i])
  # annotate(x = 2079, y = 83, geom = "text", label = "degradation from global climate policy", angle = c(-4)) +
  # annotate(x = 2079, y = 58, geom = "text", label = "degradation from forest land management", angle = c(-12)) +
  # ggtitle("Increasing degradation")
print(ppp[[i]])


mult <- c(rep(0.3, yrAtChangeStarts), seq(0.3, -0.4, length.out = N - yrAtChangeStarts))
rr <- restoration(N, M, sc, cc, yrs, scn, base, Years, co, sdval, cd, theSep, mult = mult)
i <- 5
ppp[[i]] <- gg(rr, Years, .data, ccco, cccoCol, LWD, scco, sccoCol, cccd, cccdCol, sdRib, ggplot2, theme_bw, colors)
ppp[[i]] <- ppp[[i]] +
  # annotate(x = 2010, y = 10, geom = "text", label = LETTERS[i])
  ggtitle(LETTERS[i])
  # annotate(x = 2079, y = 83, geom = "text", label = "degradation from global climate policy", angle = c(-4)) +
  # annotate(x = 2079, y = 58, geom = "text", label = "degradation from forest land management", angle = c(-12)) +
  # ggtitle("Full compensation\n(incl. climate change)")
print(ppp[[i]])

design <- "
  1111
  2345
"
p <- ppp[[1]] + ppp[[2]] + ppp[[3]] + ppp[[4]] + ppp[[5]] +
  # labs(x = NULL, y = NULL) &
  plot_layout(design = design,
              guides = "collect",
              axes = "collect",
              axis_titles = "collect") & # ylab(txtVEC) &
  theme(legend.position = "bottom")
print(p)
ggsave(file.path(op, "Degradation1.png"), width = 10, height = 8)


