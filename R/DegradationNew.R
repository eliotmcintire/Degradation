if (!require("Require")) install.packages("Require")
source("R/SpiderWeb.R")
Require::Install(c("ggplot2", "data.table", "gridExtra", "patchwork", "scales", "stringr", "box", "ggpattern"))
box::use(data.table[data.table, setnames, set],
         ggplot2[ggplot, aes, geom_line, geom_ribbon, ylim, guides,
                 annotate, labs, scale_color_manual, scale_fill_manual,
                 ggtitle, ggsave],
         patchwork[...],
         scales[...],
         stringr[str_wrap],
         ggpattern[...])
theSep <- " "

dueToGlobalClimate <- "Degradation from global climate policy"
dueToForestManagement <- "Degradation from forest management"

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
colors <- c("blue", "green", "black")
sccoCol <- "Historic"
cccoCol <- "Forecasted Future"
cccdCol <- "Current and Forecasted\nConditions"
# sccdCol <- sccd
names(colors) <- c(cccoCol, sccoCol, cccdCol)#, sccdCol)
LWD <- 1
NRVname <- "Range of Variation"

op <- "~/GitHub/Degradation/output"
N <- 100
M <- 30 # increase this to get less variation; decrease to get more variation
# sdval <- 10
sdRib <- 10 # increase this to get wider confidence intervals on thick lines
midVal <- 100
sdNRVBounds <- 0.1
startingDegraded <- 5

NRVupper <- rnorm(N, mean = midVal + rnorm(N, mean = sdRib, 1), sd = sdNRVBounds)
NRVlower <- rnorm(N, mean = midVal - rnorm(N, mean = sdRib, 1), sd = sdNRVBounds)
NRVmid <- (NRVupper - NRVlower)/2 + NRVlower

FTrend <- seq(midVal, 75, length = N)
# FRVupper <- rnorm(N, mean = FTrend, sd = sdval)
FRVupper <- rnorm(N, mean = FTrend + rnorm(N, mean = sdRib, 1), sd = sdNRVBounds)
FRVlower <- rnorm(N, mean = FTrend - rnorm(N, mean = sdRib, 1), sd = sdNRVBounds)
FRVmid <- (FRVupper - FRVlower)/2 + FRVlower

Actual <- rnorm(N, seq(midVal - startingDegraded, 50, length = N), sd = 2)
Worse <- Actual * seq(1, 0.8, length.out = N)

start <- 20
end <- 80

merge1 <- seq(0, 1, length.out = end - start)
merge2 <- 1-merge1
Actual2 <- rnorm(N, c(Actual[1:start],
                      Actual[(start+1):end] * merge2 + FRVmid[(start+1):end] * merge1,
                      FRVmid[(end + 1):N]), sd = 2)

merge1 <- seq(0, 1, length.out = end - start)
merge2 <- 1-merge1
Actual3 <- rnorm(N, c(Actual[1:start],
                      Actual[(start+1):end] * merge2 + FRVlower[(start+1):end] * merge1,
                      FRVlower[(end + 1):N]), sd = 2)

merge1 <- seq(0, 1, length.out = end - start)
merge2 <- 1-merge1
Actual4 <- rnorm(N, c(Actual[1:start],
                      Actual[(start+1):end] * merge2 + Worse[(start+1):end] * merge1,
                      Worse[(end + 1):N]), sd = 2)

end <- N
merge1 <- seq(0, 1, length.out = end - start)
merge2 <- 1-merge1
Actual5 <- rnorm(N, c(Actual[1:start],
                      Actual[(start+1):end] * merge2 + NRVmid[(start+1):end] * merge1)
                 # NRVmid[(end + 1):N]
                 , sd = 2)

df <- data.frame(Year = seq_len(N) + 1999, NRVupper, NRVlower, FRVupper, FRVlower,
                 Actual, Actual2, Actual3, Actual4, Actual5,
                 Degradation = dueToGlobalClimate,
                 ForestDegradation = dueToForestManagement)

pat <- c("circle", "circle")
names(pat) <- c(dueToGlobalClimate, dueToForestManagement)

sd

ppp <- list()
i <- 1
ppp[[i]] <- ggplot(df, aes(x = Year)) + #geom_point() +
  geom_line(aes(y = Actual, colour = cccdCol), lwd = LWD, alpha = 0.7) +
  geom_ribbon(aes(ymin = NRVlower,
                  ymax = NRVupper, #colour = sccoCol,
                  fill = sccoCol),
              alpha=0.25) +
  geom_ribbon(aes(ymin = FRVlower,
                  ymax = FRVupper, # colour = cccoCol,
                  fill = cccoCol),
              alpha=0.25) +
  geom_ribbon_pattern(aes(ymin = FRVlower, ymax = NRVlower,
                          pattern = dueToGlobalClimate)#, pattern_type = dueToGlobalClimate)
                      , fill = "grey"
                      # , color = "white"
                      # pattern_type = "hash",
                      , pattern_fill = "black"
                      # pattern_fill2 = "blue",
                      , alpha = 0.1
                      # pattern_angle = 45,
                      , pattern_density = 0.1
                      # pattern_spacing = 0.05
                      ) +

  #                        pattern_density = "Due to global climate policy"),
                      # pattern_density = 0.1,
  #                    pattern_fill = "blue",
  #                    alpha = 0.5, # fill = "green"# , # colour = "",
  #                    , pattern = "stripe"
  #                    ) +
  ggplot2::theme_bw() + ylim(0, 120) + # guides() + #guides(color = guide_legend(rev(colors))) + #, fill = guide_legend(rev(colors))) + # guides(colour = "none") + #, colour = "none") +
  labs(y = txtVEC, x = "Year", color = "", fill = NRVname, pattern = "Degradation", pattern_type = "hope") +
  scale_color_manual(values = rev(colors)) +
  scale_fill_manual(values = rev(colors)) +
  guides(pattern = guide_legend(override.aes = list(pattern_fill = "yellow")),
         fill = guide_legend(override.aes = list(pattern = "stripe"))) +
  scale_pattern_manual(values = pat) +
  # scale_pattern_type(values=c("striped")) +
  # theme(legend.key.size = unit(1.5, 'cm')) +
  # scale_pattern_discrete() +
  ggtitle(LETTERS[i])
print(ppp[[i]])

i <- 2
ppp[[i]] <- ggplot(df, aes(x = Year)) + #geom_point() +
  geom_line(aes(y = Actual2, color = cccdCol), lwd = LWD, alpha = 0.7) +
  geom_ribbon(aes(ymin = NRVlower,
                  ymax = NRVupper, fill = sccoCol),
              alpha=0.25) +
  geom_ribbon(aes(ymin = FRVlower,
                  ymax = FRVupper, fill = cccoCol),
              alpha=0.25) +
  ggplot2::theme_bw() + ylim(0, 120)  + guides() +
  labs(y = txtVEC, x = "Year", color = "", fill = NRVname) +
  scale_color_manual(values = rev(colors)) +
  scale_fill_manual(values = rev(colors)) +
  ggtitle(LETTERS[i])

i <- 3
ppp[[i]] <- ggplot(df, aes(x = Year)) + #geom_point() +
  geom_line(aes(y = Actual3, color = cccdCol), lwd = LWD, alpha = 0.7) +
  geom_ribbon(aes(ymin = NRVlower,
                  ymax = NRVupper, fill = sccoCol),
              alpha=0.25) +
  geom_ribbon(aes(ymin = FRVlower,
                  ymax = FRVupper, fill = cccoCol),
              alpha=0.25) +
  ggplot2::theme_bw() + ylim(0, 120)  + guides() +
  labs(y = txtVEC, x = "Year", color = "", fill = NRVname) +
  scale_color_manual(values = rev(colors)) +
  scale_fill_manual(values = rev(colors)) +
  ggtitle(LETTERS[i])
i <- 4
ppp[[i]] <- ggplot(df, aes(x = Year)) + #geom_point() +
  geom_line(aes(y = Actual4, color = cccdCol), lwd = LWD, alpha = 0.7) +
  geom_ribbon(aes(ymin = NRVlower,
                  ymax = NRVupper, fill = sccoCol),
              alpha=0.25) +
  geom_ribbon(aes(ymin = FRVlower,
                  ymax = FRVupper, fill = cccoCol),
              alpha=0.25) +
  ggplot2::theme_bw() + ylim(0, 120)  + guides() +
  labs(y = txtVEC, x = "Year", color = "", fill = NRVname) +
  scale_color_manual(values = rev(colors)) +
  scale_fill_manual(values = rev(colors)) +
  ggtitle(LETTERS[i])
i <- 5
ppp[[i]] <- ggplot(df, aes(x = Year)) + #geom_point() +
  geom_line(aes(y = Actual5, color = cccdCol), lwd = LWD, alpha = 0.7) +
  geom_ribbon(aes(ymin = NRVlower,
                  ymax = NRVupper, fill = sccoCol),
              alpha=0.25) +
  geom_ribbon(aes(ymin = FRVlower,
                  ymax = FRVupper, fill = cccoCol),
              alpha=0.25) +
  ggplot2::theme_bw() + ylim(0, 120)  + guides() +
  labs(y = txtVEC, x = "Year", color = "", fill = NRVname) +
  scale_color_manual(values = rev(colors)) +
  scale_fill_manual(values = rev(colors)) +
  ggtitle(LETTERS[i])
# print(ppp[1:3])

ppp[[1]] <- ppp[[1]] +
 annotate(x = 2079, y = 90, geom = "text", label = "degradation from global climate policy", angle = c(-4)) +
 annotate(x = 2079, y = 56, geom = "text", label = "degradation from forest land management", angle = c(-9))


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
ggsave(file.path(op, "DegradationNew.png"), width = 10, height = 8)
