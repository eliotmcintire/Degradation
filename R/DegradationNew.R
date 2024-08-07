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

dueToGlobalClimate <- "Global climate policy"
dueToForestManagement <- "Forest management"

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
startingDegraded <- 85
endLowpoint <- 40

NRVupper <- rnorm(N, mean = midVal + rnorm(N, mean = sdRib, 1), sd = sdNRVBounds)
NRVlower <- rnorm(N, mean = midVal - rnorm(N, mean = sdRib, 1), sd = sdNRVBounds)
NRVmid <- (NRVupper - NRVlower)/2 + NRVlower

FTrend <- seq(midVal, 75, length = N)
# FRVupper <- rnorm(N, mean = FTrend, sd = sdval)
FRVupper <- rnorm(N, mean = FTrend + rnorm(N, mean = sdRib, 1), sd = sdNRVBounds)
FRVlower <- rnorm(N, mean = FTrend - rnorm(N, mean = sdRib, 1), sd = sdNRVBounds)
FRVmid <- (FRVupper - FRVlower)/2 + FRVlower

degradationConsistently <- FTrend - (midVal - startingDegraded)
Actual1 <- rnorm(N, seq(startingDegraded, endLowpoint, length = N), sd = 2)
Worse <- Actual1 * seq(1, 0.8, length.out = N)

start <- 20
end <- 80

merge1 <- seq(0, 1, length.out = end - start)
merge2 <- 1-merge1
Actual2 <- rnorm(N, c(Actual1[1:start],
                      Actual1[(start+1):end] * merge2 + FRVmid[(start+1):end] * merge1,
                      FRVmid[(end + 1):N]), sd = 2)

merge1 <- seq(0, 1, length.out = end - start)
merge2 <- 1-merge1
Actual3 <- rnorm(N, c(Actual1[1:start],
                      Actual1[(start+1):end] * merge2 + degradationConsistently[(start+1):end] * merge1,
                      degradationConsistently[(end + 1):N]), sd = 2)

merge1 <- seq(0, 1, length.out = end - start)
merge2 <- 1-merge1
Actual4 <- rnorm(N, c(Actual1[1:start],
                      Actual1[(start+1):end] * merge2 + Worse[(start+1):end] * merge1,
                      Worse[(end + 1):N]), sd = 2)

end <- N
merge1 <- seq(0, 1, length.out = end - start)
merge2 <- 1-merge1
Actual5 <- rnorm(N, c(Actual1[1:start],
                      Actual1[(start+1):end] * merge2 + NRVmid[(start+1):end] * merge1)
                 # NRVmid[(end + 1):N]
                 , sd = 2)

deg1lower <- Actual1
deg1upper <- FRVlower

deg2lower <- Actual2
deg2upper <- FRVlower
deg3lower <- Actual3
deg3upper <- FRVlower
deg4lower <- Actual4
deg4upper <- FRVlower
deg5lower <- Actual5
deg5upper <- FRVlower
comp5lower <- FRVupper
comp5upper <- Actual5

df <- data.frame(Year = seq_len(N) + 1999, NRVupper, NRVlower, FRVupper, FRVlower,
                 Actual1, Actual2, Actual3, Actual4, Actual5,
                 Degradation = dueToGlobalClimate,
                 ForestDegradation = dueToForestManagement)

pat <- c("stripe", "circle")
names(pat) <- c(dueToGlobalClimate, dueToForestManagement)

gg2 <- function(df, sccoCol, cccoCol, cccdCol, txtVEC, NRVname, colors, i, pat, LWD, needPattern = TRUE,
                patternCol = "dark grey", degradationStarts = "FRVlower") {
  a <- ggplot(df, aes(x = Year)) +
    geom_ribbon(aes(ymin = NRVlower,
                    ymax = NRVupper, fill = sccoCol),
                alpha=0.25) +
    # geom_line(aes(y = NRVmid, color = sccoCol), lwd = LWD, alpha = 0.4) +
    geom_ribbon(aes(ymin = FRVlower,
                    ymax = FRVupper, fill = cccoCol),
                alpha=0.25) +
    # geom_line(aes(y = FRVmid, color = cccoCol), lwd = LWD, alpha = 0.4) +
    ggplot2::theme_bw() + ylim(0, 120)  + guides() +
    labs(y = txtVEC, x = "Year", color = "", fill = NRVname, pattern = "Degradation due to: ") +
    scale_color_manual(values = rev(colors), breaks = names(colors)[3]) +
    scale_fill_manual(values = rev(colors)) +
    ggtitle(LETTERS[i])

  if (needPattern) {
    a <- a +
      geom_ribbon_pattern(aes(ymin = FRVmid, ymax = NRVmid,
                              pattern = Degradation)#, pattern_type = dueToGlobalClimate)
                          , fill = "transparent"
                          , pattern_fill = patternCol
                          , pattern_colour = patternCol
                          , pattern_density = 0.1
      )

    # compensation <- tail(df[[paste0("Actual", i)]], 1) > tail(FRVmid, 1)
    degr <- ifelse(df[[paste0("Actual", i)]] > df[[degradationStarts]],
                   df[[degradationStarts]], df[[paste0("Actual", i)]])
    # comp <- ifelse(df[[paste0("Actual", i)]] < FRVmid, FRVmid, df[[paste0("Actual", i)]])
    # fd <- df[["ForestDegradation"]]
    a <- a + geom_ribbon_pattern(aes(ymin = degr, ymax = .data[[degradationStarts]],
                                     pattern = ForestDegradation)
                                 , fill = "transparent"
                                 , pattern_colour = patternCol
                                 , pattern_fill = patternCol
                                 , pattern_angle = 135,
                                 , pattern_density = 0.2) +
      scale_pattern_manual(values = pat) +
      geom_line(aes(y = .data[[paste0("Actual", i)]], color = cccdCol), lwd = LWD, alpha = 1) +
      theme(legend.key.size = unit(0.95, 'cm'))
  }

  a <- a +
    geom_line(aes(y = NRVmid, color = sccoCol), lwd = LWD, alpha = 0.4) +
    geom_line(aes(y = FRVmid, color = cccoCol), lwd = LWD, alpha = 0.4)

  a
}

ppp <- list()
for (i in 1:5)
  ppp[[i]] <- gg2(df, sccoCol, cccoCol, cccdCol, txtVEC, NRVname, colors, i, pat, LWD)



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
  theme(legend.position = "right")
print(p)
ggsave(file.path(op, "DegradationNew.png"), width = 10, height = 8)



