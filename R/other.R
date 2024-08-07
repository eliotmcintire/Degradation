set.seed(35)
df <- data.frame(Class = factor(rep(c(1,2),times = 80), labels = c("Math","Science")),
                 StudyTime = factor(sort(sample(1:4, 16, prob = c(0.25,0.3,0.3,0.15), replace = TRUE)),labels = c("<5","5-10","10-20",">20")),
                 Nerd = factor(sapply(rep(c(0.1,0.3,0.5,0.8),c(30,50,50,30)), function(x)sample(c("Nerd","NotNerd"),size = 1, prob = c(x,1-x))),levels = c("NotNerd","Nerd")))
library(ggplot2)
#remotes::install_github("coolbutuseless/ggpattern")
library(ggpattern)
ggplot(data = df, aes(x = Class, fill = StudyTime, pattern = Nerd)) +
  geom_bar_pattern(position = position_dodge(preserve = "single"),
                   color = "black",
                   pattern_fill = "black",
                   pattern_angle = 45,
                   pattern_density = 0.1,
                   pattern_spacing = 0.025,
                   pattern_key_scale_factor = 0.6) +
  scale_fill_manual(values = colorRampPalette(c("#0066CC","#FFFFFF","#FF8C00"))(4)) +
  scale_pattern_manual(values = c(Nerd = "stripe", NotNerd = "none")) +
  labs(x = "Class", y = "Number of Students", pattern = "Nerd?") +
  guides(pattern = guide_legend(override.aes = list(fill = "white")),
         fill = guide_legend(override.aes = list(pattern = "none")))





set.seed(40)
df2 <- data.frame(Row = rep(1:9,times=9), Column = rep(1:9,each=9),
                  Evaporation = runif(81,50,100),
                  TreeCover = sample(c("Yes", "No"), 81, prob = c(0.3,0.7), replace = TRUE))

ggplot(data=df2, aes(x=as.factor(Row), y=as.factor(Column),
                     pattern = TreeCover, fill= Evaporation)) +
  geom_tile_pattern(pattern_color = NA,
                    pattern_fill = "black",
                    pattern_angle = 45,
                    pattern_density = 0.5,
                    pattern_spacing = 0.025,
                    pattern_key_scale_factor = 1) +
  scale_pattern_manual(values = c(Yes = "stripe", No = "none")) +
  scale_fill_gradient(low="#0066CC", high="#FF8C00") +
  coord_equal() +
  labs(x = "Row",y = "Column") +
  guides(pattern = guide_legend(override.aes = list(fill = "white")))



ggplot(df, aes(x = Year)) + #geom_point() +
  geom_line(aes(y = Actual, colour = cccdCol), lwd = LWD, alpha = 0.7) +
  geom_ribbon(aes(ymin = NRVlower,
                  ymax = NRVupper, #colour = sccoCol,
                  fill = sccoCol),
              alpha=0.25) +
  geom_ribbon(aes(ymin = FRVlower,
                  ymax = FRVupper, # colour = cccoCol,
                  fill = cccoCol),
              alpha=0.25) +
  geom_ribbon_pattern(aes(ymin = FRVmid, ymax = NRVmid,
                          pattern = Degradation)#, pattern_type = dueToGlobalClimate)
                      , fill = "transparent"
                      , color = "black"
                      # , pattern_type = "circles",
                      , pattern_fill = "red"
                      , pattern_fill2 = "transparent"
                      # , alpha = 0.1
                      # pattern_angle = 45,
                      , pattern_density = 0.25
                      # , pattern_spacing = 0.5
  ) +
  geom_ribbon_pattern(aes(ymin = Actual, ymax = FRVmid,
                          pattern = ForestDegradation)#, pattern_type = dueToGlobalClimate)
                      , fill = "transparent"
                      , color = "black"
                      # pattern_type = "hash",
                      , pattern_fill = "yellow"
                      , pattern_fill2 = "transparent"
                      # , alpha = 0.1
                      , pattern_angle = 135,
                      , pattern_density = 0.25
                      # , pattern_spacing = 0.5
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
  # guides(pattern = guide_legend(override.aes = list(pattern = "stripe"))) +
         # fill = guide_legend(override.aes = list(pattern = "stripe"))) +
  scale_pattern_manual(values = pat) +
  # scale_pattern_type(values=c("striped")) +
  # theme(legend.key.size = unit(0.5, 'cm')) +
  # scale_pattern_discrete() +
  ggtitle(LETTERS[i])

