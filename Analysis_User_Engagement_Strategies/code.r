# install.packages("dplyr")
# install.packages("tidyr")
# install.packages("ggsignif")
# install.packages("cowplot")
# install.packages("ggpubr")
# install.packages("gghighlight")

library(dplyr)
library(tidyr)
library(ggplot2)
library(ggsignif)
library(cowplot)
library(ggpubr)
library(gghighlight)
library(multcompView)

dataset <- read.csv("C:\\Users\\20174715\\OneDrive - Higher Education Commission\\TUe\\HCOMP-2021 (inshALLAH)\\Data set\\task_survey - Copy.csv", header = TRUE)

dataset1 <- filter(dataset, !id %in% c(446, 455, 520, 531, 570, 571, 651, 656))
#dataset1 <- filter(dataset1, stress_code == "IR")

#filter(as.integer(filler_type_code) == 2)



#levels(dataset1$stress_code)
levels(dataset1$delay)
#levels(dataset1$filler_type_code)

# dataset1$stress_code <- factor(dataset1$stress_code)
#dataset1$delay <- factor(dataset1$delay)
#dataset1$filler_type_code <- factor(dataset1$filler_type_code)

dataset1$stress_code <- factor(dataset1$stress_code, levels = c(1, 2), labels = c("Stress", "IR"))
dataset1$delay <- factor(dataset1$delay, levels = c(8, 16, 32), labels = c("8", "16", "32"))
dataset1$filler_type_code <- factor(dataset1$filler_type_code, levels = c(1,2,3,4,5,6,7,8), labels = c("Convo", "Ellipses", 
                                                                                                       "Progress", "Nature", "Manmade", 
                                                                                                       "Micro", "Breathing", "No-filler"))

# BOX-plot
box1 <- dataset1 %>% 
  # mutate( type=ifelse(class=="subcompact","Highlighted","Normal")) %>%
  ggplot(aes(x=filler_type_code,y=cognitive, fill = stress_code)) +
  geom_boxplot(notch=FALSE) + 
  geom_jitter(alpha = 1/10) + 
  scale_color_viridis_d(option = "plasma") +
  xlab("Filler Type") +
  ylab("Cognitive Score") +
  guides(fill=guide_legend(title="Task Type"))+
  theme(axis.text = element_text(size = 16),
        legend.title = element_text(color = "black", size = 14),
        legend.text = element_text(color = "black", size = 12),
        axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5),
        axis.title = element_text(size = 20),
        plot.title = element_text(size = 25, face = "bold"),
        axis.title.x = element_blank(),
        panel.border = element_rect(color = "black", fill = NA, size = 1.2))+ 
  geom_signif(
    comparisons = list(c("Micro", "Convo"), c("Micro", "Ellipses"),
                       c("Micro", "Progress"), c("Micro", "Nature"),
                       c("Micro", "Breathing"), c("Micro", "No-filler")),
    map_signif_level = TRUE
  )


myColors <- ifelse(levels(dataset1$filler_type_code)=="Nature" , rgb(0.1,0.1,0.7,0.5) ,
                   ifelse(levels(dataset1$filler_type_code)=="Progress", rgb(0.8,0.1,0.3,0.6),
                          "grey48" ) )

# BOX-plot
box2 <- dataset1 %>% 
  ggplot(aes(x=filler_type_code,y=FI_final, fill = stress_code)) +
  geom_boxplot(notch=FALSE) + 
  geom_jitter(alpha = 1/10) + 
  facet_grid(. ~ stress_code)+
  scale_color_viridis_d(option = "plasma") +
  xlab("Filler Type") +
  ylab("Focused Immersion") +
  guides(fill=guide_legend(title="Task Type"))+
  theme(axis.text = element_text(size = 16),
        legend.title = element_text(color = "black", size = 14),
        legend.text = element_text(color = "black", size = 12),
        axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5),
        axis.title = element_text(size = 20),
        plot.title = element_text(size = 25, face = "bold"),
        axis.title.x = element_blank(),
        panel.border = element_rect(color = "black", fill = NA, size = 1.2)) + 
  geom_signif(
    comparisons = list(c("Micro", "Convo"), c("Micro", "Ellipses"),
                       c("Micro", "Progress"), c("Micro", "Nature"),
                       c("Micro", "Breathing"), c("Micro", "No-filler")),
    map_signif_level = TRUE
  )
box2


plot_grid(box1, box2, labels = c('A', 'B'), label_size = 20, label_colour = "blue")



# bar chart
fig1 <- dataset1 %>% group_by(stress_code, filler_type_code) %>% summarise(cognitive_mean = mean(cognitive), sd = sd(cognitive),
                                                                   N = sum(!is.na(cognitive)),
                                                                   upper_limit = cognitive_mean + sd/sqrt(N),
                                                                   lower_limit = cognitive_mean - sd/sqrt(N)) %>%
  ggplot(aes(x = filler_type_code, y = cognitive_mean, fill = stress_code)) +
  geom_col(position = "dodge")+
  geom_errorbar(aes(ymin=lower_limit, ymax=upper_limit), width=.2,
                position=position_dodge(.9))+
  geom_jitter(alpha = 1/10) +
  scale_color_viridis_d(option = "inferno") +
  xlab("Filler Type") +
  ylab("Cognitive Score") +
  guides(fill=guide_legend(title="Task Type"))+
  theme(axis.text = element_text(size = 16),
        legend.title = element_text(color = "black", size = 14),
        legend.text = element_text(color = "black", size = 12),
        axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5),
        axis.title = element_text(size = 20),
        plot.title = element_text(size = 25, face = "bold"),
        axis.title.x = element_blank(),
        # legend.position = "none",
        panel.border = element_rect(color = "black", fill = NA, size = 1.2))



fig2 <- dataset1 %>% group_by(stress_code, filler_type_code) %>% summarise(FI_mean = mean(FI_final), sd = sd(FI_final),
                                                                           N = sum(!is.na(FI_final)),
                                                                           upper_limit = FI_mean + sd/sqrt(N),
                                                                           lower_limit = FI_mean - sd/sqrt(N)) %>%
  ggplot(aes(x = filler_type_code, y = FI_mean, fill = stress_code)) +
  geom_col(position = "dodge")+
  geom_errorbar(aes(ymin=lower_limit, ymax=upper_limit), width=.2,
                position=position_dodge(.9))+
  geom_jitter(alpha = 1/10) +
  scale_color_viridis_d(option = "inferno") +
  xlab("Filler Type") +
  ylab("Focused Immersion") +
  guides(fill=guide_legend(title="Task Type"))+
  theme(axis.text = element_text(size = 16),
        legend.title = element_text(color = "black", size = 14),
        legend.text = element_text(color = "black", size = 12),
        axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5),
        axis.title = element_text(size = 20),
        plot.title = element_text(size = 25, face = "bold"),
        axis.title.x = element_blank(),
        panel.border = element_rect(color = "black", fill = NA, size = 1.2))

# plot_grid(p1, p2, align = "h", rel_widths = c(1, 1.3))

plot_grid(fig1, fig2, labels = c('A', 'B'), label_size = 20, label_colour = "blue")



# bar chart
fig3 <- dataset1 %>% group_by(delay, filler_type_code) %>% summarise(FI_mean = mean(FI_final), sd = sd(FI_final),
                                                                     N = sum(!is.na(FI_final)),
                                                                     upper_limit = FI_mean + sd/sqrt(N),
                                                                     lower_limit = FI_mean - sd/sqrt(N)) %>%
  ggplot(aes(x = filler_type_code, y = FI_mean, fill = delay)) +
  geom_col(position = "dodge")+
  geom_errorbar(aes(ymin=lower_limit, ymax=upper_limit), width=.2,
                position=position_dodge(.9))+
  geom_jitter(alpha = 1/10) +
  scale_color_viridis_d(option = "inferno") +
  xlab("Filler Type") +
  ylab("Focused Immersion") +
  guides(fill=guide_legend(title="Delay"))+
  theme(axis.text = element_text(size = 16),
        legend.title = element_text(color = "black", size = 14),
        legend.text = element_text(color = "black", size = 12),
        axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5),
        axis.title = element_text(size = 20),
        plot.title = element_text(size = 25, face = "bold"),
        axis.title.x = element_blank(),
        # legend.position = "none",
        panel.border = element_rect(color = "black", fill = NA, size = 1.2))



fig4 <- dataset1 %>% group_by(delay, filler_type_code) %>% summarise(TD_mean = mean(TD_final), sd = sd(TD_final),
                                                                           N = sum(!is.na(TD_final)),
                                                                           upper_limit = TD_mean + sd/sqrt(N),
                                                                           lower_limit = TD_mean - sd/sqrt(N)) %>%
  ggplot(aes(x = filler_type_code, y = TD_mean, fill = delay)) +
  geom_col(position = "dodge")+
  geom_errorbar(aes(ymin=lower_limit, ymax=upper_limit), width=.2,
                position=position_dodge(.9))+
  geom_jitter(alpha = 1/10) +
  scale_color_viridis_d(option = "inferno") +
  xlab("Filler Type") +
  ylab("Temporal Dissociation") +
  guides(fill=guide_legend(title="Delay"))+
  theme(axis.text = element_text(size = 16),
        legend.title = element_text(color = "black", size = 14),
        legend.text = element_text(color = "black", size = 12),
        axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=0.5),
        axis.title = element_text(size = 20),
        plot.title = element_text(size = 25, face = "bold"),
        axis.title.x = element_blank(),
        panel.border = element_rect(color = "black", fill = NA, size = 1.2))
# 
# dataset1$fillcol <- NA
# dataset1$fillcol[dataset1$filler_type_code %in% c("Breathing", "Convo") & dataset1$stress_code == "Stress"] <- "green3"
# dataset1$fillcol[dataset1$filler_type_code %in% c("Micro", "Manmade", "Nature") & dataset1$stress_code == "Stress"] <- "indianred1"
# dataset1$fillcol[dataset1$filler_type_code %in% c("Progress", "Micro", "Breathing") & dataset1$stress_code == "IR"] <- "green3"
# dataset1$fillcol[dataset1$filler_type_code %in% c("Manmade") & dataset1$stress_code == "IR"] <- "indianred1"
# 
# # Env$fillcol[Env$Segment %in% c(2,10,12) & Env$Group == "B"] <- "blue"

# FI_task_plot <- ggplot(data=subset(dataset1, !is.na(filler_type_code)),aes(x = filler_type_code, y = FI_final, fill = fillcol)) 

farb <- c("#428953", "#CE2929", "#A3DD57")

fig5 <- dataset1 %>% group_by(stress_code, delay, filler_type_code) %>% summarise(PWT_mean = mean(PWT), sd = sd(PWT),
                                                                     N = sum(!is.na(PWT)),
                                                                     upper_limit = PWT_mean + sd/sqrt(N),
                                                                     lower_limit = PWT_mean - sd/sqrt(N)) %>%
  ungroup() %>% 
  ggplot(aes(x = filler_type_code, y = PWT_mean, fill=delay)) +
  #scale_colour_manual(values = c( "#fc8d59", "#ffffbf", "#91cf60"))+
  geom_col(position = "dodge", colour = "black", size=0.5)+
  # geom_errorbar(aes(ymin=lower_limit, ymax=upper_limit), width=.2,
  #               position=position_dodge(.9))+
  geom_jitter(alpha = 1/10) +
  facet_grid(delay~stress_code, scales = "free")+
  #scale_color_viridis_d(option = "inferno") +
 
  xlab("Filler Type") +
  ylab("Perceived Waiting Time") +
  guides(fill=guide_legend(title="Delay"))+
  theme_bw()+
  scale_fill_manual(breaks = c("8", "16", "32"), 
                    values=c("#ccece6", "#66c2a4", "#41ae76")) +
  # coord_flip()+
  theme(axis.text = element_text(size = 16),
        strip.text.x = element_text(
          size = 10, color = "black", face = "bold"
        ),
        strip.text.y = element_text(
          size = 10, color = "black", face = "bold"
        ),
        legend.title = element_text(color = "black", size = 12),
        legend.text = element_text(color = "black", size = 12),
        axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=0.5, size=9),
        axis.text.y = element_text(angle = 0, vjust = 0.5, hjust=0.5, size=9),
        axis.title = element_text(size = 14),
        plot.title = element_text(size = 25, face = "bold"),
        axis.title.x = element_blank(),
        # axis.title.y = element_blank(),
        panel.border = element_rect(color = "black", fill = NA, size = 1.2),
        legend.position="right")+
  geom_text(aes(label = round(PWT_mean, 1)), size = 4, hjust=0.5, vjust=1, position =  position_dodge(width = 1))

fig5
# p <- qplot(PWT, filler_type_code, dfillata=dataset1, facets = delay ~ stress_code)





mean_wt <- data.frame(delay = c("8", "16"), wt = c(8, 16))
fig5 + geom_hline(aes(yintercept = wt), mean_wt)

dataset1$delay <- factor(dataset1$delay,      # Reordering group factor levels
                         levels = c("8", "16", "32"))

# plot_grid(p1, p2, align = "h", rel_widths = c(1, 1.3))

plot_grid(fig3, fig4, labels = c('A', 'B'), label_size = 20, label_colour = "blue")


# Box plot facetted by "dose"
p1 <- ggboxplot(dataset1, x = "filler_type_code", y = "FI_final",
               color = "filler_type_code", palette = "jco", ggtheme = theme_gray(),
               # add = "jitter",
               facet.by = "delay", short.panel.labs = FALSE) 
# labels <- c("8" = "8-sec", "16" = "16-sec", "32" = "32-sec")
# p1 + facet_grid(. ~ delay, labeller=labeller(delay = labels))

p1 <- p1 + rotate()

p1 <- ggpar(p1,
            # title = "Box Plot created with ggpubr",
            # subtitle = "Length by dose",
            # caption = "Source: ggpubr",
            ylab ="Focused Immersion",
            #xlab = "Teeth length",
            legend.title = "Filler Type",
            legend = "top",
            font.x = c(14, "bold"),
            font.y = c(14, "bold")) +
  font("legend.title", face = "bold")

p1 + stat_compare_means(label = "p.signif", method = "t.test", ref.group = ".all.", hide.ns = TRUE) + rremove("ylab") +
  theme(axis.text = element_text(size = 16),
        strip.text.x = element_text(
          size = 12, color = "black", face = "bold"
        ),
        legend.title = element_text(color = "black", size = 14),
        legend.text = element_text(color = "black", size = 12),
        axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=0.5, size=12),
        axis.text.y = element_text(angle = 45, vjust = 0.5, hjust=0.5, size=12),
        axis.title = element_text(size = 20),
        plot.title = element_text(size = 25, face = "bold"),
        # axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.border = element_rect(color = "black", fill = NA, size = 1.2),
        legend.position="top")

# regrDF <- data.frame(Type = c('a','b','c'), lbl = c('Regression_a', 'Regression_b', 'Regression_c'))


# my_comparisons <- list( c("Convo", "No-filler"), c("Nature", "No-filler"))
#temporal dissociation
p2 <- ggboxplot(dataset1, x = "filler_type_code", y = "TD_final",
                color = "filler_type_code", palette = "jco", ggtheme = theme_gray(),
                # add = "jitter",
                facet.by = "delay", short.panel.labs = FALSE) 
# labels <- c("8" = "8-sec", "16" = "16-sec", "32" = "32-sec")
# p1 + facet_grid(. ~ delay, labeller=labeller(delay = labels))

p2 <- p2 + rotate()

p2 <- ggpar(p2,
            # title = "Box Plot created with ggpubr",
            # subtitle = "Length by dose",
            # caption = "Source: ggpubr",
            ylab ="Temporal Dissociation",
            #xlab = "Teeth length",
            legend.title = "Filler Type",
            legend = "top",
            font.x = c(14, "bold"),
            font.y = c(14, "bold")) +
  font("legend.title", face = "bold") 

p2 + stat_compare_means(label = "p.signif", method = "t.test", ref.group = ".all.", hide.ns = TRUE) + rremove("ylab") +

# stat_compare_means(comparisons = my_comparisons)+ # Add pairwise comparisons p-value
#   stat_compare_means(label.y = 8) 


  theme(axis.text = element_text(size = 16),
        strip.text.x = element_text(
          size = 12, color = "black", face = "bold"
        ),
        legend.title = element_text(color = "black", size = 14),
        legend.text = element_text(color = "black", size = 12),
        axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=0.5, size=12),
        axis.text.y = element_text(angle = 45, vjust = 0.5, hjust=0.5, size=12),
        axis.title = element_text(size = 20),
        plot.title = element_text(size = 25, face = "bold"),
        # axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.border = element_rect(color = "black", fill = NA, size = 1.2),
        legend.position="top")


# Focused Immersion----------------------------------------------------------------------------------------------------

dataset1$fillcol <- NA
dataset1$fillcol[dataset1$filler_type_code %in% c("Convo","Ellipses", "No-filler") & dataset1$delay == "8"] <- "#2ca25f"
dataset1$fillcol[dataset1$filler_type_code %in% c("Nature","Manmade") & dataset1$delay == "8"] <- "#99d8c9"
dataset1$fillcol[dataset1$filler_type_code %in% c("Convo", "Progress", "Breathing", "Micro") & dataset1$delay == "16"] <- "#2ca25f"
dataset1$fillcol[dataset1$filler_type_code %in% c("Manmade", "Nature") & dataset1$delay == "16"] <- "#99d8c9"
dataset1$fillcol[dataset1$filler_type_code %in% c("Micro", "Breathing", "Progress") & dataset1$delay == "32"] <- "#2ca25f"
dataset1$fillcol[dataset1$filler_type_code %in% c("Manmade", "Nature") & dataset1$delay == "32"] <- "#99d8c9"
# Env$fillcol[Env$Segment %in% c(2,10,12) & Env$Group == "B"] <- "blue"

FI_plot <- ggplot(data=subset(dataset1, !is.na(filler_type_code)),aes(x = filler_type_code, y = FI_final, fill = fillcol)) +
  geom_boxplot()+
  stat_summary(fun=mean, geom="point", shape=20, size=5, color="black", fill="black") +
  facet_wrap(~ delay)+
  scale_fill_identity()+
  coord_flip() + 
  theme_bw()+
  xlab("Filler Type") +
  ylab("Focused Immersion") +
  theme(axis.text = element_text(size = 16),
        strip.text.x = element_text(
          size = 12, color = "black", face = "bold"
        ),
        legend.title = element_text(color = "black", size = 14),
        legend.text = element_text(color = "black", size = 12),
        axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=0.5, size=12),
        axis.text.y = element_text(angle = 45, vjust = 0.5, hjust=0.5, size=12),
        axis.title = element_text(size = 20),
        plot.title = element_text(size = 25, face = "bold"),
        # axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.border = element_rect(color = "black", fill = NA, size = 1.2),
        legend.position="top")

FI_plot

#-----------------------------------------------------------------------------------
# Temporal Dissociation
                                              ##99d8c9 = > light green
                                               ##2ca25f => dark green

dataset1$fillcol <- NA
dataset1$fillcol[dataset1$filler_type_code %in% c("Convo","Nature") & dataset1$delay == "8"] <- "#2ca25f"
dataset1$fillcol[dataset1$filler_type_code %in% c("No-filler") & dataset1$delay == "8"] <- "#99d8c9"
dataset1$fillcol[dataset1$filler_type_code %in% c("Breathing", "Micro") & dataset1$delay == "16"] <- "#2ca25f"
dataset1$fillcol[dataset1$filler_type_code %in% c("Convo") & dataset1$delay == "16"] <- "#99d8c9"
dataset1$fillcol[dataset1$filler_type_code %in% c("Micro") & dataset1$delay == "32"] <- "#2ca25f"
dataset1$fillcol[dataset1$filler_type_code %in% c("Convo", "progress", "Nature", "No-filler") & dataset1$delay == "32"] <- "#99d8c9"
# Env$fillcol[Env$Segment %in% c(2,10,12) & Env$Group == "B"] <- "blue"

TD_plot <- ggplot(data=subset(dataset1, !is.na(filler_type_code)),aes(x = filler_type_code, y = TD_final, fill = fillcol)) +
  geom_boxplot()+
  stat_summary(fun=mean, geom="point", shape=20, size=5, color="black", fill="black") +
  facet_wrap(~ delay)+
  scale_fill_identity()+
  coord_flip() +
  theme_bw()+
  xlab("Filler Type") +
  ylab("Temporal Dissociation") +
  theme(axis.text = element_text(size = 16),
        strip.text.x = element_text(
          size = 12, color = "black", face = "bold"
        ),
        legend.title = element_text(color = "black", size = 14),
        legend.text = element_text(color = "black", size = 12),
        axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=0.5, size=12),
        axis.text.y = element_text(angle = 45, vjust = 0.5, hjust=0.5, size=12),
        axis.title = element_text(size = 20),
        plot.title = element_text(size = 25, face = "bold"),
        # axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.border = element_rect(color = "black", fill = NA, size = 1.2),
        legend.position="top") 

TD_plot
#plot_grid(FI_plot, TD_plot, labels = c('A', 'B'), label_size = 20, label_colour = "blue")
plot_grid(FI_plot, TD_plot)



#--------------------------------------------------------------------------------------------------------------------------------------
# Cognitive scores

##99d8c9 = > light green
##2ca25f => dark green


dataset1$fillcol <- NA
dataset1$fillcol[dataset1$filler_type_code %in% c("Micro")& dataset1$stress_code == "IR"] <- "#2ca25f"
dataset1$fillcol[dataset1$filler_type_code %in% c("Convo", "Ellipses", "Progress", "Nature", "Breathing", "No-filler") & dataset1$stress_code == "IR"] <- "#99d8c9"

cog_plot_new <- ggplot(data=subset(dataset1, !is.na(filler_type_code)),aes(x = filler_type_code, y = cognitive, fill = fillcol)) +
  geom_boxplot()+
  stat_summary(fun=mean, geom="point", shape=20, size=5, color="black", fill="black") +
  scale_fill_identity()+
  facet_wrap(~ stress_code)+
  coord_flip() +
  theme_bw()+
  xlab("Filler Type") +
  ylab("Cognitive") +
  theme(axis.text = element_text(size = 16),
        strip.text.x = element_text(
          size = 12, color = "black", face = "bold"
        ),
        legend.title = element_text(color = "black", size = 14),
        legend.text = element_text(color = "black", size = 12),
        axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=0.5, size=12),
        axis.text.y = element_text(angle = 45, vjust = 0.5, hjust=0.5, size=9),
        axis.title = element_text(size = 20),
        plot.title = element_text(size = 25, face = "bold"),
        # axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.border = element_rect(color = "black", fill = NA, size = 1.2),
        legend.position="top") 

cog_plot_new

dataset1$fillcol <- NA
dataset1$fillcol[dataset1$filler_type_code %in% c("Micro", "Breathing", "Convo", "Progress", "Ellipses", "No-filler") & dataset1$stress_code == "Stress"] <- "#2ca25f"
# dataset1$fillcol[dataset1$filler_type_code %in% c("Micro") & dataset1$stress_code == "Stress"] <- "greenyellow"
dataset1$fillcol[dataset1$filler_type_code %in% c("Manmade", "Nature") & dataset1$stress_code == "Stress"] <- "#99d8c9"
dataset1$fillcol[dataset1$filler_type_code %in% c("Progress", "Micro", "Breathing") & dataset1$stress_code == "IR"] <- "#2ca25f"
dataset1$fillcol[dataset1$filler_type_code %in% c("Manmade") & dataset1$stress_code == "IR"] <- "#99d8c9"

# Env$fillcol[Env$Segment %in% c(2,10,12) & Env$Group == "B"] <- "blue"

my_comparisons <- list( c("Breathing", "Manmade"), c("Micro", "Breathing"), c("Micro", "Progress"))

FI_task_plot <- ggplot(data=subset(dataset1, !is.na(filler_type_code)),aes(x = filler_type_code, y = FI_final, fill = fillcol)) +
  geom_boxplot()+
  stat_summary(fun=mean, geom="point", shape=20, size=5, color="black", fill="black") +
  # stat_summary(fun.data=mean_cl_boot, geom="errorbar", colour="black", width=0.2) +
  facet_wrap(~ stress_code)+
  scale_fill_identity()+
  coord_flip() +
  theme_bw()+
  xlab("Filler Type") +
  ylab("Focused Immersion") +
  theme(axis.text = element_text(size = 16),
        strip.text.x = element_text(
          size = 12, color = "black", face = "bold"
        ),
        legend.title = element_text(color = "black", size = 14),
        legend.text = element_text(color = "black", size = 12),
        axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=0.5, size=12),
        axis.text.y = element_text(angle = 45, vjust = 0.5, hjust=0.5, size=9),
        axis.title = element_text(size = 20),
        plot.title = element_text(size = 25, face = "bold"),
        # axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.border = element_rect(color = "black", fill = NA, size = 1.2),
        legend.position="top")
FI_task_plot
#----------------------------------------------------------------------------------------------------------
#Enjoyment
dataset1$fillcol <- NA
dataset1$fillcol[dataset1$filler_type_code %in% c("Breathing") ] <- "#2ca25f"
dataset1$fillcol[dataset1$filler_type_code %in% c("Micro", "Manmade", "Nature", "Progress", "Convo") ] <- "#99d8c9"


HE <- ggplot(data=subset(dataset1, !is.na(filler_type_code)),aes(x = filler_type_code, y = HE_final, fill = fillcol)) +
  geom_boxplot()+
  stat_summary(fun=mean, geom="point", shape=20, size=5, color="black", fill="black") +
  # stat_summary(fun.data=mean_cl_boot, geom="errorbar", colour="black", width=0.2) +
  # facet_wrap(~ stress_code)+
  scale_fill_identity()+
  theme_bw()+
  # coord_flip() +
  xlab("Filler Type") +
  ylab("Heightened Enjoyment") +
  theme(axis.text = element_text(size = 16),
        strip.text.x = element_text(
          size = 20, color = "black", face = "bold"
        ),
        legend.title = element_text(color = "black", size = 14),
        legend.text = element_text(color = "black", size = 12),
        axis.text.x = element_text(angle = 25, vjust = 0.5, hjust=0.5, size=9),
        axis.text.y = element_text(angle = 0, vjust = 0.5, hjust=0.5, size=12),
        axis.title = element_text(size = 12),
        plot.title = element_text(size = 25, face = "bold"),
        axis.title.x = element_blank(),
        # axis.title.y = element_blank(),
        panel.border = element_rect(color = "black", fill = NA, size = 1.2),
        legend.position="top")
HE

#plot_grid(cog_plot_new, FI_task_plot, labels = c('A', 'B'), label_size = 20, label_colour = "blue")
plot_grid(cog_plot_new, FI_task_plot)

