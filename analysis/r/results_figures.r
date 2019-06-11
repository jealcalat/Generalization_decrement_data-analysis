source("~/Documentos/2019A/paper_gen_decrement/analysis/r/wrapper.r")

# ==============================================================================
# Load data ----
path_analysis = "~/Documentos/2019A/paper_gen_decrement/analysis/"
path_data_results = path_analysis + "data_results/"
path_figures = path_analysis + "figures/"

load(path_data_results + "rate_norm.RData")
load(path_data_results + 'dkl.RData')
load(path_data_results + 'qqIRI_data.RData')
load(path_data_results + 'contiguity.RData')
load(list.files(path = path_data_results, 
                pattern = "^start(.*)RData",
                full.names = T))

load(path_data_results + "overall_rate_data.RData")
load(path_data_results + "correlation_data.RData")
# ==============================================================================
# See plots in new window
options(device = "x11")
# ggplot theme for all plots
theme_set(pub_theme())
# ==============================================================================

# ---------------- Figure 2 - Overall rate -------------------------------------


spread_ovrate <- spread(ov_rate2ph, cde, rate)
colnames(spread_ovrate)[4:5] <- c("cde1","cde2")

orate_mean <- spread_ovrate %>%
  group_by(phase,sesion,sujeto) %>%
  summarise(mcde1 = mean(cde1),
            mcde2 = mean(cde2),
            pMean = (mcde1 + mcde2)/2) %>% 
  group_by(phase,sesion) %>%
  mutate(gMean = mean(c(mcde1,mcde2)))

orate_mean$adj <- orate_mean$gMean - orate_mean$pMean

orate_mean$mcde1_adj <- orate_mean$mcde1 + orate_mean$adj
orate_mean$mcde2_adj <- orate_mean$mcde2 + orate_mean$adj

adj_rates <- orate_mean %>% 
  as.data.frame() %>%
  select(phase,sesion,mcde1_adj,mcde2_adj) %>%
  gather(cde,rate,mcde1_adj,mcde2_adj) %>%
  group_by(phase,sesion,cde) %>%
  summarise(mrate = mean(rate),
            min.y = lower(rate),
            max.y = upper(rate))

adj_rates$cde %<>% as.factor()

levels(adj_rates$cde) <- c("1","2")

pdf(path_figures + "Figure_2.pdf",
    height = 7 * 0.5,
    width = 7,
    pointsize = 11)

ggplot(adj_rates,
       aes(x = sesion,
           y = mrate)) +
  geom_path(aes(color = cde),size = 0.3) +
  geom_errorbar(aes(ymin = min.y,
                    ymax = max.y),
                size = 0.3) +
  geom_point(aes(color = cde,
                 fill = cde),
             size = 1.5,
             shape = 21) +
  scale_fill_manual(name ="Component",values = fill_param,
                    labels=c("1" = "Delayed","2"="Immediate")) +
  scale_color_manual(name ="Component",values = col_param,
                     labels=c("1" = "Delayed","2"="Immediate")) +
  scale_y_continuous(breaks = seq(0,30,5),
                     limits = c(0,30),
                     expand = c(0,0),
                     labels = every_nth(seq(0,30,5),2,T,T)) +
  scale_x_continuous(breaks = seq(0,40,5),
                     labels = every_nth(seq(0,40,5),2,T,T)) +
  facet_wrap(~phase,scales = "free_x") +
  labs(y = TeX('Mean rate (r/min) $\\pm$ SEM'),x = "Sessions") +
  theme(legend.position = c(0.95,0.87))

dev.off()

# ==============================================================================

# ---------------- Figure 3 - Correlation coefficient by session ---------------

pdf(path_figures + "Figure_3.pdf",
    height = 3,
    width = 6,
    pointsize = 10) 

ggplot(rho_2ph, aes(x = sesion, y = rho)) +
  geom_line(aes(linetype = factor(cde))) +
  geom_point(size = 1.8,shape = 21,
             aes(fill = factor(cde))) +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0,0.5),
                     breaks = seq(0,0.5,0.05),
                     labels = every_nth(seq(0,0.5,0.05),2,T,T)) +
  scale_fill_manual(name ="Component",values = fill_param,
                    labels=c("1" = "Delayed","2"="Immediate")) + 
  scale_x_continuous(breaks = seq(0,50,5),
                     expand = c(0,1),
                     labels = every_nth(seq(0,50,5),2,T,T)) +
  scale_linetype_manual(name ="Component",values = c(1,2),
                        labels=c("1" = "Delayed","2"="Immediate")) + 
  facet_wrap(~phase, scales = "free_x") +
  # guides(color = guide_legend(override.aes = list(size=1.7))) +
  scale_color_manual(values = col_param) +
  labs(y = "Correlation coefficient",x = "Sessions") +
  theme(legend.position = c(0.2,0.86))

dev.off()

# ==============================================================================

# ---------------- Figure 4 - Contiguity ---------------------------------------

pdf(path_figures + "Figure_4.pdf",
    width = 7,
    height = 3.5,
    pointsize = 10)

ggplot(contiguity_both,
       aes(x = abs_err + 1,
           y = ..density..,
           fill = factor(cde))) +
  geom_step_hist(size = 0.4,
                 position='identity',
                 aes(color = factor(cde)),
                 alpha = 0.9) + 
  geom_histogram(size = 0.4,
                 position='identity',
                 alpha = 0.4) +
  scale_x_log10("Time to next reinforcer (s)",
                expand = c(0.02,0),
                breaks = c(1,6,20,60,200)) +
  scale_y_continuous("Density",
                     limits = c(0,2),
                     expand = c(0,0),
                     breaks = seq(0,2,0.25),
                     labels = every_nth(seq(0,2,0.25),2,T,T)) +
  scale_color_manual(name ="Component",values = col_param,
                     labels=c("1" = "Delayed","2" = "Immediate"))+
  scale_fill_manual(name = "Component",values = fill_param,
                    labels = c("1" = "Delayed","2" = "Immediate"))+
  expand_limits(x = c(0,500)) +
  facet_wrap(~phase,scales = "free_x") + 
  # geom_vline(xintercept = c(6,20),linetype = 2)+
  annotation_logticks(sides = "b",
                      short = unit(0.5,"mm"),
                      mid = unit(0.7,"mm"),
                      long = unit(0.9,"mm"),
                      size = 0.3) + 
  theme(legend.position = c(0.9,0.75))

dev.off()

# ==============================================================================

# ---------------- Figure 5 - Peak rate and DKL --------------------------------

shape_param <- c("1" = 21, "2" = 21)
## Normalized response rate
BL_norm <- 
  resp_norm_plot(norm_rate_BL, col_param, "lever",
                 fill_param,shape_param,1,
                 p_size = 0.16) +
  theme(plot.margin = margin(0.2,0.5,0,0, "cm"),
        legend.position = c(0.16,0.9),
        legend.text = element_text(size = 7.5),
        legend.title = element_text(size = 8))

BL_head <- 
  resp_norm_plot(norm_rate_BL, col_param, "head",
                 fill_param,shape_param,1,
                 p_size = 0.2) +
  theme(legend.position = 'none',
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        plot.margin = margin(0.2,0.5,0,0, "cm"))

Exp_norm <- 
  resp_norm_plot(norm_rate_Exp, col_param, "lever",
                 fill_param, shape_param,1,
                 p_size = 0.16) +
  theme(plot.margin = margin(0.2,0.5,0,0, "cm"))

Exp_head <- 
  resp_norm_plot(norm_rate_Exp, col_param, "head",
                 fill_param, shape_param,1,
                 p_size = 0.2) +
  theme(legend.position = 'none',
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        plot.margin = margin(0.2,0.5,0,0, "cm"))

## DKL plot
col_param2  <- c("Baseline" = "black","Experimental" = "black")
fill_param2 <- c("Baseline" = "#DADADA","Experimental" = "white")

dkl_plt <- 
  ggplot(d %>% na.omit(), aes(x = phase, y = kld,
                              color = factor(phase),
                              fill = factor(phase))) +  
  geom_jitter(width = 0.2,
              shape = 21,
              size = 1.9,
              stroke = 0.4) +
  geom_crossbar(data = dsum, aes(ymin = kld, ymax = kld),
                size = 0.4,col = "black", width = .6) +
  scale_x_discrete(labels = c("Baseline","Experimental"))+
  scale_y_continuous(breaks = seq(-1,2.5,.5),
                     expand = c(0,0),
                     labels = every_nth(seq(-1,2.5,.5),2,T,F))+
  expand_limits(y = c(-0.5,2.5)) +
  scale_color_manual(name ="",values = col_param2,
                     labels=c("Baseline" = "Baseline",
                              "Experimental" = "Experimental"))+
  scale_fill_manual(name ="",values = fill_param2,
                    labels = c("Baseline" = "Baseline",
                               "Experimental" = "Experimental"))+
  labs(x = "Phase", y = TeX('$ D_{KL}(\\textit{P} || \\textit{Q})$')) +
  theme(axis.title.y = element_text(family = 'sans',
                                    size = 12))

# - All together -

pdf(path_figures + "Figure_5.pdf",
    height = 7.3,
    width = 3.3,
    pointsize = 12)

grid.arrange(BL_norm + 
               labs(x = "", y = "") +
               theme(plot.margin = margin(1,0.3,0,0.25, "cm")),
             Exp_norm + 
               labs(y = "") +
               theme(plot.margin = margin(0.5,0.3,0,0.3, "cm")),
             dkl_plt + 
               theme(plot.margin = margin(0.7,0.1,0,0.3, "cm")),
             ncol = 2,
             layout_matrix = rbind(c(1,1,1,1),
                                   c(1,1,1,1),
                                   c(1,1,1,1),
                                   c(2,2,2,2),
                                   c(2,2,2,2),
                                   c(2,2,2,2),
                                   c(3,3,3,3),
                                   c(3,3,3,3),
                                   c(3,3,3,3)))

grid.text(TeX("Normalized response rate ($\\pm SEM$)"),
          x = 0.04,y = 0.63, rot = 90,
          gp = gpar(fontface = "bold",
                    size = 12))

print(BL_head, 
      vp = grid::viewport(width = 0.394, 
                          height = 0.17,
                          x = unit(0.83, "npc"), 
                          y = unit(0.91, "npc")))
print(Exp_head, 
      vp = grid::viewport(width = 0.394,
                          height = 0.17,
                          x = unit(0.83, "npc"), 
                          y = unit(0.6, "npc")))

grid.text(c("A","B","C"),
          x = c(0.15,0.15,0.15),y = c(0.96,0.66,0.32),
          gp = gpar(fontface = "bold",
                    size = 14))
dev.off()


# ==============================================================================

# ---------------- Figure 6 - QQplot of IRI ------------------------------------

## Was done in Python
# ggplot(qq_2ph,
#        aes(x = Delay,y = Immediate)) +
#   geom_point(size = 1.5,
#              stroke = 0.2,
#              shape = 21,
#              color = "black",
#              fill = "#bdbdbd") +
#   geom_rug(col=rgb(.5,0,0,alpha=.2)) +
#   facet_wrap(~phase,scales = "free_y") +
#   scale_y_continuous(limits = c(60,95),
#                      breaks = seq(60,95,5),
#                      labels = every_nth(seq(60,95,5),2,T,T)) +
#   scale_x_continuous(breaks = seq(60,95,5),
#                      limits = c(60,95),
#                      labels = every_nth(seq(60,95,5),2, T,T)) +
#   geom_abline(intercept = 0,slope = 1,linetype = 2,size = 0.5) +
#   xlab("Delayed IRI (s)")+
#   ylab("Immediate IRI (s)")



# ==============================================================================

# --------------- Figure 7 - Start, stops, FWHM and Peak time ------------------
fwhm_2phases %<>% as.data.frame()
start_den <- 
  ggplot(df_p_lhl,aes(x = start,..density..)) +
  geom_step_hist(size = 0.4,
                 position='identity',
                 aes(color = factor(cde))) + 
  geom_histogram(size = 0.5,position='identity',
                 alpha = 0.5,
                 aes(fill = factor(cde))) +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0,0.05),
                     breaks = seq(0,0.05,0.01),
                     labels = every_nth(seq(0,0.05,0.01),2,T,F)) +
  scale_x_continuous(breaks = seq(0,180,30),
                     limits = c(0,180),
                     expand = c(0,0),
                     labels = every_nth(seq(0,180,30),2, T,T)) +
  scale_color_manual(name ="Component",values = col_param,
                     labels=c("1" = "Delayed","2"="Immediate"))+
  scale_fill_manual(name ="Component",values = fill_param,
                    labels=c("1" = "Delayed","2"="Immediate"))+
  facet_wrap(~phase) +
  labs(x = "",y = "")+
  theme(legend.position = c(0.45,0.65),
        legend.text = element_text(size = 9),
        legend.title = element_text(size = 9),
        legend.key.size = unit(0.5,"cm"))

stop_den <- 
  ggplot(df_p_lhl,aes(x = stop,..density..)) +
  geom_step_hist(size = 0.4,
                 position='identity',
                 aes(color = factor(cde))) + 
  geom_histogram(size = 0.5,position='identity',
                 alpha = 0.5,
                 aes(fill = factor(cde))) +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0,0.05),
                     breaks = seq(0,0.05,0.01),
                     labels = every_nth(seq(0,0.05,0.01),2,T,F)) +
  scale_x_continuous(breaks = seq(0,180,30),
                     limits = c(0,180),
                     expand = c(0,0),
                     labels = every_nth(seq(0,180,30),2, T,T)) +
  scale_color_manual(name ="Component",values = col_param,
                     labels=c("1" = "Delayed","2"="Immediate"))+
  scale_fill_manual(name ="Component",values = fill_param,
                    labels=c("1" = "Delayed","2"="Immediate"))+
  facet_wrap(~phase) +
  labs(x = "Time in trial (s)",y = "")

p1 <- 
  ggplot(fwhm_2phases, 
         aes(x = phase, 
             y = fwhm)) +
  aes(color = cde) +
  geom_point(position = position_jitterdodge(),
             size = 1.4,
             alpha = 0.8,
             shape = 21,
             aes(fill = cde)) +
  stat_summary(fun.y="median",geom="crossbar",
               mapping=aes(ymin=..y.., ymax=..y..), 
               width=0.5,size = 0.3,
               position=position_dodge(0.8),show.legend = FALSE) +
  scale_x_discrete(labels = c("Baseline","Experimental")) +
  scale_y_continuous(breaks = seq(0,180,30),
                     expand = c(0,0),
                     labels = every_nth(seq(0,180,30),2,inverse = TRUE)) +
  expand_limits(y = c(0,180)) +
  scale_color_manual(name ="Component:",values = col_param,
                     labels=c("1" = "Delayed","2" = "Immediate"))+
  scale_fill_manual(name = "Component:",values = fill_param,
                    labels = c("1" = "Delayed","2" = "Immediate"))+
  labs(x = "",y = "FWHM (s)") +
  guides(color = guide_legend(override.aes = list(size=2))) +
  theme(legend.position = "top",
        legend.text = element_text(size = 9),
        legend.title = element_text(size = 9),
        legend.margin=margin(t = 0, b = 0, unit='cm'),
        legend.key.size = unit(0.4,"cm"))


p2 <- ggplot(fwhm_2phases, 
             aes(x = phase, 
                 y = peak, 
                 color = cde,
                 group = cde)) +
  geom_point(position = position_jitterdodge(0.5),
             size = 1.4,
             shape = 21,
             alpha = 0.8,
             aes(fill = cde)) +
  stat_summary(fun.y="mean",geom="crossbar", 
               mapping=aes(ymin=..y.., ymax=..y..), 
               width=0.5,size = 0.3,
               position=position_dodge(0.8),show.legend = FALSE) +
  scale_x_discrete(labels = c("Baseline","Experimental")) +
  scale_y_continuous(breaks = seq(0,180,30),
                     expand = c(0,0),
                     labels = every_nth(seq(0,180,30),2,inverse = TRUE)) +
  expand_limits(y = c(0,180)) +
  scale_color_manual(name ="Component",values = col_param,
                     labels=c("1" = "Delayed","2" = "Immediate"))+
  scale_fill_manual(name = "Component",values = fill_param,
                    labels = c("1" = "Delayed","2" = "Immediate"))+
  labs(x = "Phase",y = "Middle time (s)") 

pdf("Figure_7.pdf",
    height = 6.5,
    width = 6.5,
    pointsize = 8)

grid.arrange(start_den + theme(plot.margin=margin(0.4, 0.3, 0, 0.4, "cm")),
             p1 + theme(plot.margin=margin(0.4, 0.5, 0, 0.3, "cm")),
             stop_den + theme(plot.margin=margin(0.4, 0.3, 0, 0.4, "cm")),
             p2 + theme(plot.margin=margin(0.7, 0.5, 0, 0.3, "cm")),
             left = textGrob("Density",
                             gp = gpar(fontsize=12), rot=90),
             nrow = 2)

grid.text("A",x = 0.08,y=0.97,gp = gpar(fontface = "bold",fontsize=14))
grid.text("B",x = 0.08,y=0.47,gp = gpar(fontface = "bold",fontsize=14))
grid.text("C",x = 0.55,y=0.97,gp = gpar(fontface = "bold",fontsize=14))
grid.text("D",x = 0.55,y=0.47,gp = gpar(fontface = "bold",fontsize=14))

dev.off()

# ==============================================================================

# --------------- Figure 8 - Optimal clusters ----------------------------------

kmedoids %<>% as.data.frame
kmedoids_sum <- kmedoids %>%
  group_by(phase,sujeto,cde,opt_k) %>%
  summarise(counts = n()) %>%
  as.data.frame()

kmedoids_sum$phase <- factor(kmedoids_sum$phase)
levels(kmedoids_sum$phase) <- c("Experimental","Baseline")
kmedoids_sum$phase <- factor(kmedoids_sum$phase,
                             levels = c("Baseline","Experimental"))

pdf("Figure_8.pdf",
    width = 6,
    height = 3,
    pointsize = 10)

ggplot(kmedoids_sum,
       aes(x = opt_k,y = counts)) +
  geom_boxplot(aes(x = factor(opt_k),
                   color = factor(cde),
                   fill = factor(cde)),
               alpha = 0.5,
               # outlier.shape = NA,
               lwd = 0.4) +
  facet_wrap(~phase) + 
  scale_y_continuous(breaks = seq(1,28,2),
                     expand = c(0,0),
                     limits = c(0,27),
                     labels = every_nth(seq(1,28,2),2,T,T)) +
  scale_color_manual(name ="Component",values = col_param,
                     labels=c("1" = "Delayed","2" = "Immediate")) +
  scale_fill_manual(name ="Component",values = fill_param,
                    labels=c("1" = "Delayed","2" = "Immediate")) +
  labs(x = "Optimal number of clusters", y = "Counts") +
  theme(legend.position = c(0.1,0.8),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 8))

dev.off()