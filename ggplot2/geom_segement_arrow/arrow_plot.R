plot_data<- readr::read_tsv(file.path("/home/huff/project/data_for_plot.txt"))

plot_data %>%
  
ggplot() +
  geom_segment(data=plot_data,mapping = aes(
    x = x1,
    y = y1,
    xend = x2,
    yend = y2,
    colour = "#99D594",
    linetype = "solid",
    size=10
  ),
    arrow = arrow(length = unit(0.03, "npc"))
  ) +
  scale_color_manual(values="#FC8D59") +
  geom_text(aes(x=x2+3,y=y2,label=`Median time`)) +
  scale_y_discrete(limits = plot_data$y1) +
  my_theme +
  theme(
    axis.title.y = element_blank(),
    legend.position = "none",
    panel.grid = element_line(color=NULL)
  ) +
  labs(x="Time (days)")
ggsave("/home/huff/project/arrow_plot.pdf",device = "pdf",width = 6,height = 3)
ggsave("/home/huff/project/arrow_plot.png",device = "png",width = 6,height = 3)
