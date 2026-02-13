
#print("+ + + start bulk_donut + + +")

g_label = c("keine Angabe", "weiblich", "männlich")
g_count = c(1, 17, 8)
#g_count = c(1, 47, 38)
g_perc = round(g_count / (sum(g_count) / 100), 1)

gender_df = data.frame(Gender = g_label, g_count, g_perc)

#hsize = 4
hsize = 1.8

gender_df$hsize = hsize

## https://r-charts.com/part-whole/donut-chart-ggplot2/

require(ggplot2)
p_insert =  ggplot(gender_df, aes(x = hsize, y = g_count, fill = Gender)) +
  geom_col(color = "black") +
  geom_text(aes(label = g_perc),
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") +
  scale_fill_manual(values = c("#D0D1E6",
                               "#74A9CF", "#0570B0"),
                    guide = guide_legend(reverse = TRUE)) + #reorder legend
  #xlim(c(0.5, hsize + 1,5)) +
  #xlim(c(0.5, hsize + 1)) +
  xlim(c(0.6, hsize + .7)) +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank())


#print("+ + + end bulk_donut + + +")

p_insert


