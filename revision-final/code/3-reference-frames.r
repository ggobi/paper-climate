source("models-nasa.r")
source("maps.r")
source("glyph.r")

month_preds <- glyphs(month_preds, "long", "month", "lat", "pred")

ggplot(month_preds, aes(gx, gy, group = gid)) + 
  map_nasa +
  add_ref_lines(month_preds) +
  geom_path() + 
  theme_fullframe() 
ggsave("../images/ref-line.pdf", width = 6, height = 6)

ggplot(month_preds, aes(gx, gy, group = gid)) + 
  map_nasa +
  geom_path() + 
  add_ref_boxes(month_preds) +
  theme_fullframe() 
ggsave("../images/ref-box.pdf", width = 6, height = 6)

ggplot(month_preds, aes(month, pred)) + 
  geom_line(aes(group = gid), alpha = 1/10) + 
  opts(aspect.ratio = 1) + 
  xlab("Month") + 
  ylab("Average temperature (K)")
ggsave("../images/ref-legend.pdf", width = 4, height = 4)

