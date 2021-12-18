
library(pacman)
p_load(tidyverse,ggridges,tidyfst,showtext,patchwork,magick,cowplot,lubridate)

read_csv("CAS-Fellow-2001-2021.csv") %>% 
  transmute(name = 姓名,age = 年龄,ins = 工作单位,
            subject = 专业,year = as.factor(年份),field = 学部)-> cas

cas

theme_cas = function(base_size = 12){
  require(showtext)
  font_add(family = "kaiti",regular = "STKAITI.TTF")
  font_add(family = "times",regular = "times.ttf")
  showtext_auto()
  
  half_line <- base_size/2
  theme(
    plot.title = element_text(size = base_size * 1.8, hjust = 0, vjust = 1, family = "kaiti",
                              face = "bold", margin = margin(b = half_line * 1.2)), 
    plot.caption = element_text(size = rel(1), family = "kaiti",
                                hjust = 1, vjust = 1, margin = margin(t = half_line * 0.9)),
    plot.background = element_rect(fill = "skyblue",color = "transparent"),
    plot.margin = margin(base_size, base_size, base_size, base_size),
    panel.background = element_rect(fill = "transparent",color = "transparent"), 
    axis.ticks = element_blank(),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    axis.title = element_text(family = "kaiti",size = base_size * 1.3,face = "bold"),
    axis.text = element_text(family = "times",size = base_size * 1.1),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10),angle = 90),
    legend.title = element_text(family = "kaiti",size = base_size * 1.1),
    legend.text = element_text(family = "times",size = base_size),
    legend.background = element_rect(fill = "transparent",color = "transparent")
  )
}

theme_set(theme_cas())

yearly_age =  cas %>% 
  mutate_dt(median = median(age),no = .N,by = year) %>% 
  mutate(median_all = median(age)) %>% 
  ggplot(aes(year,age)) +
  geom_violin(aes(fill = no),color = "transparent", trim = F) +
  geom_boxplot(width = .1) +
  stat_summary(fun=median, size = 2,
               geom="point", color="red") +
  geom_hline(aes(yintercept = median(cas$age)),
             linetype = "dashed",color = "blue") +
  scale_fill_distiller(palette = "YlGn",direction = 1,name = "增选人数") +
  theme(legend.position = c(.8,.88),legend.direction="horizontal") +
  labs(y = "入选年龄",x = "参评年份")

field_age = cas %>% 
  mutate_dt(median_age = median(age),by = field) %>% 
  mutate(field = reorder(field,median_age)) %>% 
  ggplot(aes(x = age,y = field))+
  geom_density_ridges(aes(fill = field),
                      quantile_lines = TRUE, quantiles = 2,
                      show.legend = F,alpha = .75) +
  geom_label(aes(x = median_age + 4, label = formatC(median_age, digits = 1, format = "f")),
             family = "times",vjust = -2) +
  labs(y = NULL,x = "入选年龄") +
  expand_limits(y = 8) +
  scale_fill_manual(values = c(
    "#FF3200",
    "#E9E4A6",
    "#0076BB",
    "#E9A17C",
    "#1BB6AF",
    "#172869"
  )) +
  theme(axis.text.y = element_text(family = "kaiti")) 

# 调色板来源：https://github.com/johannesbjork/LaCroixColoR
# install.packages("devtools")
# devtools::install_github("johannesbjork/LaCroixColoR")
# library(LaCroixColoR)
# lacroix_palette("PeachPear", type = "discrete") %>% as.character()

# # 可以选择包含日期信息
# get_cn_date = \(){
#   Sys.Date() -> x
#   str_glue("{year(x)}年{month(x)}月{day(x)}日")
# }
# 
# yearly_age + field_age +
#   plot_annotation(
#     title = "中国科学院院士年龄分布（2001-2021年）",
#     caption=(str_glue(
#       "
#     注：左图中红点为当年院士入选年龄中位数，蓝色虚线为2001-2021年院士入选年龄中位数（56）；右图中标志数据为年龄的中位数
#     
#     黄天元 绘制（huang.tian-yuan@qq.com） | 绘制日期：{get_cn_date()} | 数据来源：中国科学院 （http://casad.cas.cn/）"
#     ))) +
#   plot_layout(widths = c(.6,.4)) -> g0

yearly_age + field_age +
  plot_annotation(
    title = "中国科学院院士入选年龄分布（2001-2021年）",
    caption=(str_glue(
      "
    注：左图中红点为当年院士入选年龄中位数，蓝色虚线为2001-2021年院士入选年龄中位数（56）；右图中标志数据为不同学部院士入选年龄的中位数
    
    黄天元 绘制（huang.tian-yuan@qq.com）| 数据来源：中国科学院 （http://casad.cas.cn/, 2021）"
    ))) +
  plot_layout(widths = c(.6,.4)) -> g0

ggdraw() +
  draw_plot(g0) +
  draw_image("院徽.png",x=-.45,y=-.39,scale = .23)

ggsave(filename = "中科院院士年龄分布.pdf",
       width = 12,height = 5,dpi = 100)

image_read_pdf("中科院院士年龄分布.pdf") %>% 
  image_convert(format = "png") %>% 
  image_write("中科院院士年龄分布.png")
