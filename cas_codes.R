
library(pacman)
p_load(tidyverse,ggridges,tidyfst,showtext,patchwork,magick,cowplot,lubridate)

read_csv("CAS-Fellow-2001-2021.csv") %>% 
  transmute(name = ����,age = ����,ins = ������λ,
            subject = רҵ,year = as.factor(���),field = ѧ��)-> cas

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
  scale_fill_distiller(palette = "YlGn",direction = 1,name = "��ѡ����") +
  theme(legend.position = c(.8,.88),legend.direction="horizontal") +
  labs(y = "��ѡ����",x = "�������")

field_age = cas %>% 
  mutate_dt(median_age = median(age),by = field) %>% 
  mutate(field = reorder(field,median_age)) %>% 
  ggplot(aes(x = age,y = field))+
  geom_density_ridges(aes(fill = field),
                      quantile_lines = TRUE, quantiles = 2,
                      show.legend = F,alpha = .75) +
  geom_label(aes(x = median_age + 4, label = formatC(median_age, digits = 1, format = "f")),
             family = "times",vjust = -2) +
  labs(y = NULL,x = "��ѡ����") +
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

# ��ɫ����Դ��https://github.com/johannesbjork/LaCroixColoR
# install.packages("devtools")
# devtools::install_github("johannesbjork/LaCroixColoR")
# library(LaCroixColoR)
# lacroix_palette("PeachPear", type = "discrete") %>% as.character()

# # ����ѡ�����������Ϣ
# get_cn_date = \(){
#   Sys.Date() -> x
#   str_glue("{year(x)}��{month(x)}��{day(x)}��")
# }
# 
# yearly_age + field_age +
#   plot_annotation(
#     title = "�й���ѧԺԺʿ����ֲ���2001-2021�꣩",
#     caption=(str_glue(
#       "
#     ע����ͼ�к��Ϊ����Ժʿ��ѡ������λ������ɫ����Ϊ2001-2021��Ժʿ��ѡ������λ����56������ͼ�б�־����Ϊ�������λ��
#     
#     ����Ԫ ���ƣ�huang.tian-yuan@qq.com�� | �������ڣ�{get_cn_date()} | ������Դ���й���ѧԺ ��http://casad.cas.cn/��"
#     ))) +
#   plot_layout(widths = c(.6,.4)) -> g0

yearly_age + field_age +
  plot_annotation(
    title = "�й���ѧԺԺʿ����ֲ���2001-2021�꣩",
    caption=(str_glue(
      "
    ע����ͼ�к��Ϊ����Ժʿ��ѡ������λ������ɫ����Ϊ2001-2021��Ժʿ��ѡ������λ����56������ͼ�б�־����Ϊ��ͬѧ��Ժʿ��ѡ�������λ��
    
    ����Ԫ ���ƣ�huang.tian-yuan@qq.com��| ������Դ���й���ѧԺ ��http://casad.cas.cn/, 2021��"
    ))) +
  plot_layout(widths = c(.6,.4)) -> g0

ggdraw() +
  draw_plot(g0) +
  draw_image("Ժ��.png",x=-.45,y=-.39,scale = .23)

ggsave(filename = "�п�ԺԺʿ����ֲ�.pdf",
       width = 12,height = 5,dpi = 100)

image_read_pdf("�п�ԺԺʿ����ֲ�.pdf") %>% 
  image_convert(format = "png") %>% 
  image_write("�п�ԺԺʿ����ֲ�.png")