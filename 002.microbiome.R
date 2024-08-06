### 肠道微生物
library(tidyverse)
rm(list = ls())
gc()

### 公司的肠道微生物风险预测
risk_df = readxl::read_xlsx("/home/shimw/project/200Human/microbiome/CRC_LDT_201例_检测结果_T3-07_4500-1_诊断结果_20231109 - 副本.xlsx")%>%
  tibble::column_to_rownames("Metaboltes")%>%
  t()%>%as.data.frame()%>%
  tibble::rownames_to_column("sample")%>%
  as_tibble()%>%
  mutate(sample = stringr::str_split_fixed(sample, "_",2)[,1])%>%
  mutate(sample = stringr::str_to_title(sample))
sample <- readxl::read_xlsx("/home/shimw/project/200Human/microbiome/华农陈老师.xlsx")%>%
  rename("id" = "序号", sample = "迈维编号")%>%
  select(id, sample)

metadata <- readxl::read_xlsx("/home/shimw/project/200Human/data/microbiome/粪便采样编号.xlsx")%>%
  rename("id" = "编号", name = "姓名")%>%
  mutate(id = str_c("B2",id, sep = "-"))
clinical_indicator <- readr::read_csv("/home/shimw/project/200Human/问卷/000.all_clinical_indicator.csv")%>%
  select(name,sex, age)

metadata[!metadata$name%in%clinical_indicator$name,]


risk_df <- risk_df%>%
  left_join(sample)%>%
  left_join(metadata)%>%
  filter(name%in%clinical_indicator$name)%>%
  left_join(clinical_indicator)%>%
  mutate(level = case_when(`100*Score`>=70~"高风险",
                           `100*Score`>=45~"中风险",
                           `100*Score`<45~"低风险"))

readr::write_excel_csv(risk_df, "/home/shimw/project/200Human/microbiome/肠癌风险.csv")
library(ggplot2)
library(ggpubr)
my_comparisons <- list(c("高风险", "中风险"), c("中风险", "低风险"),c("高风险", "低风险"))

risk_df%>%
  mutate(level = factor(level, levels = c("高风险", "中风险", "低风险")))%>%
ggplot(., aes(level, age, fill = level)) + 
  geom_violin(adjust = .5) +
  geom_boxplot(width=0.1, fill="white")+
  stat_compare_means(comparisons = my_comparisons,label = "p.signif", tip.length=0.02, method = "t.test")+
theme_classic(base_size = 18,base_line_size = 1)+
  labs(y="Age", x=NULL, fill=NULL)+
  scale_fill_manual(values=c("Cluster 1" = "#CB9C7A", "Cluster 2" = "#8696a7", "Cluster 3" = "#CDB97D",
                             "Cluster 4" = "#7b8b6f", "Cluster 5" = "#A59B95"))


### GMWI计算 ###

gmwi = readr::read_csv("/home/shimw/project/200Human/data/microbiome/gmwi_analysis.csv")%>%
  mutate(Sample = str_remove(Sample,"B_kneaddata_metaphlan"))%>%
  mutate(Sample = str_remove(Sample,"A_kneaddata_metaphlan"))%>%
  left_join(metadata, by = c("Sample" = "id"))%>%
  mutate("多样性风险" = case_when(Shannon<1 ~"高风险",
                                Shannon<1.5 ~"中风险",
                                Shannon<2 ~"低风险",
                                T ~"正常"),
         "致病菌风险" = case_when(GMWI< -3.59 ~"高风险",
                            GMWI< -2.74 ~"中风险",
                            GMWI< -1.88 ~"低风险",
                            T ~"正常"))

write_excel_csv(gmwi, "/home/shimw/project/200Human/data/microbiome/gmwi_summary.csv")

(1:201)[!1:201%in%as.numeric(str_remove(gmwi$Sample, "B2-"))]



cor.test(gmwi$GMWI,gmwi$Shannon)

table(case_when(gmwi$Shannon<1 ~"高风险",
                gmwi$Shannon<1.5 ~"中风险",
                gmwi$Shannon<2 ~"低风险",
                T ~"正常"))
### 暂定为这个值，实在是有部分人群的情况比较差
mean(gmwi$GMWI) - 0.5*sd(gmwi$GMWI)
mean(gmwi$GMWI) - 1.5*sd(gmwi$GMWI)

table(case_when(gmwi$GMWI< -3.59 ~"高风险",
                gmwi$GMWI< -2.74 ~"中风险",
                gmwi$GMWI< -1.88 ~"低风险",
                T ~"正常"))




table(gmwi$GMWI > -2.5)


ggplot(gmwi, aes(x = GMWI)) + 
  geom_histogram(colour = 1, fill = "white")

ggplot(gmwi, aes(x = Shannon)) + 
  geom_histogram(colour = 1, fill = "white")

mean(gmwi$Shannon)

sd(gmwi$Shannon)

table(gmwi$GMWI< (mean(gmwi$GMWI) - sd(gmwi$GMWI)))

