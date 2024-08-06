
library(tidyverse)
rm(list = ls())
gc()

#### 加载代谢物数据 ####
setwd("/home/shimw/project/200Human")
rawdata = readxl::read_xlsx("./metabolites/data/ALL_sample_data.xlsx")%>%
  dplyr::select(Compounds, `Class I`, `Class II`,cpd_ID,HMDB, `Blood-TM-1`:`Blood-TM-201`)



## 筛选数据（在50%样本中信号过500）
filter_undetected_metabolites <- function(data, threshold = 0.5) {
  # 统计每行（代谢物）中检测到的样本数量
  detected_counts <- rowSums(data > 500)
  
  # 计算检测到的样本百分比
  detection_percentage <- detected_counts / ncol(data)
  
  # 筛选出在50%样本中检测不到的代谢物索引
  undetected_metabolites_idx <- which(detection_percentage <= threshold)
  return(undetected_metabolites_idx)
}
rawdata%>%
  dplyr::select(`Blood-TM-1`:`Blood-TM-201`)%>%
  filter_undetected_metabolites()
## 没有代谢物低于50%（公司给的应该已经经过过滤和knn填充了）

#### PCA聚类 ####
## PCA聚类检测离群值
library(ggplot2)
library(factoextra)

# 计算PCA主成分
pca_data <- rawdata%>%
  dplyr::select(`Blood-TM-1`:`Blood-TM-201`)%>%
  log10()%>% # 或许不需要进行scale
  t()%>%prcomp(, scale. = F)
pca_result <- as.data.frame(pca_data$x)
kk <- get_pca_ind(pca_data)

p <- fviz_pca_ind(pca_data,
             geom.ind = "point", # 指定点的形状
             col.ind = "blue",   # 指定点的颜色
             addEllipses = TRUE,
             title = "PCA Analysis")
## 看着有一部分离群值，但是不知道离了多远
get_ellipses_outside <- function(p){
  build <- ggplot_build(p)$data
  points <- build[[1]]
  ell <- build[[2]]
  return(as.logical(sp::point.in.polygon(points$x, points$y, ell$x, ell$y)))
}
which(!get_ellipses_outside(p))
# 5 8 44 91 93 135 158 174 195 196 在置信椭圆外  默认值0.95

# 计算马氏距离
mahalanobis_distance <- mahalanobis(pca_result[,1:15], colMeans(pca_result[,1:15]), cov(pca_result[,1:15]))

## 设置阈值
mahalanobis_threshold <- qchisq(0.95, df = ncol(pca_result[,1:15]))
outliers <- which(mahalanobis_distance > mahalanobis_threshold)

## 原矩阵求距离
t_log_data <- rawdata%>%
  dplyr::select(`Blood-TM-1`:`Blood-TM-201`)%>%
  log10()%>%t()
mahalanobis_distance <- mahalanobis(t_log_data, colMeans(t_log_data), cov(t_log_data))
## 报错，协方差矩阵为奇异矩阵。计算发现协方差矩阵的秩只有200，这说明大部分的代谢物有极大程度是相关的

# 使用聚类筛选
log_data <- rawdata%>%
  dplyr::select(`Blood-TM-1`:`Blood-TM-201`)%>%
  log10()
sampleTree <- hclust(dist(t(log_data)), method = "average")
plot(sampleTree, cex.lab = 1.5, cex.axis = 1.5, cex.main = 2)
# 从图中观察到158 和 124 聚类较远，属于离群值
cor_matrix <- cor(log_data)
pheatmap::pheatmap(cor_matrix, 
         main = "Metabolite Correlation Heatmap",
         clustering_method = "ward.D") 
min(cor_matrix)
# 样本相关性最低位0.82

# 计算sd筛选离群值
log_data <- rawdata%>%
  dplyr::mutate(across(matches("Blood"), log)) # 之后都是使用log_data

# calculate SD
filter_outlier_subjects <- function(.x, threshold = 3) {
  # 计算sd和mean
  sd_value <- sd(.x)
  mean_value <- mean(.x)
  
  # 筛选离群值的志愿者
  outlier_subjects_idx <- which(.x > mean_value + threshold*sd_value | .x < mean_value - threshold*sd_value)
  return(outlier_subjects_idx)
}



outlier_subjects <- log_data%>%
  dplyr::select(`Blood-TM-1`:`Blood-TM-201`)%>%
  apply(., 1, filter_outlier_subjects)%>%unlist()%>%table()
# 大多数样本都会有一两个值超过5倍的sd，其中124号样本离群值最多，96个代谢物差异较大
# 如果是3倍sd时，就是158号异常值最多，有296个。以这个标准来看，几乎所有样本都有一两个值有问题
# 如果是2倍sd，158有约752个值异常了，超过了40%的代谢物异常，这个要重点关注一下
# 这与之前聚类的结果也一致
# 暂时不去掉样本


#### 性别间差异的代谢物统计
log_data <- rawdata%>%
  dplyr::mutate(across(matches("Blood"), log)) 

q1 <- readxl::read_xlsx("/home/shimw/project/200Human/问卷/体检问卷结果.xlsx")%>%
  dplyr::select(name,age) # 加入年龄

meta_data <- readxl::read_xlsx("/home/shimw/project/200Human/问卷/治未病采血登记表.xlsx", skip = 2)%>%
  dplyr::select("编号", "姓名",   "性别")%>%
  dplyr::rename(ID="编号", name="姓名", sex="性别")%>%
  dplyr::left_join(q1) # 合并年龄


male_idx <- which(meta_data$sex=="男") # 获取性别的索引号
female_idx <- which(meta_data$sex=="女")

pvalue <- log_data%>%
  dplyr::select(`Blood-TM-1`:`Blood-TM-201`)%>%
  apply(.,1,function(x){wilcox.test(x[male_idx],x[female_idx])$p.value}) # 进行秩和检验
p_adj <- p.adjust(pvalue, method = 'BH')



# 进行anova方差分析矫正年龄
sex_anova <- function(.x){
  .d = tibble("metabolite" = .x,
                  "sex" = meta_data$sex,
                  "age" = meta_data$age)
  # lm_total <- lm(metabolite~sex+age,data = .d)
  # anova(lm_total) # 两个函数结果是一样的
  res = summary(aov(metabolite~sex+age,data = .d))
  return(res[[1]][["Pr(>F)"]][1]) # 返回sex对应的p值
}
p_adj <- log_data%>%
  dplyr::select(`Blood-TM-1`:`Blood-TM-201`)%>%
  apply(.,1,sex_anova)%>%p.adjust(., method = 'BH')
table(p_adj<0.05)
table(p_adj<0.01)
table(p_adj<0.001)
# 加入年龄矫正之后，结果依然相差不大，37.3%的代谢物存在性别差异






#### 构建代谢物自身的网络 ####
rm(list = ls())
gc()
rawdata = readxl::read_xlsx("./metabolites/data/ALL_sample_data.xlsx")%>%
  dplyr::select(Compounds, `Class I`, `Class II`,cpd_ID,HMDB, `Blood-TM-1`:`Blood-TM-201`)
q1 <- readxl::read_xlsx("/home/shimw/project/200Human/问卷/体检问卷结果.xlsx")%>%
  dplyr::select(name,age) # 加入年龄

meta_data <- readxl::read_xlsx("/home/shimw/project/200Human/问卷/治未病采血登记表.xlsx", skip = 2)%>%
  dplyr::select("编号", "姓名",   "性别")%>%
  dplyr::rename(ID="编号", name="姓名", sex="性别")%>%
  dplyr::left_join(q1) # 合并年龄

clinical_indicator <- readr::read_csv("/home/shimw/project/200Human/问卷/000.all_clinical_indicator.csv")%>%
  left_join(., meta_data[,1:2])%>%
  arrange(ID)
# https://blog.csdn.net/shwan_ma/article/details/80154888
# 在进行线性回归时是不需要标准化的，因此使用rawdata
# which(is.na(clinical_indicator), arr.ind = TRUE)# 检测NA值

metabolomics_data <- rawdata%>%
  dplyr::select(paste("Blood-TM", clinical_indicator$ID, sep="-"))%>%
  t()%>%as_tibble()%>%rename_all(~rawdata$Compounds)%>%
  dplyr::select(-Creatinine, -`Uric acid`)%>% #临床指标中有这两个值
  bind_cols(clinical_indicator, .)%>%
  dplyr::select(-`霉菌毒素(mycotoxins)`, -`草甘膦(Glyphosate)`)%>%# 这两个值全是阴性，不用做
  dplyr::mutate(`抗核抗体(ANA)总抗体` = if_else(`抗核抗体(ANA)总抗体`=="阴性", 0, 1))
# variable_pairs <- t(combn(names(metabolomics_data)[4:ncol(metabolomics_data)], 2, simplify = TRUE))

"L-Leucine"%in%names(metabolomics_data)
"L-Norvaline"
"L-Isoleucine"
metabolomics_data%>%
  dplyr::select("name", "sex", "age", "BMI", "Vitamin A", "L-Leucine", "L-Norvaline", "L-Isoleucine", "A/G")%>%
  write_excel_csv("/home/shimw/project/200Human/result/001.metabolites.some.disease.csv")

variable_pairs <- expand.grid(names(clinical_indicator)[4:123],
                              c(names(clinical_indicator)[3:122], 
                                rawdata$Compounds), stringsAsFactors = F)
variable_pairs <- variable_pairs%>%
  dplyr::filter(Var1!=Var2)

## 线性回归模型矫正性别和年龄。其实应该加上BMI的，之后在讨论吧
perform_regression <- function(metabolite) {
  metabolite = as.vector(metabolite)
  one_formula <- as.formula(paste("`",metabolite[1], "` ~ `", metabolite[2], "` + age + sex", sep = "")) 
  if (metabolite[1]=="抗核抗体(ANA)总抗体") {
    fit = glm(one_formula, data = metabolomics_data)
  } else{
    .d = metabolomics_data%>%
      dplyr::select(metabolite[1], metabolite[2], age, sex, BMI)%>%
      tidyr::drop_na()%>%
      dplyr::mutate_at(c(metabolite[1], metabolite[2]), ~ as.numeric(gsub("<|>", "", .)))
    if (length(unique(.d$sex))==1) {
      one_formula <- as.formula(paste("`",metabolite[1], "` ~ `", metabolite[2], "` + age", sep = "")) 
    }
    fit <- lm(one_formula, data = .d, )
  }
  sumx <- summary(fit)
  ct <- sumx$coefficients
  pvalue = min(ct[-c(1, nrow(ct)),'Pr(>|t|)'])
  beta = ct[-c(1, nrow(ct)),"Estimate"][which.min(ct[-c(1, nrow(ct)),'Pr(>|t|)'])]
  df = tibble::tibble("factor1"=metabolite[1],
                 "factor2"=metabolite[2],
                 "pvalue"=pvalue,
                 "beta" = beta)
  return(df)
}

metabolite_regression <- apply(variable_pairs, 1, perform_regression)%>%
  dplyr::bind_rows()
readr::write_csv(metabolite_regression, "/home/shimw/project/200Human/result/001.metabolite_regression.csv")

metabolite_regression <- readr::read_csv("/home/shimw/project/200Human/result/001.metabolite_regression.csv")
kk = metabolite_regression%>%
  filter(factor1=="抗缪勒管激素(AMH)")%>%
  dplyr::mutate(padj=p.adjust(pvalue, method = "bonferroni"))

kk[kk$pvalue<0.05,]$factor2

kk_net <- readxl::read_xlsx("/home/shimw/project/200Human/metabolites/data/ALL_sample_data.xlsx")%>%
  dplyr::select(Compounds, `Class I`, `物质一级分类`,`Class II`,`物质二级分类`,cpd_ID,HMDB)%>%
  dplyr::filter(Compounds%in%kk[kk$pvalue<0.05,]$factor2)%>%
  select(HMDB, cpd_ID, Compounds, `Class I`, `物质一级分类`,`Class II`,`物质二级分类`)

table(kk_net$`物质一级分类`)%>%
  as.data.frame()%>%
  dplyr::mutate(Var1 = factor(Var1, levels = rev(c("脂肪酰类", "甘油磷脂类","氨基酸及其代谢物", "杂环化合物","有机酸及其衍生物","苯及其衍生物","激素及激素相关物质","醇、胺类", "其他","核苷酸及其代谢物","鞘脂类", "甘油脂类","生物碱","辅酶和维生素","碳水化合物及其代谢物","醛、酮、酯类","胆汁酸","色胺、胆碱、色素","萜类"))))%>%
  ggplot(.,aes(Var1, Freq)) + geom_col()+theme_classic() +
  geom_text(aes(label = Freq), position = position_dodge(0.9), hjust = -0.2, size = 5)+
  theme(
    legend.position="none",
    axis.title.x = element_blank(),
    axis.title.y =element_blank(),
    axis.text.x = element_text(size = 16),
    axis.text.y = element_text(size = 16)
  )+
  coord_flip()+
  scale_y_continuous(expand = c(0,0), limits = c(0,85))



metabolite_regression <- metabolite_regression%>%
  arrange(factor(factor1, levels = names(clinical_indicator)[4:123]))%>%
  dplyr::mutate(padj=p.adjust(pvalue, method = "bonferroni"))%>%
  dplyr::filter(padj<0.05)%>%
  dplyr::mutate(edge=-log10(padj))



metabolite_regression%>%
  dplyr::filter(factor1=="HOMA-IR")

# 选择与对应变量相关的代谢物
normalize_clinical_indicator <- readr::read_csv("/home/shimw/project/200Human/体检报告/000.normalize_clinical_indicator.csv")%>%
  left_join(., meta_data[,1:2])%>%
  arrange(ID)




blood_pressure <- c("Blood pressure-systolic", "Blood pressure-diastolic") # 血压
blood_lipid <- c("Total cholesterol", "Triglycerides", "LDL-C", "HDL-C", "TG/HDL") # 血脂 
blood_glucose <- c("Fasting Glucose",  "postprandial blood glucose") # 血糖 
liver_func <- c("BUN",  "Creatinine",  "Uric acid") # 肝功能

blood_pressure_net <- metabolite_regression%>%
  dplyr::filter(factor1%in%blood_pressure|factor1%in%blood_pressure)


blood_lipid_net <- metabolite_regression%>%
  dplyr::filter(factor1%in%blood_lipid|factor1%in%blood_lipid)


blood_glucose_net <- metabolite_regression%>%
  dplyr::filter(factor1%in%blood_glucose|factor1%in%blood_glucose)

blood_liver_net <- metabolite_regression%>%
  dplyr::filter(factor1%in%liver_func|factor1%in%liver_func)


## 分血液检查的类型进行网络观察
# 身体 BMI 
# 血压 

# 肾功能 

# 血常规 Leukocyte ：Basophil


nurtition_net <- metabolite_regression%>%
  dplyr::filter(factor1%in%names(clinical_indicator)[81:117])

disease_net <- metabolite_regression%>%
  dplyr::filter(factor1%in%names(clinical_indicator)[1:64])

disease_net <- disease_net%>%
  dplyr::group_by(factor1)%>%
  dplyr::mutate(BP = row_number())


disease_count = disease_net%>%
  group_by(factor1)%>%
  summarise(count = n())%>%
  arrange(factor(factor1, levels = unique(metabolite_regression$factor1)[1:27]))%>%
  mutate(Length = 50)

chr_pos <- disease_count%>%
  mutate(total = cumsum(Length) - Length)
Snp_pos <- chr_pos %>%
  left_join(disease_net, ., by="factor1")%>%
  mutate(BPcum = if_else(count>50, Length/count * BP  + total, 
                         total + 25 - count/2 + BP))%>%
  mutate(factor1 = factor(factor1, levels = disease_count$factor1))
X_axis <-  Snp_pos %>% group_by(factor1) %>% summarize(center=( max(BPcum) + min(BPcum) ) / 2 )
library(ggrepel)
p <- ggplot(Snp_pos, aes(x=BPcum, y=-log10(padj))) +
  #设置点的大小，透明度
  geom_point(aes(color=factor1), size=1.3) +
  geom_label_repel( data=Snp_pos[Snp_pos$factor1=="血糖"&Snp_pos$edge>30,], aes(label=factor2), size=3)+
  #设置颜色
  scale_color_manual(values = rep(c("grey", "skyblue"), 14 )) +
  #设定X轴
  scale_x_continuous(label = X_axis$factor1, breaks= X_axis$center ) +
  #去除绘图区和X轴之间的gap
  #设置主题
  theme_bw() +
  theme(
    legend.position="none",
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 12),
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5, size = 12),
    axis.text.y = element_text(size = 10),
    axis.ticks.length.x = unit(1,"mm"),
    axis.ticks.length.y = unit(1,"mm")
  )
p
library(showtext) 
showtext_auto (enable = TRUE) 

ggsave("/home/shimw/project/200Human/result/001.metabolites.disease_manhattan.pdf", width = 8, height = 6)

Snp_pos[Snp_pos$factor1=="血糖"&Snp_pos$edge>30,]$factor2

rawdata%>%
  dplyr::filter(Compounds%in%disease_net$factor2)%>%
  select(HMDB, cpd_ID, Compounds)%>%
  dplyr::rename(HMDBID=HMDB, KEGGID=cpd_ID, Name=Compounds)%>%
  readr::write_excel_csv("/home/shimw/project/200Human/result/001.metabolites.disease_origin.csv")


HOMA_net <- disease_net%>%
  dplyr::filter(factor1=="HOMA-IR")
HOMA_net <- readxl::read_xlsx("./metabolites/data/ALL_sample_data.xlsx")%>%
  dplyr::select(Compounds, `Class I`, `物质一级分类`,`Class II`,`物质二级分类`,cpd_ID,HMDB)%>%
  dplyr::filter(Compounds%in%HOMA_net$factor2)%>%
  select(HMDB, cpd_ID, Compounds, `Class I`, `物质一级分类`,`Class II`,`物质二级分类`)%>%
  left_join(HOMA_net, ., by = c("factor2" = "Compounds"))

table(HOMA_net$`物质一级分类`)%>%
  as.data.frame()%>%
  dplyr::mutate(Var1 = factor(Var1, levels = rev(c(
    "甘油脂类", "甘油磷脂类", "鞘脂类", "氨基酸及其代谢物",
    "脂肪酰类", "甾醇脂类", "醛、酮、酯类", "碳水化合物及其代谢物"
  ))))%>%
  ggplot(.,aes(Var1, Freq)) + geom_col()+theme_classic2() +
  geom_text(aes(label = Freq), position = position_dodge(0.9), hjust = -0.2, size = 5)+
  theme(
    legend.position="none",
    axis.title.x = element_blank(),
    axis.title.y =element_blank(),
    axis.text.x = element_text(size = 16),
    axis.text.y = element_text(size = 16)
  )+
  coord_flip()+
  scale_y_continuous(expand = c(0,0), limits = c(0,85))
ggsave("/home/shimw/project/200Human/result/001.metabolites.HOMA_class.pdf", width = 5, height = 3.3)





#### 随机森林 ####

library(tidygraph)
meta_data <- readxl::read_xlsx("/home/shimw/project/200Human/问卷/治未病采血登记表.xlsx", skip = 2)%>%
  dplyr::select("编号", "姓名")%>%
  dplyr::rename(ID="编号", name="姓名")

subject_HOMA = readr::read_csv("/home/shimw/project/200Human/体检报告/all_clinical.csv")%>%
  dplyr::filter(feature=="HOMA-IR")%>%
  dplyr::select(name, res)%>%
  left_join(meta_data)%>%
  arrange(ID)%>%
  dplyr::mutate(ID=paste("Blood-TM", ID, sep="-"))

data <- readxl::read_xlsx("./metabolites/data/ALL_sample_data.xlsx")%>%
  dplyr::select(Compounds, subject_HOMA$ID)%>%
  dplyr::filter(Compounds%in%HOMA_net$factor2)%>%
  tibble::column_to_rownames("Compounds")%>%
  t()%>%as.data.frame()%>%
  mutate(HOMA_IR = factor(subject_HOMA$res))

met_HOMA = names(data)
names(data)[1:111] = paste("m",1:111, sep = "")



library(randomForest)
library(pROC)
set.seed(888)#888
# 使用留一法进行训练和评估
samp = sample(1:197, 197*0.7)
data_train <- data[samp, ]
data_test <- data[-samp, ]
HOMA_rf = randomForest(HOMA_IR ~., data = data_train, importance = TRUE, proximity = TRUE, ntree = 500)
rf_pred <- predict(HOMA_rf, data_test, type = "prob")
HOMAroc <- roc(data_test$HOMA_IR, 
               rf_pred[,2],
               # smooth = TRUE,
               ci = T,
               auc = T)
auc(HOMAroc) # 0.84
library(ROCR)
HOMApred <- prediction(rf_pred[,2], 
                       data_test$HOMA_IR)
HOMAperf <- performance(HOMApred,"tpr","fpr")
HOMAx <- unlist(HOMAperf@x.values)  ##提取x值
HOMAy <- unlist(HOMAperf@y.values)
HOMAplotdata <- data.frame("x"=HOMAx,"y"=HOMAy) 
p4 <- ggplot() + 
  geom_path(data=HOMAplotdata,aes(x = x, y = y), colour = '#d31919',size=2, linetype='solid') +
  geom_line(aes(x=c(0,1),y=c(0,1)),color = "black",size = 1.1,linetype=2) +
  annotate("text", x = 0.73, y = 0.25, label = "AUC = 0.83",
           color="black",size = 10, angle=0, fontface="bold") +
  theme_bw()+
  scale_colour_gradient(name = 'False positive rate', low = 'blue', high = 'red') +
  # 坐标轴保留一位小数
  scale_x_continuous(expand = c(0,0),limits = c(0,1),
                     breaks = seq(0,1,by = 0.2),
                     labels = scales::number_format(accuracy = 0.1)) +
  scale_y_continuous(expand = c(0,0),limits = c(0,1),
                     breaks = seq(0,1,by = 0.2),
                     labels = scales::number_format(accuracy = 0.1)) +
  theme(plot.title = element_text(face = 'bold',size=15)) +
  theme(plot.margin = ggplot2::margin(0.3, 0.4, 0.1, 0.1, "cm")) +
  # theme_bw(base_family = "sans",base_size = 18,
  #          base_line_size = 1,base_rect_size = 1) +
  theme(legend.title=element_blank()) +
  theme(plot.title = element_text(size = 18,hjust = 0.5),
        legend.background = element_blank(),
        legend.position = 'none') +
  theme(axis.text.x = element_text(size = 16 , color = 'black',face='bold',
                                   vjust = 0.5, hjust = 0.5, angle = 0),
        axis.text.y = element_text(size = 16, color = 'black',face='bold',
                                   vjust = 0.5, hjust = 0.5, angle = 0),
        axis.title.x=element_text(size=25,angle = 0,face='bold'),
        axis.title.y=element_text(size=25,angle = 90,face='bold'),
        panel.grid = element_blank() # 删除网格线
  ) +
  # 加粗边框
  theme(panel.border = element_rect(fill=NA,color="black", 
                                    size=1.1, linetype="solid")) +
  # 调整坐标轴刻度线长度
  theme(axis.ticks.length = unit(-0.1,"cm")) +
  labs(title = NULL,x = "False positive rate", y = "Ture positive rate")
ggsave(p4, file='/home/shimw/project/200Human/result/001.HOMA-rf.top10.pdf', width=5, height=5)
myfeature_importance <- as.data.frame(importance(HOMA_rf))
myfeature_importance$feature = met_HOMA[-112]
top20_myfeature_importance <- myfeature_importance%>%arrange(desc(MeanDecreaseGini))%>%
  head(n = 20)%>%
  mutate(feature = factor(feature, levels = rev(feature)))
top20_myfeature_importance%>%
  ggplot(.,aes(feature, MeanDecreaseGini)) + geom_col()+
  theme_classic2()+
  theme(
    legend.position="none",
    axis.title.y =element_blank(),
    axis.title.x =element_text(size = 16),
    axis.text.x = element_text(size = 16),
    axis.text.y = element_text(size = 16)
  )+
    coord_flip()+labs(y = "Gini index")
ggsave(file='/home/shimw/project/200Human/result/001.HOMA-rf_feature.importance.pdf', width=5, height=4.7)



data <- readxl::read_xlsx("./metabolites/data/ALL_sample_data.xlsx")%>%
  dplyr::select(Compounds, subject_HOMA$ID)%>%
  dplyr::filter(Compounds%in%top20_myfeature_importance$feature[1:10])%>%
  tibble::column_to_rownames("Compounds")%>%
  t()%>%as.data.frame()%>%
  mutate(HOMA_IR = factor(subject_HOMA$res))
met_HOMA = names(data)
names(data)[1:(length(met_HOMA)-1)] = paste("m",1:(length(met_HOMA)-1), sep = "")



#### 范围统计 ####
rawdata = readxl::read_xlsx("./metabolites/data/ALL_sample_data.xlsx")%>%
  dplyr::select(Compounds,`物质`, `Class I`, `物质一级分类`,`Class II`,`物质二级分类`, `Blood-TM-1`:`Blood-TM-201`)


q1 <- readxl::read_xlsx("/home/shimw/project/200Human/问卷/体检问卷结果.xlsx")%>%
  dplyr::select(name,age) # 加入年龄

meta_data <- readxl::read_xlsx("/home/shimw/project/200Human/问卷/治未病采血登记表.xlsx", skip = 2)%>%
  dplyr::select("编号", "姓名",   "性别")%>%
  dplyr::rename(ID="编号", name="姓名", sex="性别")%>%
  dplyr::left_join(q1) # 合并年龄

clinical_indicator <- readr::read_csv("/home/shimw/project/200Human/问卷/000.all_clinical_indicator.csv")%>%
  left_join(., meta_data[,1:2])%>%
  arrange(ID)%>%
  dplyr::mutate(ID=paste("Blood-TM", ID, sep="-"))%>%
  select(ID, name,sex,age)


kk = lapply(rawdata$Compounds, function(x){
  .df = rawdata%>%
    filter(Compounds==x)%>%
    select(,-c(1:6))
  .num = as.numeric(.df) # 不 scale
  # .num = as.numeric(scale(as.numeric(.df))) # scale
  mean_value = mean(.num)
  sd_value = sd(.num)
  # outlier_subjects_idx <- which(.num > mean_value + 3*sd_value | .num < mean_value - 3*sd_value)
  # .df[,outlier_subjects_idx]
  return(tibble("Compounds"= x,
                "ID" = names(.df),
                "value" = .num,
                "mean" = mean_value,
                "sd_value" = sd_value))
})%>%bind_rows()%>%
  filter(ID%in%clinical_indicator$ID)%>%
  left_join(clinical_indicator)
kk = kk%>%mutate(status = case_when(
  value > mean + (3*sd_value) ~ "higher",
  value < mean - (3*sd_value) ~ "lower",
  TRUE ~ "normal"
))%>%left_join(rawdata[,c(1:6)])%>%
  select(Compounds, `物质`, `Class I`, `物质一级分类`,`Class II`,`物质二级分类`, value, name,  sex, age, status, mean, sd_value)

readr::write_excel_csv(kk, "/home/shimw/project/200Human/result/001.metabolites.status.csv")

aa = kk%>%
  group_by(Compounds, status)%>%
  summarise(count = n())
readr::write_excel_csv(aa, "/home/shimw/project/200Human/result/001.metabolites.proporion.csv")


bb = kk%>%
  group_by(name, status)%>%
  summarise(count = n())%>%
  filter(status=="normal")%>%
  mutate(out_metabolites = (2325 - count)/2325*100)%>%
  select(name,out_metabolites)%>%
  left_join(clinical_indicator)%>%
  select(-ID)
cor.test(bb$out_metabolites,bb$age, method = "spearman")
readr::write_excel_csv(bb, "/home/shimw/project/200Human/result/001.metabolites.subject.csv")

library(ggprism)
ggplot(bb, aes(x=out_metabolites))+
  geom_histogram(binwidth = 0.2, alpha=0.8,colour="black",size=0.4)+
  theme_prism(base_size = 14)+
  scale_y_continuous(limits = c(0, 40), expand = c(0, 0))+
  labs(x = "proportion")

ggplot(bb, aes(x = age, y = out_metabolites))+
  geom_point()+
  geom_smooth(method = 'lm', se = F)+
  theme_prism(base_size = 14)+
  scale_y_continuous(expand = c(0, 0))+
  annotate("text", x = 60, y = 7, label = "R = 0.16, p = 0.02",
           color="black",size = 5, angle=0, fontface="bold")+
  labs(y="proportion")




#### 试图标准化 ####
library(demoData)
library(MetNormalizer)
path <- system.file("MetNormalizer", package = "demoData")
file.copy(from = path, to = ".", overwrite = TRUE, recursive = TRUE)
new.path <- file.path("./MetNormalizer")












#### CAD评估 神经酰胺####
# 参考 https://academic.oup.com/eurheartj/article/41/3/371/5519585
# CERT2评分
## 计算Cer(d18:1/24:1) / Cer(d18:1/24:0), Cer(d18:1/16:0) / PC 16:0/22:5
## Cer(d18:1/16:0) / PC(14:0/22:6), PC 16:0/16:0
## 个体的所有三个神经酰胺比率和每个的浓度与整个研究人群进行比较。
## 如果该变量属于第 2 个四分位数, 则得分+1, 如果属于第 3 个四分位数, 则得分+2, 第四个则+3分。
## 得分范围从 0 到 12, 根据得分, 受试者被分为四个风险类别(0-3、4-6、7-8 和 9-12)
as.numeric(rawdata[rawdata$Compounds=="Cer(d18:1/24:1)",7:207])
as.numeric(rawdata[rawdata$Compounds=="Cer(d18:1/24:0)",7:207])
as.numeric(rawdata[rawdata$Compounds=="Cer(d18:1/16:0)",7:207])
as.numeric(rawdata[rawdata$Compounds=="PC(16:0_22:5)",7:207])
as.numeric(rawdata[rawdata$Compounds=="PC(14:0_22:6)",7:207])
as.numeric(rawdata[rawdata$Compounds=="PC(16:0_16:0)",7:207])

a1 = as.numeric(rawdata[rawdata$Compounds=="Cer(d18:1/24:1)",7:207])/as.numeric(rawdata[rawdata$Compounds=="Cer(d18:1/24:0)",7:207])
a2 = as.numeric(rawdata[rawdata$Compounds=="Cer(d18:1/16:0)",7:207])/as.numeric(rawdata[rawdata$Compounds=="PC(16:0_22:5)",7:207])
a3 = as.numeric(rawdata[rawdata$Compounds=="Cer(d18:1/16:0)",7:207])/as.numeric(rawdata[rawdata$Compounds=="PC(14:0_22:6)",7:207])
a4 = as.numeric(rawdata[rawdata$Compounds=="PC(16:0_16:0)",7:207])


cert2 = case_when(
  a1<=quantile(a1)[2] ~ 0,
  a1<=quantile(a1)[3] ~ 1,
  a1<=quantile(a1)[4] ~ 2,
  a1<=quantile(a1)[5] ~ 3
)+
case_when(
  a2<=quantile(a2)[2] ~ 0,
  a2<=quantile(a2)[3] ~ 1,
  a2<=quantile(a2)[4] ~ 2,
  a2<=quantile(a2)[5] ~ 3
)+
case_when(
  a3<=quantile(a3)[2] ~ 0,
  a3<=quantile(a3)[3] ~ 1,
  a3<=quantile(a3)[4] ~ 2,
  a3<=quantile(a3)[5] ~ 3
)+
case_when(
  a4<=quantile(a4)[2] ~ 0,
  a4<=quantile(a4)[3] ~ 1,
  a4<=quantile(a4)[4] ~ 2,
  a4<=quantile(a4)[5] ~ 3
)
## cert1 评分好像效果更好
b1 = as.numeric(rawdata[rawdata$Compounds=="Cer(d18:1/16:0)",7:207])
b2 = as.numeric(rawdata[rawdata$Compounds=="Cer(d18:1/18:0)",7:207])
b3 = as.numeric(rawdata[rawdata$Compounds=="Cer(d18:1/24:1)",7:207])
b4 = as.numeric(rawdata[rawdata$Compounds=="Cer(d18:1/16:0)",7:207])/as.numeric(rawdata[rawdata$Compounds=="Cer(d18:1/24:0)",7:207])
b5 = as.numeric(rawdata[rawdata$Compounds=="Cer(d18:1/18:0)",7:207])/as.numeric(rawdata[rawdata$Compounds=="Cer(d18:1/24:0)",7:207])
b6 = as.numeric(rawdata[rawdata$Compounds=="Cer(d18:1/24:1)",7:207])/as.numeric(rawdata[rawdata$Compounds=="Cer(d18:1/24:0)",7:207])
cert1 = case_when(
  b1<=quantile(b1)[3] ~ 0,
  b1<=quantile(b1)[4] ~ 1,
  b1<=quantile(b1)[5] ~ 2
)+
  case_when(
    b2<=quantile(b2)[3] ~ 0,
    b2<=quantile(b2)[4] ~ 1,
    b2<=quantile(b2)[5] ~ 2
  )+
  case_when(
    b3<=quantile(b3)[3] ~ 0,
    b3<=quantile(b3)[4] ~ 1,
    b3<=quantile(b3)[5] ~ 2
  )+
  case_when(
    b4<=quantile(b4)[3] ~ 0,
    b4<=quantile(b4)[4] ~ 1,
    b4<=quantile(b4)[5] ~ 2
  )+
  case_when(
    b5<=quantile(b5)[3] ~ 0,
    b5<=quantile(b5)[4] ~ 1,
    b5<=quantile(b5)[5] ~ 2
  )+
  case_when(
    b5<=quantile(b5)[3] ~ 0,
    b5<=quantile(b5)[4] ~ 1,
    b5<=quantile(b5)[5] ~ 2
  )
  


cert = tibble("ID" = names(rawdata[,7:207]),
              "cert1" = cert1,
              "cert2" = cert2)


clinical_indicator <- readr::read_csv("/home/shimw/project/200Human/问卷/000.all_clinical_indicator.csv")%>%
  left_join(., meta_data[,1:2])%>%
  arrange(ID)%>%
  dplyr::mutate(ID=paste("Blood-TM", ID, sep="-"))%>%
  left_join(cert)


cor.test(clinical_indicator$`LDL-C`/clinical_indicator$`HDL-C`, clinical_indicator$cert1)

clinical_indicator%>%
  select(name, sex, age, cert1,cert2)%>%
  mutate(risk = case_when(
    cert2 <= 3 ~ "低风险",
    cert2 <= 6 ~ "中风险",
    cert2 <= 8 ~ "较高风险",
    cert2 <= 12 ~ "高风险"
    ))%>%
  readr::write_excel_csv("/home/shimw/project/200Human/result/001.metabolites.CAD.risk.csv")


