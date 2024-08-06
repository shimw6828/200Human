library(tidyverse)
rm(list = ls())
gc()

#### 加载代谢物数据 ####
setwd("/home/shimw/project/200Human")
rawdata = readxl::read_xlsx("./metabolites/data/gut_ALL_sample_data.xlsx")%>%
  dplyr::select(Compounds, `Class I`, `Class II`,cpd_ID,HMDB, starts_with("Feces-TM"))
library(ggplot2)
library(factoextra)
# 计算PCA主成分
pca_data <- rawdata%>%
  dplyr::select(starts_with("Feces-TM"))%>%
  log10()%>% # 或许不需要进行scale
  t()%>%prcomp(, scale. = F)
pca_result <- as.data.frame(pca_data$x)
kk <- get_pca_ind(pca_data)

p <- fviz_pca_ind(pca_data,
                  geom.ind = "point", # 指定点的形状
                  col.ind = "blue",   # 指定点的颜色
                  addEllipses = TRUE,
                  title = "PCA Analysis")

get_ellipses_outside <- function(p){
  build <- ggplot_build(p)$data
  points <- build[[1]]
  ell <- build[[2]]
  return(as.logical(sp::point.in.polygon(points$x, points$y, ell$x, ell$y)))
}
which(!get_ellipses_outside(p))


#### 构建代谢物自身的网络 ####
rawdata = readxl::read_xlsx("./metabolites/data/gut_ALL_sample_data.xlsx")%>%
  dplyr::select(Compounds, `Class I`, `Class II`,cpd_ID,HMDB, starts_with("Feces-TM"))
q1 <- readxl::read_xlsx("/home/shimw/project/200Human/问卷/体检问卷结果.xlsx")%>%
  dplyr::select(name,age) # 加入年龄
blood_meta_data <- readxl::read_xlsx("/home/shimw/project/200Human/问卷/治未病采血登记表.xlsx", skip = 2)%>%
  dplyr::select("编号", "姓名",   "性别")%>%
  dplyr::rename(BID="编号", name="姓名", sex="性别")
meta_data <- readxl::read_xlsx("/home/shimw/project/200Human/microbiome/粪便采样编号.xlsx")%>%
  rename("ID" = "编号", name = "姓名")%>%
  dplyr::left_join(q1)%>% # 合并年龄
  dplyr::left_join(blood_meta_data)

clinical_indicator <- readr::read_csv("/home/shimw/project/200Human/问卷/000.all_clinical_indicator.csv")%>%
  left_join(., meta_data[,c(1,2,4)])%>%
  arrange(BID)
rawdata <- rawdata%>%
  dplyr::select(Compounds, `Class I`, `Class II`,cpd_ID,HMDB, paste("Feces-TM", clinical_indicator$ID, sep="-"))%>% # 按照blood编号的顺序，这个是其他组也在用的编号
  dplyr::rename_all(function(.){c("Compounds", "Class I", "Class II","cpd_ID","HMDB",
                           paste("Feces-TM", clinical_indicator$BID, sep="-")
                           )})


metabolomics_data <- rawdata%>%
  dplyr::select(paste("Feces-TM", clinical_indicator$BID, sep="-"))%>%
  t()%>%as_tibble()%>%rename_all(~rawdata$Compounds)%>%
  dplyr::select(-Creatinine, -`Uric acid`, -`Vitamin E`)%>% #临床指标中有这三个值
  bind_cols(clinical_indicator, .)%>%
  dplyr::select(-`霉菌毒素(mycotoxins)`, -`草甘膦(Glyphosate)`)%>%# 这两个值全是阴性，不用做
  dplyr::mutate(`抗核抗体(ANA)总抗体` = if_else(`抗核抗体(ANA)总抗体`=="阴性", 0, 1))

variable_pairs <- expand.grid(names(clinical_indicator)[3:122], 
                              c(names(clinical_indicator)[3:122], 
                                rawdata$Compounds), stringsAsFactors = F)%>%
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
readr::write_csv(metabolite_regression, "/home/shimw/project/200Human/result/001.gutmetabolite_regression.csv")


#### 范围统计 ####
rawdata = readxl::read_xlsx("./metabolites/data/gut_ALL_sample_data.xlsx")%>%
  dplyr::select(Compounds,`物质`, `Class I`, `物质一级分类`,`Class II`,`物质二级分类`, starts_with("Feces-TM"))
q1 <- readxl::read_xlsx("/home/shimw/project/200Human/问卷/体检问卷结果.xlsx")%>%
  dplyr::select(name,age) # 加入年龄
blood_meta_data <- readxl::read_xlsx("/home/shimw/project/200Human/问卷/治未病采血登记表.xlsx", skip = 2)%>%
  dplyr::select("编号", "姓名",   "性别")%>%
  dplyr::rename(BID="编号", name="姓名", sex="性别")
meta_data <- readxl::read_xlsx("/home/shimw/project/200Human/microbiome/粪便采样编号.xlsx")%>%
  rename("ID" = "编号", name = "姓名")%>%
  dplyr::left_join(q1)%>% # 合并年龄
  dplyr::left_join(blood_meta_data)
clinical_indicator <- readr::read_csv("/home/shimw/project/200Human/问卷/000.all_clinical_indicator.csv")%>%
  left_join(., meta_data[,c(1,2,4)])%>%
  arrange(BID)%>%
  dplyr::mutate(BID=paste("Feces-TM", BID, sep="-"))

rawdata = readxl::read_xlsx("./metabolites/data/ALL_sample_data.xlsx")%>%
  dplyr::select(Compounds, `Class I`, `Class II`,cpd_ID,HMDB, `Blood-TM-1`:`Blood-TM-201`)
q1 <- readxl::read_xlsx("/home/shimw/project/200Human/问卷/体检问卷结果.xlsx")%>%
  dplyr::select(name,age) 

rawdata <- rawdata%>%
  dplyr::select("Compounds","物质", "Class I", "物质一级分类","Class II","物质二级分类", paste("Feces-TM", clinical_indicator$ID, sep="-"))%>% # 按照blood编号的顺序，这个是其他组也在用的编号
  dplyr::rename_all(function(.){c("Compounds","物质", "Class I", "物质一级分类","Class II","物质二级分类",
                                 clinical_indicator$BID
  )})


kk = lapply(rawdata$Compounds, function(x){
  .df = rawdata%>%
    filter(Compounds==x)%>%
    select(,-c(1:6))
  .num = as.numeric(scale(as.numeric(.df)))
  mean_value = mean(.num)
  sd_value = sd(.num)
  # outlier_subjects_idx <- which(.num > mean_value + 3*sd_value | .num < mean_value - 3*sd_value)
  # .df[,outlier_subjects_idx]
  return(tibble("Compounds"= x,
                "ID" = names(.df),
                "value" = .num))
})%>%bind_rows()%>%
  filter(ID%in%clinical_indicator$BID)%>%
  left_join(clinical_indicator[,c("BID", "name","sex","age")], by = c("ID" = "BID"))%>%
  mutate(status = case_when(
    value > 3 ~ "higher",
    value < -3 ~ "lower",
    TRUE ~ "normal"
  ))%>%left_join(rawdata[,c(1:6)])%>%
    select(Compounds, `物质`, `Class I`, `物质一级分类`,`Class II`,`物质二级分类`, value, name,  sex, age, status)

readr::write_excel_csv(kk, "/home/shimw/project/200Human/result/001.gut.metabolites.status.csv")

aa = kk%>%
  group_by(Compounds, status)%>%
  summarise(count = n())
readr::write_excel_csv(aa, "/home/shimw/project/200Human/result/001.gut.metabolites.proporion.csv")

bb = kk%>%
  group_by(name, status)%>%
  summarise(count = n())%>%
  filter(status=="normal")%>%
  mutate(out_metabolites = (2424 - count)/2424*100)%>%
  select(name,out_metabolites)%>%
  left_join(clinical_indicator)%>%
  select(-ID)
cor.test(bb$out_metabolites,bb$age, method = "spearman")
readr::write_excel_csv(bb, "/home/shimw/project/200Human/result/001.gut.metabolites.subject.csv")

library(ggprism)
ggplot(bb, aes(x=out_metabolites))+
  geom_histogram(binwidth = 0.2, alpha=0.8,colour="black",size=0.4)+
  theme_prism(base_size = 14)+
  scale_y_continuous(limits = c(0, 30), expand = c(0, 0))+
  labs(x = "proportion")

ggplot(bb, aes(x = age, y = out_metabolites))+
  geom_point()+
  geom_smooth(method = 'lm', se = F)+
  theme_prism(base_size = 14)+
  scale_y_continuous(expand = c(0, 0))+
  annotate("text", x = 60, y = 6, label = "R = -0.09, p = 0.22",
           color="black",size = 5, angle=0, fontface="bold")+
  labs(y="proportion")



metabolite_regression <- readr:: read_csv("/home/shimw/project/200Human/result/001.gutmetabolite_regression.csv")
metabolite_regression <- metabolite_regression%>%
  arrange(factor(factor1, levels = names(clinical_indicator)[4:123]))%>%
  dplyr::mutate(padj=p.adjust(pvalue, method = "bonferroni"))%>%
  dplyr::filter(padj<0.05)%>%
  dplyr::mutate(edge=-log10(padj))


disease_net <- metabolite_regression%>%
  dplyr::filter(factor1%in%names(clinical_indicator)[1:64])

disease_net <- disease_net%>%
  dplyr::group_by(factor1)%>%
  dplyr::mutate(BP = row_number())

readxl::read_xlsx("/home/shimw/project/200Human/metabolites/data/gut_ALL_sample_data.xlsx")%>%
  dplyr::filter(Compounds%in%disease_net$factor2)%>%
  select(HMDB, cpd_ID, Compounds)%>%
  dplyr::rename(HMDBID=HMDB, KEGGID=cpd_ID, Name=Compounds)%>%
  readr::write_excel_csv("/home/shimw/project/200Human/result/001.gutmetabolites.disease_origin.csv")
