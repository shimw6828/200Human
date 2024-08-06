### 肠道代谢与血液代谢的相关性
setwd("/home/shimw/project/200Human")

blood_meta_data <- readxl::read_xlsx("/home/shimw/project/200Human/问卷/治未病采血登记表.xlsx", skip = 2)%>%
  dplyr::select("编号", "姓名",   "性别")%>%
  dplyr::rename(ID="编号", name="姓名", sex="性别")
blood_rawdata = readxl::read_xlsx("./metabolites/data/ALL_sample_data.xlsx")%>%
  dplyr::select(Compounds, `Class I`, `Class II`,cpd_ID,HMDB, `Blood-TM-1`:`Blood-TM-201`)



gut_rawdata = readxl::read_xlsx("./metabolites/data/gut_ALL_sample_data.xlsx")%>%
  dplyr::select(Compounds, `Class I`, `Class II`,cpd_ID,HMDB, starts_with("Feces-TM"))
blood_meta_data <- readxl::read_xlsx("./问卷/治未病采血登记表.xlsx", skip = 2)%>%
  dplyr::select("编号", "姓名",   "性别")%>%
  dplyr::rename(BID="编号", name="姓名", sex="性别")
gut_meta_data <- readxl::read_xlsx("./microbiome/粪便采样编号.xlsx")%>%
  rename("ID" = "编号", name = "姓名")%>%
  dplyr::left_join(blood_meta_data)%>%
  dplyr::filter(paste("Feces-TM", ID, sep="-")%in%names(gut_rawdata))%>%
  arrange(BID)
gut_rawdata <- readxl::read_xlsx("./metabolites/data/gut_ALL_sample_data.xlsx")%>%
  dplyr::select(Compounds, `Class I`, `Class II`,cpd_ID,HMDB, paste("Feces-TM", gut_meta_data$ID, sep="-"))%>% # 按照blood编号的顺序，这个是其他组也在用的编号
  dplyr::rename_all(function(.){c("Compounds", "Class I", "Class II","cpd_ID","HMDB",
                                  paste("Feces-TM", gut_meta_data$BID, sep="-")
  )})


## 保证人数相同
remain_id = intersect(blood_meta_data$ID, gut_meta_data$BID)
blood_metabolomics_data <- blood_rawdata%>%
  dplyr::select(paste("Blood-TM", remain_id, sep="-"))%>%
  t()%>%as_tibble()%>%rename_all(~blood_rawdata$Compounds)
gut_metabolomics_data <- gut_rawdata%>%
  dplyr::select(paste("Feces-TM", remain_id, sep="-"))%>%
  t()%>%as_tibble()%>%rename_all(~gut_rawdata$Compounds)
## 提取年龄和性别
sex_info = blood_meta_data
q1 <- readxl::read_xlsx("/home/shimw/project/200Human/问卷/体检问卷结果.xlsx")%>%
  dplyr::select(name,age) # 加入年龄
sex_info = blood_meta_data%>%
  dplyr::filter(ID%in%remain_id)%>%
  left_join(q1)


perform_regression <- function(metabolite) {
  metabolite = as.vector(metabolite)
  one_formula <- as.formula("blood ~ gut + sex + age") 
  .d = data_frame("blood" = blood_metabolomics_data[, metabolite[1]][[1]],
              "gut" = gut_metabolomics_data[, metabolite[2]][[1]],
              "sex" = sex_info$sex,
              "age" = sex_info$age)
  fit <- lm(one_formula, data = .d, )
  sumx <- summary(fit)
  ct <- sumx$coefficients
  pvalue = min(ct[-c(1, nrow(ct)),'Pr(>|t|)'])
  beta = ct[-c(1, nrow(ct)),"Estimate"][which.min(ct[-c(1, nrow(ct)),'Pr(>|t|)'])]
  df = tibble::tibble("blood"=metabolite[1],
                      "gut"=metabolite[2],
                      "pvalue"=pvalue,
                      "beta" = beta)
  return(df)
}

variable_pairs <- expand.grid(names(blood_metabolomics_data), 
                              names(gut_metabolomics_data), stringsAsFactors = F)

blood_gut_regression <- apply(variable_pairs, 1, perform_regression)%>%
  dplyr::bind_rows()%>%
  dplyr::mutate(padj=p.adjust(pvalue, method = "bonferroni"))

write_csv(blood_gut_regression, "/home/shimw/project/200Human/result/004.metabolite_network.blood_gut_regression.csv")






## 计算两者同时相关的临床指标 ####
clinical_type <- readr::read_csv( "/home/shimw/project/200Human/体检报告/clinical_type.csv")%>%
  rename(clinical = feature)
clinical_indicator <- readr::read_csv("/home/shimw/project/200Human/问卷/000.all_clinical_indicator.csv")
blood_metabolite_regression <- readr::read_csv("/home/shimw/project/200Human/result/001.metabolite_regression.csv")%>%
  dplyr::mutate(padj=p.adjust(pvalue, method = "bonferroni"))%>%
  dplyr::filter(padj<0.05)%>%
  dplyr::rename(clinical = factor1, blood = factor2,
                "blood_beta" = beta, "blood_padj" = padj)%>%
  select(clinical,blood,blood_beta,blood_padj)%>%
  left_join(clinical_type)%>%
  filter(clinical!="钠(Na)")#钠离子太多了，因此我删一下

gut_metabolite_regression <- readr::read_csv("/home/shimw/project/200Human/result/001.gutmetabolite_regression.csv")%>%
  dplyr::mutate(padj=p.adjust(pvalue, method = "bonferroni"))%>%
  dplyr::filter(padj<0.05)%>%
  dplyr::rename(clinical = factor1, gut = factor2,
                "gut_beta" = beta, "gut_padj" = padj)%>%
  select(clinical,gut,gut_beta,gut_padj)%>%
  left_join(clinical_type)%>%
  filter(clinical!="钠(Na)")

kk = as.data.frame(table(blood_metabolite_regression$type))
kk$type = "血液代谢"
aa = as.data.frame(table(gut_metabolite_regression$type))
aa$type = "肠道代谢"
library(ggsci)
ggplot(rbind(aa,kk), aes(x = type, y = Freq, fill = Var1)) +
  geom_bar(stat = "identity", position = "stack", width = 0.5) +
  labs(title = "不同类别指标相关代谢物数量",
       x = " ",
       y = "代谢物数量",
       fill = "") +
  theme_bw()+
  theme(text = element_text(size = 14)) +
  scale_fill_simpsons()
ggsave("/home/shimw/project/200Human/result/004.metabolite_relevant_metabolitesnumber.pdf",width = 4,height = 4)

total_counts <- aggregate(Freq ~ type, data = rbind(aa, kk), sum)
combined_data <- merge(rbind(aa, kk), total_counts, by = "type", suffixes = c("_metabolite", "_total"))
combined_data$proportion <- combined_data$Freq_metabolite / combined_data$Freq_total
ggplot(combined_data, aes(x = type, y = proportion, fill = Var1)) +
  geom_bar(stat = "identity", position = "stack", width = 0.5) +
  labs(title = "不同类别指标相关代谢物比例",
       x = " ",
       y = "代谢物比例",
       fill = "") +
  theme_bw()+
  theme(text = element_text(size = 14)) +
  scale_fill_simpsons()
ggsave("/home/shimw/project/200Human/result/004.metabolite_relevant_metabolites_proportion.pdf",width = 4,height = 4)

intersect(blood_metabolite_regression$clinical, gut_metabolite_regression$clinical)
# 共有的临床指标有28个, 但是特别奇怪的是Na离子相关的代谢物太多了一个就有5597130个配对，其他配对不到他的零头
all_regression = left_join(blood_metabolite_regression, gut_metabolite_regression)%>%
  tidyr::drop_na()


## 筛选临床病理代谢物

clinical_regression <- all_regression%>%
  dplyr::filter(clinical%in%names(clinical_indicator)[1:64])
## 然后结果是6264个配对,15个病理指标
## 两个项目共有的代谢物有608个，但是与相同临床指标相关且代谢物相同的情况只有4种
# 1 Total cholesterol     Uric acid  0.0454       2.01e- 2 Uric…  4.54e-2 2.09e- 2
# 2 血糖                  N-acetyl-… 0.0000104    5.26e-36 N-ac…  5.61e-7 6.08e-11
# 3 糖化血红蛋白(HBA1c)   N-acetyl-… 0.00000471   3.95e-19 N-ac…  2.34e-7 4.68e- 4
# 4 超敏C-反应蛋白(hsCRP) N-acetyl-… 0.0000466    7.67e-14 N-ac…  4.02e-6 1.34e-19










