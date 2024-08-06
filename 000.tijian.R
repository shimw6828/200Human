library(dplyr)
q1 <- readxl::read_xlsx("/home/shimw/project/200Human/问卷/体检问卷结果.xlsx")%>%
  dplyr::select(name,age) # 加入年龄

meta_data <- readxl::read_xlsx("/home/shimw/project/200Human/问卷/治未病采血登记表.xlsx", skip = 2)%>%
  dplyr::select("编号", "姓名",   "性别")%>%
  dplyr::rename(ID="编号", name="姓名", sex="性别")%>%
  dplyr::left_join(q1) # 合并年龄

# 金域的结果整理在了songxm的000tijian.R中
data1 <- readxl::read_excel("/home/shimw/project/200Human/体检报告/迈维代谢.xlsx")%>%
  dplyr::select(-`正常范围`)%>%
  na.omit()%>%
  tibble::column_to_rownames(.,"检测项目")%>%
  t()%>%as.data.frame()%>%
  tibble::rownames_to_column("name")


data2 <- readxl::read_xlsx("/home/shimw/project/200Human/体检报告/诺敏检测结果-整理.xlsx", sheet = 3)%>%
  dplyr::select(-c(1,3,4))%>%
  dplyr::rename(name = "姓名")%>%
  left_join(data1)

data3 <- readxl::read_excel("/home/songxm/project/200Human/20231219.xlsx")%>%
  # tidyr::drop_na(`李梦境`)%>% # 注意，这里过滤的时候把男女特有的特征删掉了，比如孕酮
  tibble::column_to_rownames(.,"项目")%>%
  t()%>%as.data.frame()%>%
  tibble::rownames_to_column("name")%>%
  left_join(data2)%>%
  mutate("血糖"=as.numeric(`血糖`), "空腹胰岛素(ins)"=as.numeric(`空腹胰岛素(ins)`))%>%
  mutate("HOMA-IR"= `血糖`*`空腹胰岛素(ins)`/22.5)



data4 <- readr::read_csv("/home/shimw/project/200Human/体检报告/000.clinical_indicator.csv")%>%
  left_join(., meta_data[,1:2])
data5 <- left_join(data3, data4)


w_h_data = readxl::read_xlsx("/home/shimw/project/200Human/问卷/体检问卷结果.xlsx")%>%
  mutate("腰臀比" = as.numeric(waistline)/as.numeric(hips),
         "FEV/FVC" = as.numeric(FEV)/as.numeric(FVC))%>%
  select(name, "腰臀比", "FEV/FVC", "FEV", "FVC")

data <- left_join(data5, w_h_data)%>%
  dplyr::mutate("中性粒细胞在白细胞中的百分比" = (Neutrophil/Leukocyte)*100,
                "Free T3 to reverse T3 ratio" = as.numeric(`游离三碘甲状腺原氨酸(FT3)`)/as.numeric(`反三碘甲状腺原氨酸(rT3)`),
                "Cu/Zn" = as.numeric(`全血铜(Cu)`)/as.numeric(`全血锌(Zn)`)/100,
                "脉压(Pulse pressure)" = as.numeric(`Blood pressure-systolic`)-as.numeric(`Blood pressure-diastolic`),
                "平均动脉压(Mean arterial pressure)" = (as.numeric(`Blood pressure-systolic`) + 2*as.numeric(`Blood pressure-diastolic`))/3
                )



glucose_range = list(
  "空腹胰岛素(ins)" = c(1.8, 11.8), 
  "血糖" = c(3.60, 6.10), 
  "postprandial blood glucose" = c(3.9, 7.8),
  "糖化血红蛋白(HBA1c)" = c(4.0, 6.0),
  "HOMA-IR" = c(0, 1.4)
)

lipids_range = list(
  "BMI" = c(18.5,23.9),
  "腰臀比" = list("男" = c(0, 0.9),"女" = c(0, 0.85)),
  "小而密低密度脂蛋白胆固醇(sd LDL-C)" = list(
    "男" = list("年轻"=c(0.246, 1.393), "老年"=c(0.264, 1.362)),
    "女" = list("年轻"=c(0.243, 1.109), "老年"=c(0.264, 1.362))
  ),
  "Total cholesterol" = c(0,5.2),
  "HDL-C"=c(0.8, 2.1),
  "Triglycerides" = c(0, 1.7),
  "TG/HDL" = c(0, 1),
  "LDL-C" = c(0,3.33),
  "载脂蛋白A1(APOA1)" = list("男" = c(1.05, 1.75),
                         "女" = c(1.05, 2.05)),
  "载脂蛋白B(APOB)" = list("男"=c(0.60, 1.40), "女"=c(0.55, 1.30)),
  "Platelets" = c(125, 350)
)
pressure_range = list(
  "heart_rate" = c(60, 100), 
  "Blood pressure-systolic" = c(90, 140), 
  "Blood pressure-diastolic" = c(60, 89),
  "脉压(Pulse pressure)" = c(30, 65),
  "平均动脉压(Mean arterial pressure)" = c(70, 105)
)

inflammation_range = list(
  "人氧化型低密度脂蛋白(Ox-LDL)" = c(0, 60),
  "超敏C-反应蛋白(hsCRP)" = c(0, 4.00),
  "同型半胱氨酸(HCY)" = list("小于51岁" = c(0, 15),
                       "大于50岁" = c(0, 20)),
  "中性粒细胞在白细胞中的百分比" = c(40, 75)
)
immunity_range = list(
  "补体4(C4)" = c(0.17, 0.48),
  "人转化生长因子β1(TGF-β1)" = c(7.58, 18.70),
  "人黑色素细胞刺激素(MSH)" = c(0.87, 2.58)
)
thyroid_range = list(
  "游离三碘甲状腺原氨酸(FT3)" = c(2.43, 6.01),
  "游离甲状腺素(FT4)" = c(9.01,19.05),
  "超敏促甲状腺素(TSH)" = c(0.3500, 4.9400),
  "反三碘甲状腺原氨酸(rT3)" = c(0.31, 0.95),
  "Free T3 to reverse T3 ratio" = c(0.02, 100000)# 是大于0.02
)

kidney_range = list(
  "BUN" = c(2.6, 9.5),
  "Creatinine" = list(
    "男" = list("小于60"=c(57, 97), "大于60"=c(57, 111)),
    "女" = list("小于60"=c(41, 73), "大于60"=c(41, 81))
  ),
  "Uric acid" = c(135, 425)
)
liver_range = list(
  "AST" = c(0, 40),
  "ALT" = c(0, 50),
  "乳酸脱氢酶(LDH)" = c(120, 250),
  "Albumin" = c(40, 55),
  "Globulin" = c(20, 40),
  "A/G" = c(1.2, 2.4),
  "GGT" = c(7, 60),
  "ALP" = c(35, 135),
  "Total Bilirubin" = c(2,21),
  "Total Protein" = c(65, 85)
)
lung_range = list(
  "FEV/FVC" = c(0.7, 1)
)

gut_range = list(
  "抗组织谷氨酰胺转移酶抗体 IgG" = c(0.00, 20.00),
  "抗去酰胺基麦胶蛋白抗体 IgA" = c(0, 10, 15),
  "抗去酰胺基麦胶蛋白抗体 IgG" = c(0, 10, 15),
  "抗SCL-70抗体" =  c(0, 20),
  "抗核糖体P蛋白抗体,定量" = c(0, 20),
  "抗SSA抗体(SS-A/Ro60)" = c(0, 20),
  "抗Ro52抗体,定量"  = c(0, 20),
  "抗SSB抗体(SS-B/La)"  = c(0, 20),
  "抗Jo-1抗体"  = c(0, 20),
  "抗Sm抗体"  = c(0, 20),
  "抗U1RNP抗体"= c(0, 20),
  "抗着丝点蛋白B抗体(CENP-B)"  = c(0, 20),
  "抗双链DNA抗体定量" = c(0, 20),
  "抗核抗体(ANA)总抗体" = c("阴性") # 结果为诊断结果（阴性阳性）
)

hormones_range = list(
  "皮质醇(COR)" = c(133,537),
  "人孕烯醇酮(PREG)" = c(2.2, 10.0),
  "脱氢表雄酮(DHEA)" = list("男" = c(1.33, 6.48), "女" = c(1, 5.86)),
  "雌二醇(E2)" = c(10, 649),
  "肾素浓度(Renin)" = c(3.8,38.8),
  "醛固酮(ALD)" = c(40.00, 310.00), # 从范围判断为立位
  "多巴胺(DA)" = c(0,195.7),
  "肾上腺素(E)" = c(0,769),
  "去甲肾上腺素(NE)" = c(1182.8,10054),
  "甲状旁腺激素(PTH)" = c(1.60, 6.90),
  "孕酮(P)" = c(0.1, 242.50),
  "促卵泡激素(FSH)" = c(1.38, 133.41),
  "抗缪勒管激素(AMH)" = c(0.06,12.53),
  "游离睾酮(FT)" = c(4.94, 32.01),
  "睾酮(T)" = c(15,50)
)
anemic_range = list(
  "Hemoglobin" = c(115, 175),
  "Vitamin B9 叶酸" = c(4, 35),
  "Vitamin B12" = c(0, 52.9),
  "MCV" = c(82, 100),
  "RDW" = c(37, 50)
)
Vitamin_range = list(
  "Vitamin B6" = c(2, 25),
  "Vitamin B1" = c(0.5, 10),
  "Vitamin B2" = c(3, 20),
  "Vitamin B3" = c(12, 150),
  "Vitamin B5" = c(10, 100),
  "Vitamin B7" = c(0.1, 2),
  "Vitamin D" = c(20,100),
  "Vitamin E" = c(5000, 25000),
  "Vitamin A" = c(160, 1200),
  "Vitamin K1" = c(0.1, 2.2)
)
fatty_acids_range = list(
  "Omega-6:Omega-3" = c(1,4),
  "Omega-3指数" = c(8, 100)
  #"AA:EPA" = c(0,3)
)
element_range = list(
  "钾(K)" = c(3.50, 5.30),
  "钠(Na)" = c(137.0,147.0),
  "血总钙(CA)" = c(2.11, 2.52),
  "血磷(P)" = c(0.85, 1.81),
  "血铁(FE)" = list("男" = c(10.7, 36.7), "女" = c(7.8, 32.2)),
  "全血铁(Fe)" = list("男" = c(464.9, 683.2), "女" = c(394.5, 585.6)),
  "全血钙(Ca)" = list("男" = c(46.7, 64.1), "女" = c(53.3, 68.2)),
  "全血镁(Mg)" = list("男" = c(35.1, 50.5), "女" = c(31.5, 45.7)),
  "全血铜(Cu)" = list("男" = c(629.6,1039.7), "女" = c(713.7,1488.2)),
  "全血锌(Zn)" = list("男" = c(5.3, 8.6), "女" = c(4.6, 7.8)),
  "Cu/Zn" = c(0.54, 1.68),
  "全血硒(Se)" = c(58.4, 232.1),
  "全血钒(V)" = c(50, 4700),
  "全血铬(Cr)" = c(0.7, 28),
  "全血锰(Mn)" = list("男" = c(7.3, 24.0), "女" = c(4, 21.5)),
  "全血钴(Co)" = c(0.1, 0.7),
  "全血镍(Ni)" = c(1, 27.9),
  "全血铷(Rb)" = c(0.9, 3.5),
  "全血钼(Mo)" = c(0.2, 3.8),
  "人还原性谷胱甘肽(GSH)" = c(51.9, 68.6),
  "辅酶Q10" = c(0.46, 1.85)
)
poison_range = list(
  "全血铅(Pb)" = c(0, 400),
  "全血镉(Cd)" = c(0, 5),
  "全血微量元素汞" = c(0, 5),
  "全血砷(As)" = c(0.13, 8.54),
  "抗甲状腺球蛋白抗体(TGAb)" = c(0.00, 75.00),
  "抗甲状腺过氧化物酶抗体(TPOAb)" = c(0.0, 30.0),
  "霉菌毒素(mycotoxins)" = c("阴性"),
  "草甘膦(Glyphosate)" = c("阴性")
)

s_feature = c(
  "腰臀比",
  "载脂蛋白A1(APOA1)","载脂蛋白B(APOB)", 
  "血铁(FE)", 
  "全血铁(Fe)", "全血钙(Ca)", "全血镁(Mg)", 
  "全血铜(Cu)",  "全血锌(Zn)", "全血锰(Mn)",
  "脱氢表雄酮(DHEA)")


all_range = c(glucose_range, lipids_range, pressure_range, inflammation_range, immunity_range,
              thyroid_range, kidney_range, liver_range, lung_range, gut_range, hormones_range,
              anemic_range, Vitamin_range, fatty_acids_range, element_range, poison_range)
data$`全血铜(Cu)`
data$`全血锌(Zn)`

# readr::write_excel_csv(data[, c("name", "sex", "age",names(all_range))], "/home/shimw/project/200Human/问卷/000.all_clinical_indicator.csv")
data = readr::read_csv("/home/shimw/project/200Human/问卷/000.all_clinical_indicator.csv")

filter_outlier <- function(.x){
  tmp = unlist(data[,.x])
  
  if(.x=="霉菌毒素(mycotoxins)"|.x=="草甘膦(Glyphosate)"|.x=="抗核抗体(ANA)总抗体"){
    res = case_when(tmp=="阴性" ~ "normal",
                    TRUE ~ "higher")
    df = tibble::tibble("name" = data$name, "sex" = data$sex, "age" = data$age,
                        "feature" = .x, "res" = res, "value" = tmp) 
    return(df)
  }
  
  .tmp = as.numeric(gsub("<|>", "", tmp))
  if(.x%in%s_feature){
    res = case_when(data$sex=="男"&.tmp>=all_range[[.x]][["男"]][1]&.tmp<=all_range[[.x]][["男"]][2] ~ "normal",
                    data$sex=="女"&.tmp>=all_range[[.x]][["女"]][1]&.tmp<=all_range[[.x]][["女"]][2] ~ "normal",
                    data$sex=="男"&.tmp>all_range[[.x]][["男"]][2] ~ "higher",
                    data$sex=="女"&.tmp>all_range[[.x]][["女"]][2] ~ "higher",
                    TRUE ~ "lower")
    df = tibble::tibble("name" = data$name, "sex" = data$sex, "age" = data$age,
                        "feature" = .x, "res" = res, "value" = as.character(tmp)) 
    return(df)
  }
  if(.x=="小而密低密度脂蛋白胆固醇(sd LDL-C)"){
    res = case_when(data$sex=="男"&data$age<=44&.tmp>=0.246&.tmp<=1.393 ~ "normal",
                    data$sex=="女"&data$age<=54&.tmp>=0.243&.tmp<=1.109 ~ "normal",
                    data$sex=="男"&data$age>44&.tmp>=0.264&.tmp<=1.362 ~ "normal",
                    data$sex=="女"&data$age>54&.tmp>=0.264&.tmp<=1.362 ~ "normal",
                    data$sex=="男"&data$age<=44&.tmp>1.393 ~ "higher",
                    data$sex=="女"&data$age<=54&.tmp>1.109 ~ "higher",
                    data$sex=="男"&data$age>44&.tmp>1.362 ~ "higher",
                    data$sex=="女"&data$age>54&.tmp>1.362 ~ "higher",
                    TRUE ~ "lower")
    df = tibble::tibble("name" = data$name, "sex" = data$sex, "age" = data$age,
                        "feature" = .x, "res" = res, "value" = as.character(tmp)) 
    return(df)
  }
  if(.x=="同型半胱氨酸(HCY)"){
    res = case_when(data$age<=50&.tmp<=15 ~ "normal",
                    data$age>50&.tmp<=20 ~ "normal",
                    data$age<=50&.tmp>15 ~ "higher",
                    data$age>50&.tmp>20 ~ "higher"
    )
    df = tibble::tibble("name" = data$name, "sex" = data$sex, "age" = data$age,
                        "feature" = .x, "res" = res, "value" = as.character(tmp)) 
    return(df)
  }
  if(.x=="Creatinine"){
    res = case_when(data$sex=="男"&data$age<=59&.tmp<57 ~ "lower",
                    data$sex=="男"&data$age<=59&.tmp>97 ~ "higher",
                    data$sex=="男"&data$age>59&.tmp<57 ~ "lower",
                    data$sex=="男"&data$age>59&.tmp>111 ~ "higher",
                    data$sex=="女"&data$age<=59&.tmp<41 ~ "lower",
                    data$sex=="女"&data$age<=59&.tmp>73 ~ "higher",
                    data$sex=="女"&data$age>59&.tmp<41 ~ "lower",
                    data$sex=="女"&data$age>59&.tmp>81 ~ "higher",
                    TRUE ~ "normal"
    )
    df = tibble::tibble("name" = data$name, "sex" = data$sex, "age" = data$age,
                        "feature" = .x, "res" = res, "value" = as.character(tmp)) 
    return(df)
  }
  
  res = case_when(is.na(.tmp) ~ "NA",
                  .tmp>=all_range[[.x]][1]&.tmp<=all_range[[.x]][2] ~ "normal",
                  .tmp>all_range[[.x]][2] ~ "higher",
                  TRUE ~ "lower")
  df = tibble::tibble("name" = data$name, "sex" = data$sex, "age" = data$age,
                      "feature" = .x, "res" = res, "value" = as.character(tmp))%>%
    tidyr::drop_na()
  return(df)
}




res = purrr::map(names(all_range), filter_outlier)%>%
  dplyr::bind_rows()
readr::write_excel_csv(res, "/home/shimw/project/200Human/体检报告/all_clinical.csv")

res%>%
  dplyr::filter(res!="normal")%>%
readr::write_excel_csv(., "/home/shimw/project/200Human/体检报告/指标异常.csv")



res%>%
  dplyr::filter(feature=="小而密低密度脂蛋白胆固醇(sd LDL-C)")%>%
  readr::write_excel_csv(., "/home/shimw/project/200Human/体检报告/小而密低密度脂蛋白胆固醇.csv")


## 女性特有，前两个值与经期规律有关，差距太大了，我没办法确定取血时受试者在哪个时期
# data <- readxl::read_excel("/home/songxm/project/200Human/20231219.xlsx")
res = readr::read_csv("/home/shimw/project/200Human/体检报告/all_clinical.csv")
prop_feature <- res %>%
  group_by(feature, res) %>%
  summarise(N=n()) %>%
  group_by(feature) %>%
  mutate(prop = (N/sum(N)), Cumsum = cumsum(prop))%>%
  mutate(res = factor(res, levels = c("higher", "normal","lower")))

library(ggplot2)
library("ggsci")
library(showtext) 
showtext_auto (enable = TRUE) 

glucose_range # 糖毒性和胰岛素抵抗的检测标志（糖尿病）
lipids_range # 脂质代谢（心脑血管相关的疾病）
pressure_range # 血压心率（高血压和心脏健康状况）
inflammation_range # 炎症相关
immunity_range # 炎症引起的先天性免疫激活
thyroid_range # 甲状腺功能
kidney_range # 肾功能
liver_range # 肝损伤和肝功能
lung_range # 肺活量（慢性阻塞性肺疾病（COPD）和哮喘）
gut_range # 麸质敏感性肠病或者乳糜泻  自身免疫性疾病（抗可提取性核抗原多肽抗体8项+3=11）
hormones_range # 激素类
anemic_range # 贫血
Vitamin_range # 维生素
fatty_acids_range # 红细胞脂肪酸
element_range # 元素
poison_range # 环境毒素
all_range = c(glucose_range, lipids_range, pressure_range, inflammation_range, immunity_range,
              thyroid_range, kidney_range, liver_range, lung_range, gut_range, hormones_range,
              anemic_range, Vitamin_range, fatty_acids_range, element_range, poison_range)
kk = tibble("feature" = names(all_range),
            "class" = c(rep("血糖", length(glucose_range)),
                        rep("血脂", length(lipids_range)),
                        rep("血压", length(pressure_range)),
                        rep("炎症反应", length(inflammation_range)),
                        rep("免疫激活相关", length(immunity_range)),
                        rep("甲状腺功能", length(thyroid_range)),
                        rep("肾功能", length(kidney_range)),
                        rep("肝损伤和肝功能", length(liver_range)),
                        rep("肺活量", length(lung_range)),
                        rep("肠病", length(gut_range)),
                        rep("激素类", length(hormones_range)),
                        rep("贫血", length(anemic_range)),
                        rep("维生素", length(Vitamin_range)),
                        rep("红细胞脂肪酸", length(fatty_acids_range)),
                        rep("微量元素", length(element_range)),
                        rep("环境毒素", length(poison_range)))
            )
res <- readr::read_csv("/home/shimw/project/200Human/体检报告/all_clinical.csv")


type_info <- data_frame("feature" = unique(res$feature),# 顺序和表里是一致的
                        "type" = c(rep("病理类", 60), 
                                   rep("激素类", 15), 
                                   rep("营养类", 39),
                                   rep("环境毒素", 8))
)%>%
  left_join(kk)
readr::write_excel_csv(type_info, "/home/shimw/project/200Human/体检报告/clinical_type.csv")
### 输出一下临床指标分类，用于备用


plot_prop <- function(.x){
  p1 = prop_feature%>%
    filter(feature%in%.x)%>%
    mutate(feature = factor(feature, levels = rev(.x)))%>%
    ggplot(data=.,
           aes(feature,prop,fill=res))+
    scale_fill_npg()+
    geom_bar(stat="identity",position="stack", color="black", width=0.7,linewidth=0.25)+
    labs(x = "",y = "Prop")+theme_classic()+coord_flip()+labs(fill = "体检结果")+
    theme(
      axis.title.x = element_blank(),
      axis.text.x = element_text(size = 11),
      axis.text.y = element_text(size = 12),
      legend.text = element_text(size=11)
    )
  return(p1)
}


p1 = plot_prop(names(c(glucose_range,lipids_range, pressure_range)))
ggsave("/home/shimw/project/200Human/体检报告/000.1病理.pdf", p1, width=6.2,height=5.3)
p2 = plot_prop(names(c(inflammation_range,immunity_range, thyroid_range, kidney_range,liver_range)))
ggsave("/home/shimw/project/200Human/体检报告/000.2病理.pdf", p2, width=5.8,height=6.2)
p3 = plot_prop(names(c(lung_range, gut_range)))
ggsave("/home/shimw/project/200Human/体检报告/000.3病理.pdf", p3, width=5.8,height=3.6)

p4 = plot_prop(names(c(hormones_range)))
ggsave("/home/shimw/project/200Human/体检报告/000.4激素.pdf", p4, width=4.9,height=3.8)

p5 = plot_prop(names(c(anemic_range,Vitamin_range, fatty_acids_range)))
ggsave("/home/shimw/project/200Human/体检报告/000.5营养.pdf", p5, width=4.9,height=4.6)

p6 = plot_prop(names(element_range))
ggsave("/home/shimw/project/200Human/体检报告/000.6营养.pdf", p6, width=5.2,height=5.3)

p7 = plot_prop(names(poison_range))
ggsave("/home/shimw/project/200Human/体检报告/000.7毒素.pdf", p7, width=5.9,height=2.2)



library(ggplot2)
library("ggsci")

gloose <- readr::read_csv("/home/songxm/project/200Human/HOMA-IR.csv")

gloose_TG <- left_join(gloose, data)

summary(aov(BMI~`胰岛素抵抗风险`, data=gloose_TG))
# summary(aov(`Total cholesterol`~`胰岛素抵抗风险`, data=gloose_TG))
summary(aov(`HDL-C`~`胰岛素抵抗风险`, data=gloose_TG))
summary(aov(`LDL-C`~`胰岛素抵抗风险`, data=gloose_TG))
summary(aov(`Triglycerides`~`胰岛素抵抗风险`, data=gloose_TG))
# summary(aov(`Platelets`~`胰岛素抵抗风险`, data=gloose_TG))
summary(aov(`TG/HDL`~`胰岛素抵抗风险`, data=gloose_TG))

library(ggpubr)
my_comparisons <- list(c("严重的胰岛素抵抗", "正常"), c("早期胰岛素抵抗", "正常"),c("有胰岛素抵抗的风险", "正常"))
ggplot(gloose_TG, aes(x=`胰岛素抵抗风险`, y=`BMI`, fill= `胰岛素抵抗风险`)) + 
  geom_boxplot( outlier.shape = NA)+
  stat_boxplot(geom = "errorbar",aes(ymin=..ymax..),width=0.3,color="black")+
  stat_boxplot(geom = "errorbar",aes(ymax=..ymin..),width=0.3,color="black")+
  stat_compare_means(comparisons = my_comparisons,label = "p.signif", tip.length=0.02, method = "t.test") +
  scale_fill_manual(values=c('严重的胰岛素抵抗'="#c55a11", 
                             '早期胰岛素抵抗'="#ed7d31",
                             '有胰岛素抵抗的风险'="#a5a5a5", 
                             '正常'="#b6df89", 
                             '胰岛素敏感'="#92d050"))+
  theme_classic()+
  labs(title = "BMI")+
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        plot.title = element_text(hjust = 0.5))


data = readr::read_csv("/home/shimw/project/200Human/问卷/000.all_clinical_indicator.csv")
# BMI
data$BMI
case_when(
  data$BMI<18.5 ~ "体重过低(<18.5)",
  data$BMI>=18.5&data$BMI<24 ~ "正常(18.5-24)",
  data$BMI>=24&data$BMI<28 ~ "超重(24-28)",
  data$BMI>=28 ~ "肥胖(>=28)"
)%>%
  table()
list("男" = c(0, 0.9),"女" = c(0, 0.85))
case_when(
  data$`腰臀比`<=0.9&data$sex=="男" ~ "正常",
  data$`腰臀比`<=0.85&data$sex=="女" ~ "正常",
  TRUE ~ "偏胖"
)%>%
  table()

## 血压
case_when(
  data$`Blood pressure-systolic`>140|data$`Blood pressure-diastolic`>90 ~ "高血压",
  data$`Blood pressure-systolic`<90|data$`Blood pressure-diastolic`<60 ~ "低血压",
  T ~ "正常"
)%>%
  table()

# 小而密低密度脂蛋白胆固醇(sd LDL-C)
list(
  "男" = list("年轻"=c(0.246, 1.393), "老年"=c(0.264, 1.362)),
  "女" = list("年轻"=c(0.243, 1.109), "老年"=c(0.264, 1.362))
)
case_when(
  data$`腰臀比`<=0.9&data$sex=="男" ~ "正常",
  data$`腰臀比`<=0.85&data$sex=="女" ~ "正常",
  TRUE ~ "偏胖"
)%>%
  table()

prop_feature[prop_feature$feature=="小而密低密度脂蛋白胆固醇(sd LDL-C)",]

# "Total cholesterol" = c(0,5.2),
case_when(
  data$`Total cholesterol`<5.2 ~ "正常(2-5.2)",
  data$`Total cholesterol`>=5.2&data$`Total cholesterol`<6.2 ~ "边缘升高(5.2-6.2)",
  data$`Total cholesterol`>=6.2 ~ "升高(≥6.2)"
)%>%
  table()
# "" = c(0, 1.7),
case_when(
  data$`Triglycerides`<1.7 ~ "正常(0-1.7)",
  data$`Triglycerides`>=1.7&data$`Triglycerides`<2.3 ~ "边缘升高(1.7-2.3)",
  data$`Triglycerides`>=2.3 ~ "升高(2.3-5.7)"
)%>%
  table()
# "LDL-C" = c(0,3.33),
case_when(
  data$`LDL-C`<3.37 ~ "正常(0-3.37)",
  data$`LDL-C`>=3.37&data$`LDL-C`<4.12 ~ "边缘升高(3.37-4.12)",
  data$`LDL-C`>=4.14 ~ "升高(≥4.14)"
)%>%
  table()
# "抗组织谷氨酰胺转移酶抗体 IgG" = c(0.00, 20.00)
# "抗去酰胺基麦胶蛋白抗体 IgA" = c(0, 10, 15)
# "抗去酰胺基麦胶蛋白抗体 IgG" = c(0, 10, 15)
table(data$"抗组织谷氨酰胺转移酶抗体 IgG"<20&as.numeric(gsub("<|>", "", data$"抗去酰胺基麦胶蛋白抗体 IgA"))<10&as.numeric(gsub("<|>", "", data$"抗去酰胺基麦胶蛋白抗体 IgG"))<10)
table(data$"抗组织谷氨酰胺转移酶抗体 IgG"<20)
table(as.numeric(gsub("<|>", "", data$"抗去酰胺基麦胶蛋白抗体 IgA"))<10)
table(as.numeric(gsub("<|>", "", data$"抗去酰胺基麦胶蛋白抗体 IgG"))<10)
# "抗SCL-70抗体" =  c(0, 20),
# "抗核糖体P蛋白抗体,定量" = c(0, 20),
# "抗SSA抗体(SS-A/Ro60)" = c(0, 20),
# "抗Ro52抗体,定量"  = c(0, 20),
# "抗SSB抗体(SS-B/La)"  = c(0, 20),
# "抗Jo-1抗体"  = c(0, 20),
# "抗Sm抗体"  = c(0, 20),
# "抗U1RNP抗体"= c(0, 20),

table(data$"抗SCL-70抗体"<20&data$"抗核糖体P蛋白抗体,定量"<20&data$"抗SSA抗体(SS-A/Ro60)"<20&data$"抗Ro52抗体,定量" <20&data$"抗SSB抗体(SS-B/La)" <20&data$"抗Jo-1抗体"<20&data$"抗Sm抗体"<20&data$"抗U1RNP抗体"<20)



c(0, 1)
prop_feature[prop_feature$feature=="TG/HDL",]
prop_feature[prop_feature$feature=="TG/HDL",]
# "人转化生长因子β1(TGF-β1)" = c(7.58, 18.70),
prop_feature[prop_feature$feature=="人转化生长因子β1(TGF-β1)",]

# "人黑色素细胞刺激素(MSH)" = c(0.87, 2.58)
prop_feature[prop_feature$feature=="人黑色素细胞刺激素(MSH)",]
#  "FEV/FVC" = c(0, 0.7)
# "肾素浓度(Renin)" = c(3.8,38.8),
prop_feature[prop_feature$feature=="肾素浓度(Renin)",]
# "睾酮(T)" = c(15,50)
prop_feature[prop_feature$feature=="睾酮(T)",]
#  "Vitamin B2" = c(3, 20),
# "Vitamin B3" = c(12, 150),
#  "Vitamin D" = c(20,100),
prop_feature[prop_feature$feature=="Omega-3指数",]

# "Omega-3指数" = c(8, 100)
# "Vitamin B9 叶酸" = c(4, 35),
# "Vitamin B7" = c(0.1, 2),
#  "Vitamin B6" = c(2, 25),
# "Vitamin K1" = c(0.1, 2.2)
prop_feature[prop_feature$feature=="Vitamin K1",]
prop_feature[prop_feature$feature=="LDL-C",]

prop_feature[prop_feature$feature=="全血铁(Fe)",]

# "血总钙(CA)" = c(2.11, 2.52),
# "血铁(FE)" = list("男" = c(10.7, 36.7), "女" = c(7.8, 32.2)),
# "全血钙(Ca)" = list("男" = c(46.7, 64.1), "女" = c(53.3, 68.2)),
# "Cu/Zn" = c(0.54, 1.68),
prop_feature[prop_feature$feature=="皮质醇(COR)",]
prop_feature[prop_feature$feature=="Free T3 to reverse T3 ratio",]


0.7-1.0

# "载脂蛋白A1(APOA1)" = list("男" = c(1.05, 1.75),
#                        "女" = c(1.05, 2.05)),
# "载脂蛋白B(APOB)" = list("男"=c(0.60, 1.40), "女"=c(0.55, 1.30)),
prop_feature[prop_feature$feature=="载脂蛋白A1(APOA1)",]
prop_feature[prop_feature$feature=="同型半胱氨酸(HCY)",]
data$`同型半胱氨酸(HCY)`
prop_feature[prop_feature$feature=="反三碘甲状腺原氨酸(rT3)",]

food = food%>%dplyr::filter(`Q1_您的姓名`%in%q1$name)%>%
  group_by(`Q1_您的姓名`)  %>% slice( n() ) %>% tibble()%>%
  select(Q1_您的姓名, Q177_您是否正在吃黑米)%>%
  dplyr::rename(name = Q1_您的姓名, black_rice = Q177_您是否正在吃黑米)

AMH <- readr::read_csv("/home/shimw/project/200Human/问卷/000.all_clinical_indicator.csv")%>%
  dplyr::select(name, sex, age, "抗缪勒管激素(AMH)")%>%
  drop_na()%>%
  left_join(food)%>%
  dplyr::mutate(`抗缪勒管激素(AMH)` = as.numeric(gsub("<|>", "", `抗缪勒管激素(AMH)`)))%>%
  dplyr::mutate(black_rice = if_else(is.na(black_rice), "否", black_rice))%>%
  dplyr::mutate(black_rice_s = if_else(black_rice=="否", 0,1))%>%
  dplyr::filter(age<50)

table(AMH$black_rice)

wilcox.test(AMH[AMH$black_rice=="是",]$`抗缪勒管激素(AMH)`,
            AMH[AMH$black_rice!="是",]$`抗缪勒管激素(AMH)`)
summary(aov(`抗缪勒管激素(AMH)` ~ black_rice + age, data = AMH))

summary(glm(`抗缪勒管激素(AMH)` ~ black_rice_s + age, data = AMH))

ggplot(AMH, aes(age, `抗缪勒管激素(AMH)`, color = black_rice))+
  geom_point()+
  stat_smooth(alpha=0,method='loess')
AMH$`抗缪勒管激素(AMH)`


res <- readr::read_csv("/home/shimw/project/200Human/体检报告/all_clinical.csv")
FI_lab <- res%>%
  dplyr::mutate(score = if_else(res=="normal", 1, 0))%>%
  group_by(name,sex)%>%
  summarise(FI = sum(score))%>%
  dplyr::mutate(FI = if_else(sex=="女", FI/120, FI/119))


FI_lab = readr::read_csv("/home/shimw/project/200Human/问卷/000.all_clinical_indicator.csv")%>%
  dplyr::select(name, age)%>%
  left_join(FI_lab, .)

FI_lab_age <- res%>%
  dplyr::mutate(score = if_else(res=="normal", 1, 0))%>%
  group_by(feature)%>%
  summarise(
    broom::tidy(aov(age ~ score, data = cur_data())),
    .groups = "keep"
  )%>%
  select(feature,term, statistic, p.value) %>% 
  filter(term != "Residuals")%>%
  arrange(p.value)



type_info <- data_frame("feature" = unique(res$feature),# 顺序和表里是一致的
           "type" = c(rep("病理类", 60), 
                      rep("激素类", 15), 
                      rep("营养类", 39),
                      rep("环境毒素", 8))
           )
FI_lab_age <- left_join(FI_lab_age, type_info)
library(ggsci)
FI_lab_age%>%
  mutate(feature = factor(feature, levels = FI_lab_age$feature))%>%
  ggplot(., aes(feature, -log(p.value), fill = type)) + geom_col()+
  geom_hline(yintercept = -log(0.05))+
  theme_bw()+
  scale_y_continuous(expand = c(0,0))+
  theme(
    legend.title = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5),
    axis.title.x = element_blank()
  )+
  scale_fill_manual(values = c("#1d70a9", "#ff7f0e", "#d62728", "#2ca02c"))
library(showtext) 
showtext_auto (enable = TRUE) 
ggsave("/home/shimw/project/200Human/result/000.tijian.age_cor.pdf", width = 14, height = 6)


res <- readr::read_csv("/home/shimw/project/200Human/体检报告/all_clinical.csv")
FI_lab <- res%>%
  dplyr::mutate(score = if_else(res=="normal", 1, 0))%>%
  left_join(type_info)
FI_lab = readr::read_csv("/home/shimw/project/200Human/问卷/000.all_clinical_indicator.csv")%>%
  dplyr::select(name, age)%>%
  left_join(FI_lab, .)

all_FI <- FI_lab%>%
  # dplyr::filter(type=="营养类")%>%
  group_by(name, age, sex)%>%
  summarise(FI = sum(score))%>%
  dplyr::mutate(FI = if_else(sex=="女", FI/120, FI/119))
cor.test(all_FI$FI,all_FI$age)

ggplot(all_FI, aes(x = age, y = FI))+
  geom_point(col="#546de5",fill="#546de5", shape=16,alpha=0.6)+
  geom_smooth(method = 'lm', se = F, color = "black",  linewidth = 0.7)+
  theme_bw() +
  annotate("text", x = 60, y = 0.97, label = "R = -0.074, P = 0.303",
           color="black",size = 5, angle=0)+
  theme(panel.border = element_rect(fill=NA,color="black", 
                                    size=1.1, linetype="solid")) +
  theme(plot.title = element_text(size=18,hjust = 0.5),
        axis.text.x = element_text(size = 16 , color = 'black',
                                   vjust = 0.5, hjust = 0.5, angle = 0),
        axis.text.y = element_text(size = 16, color = 'black',
                                   vjust = 0.5, hjust = 0.5, angle = 0),
        axis.title.x=element_text(size=17,angle = 0),
        axis.title.y=element_text(size=17,angle = 90),
        plot.margin = ggplot2::margin(0.2, 0.8, 0.4, 0.4, "cm"),
        axis.ticks.length = unit(-0.1,"cm"),
        panel.grid = element_blank() # 删除网格线
  )+
  labs(title = "所有指标",x = '年龄', y = '正常指标比例')
library(showtext) 
showtext_auto (enable = TRUE) 
ggsave("/home/shimw/project/200Human/result/000.tijian.all_FI_age.pdf", width = 4.8, height = 4.3)


clinical_FI <- FI_lab%>%
  dplyr::filter(type=="病理类")%>%
  group_by(name, age, sex)%>%
  summarise(FI = sum(score)/60)
cor.test(clinical_FI$FI,clinical_FI$age)

ggplot(clinical_FI, aes(x = age, y = FI))+
  geom_point(col="#546de5",fill="#546de5", shape=16,alpha=0.6)+
  geom_smooth(method = 'lm', se = F, color = "black",  linewidth = 0.7)+
  theme_bw() +
  annotate("text", x = 60, y = 1, label = "R = -0.198, P = 0.0053",
           color="black",size = 5, angle=0)+
  theme(panel.border = element_rect(fill=NA,color="black", 
                                    size=1.1, linetype="solid")) +
  theme(plot.title = element_text(size=18,hjust = 0.5),
        axis.text.x = element_text(size = 16 , color = 'black',
                                   vjust = 0.5, hjust = 0.5, angle = 0),
        axis.text.y = element_text(size = 16, color = 'black',
                                   vjust = 0.5, hjust = 0.5, angle = 0),
        axis.title.x=element_text(size=17,angle = 0),
        axis.title.y=element_text(size=17,angle = 90),
        plot.margin = ggplot2::margin(0.2, 0.8, 0.4, 0.4, "cm"),
        axis.ticks.length = unit(-0.1,"cm"),
        panel.grid = element_blank() # 删除网格线
  )+
  labs(title = "病理类指标",x = '年龄', y = '正常指标比例')
library(showtext) 
showtext_auto (enable = TRUE) 
ggsave("/home/shimw/project/200Human/result/000.tijian.clinical_FI_age.pdf", width = 4.8, height = 4.3)


nutrition_FI <- FI_lab%>%
  dplyr::filter(type=="营养类")%>%
  group_by(name, age, sex)%>%
  summarise(FI = sum(score)/39)
cor.test(nutrition_FI$FI,nutrition_FI$age)

ggplot(nutrition_FI, aes(x = age, y = FI))+
  geom_point(col="#546de5",fill="#546de5", shape=16,alpha=0.6)+
  geom_smooth(method = 'lm', se = F, color = "black",  linewidth = 0.7)+
  theme_bw() +
  annotate("text", x = 60, y = 1, label = "R = 0.093, P = 0.19",
           color="black",size = 5, angle=0)+
  theme(panel.border = element_rect(fill=NA,color="black", 
                                    size=1.1, linetype="solid")) +
  theme(plot.title = element_text(size=18,hjust = 0.5),
        axis.text.x = element_text(size = 16 , color = 'black',
                                   vjust = 0.5, hjust = 0.5, angle = 0),
        axis.text.y = element_text(size = 16, color = 'black',
                                   vjust = 0.5, hjust = 0.5, angle = 0),
        axis.title.x=element_text(size=17,angle = 0),
        axis.title.y=element_text(size=17,angle = 90),
        plot.margin = ggplot2::margin(0.2, 0.8, 0.4, 0.4, "cm"),
        axis.ticks.length = unit(-0.1,"cm"),
        panel.grid = element_blank() # 删除网格线
  )+
  labs(title = "营养类指标",x = '年龄', y = '正常指标比例')
library(showtext) 
showtext_auto (enable = TRUE) 
ggsave("/home/shimw/project/200Human/result/000.tijian.nutrition_FI_age.pdf", width = 4.8, height = 4.3)




