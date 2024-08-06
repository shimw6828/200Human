library(dplyr)
library(ggplot2)
#### 01.整理问卷 ####
q1 = readxl::read_xlsx("/home/shimw/project/200Human/data/问卷/体检问卷结果.xlsx")
q2 = readxl::read_xlsx("/home/shimw/project/200Human/data/问卷/健康状况调查问卷_数据详情表_原始数据_202401291411.xlsx")%>% # 去重保留最后一次记录
  group_by(`Q1_您的姓名`)  %>% slice( n() ) %>% tibble()
q3 = readxl::read_xlsx("/home/shimw/project/200Human/data/问卷/体检报告20231216.xlsx")
q3[q3$姓名%in%q1$name,7]
q3[q3$姓名%in%q1$name,18]

#### 计算宏观衰弱指数 ####
f1 = q1[, c(1,2,3,5,6,9,10)]
f2_column = c(3,14,15,16,17,18,19,20,21,22,23,24)
f2 = q2[, f2_column]
#f_final = left_join(f1,f2, by = c("name" = "Q1_您的姓名"))

## 筛选一下没有体检报告的人
f1$name[!f1$name%in%q3$姓名]

# 缺少
c("金家贵", "吴辉")
## 进行了补充
f_final = left_join(f1[f1$name%in%q3$姓名,], q3, by = c("name" = "姓名"))
f_final[!f_final$name%in%f2$Q1_您的姓名,]
f_final = left_join(f_final[f_final$name%in%f2$Q1_您的姓名,], f2, by = c("name" = "Q1_您的姓名"))




## 计算衰弱指数的函数
calc_fi = function(.x){
  # 由于apply放进来的是向量，导致变量类型全部转成了字符，需要把他转成数字
  fi1 = as.numeric(.x[11])>=140|as.numeric(.x[12])>=90
  fi10 = .x[60]=="是"
  fi12 = as.numeric(.x[24])>=7
  fi15 = case_when(as.numeric(.x[16]) >28|as.numeric(.x[16])<18.5 ~ 1,
                   as.numeric(.x[16]) >24&as.numeric(.x[16])<=28 ~ 0.5,
                   as.numeric(.x[16]) >=18.5|as.numeric(.x[16])<=24 ~ 0)
  w_h = as.numeric(.x[4])/as.numeric(.x[5])
  fi16 = case_when(w_h >=0.95&.x[2]=="男" ~ 1,
                   w_h >=0.9&.x[2]=="女" ~ 1,
                   w_h >=0.9&.x[2]=="男" ~ 0.5,
                   w_h >=0.85&.x[2]=="女" ~ 0.5,
                   w_h < 0.9&.x[2]=="男" ~ 0,
                   w_h < 0.85&.x[2]=="女" ~ 0)
  fi17 = as.numeric(.x[10])<60|as.numeric(.x[10])>100
  fi18 = (as.numeric(.x[6])/as.numeric(.x[7]))<0.7
  fi19 = .x[52]=="是"
  fi20 = .x[53]=="是"
  fi21 = .x[54]=="是"
  fi22 = .x[55]=="是"
  fi23 = .x[56]=="是"
  fi24 = .x[57]=="是"
  fi25 = .x[58]=="是"
  fi26 = .x[59]=="是"
  fi27 = .x[61]=="是"
  fi28 = .x[62]=="是"
  final_score = sum(c(fi1, fi10, fi12,fi15,fi16,fi17,fi18,fi19,fi20,fi21,fi22,fi23,fi24,fi25,fi26,fi27,fi28),
                    na.rm = T)
  return(final_score/28)
}
f_final$FI = apply(f_final, 1,  calc_fi)
FI = tibble("name" = f_final$name, "frailty index" = f_final$FI)

# 部分人有疾病，之后需要进行添加对应值
# 简单进行年龄的相关性
cor.test(f_final$age, f_final$FI)
plot(f_final$age, f_final$FI)
ggplot(f_final, aes(x = age, y = FI))+
  geom_point()+
  geom_smooth(method = 'lm', se = F)+
  theme_bw()+
  annotate("text", x = 55, y = 0.18, label = "R = 0.035",
           color="black",size = 5, angle=0, fontface="bold")
  
#### 计算内在能力 ####
f1 = q1[, c(1,2,3,7,8,11,12)]
f2_column = c(3,23:60)
f2 = q2[, f2_column]
## 进行了补充
f_final = left_join(f1[f1$name%in%q3$姓名,], q3[,c(1,10)], by = c("name" = "姓名"))
f_final = left_join(f_final[f_final$name%in%f2$Q1_您的姓名,], f2, by = c("name" = "Q1_您的姓名"))
## 计算MNA
calc_mna = function(.x){
  # 由于apply放进来的是向量，导致变量类型全部转成了字符，需要把他转成数字
  ## MNA 问卷
  m1 =  case_when(.x[11]=="食欲无变化" ~ 2,
                   .x[11]=="中度食欲不佳" ~ 1,
                   .x[11]=="严重食欲不佳" ~ 0)
  m2 =  case_when(.x[12]=="体重减轻>3 kg" ~ 0,
                  .x[12]=="不知道" ~ 1,
                  .x[12]=="体重减轻1~3 kg" ~ 2,
                  .x[12]=="无变化" ~ 3)
  m3 =  case_when(.x[13]=="卧床或轮椅" ~ 0,
                  .x[13]=="可离开轮椅但无法自由走动" ~ 1,
                  .x[13]=="可以自由走动" ~ 2)
  m4 =  case_when(.x[14]=="是" ~ 0,
                  .x[14]=="否" ~ 2)
  m5 =  case_when(.x[15]=="严重痴呆或抑郁" ~ 0,
                  .x[15]=="轻度痴呆" ~ 1,
                  .x[15]=="无精神问题" ~ 2)
  m6 =  case_when(as.numeric(.x[8])<19 ~ 0,
                  as.numeric(.x[8])<21 ~ 1,
                  as.numeric(.x[8])<23 ~ 2,
                  TRUE ~ 3)
  m7 =  case_when(.x[16]=="是" ~ 0,
                  .x[16]=="否" ~ 1)
  m8 =  case_when(.x[17]=="是" ~ 0,
                  .x[17]=="否" ~ 1)
  m9 =  case_when(.x[18]=="是" ~ 0,
                  .x[18]=="否" ~ 1)
  m10 =  case_when(.x[19]=="一餐" ~ 0,
                  .x[19]=="两餐" ~ 1,
                  .x[19]=="三餐以上" ~ 2)
  m11 =  case_when(.x[20]=="是" ~ 0,
                  .x[20]=="否" ~ 1)
  
  m12 =  case_when(length(strsplit(.x[21], "|"))<=1 ~ 0,
                   length(strsplit(.x[21], "|"))==2 ~ 0.5,
                   length(strsplit(.x[21], "|"))==3 ~ 1,)
  m13 =  case_when(.x[22]=="少于3杯" ~ 0,
                   .x[22]=="3 ~ 5杯" ~ 0.5,
                   .x[22]=="大于5杯" ~ 1)
  m14 =  case_when(.x[23]=="无人协助则无法进食" ~ 0,
                   .x[23]=="可以自己进食但较吃力" ~ 1,
                   .x[23]=="可以自己进食" ~ 2)
  m15 =  case_when(.x[24]=="觉得自己营养非常不好" ~ 0,
                   .x[24]=="不太清楚或营养不太好" ~ 1,
                   .x[24]=="觉得自己没有营养问题" ~ 2)
  m16 =  case_when(.x[25]=="不如同龄人" ~ 0,
                   .x[25]=="不太清楚" ~ 0.5,
                   .x[25]=="和同龄人差不多" ~ 1,
                   .x[25]=="比同龄人好" ~ 2)
  m17 =  case_when(as.numeric(.x[4])<21 ~ 0,
                   as.numeric(.x[4])>=21&as.numeric(.x[4])<22 ~ 0.5,
                   as.numeric(.x[4])>=2 ~ 1)
  m18 =  case_when(as.numeric(.x[5])<31 ~ 0,
                   as.numeric(.x[5])>=31 ~ 1)
  MNA_score = sum(c(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12,m13,m14,m15,m16,m17,m18), na.rm = T)
  return(MNA_score)
}
## 计算CSDD
calc_csdd = function(.x){
  ## CSDD 问卷
  # 34 食欲 35 体重 37 入睡 38 睡眠惊醒 39 清晨 40 自杀 41 自我  
  CSDD_score = purrr::map(.x[c(26:33,36:37, 44)], function(.y){
    return(case_when(.y=="没有" ~ 0,
                     .y=="轻度或间歇" ~ 1,
                     .y=="明显或持续" ~ 2))
  })%>%unlist()%>%sum()
  CSDD_score = CSDD_score+ case_when(.x[34]=="食欲正常" ~ 0,
                                     .x[34]=="食欲不振，但仍能自己进食" ~ 1,
                                     .x[34]=="只有在他人催促和鼓励下才进食" ~ 2)
  CSDD_score = CSDD_score+ case_when(.x[35]=="没有" ~ 0,
                                     .x[35]=="超过3公斤" ~ 2)
  CSDD_score = CSDD_score+ case_when(.x[38]=="没有" ~ 0,
                                     .x[38]=="一周中只有几个晚上入睡困难" ~ 1,
                                     .x[38]=="每天都入睡困难" ~ 2)
  CSDD_score = CSDD_score+ case_when(.x[39]=="没有" ~ 0,
                                     .x[39]=="一周内只是几个晚上醒来" ~ 1,
                                     .x[39]=="每天都晚上醒来" ~ 2)
  CSDD_score = CSDD_score+ case_when(.x[40]=="没有" ~ 0,
                                     .x[40]=="醒来后又能继续睡觉" ~ 1,
                                     .x[40]=="醒来后无法继续入睡" ~ 2)
  CSDD_score = CSDD_score+ case_when(.x[41]=="没有" ~ 0,
                                     .x[41]=="只有被动的自杀念头，即觉得生活不值得过下去" ~ 1,
                                     .x[41]=="有主动的自杀愿望和任何最近的自杀企图和计划" ~ 2)
  CSDD_score = CSDD_score+ case_when(.x[42]=="没有" ~ 0,
                                     .x[42]=="轻度或间歇" ~ 1,
                                     .x[42]=="经常" ~ 2)
  CSDD_score = CSDD_score+ case_when(.x[43]=="没有" ~ 0,
                                     .x[43]=="感到悲观但能得到自己或他人安慰" ~ 1,
                                     .x[43]=="感到悲观并不能得到安慰" ~ 2)
  return(CSDD_score)
}

f_final$MNA = apply(f_final, 1,  calc_mna)
f_final$CSDD = apply(f_final, 1,  calc_csdd)

cor.test(f_final$age, f_final$MNA)
cor.test(f_final$age, f_final$CSDD)
cor.test(f_final$age, f_final$SPPB)
## 内在能力求和
ic_score = case_when(f_final$CSDD>=18 ~ 0,
          f_final$CSDD>=11&f_final$CSDD<=17 ~ 1,
          f_final$CSDD<=10 ~ 2)
ic_score = ic_score+case_when(f_final$MOCA>=27 ~ 2,
          f_final$MOCA>=10&f_final$MOCA<=26 ~ 1,
          f_final$MOCA<=9 ~ 0)
ic_score = ic_score+case_when(f_final$MNA>=24 ~ 2,
          f_final$MNA>=17&f_final$MNA<=23.5 ~ 1,
          f_final$MNA<17 ~ 0)
ic_score = ic_score+case_when(f_final$SPPB==10 ~ 0,
          f_final$SPPB==11 ~ 1,
          f_final$SPPB==12 ~ 2)
ic_score = ic_score+case_when(f_final$`Q57_视力情况（可以使用眼镜矫正）(普通近视不算)`=="正常或轻度视力损失（可以使用眼镜矫正）" ~ 1,
          f_final$`Q57_视力情况（可以使用眼镜矫正）(普通近视不算)`=="中度视力损失（无法使用眼镜矫正）" ~ 0.5,
          f_final$`Q57_视力情况（可以使用眼镜矫正）(普通近视不算)`=="完全或接近失明" ~ 0)
ic_score = ic_score+case_when(f_final$`Q58_听力状况`=="正常或轻度听力损失" ~ 1,
          f_final$`Q58_听力状况`=="中度听力损失" ~ 0.5,
          f_final$`Q58_听力状况`=="完全或接近失聪" ~ 0)

cor.test(f_final$age, ic_score)
f_final$IC = ic_score

kkk = f_final%>%dplyr::select(name,sex,age,SPPB,MNA,MOCA, CSDD, IC)
kkk$FI= FI$`frailty index`
kkk$`视力`= case_when(f_final$`Q57_视力情况（可以使用眼镜矫正）(普通近视不算)`=="正常或轻度视力损失（可以使用眼镜矫正）" ~ 1,
                    f_final$`Q57_视力情况（可以使用眼镜矫正）(普通近视不算)`=="中度视力损失（无法使用眼镜矫正）" ~ 0.5,
                    f_final$`Q57_视力情况（可以使用眼镜矫正）(普通近视不算)`=="完全或接近失明" ~ 0)
kkk$`听力`= case_when(f_final$`Q58_听力状况`=="正常或轻度听力损失" ~ 1,
                    f_final$`Q58_听力状况`=="中度听力损失" ~ 0.5,
                    f_final$`Q58_听力状况`=="完全或接近失聪" ~ 0)

readr::write_excel_csv(kkk, "/home/shimw/project/200Human/result/000.questionnaire.summary.csv")

ggplot(f_final, aes(x = age, y = IC))+
  geom_point()+
  geom_smooth(method = 'lm', se = F)+
  theme_bw()+
  annotate("text", x = 55, y = 6, label = "R = -0.042",
           color="black",size = 5, angle=0, fontface="bold")
IC = tibble("name" = f_final$name,
            "sex" = f_final$sex,
            "age" = f_final$age,
            "SPPB" = f_final$SPPB,
            "MOCA" = f_final$MOCA,
            "MNA" = f_final$MNA,
            "CSDD" = f_final$CSDD,
            "intrinsic capacity" = f_final$IC)

left_join(IC, FI)%>%
  write_excel_csv("/home/shimw/project/200Human/result/000.FI.and.IC.csv")


#### 重新挑选问题进行组合 ####
## 由于IC和FI和年龄相关性太弱了，这可能是由于他们的问题中很多是为老年人设置的
## 这会导致在统计分数时收到很多没有区分度的问题影响
## 因此决定在宏观上统合多个有区分度的问题进行组合
q3_file = "/home/shimw/project/200Human/问卷/体检报告20231216.xlsx"
q1 = readxl::read_xlsx("/home/shimw/project/200Human/问卷/体检问卷结果.xlsx")
# q2 = readxl::read_xlsx("/home/shimw/project/200Human/问卷/健康状况调查问卷_数据详情表_原始数据_202310292003.xlsx")%>% # 去重保留最后一次记录
#   group_by(`Q1_您的姓名`)  %>% slice( n() ) %>% tibble()  # 由于计算上的差距暂时不加入问卷了，下面涉及到问卷的代码报废
q3 = readxl::read_xlsx(q3_file)
q1$name[!q1$name%in%q3$姓名]
# [1] "金家贵" "彭彬"   "吴辉" 没有交体检报告
f_final = left_join(q1[q1$name%in%q3$姓名,], q3, by = c("name" = "姓名"))


#f_final = left_join(f_final[f_final$name%in%q2$Q1_您的姓名,], q2, by = c("name" = "Q1_您的姓名"))
## 

normalize_feature = function(.x){
  # 由于apply放进来的是向量，导致变量类型全部转成了字符，需要把他转成数字
  fi1 = as.numeric(as.numeric(.x[16])>=140|as.numeric(.x[17])>=90) # 高血压
  fi10 = as.numeric(.x[77]=="是") # 骨折
  fi12 = as.numeric(as.numeric(.x[29])>=7)
  fi15 = case_when(as.numeric(.x[21]) >28|as.numeric(.x[21])<18.5 ~ 1,
                   as.numeric(.x[21]) >24&as.numeric(.x[21])<=28 ~ 0.5,
                   as.numeric(.x[21]) >=18.5|as.numeric(.x[21])<=24 ~ 0)
  w_h = as.numeric(.x[7])/as.numeric(.x[8])
  fi16 = case_when(w_h >=0.95&.x[2]=="男" ~ 1,
                   w_h >=0.9&.x[2]=="女" ~ 1,
                   w_h >=0.9&.x[2]=="男" ~ 0.5,
                   w_h >=0.85&.x[2]=="女" ~ 0.5,
                   w_h < 0.9&.x[2]=="男" ~ 0,
                   w_h < 0.85&.x[2]=="女" ~ 0)
  fi17 = as.numeric(as.numeric(.x[15])<60|as.numeric(.x[15])>100)
  fi18 = as.numeric((as.numeric(.x[9])/as.numeric(.x[10]))<0.7)
  fi19 = as.numeric(.x[69]=="是")
  fi20 = as.numeric(.x[70]=="是")
  fi21 = as.numeric(.x[71]=="是")
  fi22 = as.numeric(.x[72]=="是")
  fi23 = as.numeric(.x[73]=="是")
  fi24 = as.numeric(.x[74]=="是")
  fi25 = as.numeric(.x[75]=="是")
  fi26 = as.numeric(.x[76]=="是")
  fi27 = as.numeric(.x[78]=="是")
  fi28 = as.numeric(.x[79]=="是")
  return(tibble::tibble("name" = .x[1], "sex" = .x[2], "age" = .x[3],
                        "FI1" = fi1, "FI10" = fi10, "FI12" = fi12, "FI15" = fi15, "FI16" = fi16, 
                        "FI17" = fi17, "FI18" = fi18, "FI19" = fi19, "FI20" = fi20, "FI21" = fi21, 
                        "FI22" = fi22, "FI23" = fi23, "FI24" = fi24, "FI25" = fi25, "FI26" = fi26, 
                        "FI27" = fi27, "FI28" = fi28))
}
FI1
FI15
FI20
FI24
mm = apply(f_final, 1,  normalize_feature)%>%bind_rows()
# FI10 全为0，FI12三个为1，FI16 159人为0，FI17 161为0，FI18 159为0，FI19 161为0，FI20 135为0
# FI21 165位0，FI22 全为0，FI23 170位0，FI25 167，FI26 161，FI27 173，FI28 174
mm$age = as.numeric(mm$age)
cor.test(mm$FI1,as.numeric(mm$age))
cor.test(mm$FI15,as.numeric(mm$age)) # BMI居然没有明显和年龄相关，这证明体重丧失只在年龄非常大的人中有问题
model <- lm(FI15 ~ age + sex, data = mm)# 反而是性别对BMI的影响比较大
model <- lm(`体重指数\r\n正常（18.5-23.9）` ~ age + sex, data = f_final)# 直接用值拟合反而不行
summary(model)
cor.test(mm$FI20,as.numeric(mm$age))
cor.test(mm$FI24,as.numeric(mm$age))# 只有这一个有显著性，是否每周会出门三天或三天以上
## 总结，宏观问卷的特征几乎不能用，能使用的只有FI24,mna和CSDD

#### 微观特征挑选 ####
## 构建异常检测的函数
outlier_check = function(.x, .l,.m, .n,.o, .p=0,.q=0){ 
  # .m最大值, .n最小值
  res = case_when(as.numeric(.x)>=.l&as.numeric(.x)<.m ~ 0,
                  as.numeric(.x)>=.m&as.numeric(.x)<.n ~ 0.5,
                  as.numeric(.x)>=.n&as.numeric(.x)<.o ~ 0.8,
                  as.numeric(.x)>=.o ~ 1,
                  as.numeric(.x)<.l&as.numeric(.x)>=.p ~ 0.5,
                  as.numeric(.x)<.p&as.numeric(.x)>=.q ~ 0.8,
                  as.numeric(.x)<.q ~ 1,)
  return(res)
}
## 首先计算本次的指标，之后再结合金域医学的结果
normalize_clinical_indicator = f_final%>%
  mutate(
    "heart_rate" = case_when(`血压脉搏\r\n次/分`>=60&`血压脉搏\r\n次/分`<=90 ~ 0,
                             TRUE ~ 1),
    "Blood pressure-systolic" = outlier_check(`血压收缩压\r\nmmHg\r\n正常(90-140)`,90,140,160,180,80,70),
    "Blood pressure-diastolic" = outlier_check(`血压舒张压\r\nmmHg\r\n正常(60-89)`,60,90,100,110,50,40),
    "postprandial blood glucose" = outlier_check(`餐后两小时血糖\r\nmmol/L\r\n正常(3.9-7.8)`,3.6,7.8,11.1,14.4,2.8,2.2),
    "BMI" = outlier_check(`体重指数\r\n正常（18.5-23.9）`,18.5,25,28,32),
    "Total cholesterol" = outlier_check(`总胆固醇\r\nmmol/L\r\n正常（0-5.2）`,0,5.2,7.8,10.4),
    "Triglycerides" = outlier_check(`甘油三脂\r\nmmol/L\r\n正常（<1.7）`,0,1.7,2.5,3.4),
    "LDL-C" = outlier_check(`低密度脂蛋白\r\nmmol/L\r\n正常（0-3.33）`,0,3.4,5.1,6.8),
    "HDL-C" = outlier_check(`高密度脂蛋白\r\nmmol/L\r\n正常（0.8-2.1）`,0.8,2.1,3.15,4.2,0.53,0.4),
    "TG/HDL" = outlier_check(`甘油三脂\r\nmmol/L\r\n正常（<1.7）`/`高密度脂蛋白\r\nmmol/L\r\n正常（0.8-2.1）`,0,1,3,8),
    "BUN" = outlier_check(`尿素氮（尿素）\r\nmmol/L\r\n正常（2.6-9.5）`,0,7.14,10.71,14.28),
    "Creatinine" = case_when(sex=="男"&`肌酐\r\numol/L\r\n正常（41-73）`>50&`肌酐\r\numol/L\r\n正常（41-73）`<=109 ~ 0,
                             sex=="女"&`肌酐\r\numol/L\r\n正常（41-73）`>40&`肌酐\r\numol/L\r\n正常（41-73）`<=97 ~ 0,
                             sex=="男"&`肌酐\r\numol/L\r\n正常（41-73）`>109&`肌酐\r\numol/L\r\n正常（41-73）`<=177 ~ 0.5,
                             sex=="女"&`肌酐\r\numol/L\r\n正常（41-73）`>97&`肌酐\r\numol/L\r\n正常（41-73）`<=177 ~ 0.5,
                             `肌酐\r\numol/L\r\n正常（41-73）`>177&`肌酐\r\numol/L\r\n正常（41-73）`<=442 ~ 0.8,
                             `肌酐\r\numol/L\r\n正常（41-73）`>442 ~ 1,
                             TRUE ~ 0.3),
    "Uric acid" =  outlier_check(`尿酸\r\numol/L\r\n正常（135-425）`,0,425,540,660),
    "Fasting Glucose" =  outlier_check(`空腹血糖\r\nmmol/L\r\n正常(3.6-6.1)`,3.6,5.6,7,8,2.8,2.2),
    "ALT" =  outlier_check(`谷丙转氨酶\r\nU/L\r\n正常(0-50)`,0,50,150,250),
    "AST" =  outlier_check(`谷草转氨酶\r\nU/L\r\n正常(0-40)`,0,40,120,200),
    "Total Bilirubin" =  outlier_check(`总胆红素\r\n正常(2-21)`,2,23,46,115),
    "Direct Bilirubin" =  case_when(`直接胆红素\r\numol/L\r\n正常（0-6.8）`>=0&`直接胆红素\r\numol/L\r\n正常（0-6.8）`<=6.8 ~ 0,
                                   TRUE ~ 1),
    "Indirect Bilirubin" =  case_when(`间接胆红素\r\numol/L\r\n正常（0-17）`>=0&`间接胆红素\r\numol/L\r\n正常（0-17）`<=17 ~ 0,
                                    TRUE ~ 1),
    "Total Protein" =  outlier_check(`总蛋白\r\ng/L\r\n正常（65-85）`,65,85,127,170,43,32),
    "Albumin" =  case_when(`白蛋白\r\ng/L\r\n正常（40-55）`>=45&`白蛋白\r\ng/L\r\n正常（40-55）`<=55 ~ 0,
                           `白蛋白\r\ng/L\r\n正常（40-55）`>=35&`白蛋白\r\ng/L\r\n正常（40-55）`<45 ~ 0.5,
                           `白蛋白\r\ng/L\r\n正常（40-55）`>=28&`白蛋白\r\ng/L\r\n正常（40-55）`<35 ~ 0.8,
                           `白蛋白\r\ng/L\r\n正常（40-55）`<28 ~ 1,
                           TRUE ~ 1),
    "Globulin" =  outlier_check(`球蛋白\r\ng/L\r\n正常（20-40）`,20,40,60,80,13,10),
    "A/G" =  case_when(`白球比\r\n正常（1.2-2.4）`>=1.5&`白球比\r\n正常（1.2-2.4）`<=2.4 ~ 0,
                       `白球比\r\n正常（1.2-2.4）`<1.5 ~ 0.8,
                       TRUE ~ 1),
    "ALP" =  outlier_check(`碱性磷酸酶\r\nU/L\r\n正常(35-135)`,35,135,375,625),
    "GGT" =  outlier_check(`r-谷氨酰转移酶\r\nU/L\r\n正常(7-60)`,10,60,180,300),
    "Leukocyte" =  case_when(`白细胞\r\n10^9/L\r\n正常（3.5-9.5）`>=3.5&`白细胞\r\n10^9/L\r\n正常（3.5-9.5）`<=9.5 ~ 0,
                             TRUE ~ 1),
    "RBCs" =  case_when(`红细胞\r\n10^12/L\r\n正常（3.8-5.8）`>=3.8&`红细胞\r\n10^12/L\r\n正常（3.8-5.8）`<=5.8 ~ 0,
                             TRUE ~ 1),
    
    "Hemoglobin" =  case_when(`血红蛋白\r\ng/L\r\n正常（115-175）`>=115&`血红蛋白\r\ng/L\r\n正常（115-175）`<=175 ~ 0,
                              TRUE ~ 1),
    "Platelets" =  outlier_check(`血小板\r\n10^9/L\r\n正常（125-350）`,150,450,700,900,100,75),
    "Hct" =  case_when(`红细胞压积\r\n%\r\n正常（35-50）`>=35&`红细胞压积\r\n%\r\n正常（35-50）`<=50 ~ 0,
                             TRUE ~ 1),
    "MCV" =  case_when(`红细胞平均体积\r\nfL\r\n正常（82-100）`>=82&`红细胞平均体积\r\nfL\r\n正常（82-100）`<=100 ~ 0,
                       TRUE ~ 1),
    "PCT" =  case_when(`血小板压积\r\n%\r\n正常（0.15-0.32）`>=0.15&`血小板压积\r\n%\r\n正常（0.15-0.32）`<=0.32 ~ 0,
                       TRUE ~ 1),
    "PDW" =  case_when(as.numeric(`血小板分布宽度\r\nfL\r\n正常（9-17）`)>=9&as.numeric(`血小板分布宽度\r\nfL\r\n正常（9-17）`)<=17 ~ 0,
                       TRUE ~ 1),
    "RDW_CV" =  case_when(`RDW-CV`>=11.6&`RDW-CV`<=14.6 ~ 0,TRUE ~ 1),
    "RDW" =  case_when(`红细胞分布宽度\r\nfL\r\n正常（37-50）`>=37&`红细胞分布宽度\r\nfL\r\n正常（37-50）`<=50 ~ 0,TRUE ~ 1),
    "MPV" =  case_when(as.numeric(`平均血小板体积\r\nfL\r\n正常（9-13）`)>=9&as.numeric(`平均血小板体积\r\nfL\r\n正常（9-13）`)<=13 ~ 0,
                       TRUE ~ 1),
    "Neutrophil" =  case_when(`中性粒细胞数\r\n10^9/L\r\n正常（1.8-6.3）`>=1.8&`中性粒细胞数\r\n10^9/L\r\n正常（1.8-6.3）`<=6.3 ~ 0,
                       TRUE ~ 1),
    "Lymphocytes" =  case_when(`淋巴细胞数\r\n10^9/L\r\n正常（1.1-3.2）`>=1.1&`淋巴细胞数\r\n10^9/L\r\n正常（1.1-3.2）`<=3.2 ~ 0,
                              TRUE ~ 1),
    "Eosinophil" =  case_when(`嗜酸性粒细胞\r\n10^9/L\r\n正常（0.02-0.52）`>=0.02&`嗜酸性粒细胞\r\n10^9/L\r\n正常（0.02-0.52）`<=0.52 ~ 0,
                               TRUE ~ 1),
    "Basophil" =  case_when(`嗜碱性粒细胞\r\n10^9/L\r\n正常（0-0.06）`>=0&`嗜碱性粒细胞\r\n10^9/L\r\n正常（0-0.06）`<=0.06 ~ 0,
                              TRUE ~ 1)
  )%>%
  dplyr::select(name,sex, age,heart_rate:Basophil)

readr::write_csv(normalize_clinical_indicator, "/home/shimw/project/200Human/体检报告/000.normalize_clinical_indicator.csv") # 写入文件
normalize_clinical_indicator = readr::read_csv("/home/shimw/project/200Human/体检报告/000.normalize_clinical_indicator.csv")

cor.test(normalize_clinical_indicator$age,rowSums(normalize_clinical_indicator[, 4:42]))


kk = data.frame(feature = names(colSums(normalize_clinical_indicator[, 4:42])),
           value = colSums(normalize_clinical_indicator[, 4:42]))

p1 = kk%>%
  ggplot(data=.,
         aes(feature,value))+
  scale_fill_npg()+
  geom_bar(stat="identity", color="black", width=0.7,size=0.25)+
  labs(x = "",y = "outlier number")+theme_classic()+coord_flip()




library(psych)
cor_results <- psych::corr.test(normalize_clinical_indicator[,c(-1,-2,-3)], 
                                y = normalize_clinical_indicator$age, method = "pearson")
M_cor_results <- psych::corr.test(normalize_clinical_indicator[normalize_clinical_indicator$sex=="男",c(-1,-2,-3)],
                                  y = normalize_clinical_indicator$age[normalize_clinical_indicator$sex=="男"], method = "pearson")
F_cor_results <- psych::corr.test(normalize_clinical_indicator[normalize_clinical_indicator$sex=="女",c(-1,-2,-3)],
                                  y = normalize_clinical_indicator$age[normalize_clinical_indicator$sex=="女"], method = "pearson")
result_df <- data.frame(
  Variable = colnames(normalize_clinical_indicator[,c(-1,-2,-3)]),
  Correlation = cor_results$r,
  P_Value = cor_results$p,
  M_Correlation = M_cor_results$r,
  M_P_Value = M_cor_results$p,
  F_Correlation = F_cor_results$r,
  F_P_Value = F_cor_results$p
)
result_df[result_df$P_Value<0.2,]
result_df[result_df$M_P_Value<0.05,]
result_df[result_df$F_P_Value<0.05,]
library(corrplot)
result_df[is.na(result_df)]=0
result_df%>%
  dplyr::select(2,4,6)%>%
  rename(All = Correlation, Male = M_Correlation, Female = F_Correlation)%>%
  as.matrix() ->env.cor
result_df%>%
  dplyr::select(3,5,7)%>%
  as.matrix() ->env.p
corrplot(corr =env.cor, is.corr = T, cl.ratio=2)

normalize_clinical_indicator%>%
  dplyr::select(-1,-2,-3)%>%
  apply(.,1,sum,na.rm=TRUE) ->fi
cor.test(normalize_clinical_indicator$age,fi)
normalize_clinical_indicator$FI = fi
ggplot(normalize_clinical_indicator, aes(x = age, y = FI))+
  geom_point()+
  geom_smooth(method = 'lm', se = F)+
  theme_bw()+
  annotate("text", x = 55, y = 10, label = "R = .127, p = 0.072",
           color="black",size = 5, angle=0, fontface="bold")

#### 无归一化写入文件 ####
f_final%>%
  mutate(
    "heart_rate" = `血压脉搏\r\n次/分`,
    "Blood pressure-systolic" = `血压收缩压\r\nmmHg\r\n正常(90-140)`,
    "Blood pressure-diastolic" = `血压舒张压\r\nmmHg\r\n正常(60-89)`,
    "postprandial blood glucose" = `餐后两小时血糖\r\nmmol/L\r\n正常(3.9-7.8)`,
    "BMI" = `体重指数\r\n正常（18.5-23.9）`,
    "Total cholesterol" = `总胆固醇\r\nmmol/L\r\n正常（0-5.2）`,
    "Triglycerides" = `甘油三脂\r\nmmol/L\r\n正常（<1.7）`,
    "LDL-C" = `低密度脂蛋白\r\nmmol/L\r\n正常（0-3.33）`,
    "HDL-C" = `高密度脂蛋白\r\nmmol/L\r\n正常（0.8-2.1）`,
    "TG/HDL" = `甘油三脂\r\nmmol/L\r\n正常（<1.7）`/`高密度脂蛋白\r\nmmol/L\r\n正常（0.8-2.1）`,
    "BUN" = `尿素氮（尿素）\r\nmmol/L\r\n正常（2.6-9.5）`,
    "Creatinine" = `肌酐\r\numol/L\r\n正常（41-73）`,
    "Uric acid" =  `尿酸\r\numol/L\r\n正常（135-425）`,
    "Fasting Glucose" =  `空腹血糖\r\nmmol/L\r\n正常(3.6-6.1)`,
    "ALT" =  `谷丙转氨酶\r\nU/L\r\n正常(0-50)`,
    "AST" =  `谷草转氨酶\r\nU/L\r\n正常(0-40)`,
    "Total Bilirubin" =  `总胆红素\r\n正常(2-21)`,
    "Direct Bilirubin" =  `直接胆红素\r\numol/L\r\n正常（0-6.8）`,
    "Indirect Bilirubin" =  `间接胆红素\r\numol/L\r\n正常（0-17）`,
    "Total Protein" =  `总蛋白\r\ng/L\r\n正常（65-85）`,
    "Albumin" =  `白蛋白\r\ng/L\r\n正常（40-55）`,
    "Globulin" =  `球蛋白\r\ng/L\r\n正常（20-40）`,
    "A/G" =  `白球比\r\n正常（1.2-2.4）`,
    "ALP" =  `碱性磷酸酶\r\nU/L\r\n正常(35-135)`,
    "GGT" =  `r-谷氨酰转移酶\r\nU/L\r\n正常(7-60)`,
    "Leukocyte" =  `白细胞\r\n10^9/L\r\n正常（3.5-9.5）`,
    "RBCs" =  `红细胞\r\n10^12/L\r\n正常（3.8-5.8）`,
    "Hemoglobin" =  `血红蛋白\r\ng/L\r\n正常（115-175）`,
    "Platelets" =  `血小板\r\n10^9/L\r\n正常（125-350）`,
    "Hct" =  `红细胞压积\r\n%\r\n正常（35-50）`,
    "MCV" =  `红细胞平均体积\r\nfL\r\n正常（82-100）`,
    "PCT" =  `血小板压积\r\n%\r\n正常（0.15-0.32）`,
    "PDW" =  as.numeric(`血小板分布宽度\r\nfL\r\n正常（9-17）`),
    "RDW_CV" =  `RDW-CV`,
    "RDW" =  `红细胞分布宽度\r\nfL\r\n正常（37-50）`,
    "MPV" =  as.numeric(`平均血小板体积\r\nfL\r\n正常（9-13）`),
    "Neutrophil" =  `中性粒细胞数\r\n10^9/L\r\n正常（1.8-6.3）`,
    "Lymphocytes" =  `淋巴细胞数\r\n10^9/L\r\n正常（1.1-3.2）`,
    "Eosinophil" =  `嗜酸性粒细胞\r\n10^9/L\r\n正常（0.02-0.52）`,
    "Basophil" =  `嗜碱性粒细胞\r\n10^9/L\r\n正常（0-0.06）`
  )%>%
  dplyr::select(name,sex, age,heart_rate:Basophil)%>%
  readr::write_csv(., "/home/shimw/project/200Human/体检报告/000.clinical_indicator.csv") # 写入未归一化的数据

clinical_indicator <- readr::read_csv("/home/shimw/project/200Human/体检报告/000.clinical_indicator.csv")%>%
  left_join(., meta_data[,1:2])%>%
  arrange(ID)


normal_range = list()


















#### 统计糖尿病前期人数 ####

q1 = readxl::read_xlsx("/home/shimw/project/200Human/问卷/体检问卷结果.xlsx")
q2 = readxl::read_xlsx("/home/shimw/project/200Human/问卷/健康状况调查问卷_数据详情表_原始数据_202310292003.xlsx")%>% # 去重保留最后一次记录
  group_by(`Q1_您的姓名`)  %>% slice( n() ) %>% tibble()
q3 = readxl::read_xlsx(q3_file)
q3[q3$姓名%in%q1$name,7]
q3[q3$姓名%in%q1$name,18]
#文章中关于糖尿病和糖尿病前期的划分是通过HbA1C（diabetic ≥ 6.5% > prediabetic ≥ 5.7%）
# 和fasting glucose (diabetic ≥ 7 mmol/L > prediabetic ≥ 5.6 mmol/L)
# 2h血糖 diabetic >=11 prediabetic >=7.8
safa = case_when(as.numeric(unlist(q3[q3$姓名%in%q1$name,7]))>=11|q3[q3$姓名%in%q1$name,18]>=7 ~ "diabetic",
                 as.numeric(unlist(q3[q3$姓名%in%q1$name,7]))>=7.8|q3[q3$姓名%in%q1$name,18]>=5.6 ~ "prediabetic",
         TRUE ~ "normal")


table(q1$sex)
ggplot(q1[q1$sex=="女", ], aes(age))+
  geom_histogram(color = "black",fill= "#a3cd5b",binwidth = 1)+
  ylab("人数")+xlab("年龄")+
  theme_bw()
table(q1[q1$sex=="女", ]$age)
#### 食物问卷 ####
#### 统计黑米人数 ####
food = readxl::read_xlsx("/home/shimw/project/200Human/问卷/食物频率调查问卷_数据详情表_原始数据_202311271836.xlsx")
food = food%>%dplyr::filter(`Q1_您的姓名`%in%q1$name)%>%
  group_by(`Q1_您的姓名`)  %>% slice( n() ) %>% tibble()

q1[!q1$name%in%food$Q1_您的姓名,]$name
table(food$Q177_您是否正在吃黑米)
food[food$Q177_您是否正在吃黑米=="是",]$Q1_您的姓名
table(q1$sex)
q1%>%
  dplyr::filter(sex=="女")%>%
  dplyr::filter(name%in%food[food$Q177_您是否正在吃黑米=="是",]$Q1_您的姓名)%>%
  dplyr::select(age)%>%table()


table(food$Q178_是否愿意接受黑米干预)
food[food$Q178_是否愿意接受黑米干预=="否",]$Q1_您的姓名

q3 = readxl::read_xlsx("/home/shimw/project/200Human/问卷/体检报告20231127.xlsx")%>%
  dplyr::filter(`姓名`%in%q1$name)

q3$`餐后两小时血糖\r\nmmol/L\r\n正常(3.9-7.8)`
q3$`空腹血糖\r\nmmol/L\r\n正常(3.6-6.1)`
q3$`总胆固醇\r\nmmol/L\r\n正常（0-5.2）`
q3$`低密度脂蛋白\r\nmmol/L\r\n正常（0-3.33）`
q3$`高密度脂蛋白\r\nmmol/L\r\n正常（0.8-2.1）`
q3$`血压收缩压\r\nmmHg\r\n正常(90-140)`
q3$`血压舒张压\r\nmmHg\r\n正常(60-89)`


table(q3$`餐后两小时血糖\r\nmmol/L\r\n正常(3.9-7.8)`<3.9)
table(q3$`空腹血糖\r\nmmol/L\r\n正常(3.6-6.1)`<3.6)
table(q3$`高密度脂蛋白\r\nmmol/L\r\n正常（0.8-2.1）`>2.1)
table(q3$`低密度脂蛋白\r\nmmol/L\r\n正常（0-3.33）`<3.33)
table(q3$`血压收缩压\r\nmmHg\r\n正常(90-140)`<90)
table(q3$`血压收缩压\r\nmmHg\r\n正常(90-140)`>=140)
table(q3$`血压舒张压\r\nmmHg\r\n正常(60-89)`<60)
table(q3$`血压舒张压\r\nmmHg\r\n正常(60-89)`>=90)

subject_desease = q3[,c("姓名","年龄","性别", "餐后两小时血糖\r\nmmol/L\r\n正常(3.9-7.8)",
                        "空腹血糖\r\nmmol/L\r\n正常(3.6-6.1)","总胆固醇\r\nmmol/L\r\n正常（0-5.2）", "高密度脂蛋白\r\nmmol/L\r\n正常（0.8-2.1）",
                        "低密度脂蛋白\r\nmmol/L\r\n正常（0-3.33）", "血压收缩压\r\nmmHg\r\n正常(90-140)",
                        "血压舒张压\r\nmmHg\r\n正常(60-89)"
                        )]%>%
  mutate("glucose" = if_else(`餐后两小时血糖\r\nmmol/L\r\n正常(3.9-7.8)`>7.8|`空腹血糖\r\nmmol/L\r\n正常(3.6-6.1)`>6.1, "高血糖","无"))%>%
  mutate("胆固醇" = if_else(`总胆固醇\r\nmmol/L\r\n正常（0-5.2）`>5.2, "高胆固醇","无"))%>%
  mutate("低密度脂蛋" = if_else(`低密度脂蛋白\r\nmmol/L\r\n正常（0-3.33）`>3.33, "高低密度脂蛋","无"))%>%
  mutate("血压" = if_else(`血压收缩压\r\nmmHg\r\n正常(90-140)`>140|`血压舒张压\r\nmmHg\r\n正常(60-89)`>89, "高血压","无"))

kkk = subject_desease%>%dplyr::filter(glucose=="高血糖"|`胆固醇`=="高胆固醇"|"低密度脂蛋"=="高低密度脂蛋"|`血压`=="高血压")
readr::write_excel_csv(kkk, "/home/shimw/project/200Human/问卷/三高统计.csv")




readr::read_csv("/home/shimw/project/200Human/result/000.questionnaire.summary.csv")%>%
  dplyr::select(name, sex, age,CSDD)%>%
  mutate("status" = case_when(
    CSDD >15 ~ "重度抑郁",
    CSDD >10 ~ "中度抑郁",
    CSDD >5 ~ "轻度抑郁",
    T ~ "正常"
  ))%>%
  readr::write_excel_csv("/home/shimw/project/200Human/result/000.questionnaire.CSDD.csv")



### 疾病状况问卷 ####
disease_status <- readxl::read_xlsx("/home/shimw/project/200Human/data/问卷/疾病情况调查问卷_数据详情表_编码数据_202404211113.xlsx")

bb = clinical_indicator[!clinical_indicator$name%in%disease_status$Q1,c(1:3)]
phone_number = readxl::read_excel("/home/shimw/project/200Human/data/问卷/治未病采血登记表.xlsx", skip = 2)

aa = phone_number[phone_number$姓名%in%clinical_indicator[!clinical_indicator$name%in%disease_status$Q1,]$name,]
paste(aa$联系方式, collapse= ",")

stringr::str_c(aa$联系方式)

### 计算SF分数
# https://www.doctor-network.com/Public/LittleTools/363.html
# 疼痛部分的问题少了，浙大的打分不对，我将分数进行了一定的对应
SF_summary <- disease_status%>%
  mutate(PF = (rowSums(across(Q35:Q44), na.rm=TRUE)-10)/20)%>%
  mutate(RP = (rowSums(across(Q45:Q48), na.rm=TRUE)-4)/4)%>%
  mutate(Q53_c = case_when(
    Q53 ==1 ~ 6,
    Q53 ==2 ~ 5.4,
    Q53 ==3 ~ 4.2,
    Q53 ==4 ~ 3.1,
    Q53 ==5 ~ 1
  ))%>%
  mutate(Q54_c = case_when(
    Q53==1&Q54==1 ~ 6,
    Q53==1&Q54==2 ~ 4.75,
    Q53==1&Q54==3 ~ 3.5,
    Q53==1&Q54==4 ~ 2.25,
    Q53==1&Q54==5 ~ 1,
    Q53!=1&Q54==1 ~ 5,
    Q53!=1&Q54==2 ~ 4,
    Q53!=1&Q54==3 ~ 3,
    Q53!=1&Q54==4 ~ 2,
    Q53!=1&Q54==5 ~ 1,
  ))%>%
  mutate(BP = (rowSums(across(c(Q53_c,Q54_c)), na.rm=TRUE)-2)/10)%>%
  mutate(GH = ((20-rowSums(across(c(Q33,Q34,Q66,Q68)), na.rm=TRUE))+(rowSums(across(c(Q65,Q67)), na.rm=TRUE))-5)/20
         )%>%
  mutate(VT = ((12-rowSums(across(c(Q55,Q59)), na.rm=TRUE))+(rowSums(across(c(Q61,Q63)), na.rm=TRUE))-4)/20
  )%>%
  mutate(SF = ((5-rowSums(across(c(Q52)), na.rm=TRUE))+(rowSums(across(c(Q64)), na.rm=TRUE))-2)/8
  )%>%
  mutate(RE = (rowSums(across(c(Q49,Q50,Q51)), na.rm=TRUE)-3)/3)%>%
  mutate(MH = ((12-rowSums(across(c(Q58,Q62)), na.rm=TRUE))+(rowSums(across(c(Q56,Q57,Q60)), na.rm=TRUE))-5)/25
  )%>%
  select(Q1,PF,RP,BP,GH,VT,SF,RE,MH)%>%
  mutate(PCS = (PF+RP+BP+GH)/4, MCS = (VT+SF+RE+MH)/4)%>%distinct()

readr::write_excel_csv(SF_summary, "/home/shimw/project/200Human/result/000.questionnaire.SF36.csv")
SF_summary <- readr::read_csv("/home/shimw/project/200Human/result/000.questionnaire.SF36.csv")%>%
  rename(name = Q1)
## 问卷与临床值的相关性
data = readr::read_csv("/home/shimw/project/200Human/问卷/000.all_clinical_indicator.csv")
res <- readr::read_csv("/home/shimw/project/200Human/体检报告/all_clinical.csv")
FI_lab <- res%>%
  dplyr::mutate(score = if_else(res=="normal", 1, 0))%>%
  group_by(name,sex)%>%
  summarise(FI = sum(score))%>%
  dplyr::mutate(FI = if_else(sex=="女", FI/120, FI/119))
FI_lab = readr::read_csv("/home/shimw/project/200Human/问卷/000.all_clinical_indicator.csv")%>%
  dplyr::select(name, age)%>%
  left_join(FI_lab, .)
FI_SF_summary <- left_join(SF_summary, FI_lab)
cor.test(FI_SF_summary$PCS, FI_SF_summary$age)
cor.test(FI_SF_summary$MCS, FI_SF_summary$age)

ggplot(FI_SF_summary, aes(x = age, y = PCS))+
  geom_point(col="#546de5",fill="#546de5", shape=16,alpha=0.6)+
  geom_smooth(method = 'lm', se = F, color = "black",  linewidth = 0.7)+
  theme_bw() +
  annotate("text", x = 60, y = 1, label = "R = 0.024, P = 0.799",
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
  labs(title = "PCS",x = '年龄', y = 'PCS')

library(showtext) 
showtext_auto(enable = TRUE) 
ggsave("/home/shimw/project/200Human/result/000.questionnaire.SF36_PCS_age.pdf", width = 4.8, height = 4.3)

ggplot(FI_SF_summary, aes(x = age, y = MCS))+
  geom_point(col="#546de5",fill="#546de5", shape=16,alpha=0.6)+
  geom_smooth(method = 'lm', se = F, color = "black",  linewidth = 0.7)+
  theme_bw() +
  annotate("text", x = 60, y = 1, label = "R = 0.238, P = 0.01",
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
  labs(title = "MCS",x = '年龄', y = 'MCS')

library(showtext) 
showtext_auto (enable = TRUE) 
ggsave("/home/shimw/project/200Human/result/000.questionnaire.SF36_MCS_age.pdf", width = 4.8, height = 4.3)


clinical_SF_summary <- left_join(SF_summary, data)%>%
  select(name, where(is.numeric))
library(psych)
cor.result <- corr.test(clinical_SF_summary[,c("PCS", "MCS")],
                        clinical_SF_summary[,-c(1:12)],method = "spearman", adjust = "BH") 

cor.result$p.adj
cor.result.p <- reshape2::melt(cor.result$p, value.name = "pvalue")
cor.result.r <- reshape2::melt(cor.result$r, value.name = "rho")

cor.result_PCS <- left_join(cor.result.p,cor.result.r)%>%
  filter(Var1=="PCS")

library(ggrepel)
ggplot(cor.result_PCS, aes(x = rho, y = -log10(pvalue))) +
  geom_point(aes(color = pvalue < 0.05), alpha = 0.5) + # 根据 p 值是否小于 0.05 来设置点的颜色
  geom_vline(xintercept = 0, linetype = "dashed") + 
  geom_hline(yintercept = -log10(0.05), linetype = "dashed") + # 添加 p 值小于 0.05 的虚线
  labs(x = "rho", y = "-log10(p value)") + # 添加标签
  theme_minimal() + # 使用最小主题
  scale_color_manual(values = c("grey", "red"), guide = FALSE) + # 设置颜色
  geom_text_repel(data = subset(cor.result_PCS, pvalue < 0.05), aes(label = Var2), 
                  color = "red", size = 3, show.legend = FALSE) + # 标记 p 值低于 0.05 的代谢物名称
  labs(title = "PCS")+
  theme(plot.title = element_text(hjust = 0.5))


cor.result_MCS <- left_join(cor.result.p,cor.result.r)%>%
  filter(Var1=="MCS")
ggplot(cor.result_MCS, aes(x = rho, y = -log10(pvalue))) +
  geom_point(aes(color = pvalue < 0.05), alpha = 0.5) + # 根据 p 值是否小于 0.05 来设置点的颜色
  geom_vline(xintercept = 0, linetype = "dashed") + 
  geom_hline(yintercept = -log10(0.05), linetype = "dashed") + # 添加 p 值小于 0.05 的虚线
  labs(x = "rho", y = "-log10(p value)") + # 添加标签
  theme_minimal() + # 使用最小主题
  scale_color_manual(values = c("grey", "red"), guide = FALSE) + # 设置颜色
  geom_text_repel(data = subset(cor.result_MCS, pvalue < 0.05), aes(label = Var2), 
                  color = "red", size = 3, show.legend = FALSE) + # 标记 p 值低于 0.05 的代谢物名称
  labs(title = "MCS")+
  theme(plot.title = element_text(hjust = 0.5))

### 计算PSQI分数 ####
### 有的老师时间是按照12小时制填的，需要手动进行修改
library(hms)
PSQI = disease_status%>%
  mutate(Q70 = case_when(
    Q70<=15 ~ 0,
    Q70<=30 ~ 1,
    Q70<=60 ~ 2,
    T ~ 3
  ))%>%
  mutate(Q69 = parse_hm(paste(`Q69|1|open`, `Q69|2|open`, sep = ":")))%>%
  mutate(Q71 = parse_hm(paste(`Q71|1|open`, `Q71|2|open`, sep = ":")) +lubridate::dhours(24))%>%
  mutate(Q69 = if_else(Q69>parse_hm("6:00")&Q69<parse_hm("16:00"), Q69+lubridate::dhours(12), Q69))%>%
  mutate(Q69 = if_else(Q69>=parse_hm("0:00")&Q69<parse_hm("3:00"), Q69+lubridate::dhours(24), Q69))%>%
  mutate(sleep_rate = as.numeric(Q72)*3600/as.numeric(Q71-Q69))%>%
  mutate(sleep_rate = case_when(
    sleep_rate>=0.85 ~ 0,
    sleep_rate<0.85&sleep_rate>=0.75 ~ 1,
    sleep_rate<0.75&sleep_rate>=0.65 ~ 1,
    sleep_rate<0.65 ~ 3
  ))%>%
  mutate(PSQI = rowSums(across(Q73:Q85), na.rm=TRUE) - 13 + sleep_rate + Q70)%>%  # 13个问题，全部需要-1
  select(Q1,PSQI)%>%
  mutate("睡眠质量" = case_when(
    PSQI<=5 ~ "睡眠质量很好",
    PSQI>5&PSQI<=10 ~ "睡眠质量还行",
    PSQI>10&PSQI<=15 ~ "睡眠质量一般",
    PSQI>15 ~ "睡眠质量很差",
  ))%>%distinct()

readr::write_excel_csv(PSQI, "/home/shimw/project/200Human/result/000.questionnaire.PSQI.csv")



difftime(kk$Q69, kk$Q71, units='mins')
