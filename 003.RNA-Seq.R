library(tidyverse)
library(ggplot2)
library(factoextra)

TPM = readr::read_tsv("/NAS/shimw/200Human/RNA-Seq/RNAseq.rawtpm")
pca_data <- TPM %>% 
  dplyr::select(-gene_symbol)%>%
  # make the "gene" column become the rownames of the table
  column_to_rownames("gene_ID") %>% 
  # coerce to a matrix
  as.matrix() %>% 
  # transpose the matrix so that rows = samples and columns = variables
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
rownames(pca_result)[which(!get_ellipses_outside(p))]
# 3,15,19,39,73,78,79,81,115,128
dim(TPM)
# 使用聚类筛选
TPM_df = TPM %>% 
  dplyr::select(-gene_symbol)%>%
  # make the "gene" column become the rownames of the table
  column_to_rownames("gene_ID")
sampleTree <- hclust(dist(t(TPM_df)), method = "average")
plot(sampleTree, cex.lab = 1.5, cex.axis = 1.5, cex.main = 2)


cor_matrix <- cor(TPM_df)
pheatmap::pheatmap(cor_matrix, 
                   main = "Metabolite Correlation Heatmap",
                   clustering_method = "ward.D") 
# 从热图中观察到115,15,79,81,3,73,128,19,78属于离群值

mean_exp <- apply(TPM_df, 1, mean)

correlation <- apply(TPM_df, 2, function(x) cor(x, mean_exp))
correlation[correlation<0.85]

correlation["wholeblood_39B"]

min(cor_matrix)


quality = readxl::read_xlsx("/home/shimw/project/200Human/RNA-Seq/RNA样本提取评级情况.xlsx")%>%
  mutate("aa" = str_c(`样本名称`,"B",sep = ""))



c(3,15,19,39,73,78,79,81,115,128)

names(TPM)[3:136][stringr::str_split_fixed(names(TPM)[3:136],"_",2)[,2]%in%quality$aa[quality$评级=="C"]]


quality = readxl::read_xlsx("/home/shimw/project/200Human/RNA-Seq/RNA样本提取评级情况.xlsx")
quality[quality$样本名称%in%c(3,15,19,39,73,78,79,81,115,128),]

###  预测年龄
library(RNAAgeCalc)
library(tidyverse)
rawcount = readr::read_tsv("/NAS/shimw/200Human/RNA-Seq/RNAseq.rawcount")%>%
  select(-gene_symbol)%>%
  mutate(gene_ID = stringr::str_split_fixed(gene_ID, "\\.", 2)[,1])%>%
  column_to_rownames("gene_ID")


q1 <- readxl::read_xlsx("/home/shimw/project/200Human/问卷/体检问卷结果.xlsx")%>%
  dplyr::select(name,age) # 加入年龄
meta_data <- readxl::read_xlsx("/home/shimw/project/200Human/问卷/治未病采血登记表.xlsx", skip = 2)%>%
  dplyr::select("编号", "姓名",   "性别")%>%
  dplyr::rename(ID="编号", name="姓名", sex="性别")%>%
  dplyr::left_join(q1)%>% # 合并年龄
  dplyr::mutate(sampleid = paste("wholeblood_", ID, "B", sep = ""))

chronage = data.frame(sampleid = colnames(rawcount), age = meta_data$age[match(colnames(rawcount), meta_data$sampleid)])



res = predict_age(exprdata = rawcount, tissue = "blood", exprtype = "counts", idtype= "ENSEMBL",
                  chronage = chronage)

res$RNAAge
makeplot(res)
cor.test(res$RNAAge,res$ChronAge)
### 基本不能使用，相关性太差了