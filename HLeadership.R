library(dplyr)
library(showtext)
library(stringr)
library(ggplot2)
library(psych)
library(ggraph)
library(factoextra)
library(textclean)

install.packages("extrafont")


setwd("~/Desktop/HR/data")

df <- read.csv("HLeadership_preprocessing.csv", fileEncoding = "CP949")

alpha1 <- psych::alpha(df[, 1:9])
summary(alpha1)

alpha2 <- psych::alpha(df[, 10:18])
summary(alpha2)

alpha3 <- psych::alpha(df[, 19:27])
summary(alpha3)

alpha4 <- psych::alpha(df[, 28:36])
summary(alpha4)

alpha_all <- psych::alpha(df[, 1:36])
summary(alpha_all)





fa.parallel(df[, 1:36], fa = "fa", n.iter = 100, show.legend = TRUE, main = "The Number of Factors")

efa_result <- fa(df[, 1:36], nfactors = 2, rotate = 'oblimin', fm = 'ml')
fa.diagram(efa_result, cut = 0.3, main = "EFA 2-Factors Solution Diagram")

efa_result <- fa(df[, 1:36], nfactors = 1, rotate = 'oblimin', fm = 'ml')
fa.diagram(efa_result, cut = 0.3, main = "EFA 1-Factors Solution Diagram")









factor_scores <- efa_result$scores
df$Factor1 <- factor_scores
factor_df <- data.frame(Factor1 = df$Factor1)
fviz_nbclust(factor_df, kmeans, method = "wss", k.max = 7) + 
  labs(title = "Elbow Method for Optimal Clusters")

fviz_nbclust(factor_df, kmeans, method = "silhouette", k.max = 7) +
  labs(title = "Silhouette Method for Optimal Clusters")

set.seed(123)
km_result <- kmeans(df$Factor1, centers = 3)
df$cluster <- as.factor(km_result$cluster)

fviz_cluster(list(data = df[, 1:36], cluster = df$cluster),
             geom = "point",
             ellipse.type = "convex",
             palette = c("#F8766D", "#00BA38", "#619CFF"),
             main = "K-means Clustering with K = 3")

ggplot(df, aes(x = cluster, y = Factor1, fill = cluster)) +
  geom_boxplot() +
  labs(title = "Factor Score Distribution by Cluster") +
  theme_minimal()

df_summary <- df %>%
  group_by(cluster) %>%
  summarise(
    mean_factor = mean(Factor1),
    sd_factor = sd(Factor1),
    n = n()
  )
print(df_summary)
      
 


df_lead_style <- df |> 
  select(cluster, Q37:Q44) |> 
  mutate(
    관계지향형리더 = rowMeans(across(c(Q37, Q38)), na.rm = TRUE),
    혁신지향리더   = rowMeans(across(c(Q39, Q40)), na.rm = TRUE),
    관리지향리더   = rowMeans(across(c(Q41, Q42)), na.rm = TRUE),
    성과지향리더   = rowMeans(across(c(Q43, Q44)), na.rm = TRUE)
  ) |> 
  select(cluster, 관계지향형리더, 혁신지향리더, 관리지향리더, 성과지향리더)
df_lead_avg <- df_lead_style |> 
  group_by(cluster) |> 
  summarise(across(everything(), mean, na.rm = TRUE))

df_lead_avg


df_long <- df_lead_style |> 
  pivot_longer(cols = -cluster, names_to = "리더십유형", values_to = "점수")

ggplot(df_long, aes(x = 리더십유형, y = 점수, fill = as.factor(cluster))) +
  stat_summary(fun = mean, geom = "bar", position = "dodge") +
  labs(title = "클러스터별 리더십스타일 평가",
       x = "리더십 스타일", y = "평균 점수", fill = "Cluster") +
  theme_minimal()






     




df |> 
  mutate(Q45.sol = case_when(Q45 == 1~"열정조직통합형",
                             Q45 == 2~"창의형",
                             Q45 == 3~"도전추진형",
                             Q45 == 4~"지식형",
                             Q45 == 5~"근면형",
                             Q45 == 6~"순종형",
                             TRUE~"NA")) |> 
  select(Q45.sol, Q46, Q47, cluster) -> QA
QA <- QA[3:660,]

write.csv(QA, file = "cluster3.csv", fileEncoding = "CP949")

cluster1 <- QA |> filter(cluster == 1) 
cluster2 <- QA |> filter(cluster == 2)
cluster3 <- QA |> filter(cluster == 3)

cluster1 |> 
  group_by(Q45.sol) |> 
  tally() |> 
  ggplot(aes(x=as.factor(Q45.sol), y=n, fill=as.factor(Q45.sol))) +
  geom_col(show.legend = F)+
  ggtitle("Cluster 1 Q45 응답 빈도")

cluster2 |> 
  group_by(Q45.sol) |> 
  tally() |> 
  ggplot(aes(x=as.factor(Q45.sol), y=n, fill=as.factor(Q45.sol))) +
  geom_col(show.legend = F)+
  ggtitle("Cluster 2 Q45 응답 빈도")

cluster3 |> 
  group_by(Q45.sol) |> 
  tally() |> 
  ggplot(aes(x=as.factor(Q45.sol), y=n, fill=as.factor(Q45.sol))) +
  geom_col(show.legend = F)+
  ggtitle("Cluster 3 Q45 응답 빈도")

























