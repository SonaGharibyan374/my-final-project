# Տվյալների ներբեռնում GitHub Gist-ից
df <- read.csv("https://gist.githubusercontent.com/pravalliyaram/5c05f43d2351249927b8a3f3cc3e5ecf/raw/Mall_Customers.csv")

# Առաջին տողերը ստուգում ենք
head(df)
# Տողերի և սյունակների քանակը
dim(df)

# Սյունակների տիպերը
str(df)

# Վիճակագրական ամփոփ
summary(df)

# Ստուգում ենք բացակայող արժեքների առկայությունը
colSums(is.na(df))
# Gender սյունը թվայնացնել՝ Male → 0, Female → 1
df$Gender <- ifelse(df$Gender == "Male", 0, 1)

# Ընտրում ենք clustering-ի համար անհրաժեշտ սյուները
X <- df[, c("Annual.Income..k..", "Spending.Score..1.100.")]
# Համարժեք արդյունքների համար ֆիքսում ենք seed
set.seed(42)

# Կատարում ենք կլաստերավորում՝ 5 կլաստերով
kmeans_model <- kmeans(X, centers = 5)

# Ավելացնում ենք կլաստերների պիտակները DataFrame-ում
df$Cluster <- as.factor(kmeans_model$cluster)

# Տպում ենք կլաստերի կենտրոնները
kmeans_model$centers
install.packages("ggplot2")
library(ggplot2)
ggplot(df, aes(x = Annual.Income..k..,
               y = Spending.Score..1.100.,
               color = Cluster)) +
  geom_point(size = 3, alpha = 0.8) +
  labs(title = "KMeans Clustering of Mall Customers",
       x = "Annual Income (k$)",
       y = "Spending Score (1–100)") +
  theme_minimal()
aggregate(df[, c("Annual.Income..k..", "Spending.Score..1.100.")],
          by = list(Cluster = df$Cluster),
          FUN = mean)


