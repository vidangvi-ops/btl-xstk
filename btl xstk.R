# BTL Xac suat thong ke - Bo du lieu cuong do nen be tong

#3. Tien xu ly so lieu

# Doc du lieu
concrete_data <- read.csv("concrete.csv")
dim(concrete_data)
head(concrete_data, 10)

# Kiem tra dinh dang du lieu
str(concrete_data)

# Kiem tra du lieu khuyet
colSums(is.na(concrete_data))

# Kiem tra dong trung lap
sum(duplicated(concrete_data))
df <- unique(concrete_data)
dim(df)

# Kiem tra gia tri hop le
colSums(concrete_data < 0)

#4. Thong ke mo ta

# Bang thong ke mo ta cac bien
summary(df)
bang1 <- data.frame(
  Mean   = sapply(df, mean),
  Median = sapply(df, median),
  SD     = sapply(df, sd),
  Min    = sapply(df, min),
  Max    = sapply(df, max)
)
round(bang1, 2)

# Histogram cua strength
hist(df$strength, breaks = 30, col = "steelblue", border = "white",
     main = "Histogram của cường độ chịu nén (strength)",
     xlab = "strength (MPa)", ylab = "Tần số")
abline(v = mean(df$strength),   col = "red",       lwd = 2)
abline(v = median(df$strength), col = "darkgreen", lwd = 2, lty = 2)
legend("topright", legend = c("Mean", "Median"),
       col = c("red", "darkgreen"), lwd = 2, lty = c(1, 2))

# Boxplot cua cac bien
par(mfrow = c(3, 3), oma = c(0, 0, 3, 0))
for (v in names(df)) {
  boxplot(df[[v]], main = v, col = "orange", outcol = "red")
}
mtext("Boxplot của các biến trong bộ dữ liệu", outer = TRUE, cex = 1.2, font = 2)
par(mfrow = c(1, 1), oma = c(0, 0, 0, 0))

# Do thi phan tan cua strength theo tung bien
don_vi <- c(cement = "kg/m³", slag = "kg/m³", ash = "kg/m³", water = "kg/m³",
            superplastic = "kg/m³", coarseagg = "kg/m³", fineagg = "kg/m³",
            age = "ngày")
nhom_bien <- list(c("cement", "slag", "ash", "water"),
                  c("superplastic", "coarseagg", "fineagg", "age"))
for (nhom in nhom_bien) {
  par(mfrow = c(2, 2))
  for (k in seq_along(nhom)) {
    v <- nhom[k]
    plot(df[[v]], df$strength, pch = 16, col = rgb(0, 0, 1, 0.3),
         main = paste0("(", letters[k], ") ", v),
         xlab = paste0(v, " (", don_vi[v], ")"), ylab = "strength (MPa)")
    abline(lm(df$strength ~ df[[v]]), col = "red", lwd = 2)
  }
  par(mfrow = c(1, 1))
}

# He so tuong quan giua strength va tung bien
round(sapply(names(don_vi), function(v) cor(df[[v]], df$strength)), 3)

# Ma tran tuong quan
library(corrplot)
corrplot(cor(df), method = "color", type = "upper",
         addCoef.col = "black", tl.col = "black", number.cex = 0.7)

#5.1 Hoi quy tuyen tinh boi

# Bien doi tuoi sang logarit
df$log_age <- log(df$age)

# Xay dung mo hinh va ket qua
mo_hinh <- lm(strength ~ cement + slag + ash + water + superplastic +
                coarseagg + fineagg + log_age, data = df)
summary(mo_hinh)

# Khoang tin cay 95% cho cac he so
confint(mo_hinh)

# So sanh muc do anh huong (he so chuan hoa)
bien_x <- c("cement", "slag", "ash", "water", "superplastic",
            "coarseagg", "fineagg", "log_age")
df_chuan <- as.data.frame(scale(df[, c("strength", bien_x)]))
m_chuan  <- lm(strength ~ ., data = df_chuan)
he_so_chuan <- sort(abs(coef(m_chuan)[-1]), decreasing = TRUE)
round(he_so_chuan, 3)
barplot(he_so_chuan, las = 2, col = "steelblue",
        main = "Muc do anh huong (he so chuan hoa, tri tuyet doi)",
        ylab = "|He so chuan hoa|")

# Kiem tra da cong tuyen (VIF)
for (v in bien_x) {
  r2 <- summary(lm(as.formula(paste(v, "~ .")), data = df[, bien_x]))$r.squared
  cat(v, ": VIF =", round(1 / (1 - r2), 2), "\n")
}

# Kiem tra phan du
par(mfrow = c(2, 2))
plot(mo_hinh, caption = list("(a) Residuals vs Fitted", "(b) Q-Q Residuals",
                            "(c) Scale-Location", "Cook's distance",
                            "(d) Residuals vs Leverage", "Cook's dist vs Leverage"))
par(mfrow = c(1, 1))
shapiro.test(residuals(mo_hinh))

# Tac dong khi tang mot luong cu the cua tung thanh phan
he_so <- coef(mo_hinh)
muc_tang <- c(cement = 10, slag = 10, ash = 10, water = 10,
              superplastic = 1, coarseagg = 10, fineagg = 10)
tac_dong <- he_so[names(muc_tang)] * muc_tang
data.frame(Bien = names(muc_tang), Muc_tang = muc_tang,
           Tac_dong_MPa = round(tac_dong, 3), row.names = NULL)

# Tac dong cua tuoi be tong
he_so["log_age"] * log(2)     # tuoi gap doi
he_so["log_age"] * log(4)     # tuoi gap 4 lan (vd 7 -> 28 ngay)

# Kiem chung bang ham predict()
co_so <- data.frame(cement = 300, slag = 100, ash = 50, water = 180,
                    superplastic = 6, coarseagg = 970, fineagg = 800,
                    log_age = log(28))     # cap phoi co so, tuoi 28 ngay
tang_xm <- co_so
tang_xm$cement <- tang_xm$cement + 10      # tang them 10 kg xi mang
predict(mo_hinh, co_so)
predict(mo_hinh, tang_xm)
predict(mo_hinh, tang_xm) - predict(mo_hinh, co_so)

#5.2 Phan tich phuong sai mot nhan to va kiem dinh Tukey HSD

# Chon 4 moc tuoi 3, 7, 28, 90 ngay
library(car)
concrete_filtered <- subset(df, age %in% c(3, 7, 28, 90))
concrete_filtered$age <- as.factor(concrete_filtered$age)
table(concrete_filtered$age)

# Boxplot cuong do theo ngay tuoi
boxplot(strength ~ age, data = concrete_filtered,
        main = "Cường độ nén theo ngày tuổi", xlab = "Ngày tuổi", ylab = "Cường độ (MPa)")

# Kiem tra phan phoi chuan: Q-Q plot va Shapiro-Wilk cho tung nhom
moc_tuoi <- c(3, 7, 28, 90)
par(mfrow = c(2, 2))
for (k in seq_along(moc_tuoi)) {
  group_data <- subset(concrete_filtered, age == moc_tuoi[k])$strength
  qqnorm(group_data, main = paste0("(", letters[k], ") Nhóm ", moc_tuoi[k], " ngày"))
  qqline(group_data)
  print(shapiro.test(group_data))
}
par(mfrow = c(1, 1))

# Kiem dinh phuong sai dong nhat (Levene)
leveneTest(strength ~ age, data = concrete_filtered, center = median)

# ANOVA mot nhan to
ANOVA_model <- aov(strength ~ age, data = concrete_filtered)
summary(ANOVA_model)

# So sanh boi Tukey HSD
TukeyHSD(ANOVA_model)
plot(TukeyHSD(ANOVA_model))
