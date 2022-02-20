setwd("C:/Users/User/cs102/homework07/r")
adult <- read.csv("adult_data.csv", header=T)

head(adult)

sapply(adult, function(x) sum(is.na(x)))

# 1. ������� ������ � ������ (������� sex) ������������ � ���� ������ ������?
table(adult$sex)

# 2. ����� ������� ������� (������� age) ������?
mean(subset(adult, sex == "Female")$age)

# 3. ������ ���� ������� �������� (������� native-country)?
sum(adult$native.country == "Germany") / nrow(adult)

# 4-5. ������ ������� �������� � ������������������ ���������� �������� ���, 
# ��� �������� ����� 50K � ��� (������� salary) � ���, ��� �������� ����� 50K � ���?

rich <- subset(adult, salary == ">50K")
poor <- subset(adult, salary == "<=50K")
mean(rich$age)
sd(rich$age)
mean(poor$age)
sd(poor$age)

# 6. ������ ��, ��� ����, ������� �������� ������ 50k, ����� ��� ������� ������ �����������? 
# (������� education � Bachelors, Prof-school, Assoc-acdm, Assoc-voc, Masters ��� Doctorate)

table(rich$education)

# 7. �������� ���������� �������� ��� ������ ���� (������� race) � ������� ����.
# ������� ����� ������� ������������ ������� ������ ���� Amer-Indian-Eskimo.

for(i in unique(adult$race)) {
  print(i)
  for(j in unique(adult$sex)) {
    print(j)
    print(summary(subset(adult, race == i & sex == j)$age))
  }
}

max(subset(adult, race == "Amer-Indian-Eskimo" & sex == "Male")$age)
# 8. ������ ��, ��� ����, ������� �������� ������ 50k, ����� ��� ������� ������ �����������? 
# (������� education � Bachelors, Prof-school, Assoc-acdm, Assoc-voc, Masters ��� Doctorate)

mmen <- subset(adult, sex == "Male" & (substr(marital.status, 1, 7) == "Married"))
sum(mmen$salary == ">50K") / nrow(mmen)
bmen <- subset(adult, sex == "Male" & (substr(marital.status, 1, 7) != "Married"))
sum(bmen$salary == ">50K") / nrow(bmen)

# 9.����� ������������ ����� ����� ������� �������� � ������ (������� hours-per-week)?
# ������� ����� �������� ����� ���������� ����� � ����� ����� ��� ������� �������������� �����?

max(adult$hours.per.week)
workers <- subset(adult, hours.per.week == max(adult$hours.per.week))
nrow(workers)
sum(workers$salary == ">50K") / nrow(workers) * 100

# 10. ���������� ������� ����� ������ (hours-per-week) �������������� ���� � ����� 
# (salary) ��� ������ ������ (native-country).

for(i in unique(adult$native.country)) {
  print(i)
  print(mean(subset(poor, native.country == i)$hours.per.week))
  print(mean(subset(rich, native.country == i)$hours.per.week))
}
