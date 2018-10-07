
data<-read.csv("insurance.csv")

# male female ratio
print(names(data))
male1=nrow(subset(data,smoker=="yes",sex=="male"))
female1=nrow(subset(data,smoker=="yes",sex=="female"))
ratio1=male1/female1
#print("Ratio of male to female smoker overall= " + ratio1)


# male female ratio southwest
print(names(data))
male2=nrow(subset(data,smoker=="yes",sex=="male",region=="southwest"))
female2=nrow(subset(data,smoker=="yes",sex=="female",region=="southwest"))
ratio2=male2/female2
#print("Ratio of male to female smoker southwest= "+ ratio2)


#max age male
print(subset(data,sex=="male" & age==max(age)))

#female with max childern
print(subset(data,sex=="female" & children==max(children)))

#average charge on male and female
avg=mean(subset(data,sex=="female")$charges)
print(avg)


#prediction
info<-read.csv("insurance.csv")
print(names(info))
#reg <- lm(charges ~ . , data=info)
reg <- lm(charges ~ age + sex + bmi + children + smoker + region , data=info)
print(reg)
print(summary(reg))
new<-data.frame(age=51,sex="male",bmi=32.3,children=1,smoker="no",region="northeast")
pred <- predict(reg,new)
print(pred)

#male non-smoker southeast
mns=subset(data,sex=="male" & smoker=="no" & region=="southeast")
print(mns)
reg1 <- lm(charges ~ age + bmi + children , data=mns)
print(reg1)
print(summary(reg1))
new1 <- data.frame(age=43,bmi=35.31,children=2)
pred1 <- predict(reg1,new1)
print(pred1)

new1 <- data.frame(age=52,bmi=47.74,children=1)
pred1 <- predict(reg1,new1)
print(pred1)

new1 <- data.frame(age=40,bmi=25.8,children=0)
pred1 <- predict(reg1,new1)
print(pred1)