#Read data from source
load('C:/Users/Pavan/Documents/R workspace/data/titanic.raw.rdata')
idx<- sample(1:nrow(titanic.raw),5)
titanic.raw[idx,]
summary(titanic.raw)

library(arules)
rules.all<-apriori(titanic.raw)
inspect(rules.all)

#rules with RHS containing Survived only
rules<-apriori(titanic.raw,
               control = list(verbose=F),
               parameter = list(minlen=2,supp=0.005,conf=0.8),
               appearance = list(rhs=c("Survived=Yes" , "Survived=No"),default="lhs"))
#keep 3 decimal places
quality(rules)<-round(quality(rules),digits=3)
#order rules by life
rules.sorted<-sort(rules,by="lift")
inspect(rules.sorted)

inspect(rules.sorted[1:2])
#find redundant rules 
subset.matrix<-is.subset(rules.sorted,rules.sorted)
subset.matrix[lower.tri(subset.matrix,diag = TRUE)]<-NA
redundant<-colSums(subset.matrix,na.rm = TRUE)>=1
#which redundant
which(redundant)
#remove redundant rules 
rules.pruned<-rules.sorted[!redundant]
inspect(rules.pruned)

#Interpreting rules
inspect(rules.pruned[1])
#It provides the information of survival rate of 2nd class but doesnot compare with other classes

rules<- apriori(titanic.raw,
                control = list(verbose=F),
                parameter = list(minlen=3,supp=0.002,conf=0.2),
                appearance = list(default="none",rhs=c("Survived=Yes"),
                                  lhs=c("Class=1st","Class=2nd","Class=3rd","Age=Child","Age=Adult")))
rules.sorted<-sort(rules,by="confidence")
inspect(rules.sorted)

# Visualizing Association rules
library(arulesViz)
plot(rules.all)
plot(rules.all,method = "grouped")
plot(rules.all,method="graph")
plot(rules.all,method="graph",control = list(type="items"))
plot(rules.all,method="paracoord",control=list(reorder=TRUE))
