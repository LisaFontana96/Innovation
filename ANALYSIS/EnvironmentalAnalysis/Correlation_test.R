dataset<- read.csv('/Users/u7585399/Desktop/RadiusTest/Test_correlation15.csv')
dataset$Shannon.index.home.range<- as.numeric(dataset$Shannon.index.home.range) 
dataset$Artificial.surface...home.range<- as.numeric(dataset$Artificial.surface...home.range)
dataset$Shannon.index.1.5.area<- as.numeric(dataset$Shannon.index.1.5.area)
dataset$Artificial.surface...1.5.area<- as.numeric(dataset$Artificial.surface...1.5.area)

correlation_ShannonIndex <- cor.test(dataset$Shannon.index.home.range, dataset$Shannon.index.1.5.area)
pvalue_ShannonIndex <- correlation_ShannonIndex$p.value
correlation_ArtificialSurface <- cor.test(dataset$Artificial.surface...home.range, dataset$Artificial.surface...1.5.area)
pvalue_ArtificialSurface <- correlation_ArtificialSurface$p.value

print(correlation_ShannonIndex)
print(pvalue_ShannonIndex)
print(correlation_ArtificialSurface)
print(pvalue_ArtificialSurface)
