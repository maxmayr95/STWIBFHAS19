library(openxlsx)
data = read.xlsx(xlsxFile = './STWI Exam HS19.xlsx')
data = read.xlsx(xlsxFile = './STWI_Exam_HS19.xlsx')#add file here or rename your exam file to Data.xlsx and copy it into this folder
attach(data) 
# => this sign is for copy pase : ~
# use percent => mean(stockData$`%.Fall`)
#a) height = continous
#b) mother = continous
#c) maths = continous
#d) french = discret
#e) siblings = discret
#f) present = continous
# plz = discret nominal
library(MASS) #for fractions(amountOfSeven/(6*6)) => zeigt brüchte an
library(Hmisc) 
describe(c(0,1),type=2)
describe(c(0,1))
