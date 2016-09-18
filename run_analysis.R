#url="https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
#download.file(url,"data.zip")


#temp=tempfile()
#url="https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
#temp=download.file(url,"data.zip")
#data=read.table(unz(temp, filename="UCI HAR Dataset\test\X_test.txt"))
#unlink(temp)
#temp=tempfile(unzip("data.zip"))

y1=read.table("train/y_train.txt")
y2=read.table("test/y_test.txt")
y=rbind(y1,y2)

x1=read.table("train/X_train.txt")
x2=read.table("test/X_test.txt")
x=rbind(x1,x2)

sub1=read.table("train/subject_train.txt")
sub2=read.table("test/subject_test.txt")
sub=rbind(sub1,sub2)


nomes=read.table("features.txt")
names(x)=nomes[,2]

std=grep('std()',names(x), fixed=TRUE)
mean=grep('mean()',names(x), fixed=TRUE)
kappa=sort(c(std,mean))
xkappa=x[,kappa]


atividades=read.table("activity_labels.txt")
y=cbind(y,y[,1]) # doubles the ycolumns
names(y)=c("idactivity","activity") #rename the y columns
for(i in 1:length(y[,1])){y[i,2]=as.character(atividades[(y[i,1]),2])} # Identify activity labels in y

data=cbind(y,xkappa) # bind the y data with the x full data
names(data)=gsub('()',"", names(data), fixed=TRUE)
data=data[,-c(3)]

names(sub)="subject"
data=cbind(sub,data)


subs=unique(data$subject)

data2=data[1,]
#names(data2)=names(sub1)
linha=1
for(i in 1:length(subs)){
sub1=subset(data,data$subject==subs[i])
activs=unique(sub1$idactivity)

	for(j in 1:length(activs)){
				
		sub2=subset(sub1,sub1$idactivity==activs[j])
		for(k in 3:length(sub1[1,])){
			data2[linha,k]=mean(sub2[,k])
		}
		data2[linha,1:2]=sub2[j,1:2]
		data2$activity[linha]=as.character(sub2$activity[j])
		linha=linha+1
	}	
}
write.table(data2,"tidy_data.txt")
############################################################################
View(data)
View(data2)
