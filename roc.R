#train roc

n1<-trains$time>12
n1<-as.numeric(n1)
n1
pdanger<-trains$radscore


roc1<-roc(n1,pdanger,smooth=T,ci=T)
roc_train_1<-roc(n1,pdanger,ci=T)

#tiff("roc1.tif",width=400,height=400)
#D53E4F #E7B800  #2E9FDF

plot(roc1,
     main="The ROC for Training Cohort of Radiomics Signature",
     col="#D53E4F",
     grid=c(0.1, 0.1))
#dev.off()
par(new=TRUE)
#two years

gindex<-which(trains$time<=24&trains$status==0)
gindex
traing<-trains[-gindex,]
n2<-traing$time>24
n2<-as.numeric(n2)
n2


pdanger<-traing$radscore

roc_train_2<-roc(n2,pdanger,ci=T)
roc2<-roc(n2,pdanger,smooth=T,ci=T)
#tiff("roc1.tif",width=400,height=400)
#D53E4F #E7B800  #2E9FDF
plot(roc2,
     
     col="#E7B800",
     grid=c(0.1, 0.1))
#dev.off()
par(new=TRUE)
#three years
jindex<-which(trains$time<36&trains$status==0)
trainj<-trains[-jindex,]
n3<-trainj$time>36
n3<-as.numeric(n3)
n3
length(which(n3==1))

pdanger<-trainj$radscore
roc_train_3<-roc(n3,pdanger,ci=T)

roc3<-roc(n3,pdanger,smooth=T,ci=T)
#tiff("roc1.tif",width=400,height=400)
#D53E4F #E7B800  #2E9FDF
plot(roc3,
     
     col="#2E9FDF",
     grid=c(0.1, 0.1))
#dev.off()
roc3


#test ROC

nt4<-tests$time>12
nt4<-as.numeric(nt4)
nt4
ptdanger<-tests$radscore

roc_test_1<-roc(nt4,ptdanger,ci=T)
roc4<-roc(nt4,ptdanger,smooth=T,ci=T)
#tiff("roc1.tif",width=400,height=400)
#D53E4F #E7B800  #2E9FDF

plot(roc4,
     main="The ROC for Validation Cohort of Radiomics Signature",
     col="#D53E4F",
     grid=c(0.1, 0.1))
#dev.off()
par(new=TRUE)
#two years
gtindex<-which(tests$time<=24&tests$status==0)
gtindex
nt5<-tests$time>24
nt5<-as.numeric(nt5)
nt5

roc_test_2<-roc(nt5,ptdanger,ci=T)

roc5<-roc(nt5,ptdanger,smooth=T,ci=T)
#tiff("roc1.tif",width=400,height=400)
#D53E4F #E7B800  #2E9FDF
plot(roc5,
     
     col="#E7B800",
     grid=c(0.1, 0.1))
#dev.off()
par(new=TRUE)
#three years
mindex<-which(tests$time<36&tests$status==0)
mindex
testsm<-tests[-mindex,]
nt6<-testsm$time>36
nt6<-as.numeric(nt6)
nt6
length(which(nt6==1))
ptdanger<-testsm$radscore
roc_test_3<-roc(nt6,ptdanger,ci=T)

roc6<-roc(nt6,ptdanger,smooth=T,ci=T)
#tiff("roc1.tif",width=400,height=400)
#D53E4F #E7B800  #2E9FDF
plot(roc6,
     
     col="#2E9FDF",
     grid=c(0.1, 0.1))
####################################################
################clinical data###################
traincc<-train[,index_clinic]
dd=as.matrix(traincc)
tccox<-coxph(y~dd)
ptccox<-predict(tccox)
ptccox
summary(tccox)
{a<-rcorrcens(y ~ ptccox)#c-index
  b<-a[1]
  b<-1-b
  c<-a[4]
  c<-1.96*(c/2)
  d<-b-c
  e<-b+c
  CI<-c(d,e)
  print(b)
  CI}
roc_ctrain_1<-roc(n1,ptccox,ci=T)
roc7<-roc(n1,ptccox,smooth=T,ci=T)
plot(roc7,
     main="The ROC for Training Cohort of Clinical Model",
     col="#D53E4F",
     grid=c(0.1, 0.1))
par(new=TRUE)
########two years#########

pdanger<-ptccox[-gindex]
roc_ctrain_2<-roc(n2,pdanger,ci=T)
roc8<-roc(n2,pdanger,smooth=T,ci=T)
plot(roc8,
     
     col="#E7B800",
     grid=c(0.1, 0.1))
par(new=TRUE)
############three years###################
pdanger<-ptccox[-jindex]
roc_ctrain_3<-roc(n3,pdanger,ci=T)
roc9<-roc(n3,pdanger,smooth=T,ci=T)
#tiff("roc1.tif",width=400,height=400)
#D53E4F #E7B800  #2E9FDF
plot(roc9,
     
     col="#2E9FDF",
     grid=c(0.1, 0.1))



#############test roc clinical data############################



testcc<-test[,index_clinic]
dd=as.matrix(testcc)

ptestccox<-predict(tccox,newdata = testcc)
ptestccox<-as.numeric(ptestccox)
ptestccox
{a<-rcorrcens(y1 ~ ptestccox)#c-index
  b<-a[1]
  b<-1-b
  c<-a[4]
  c<-1.96*(c/2)
  d<-b-c
  e<-b+c
  CI<-c(d,e)
  print(b)
  CI}
roc_ctest_1<-roc(nt4,ptestccox,ci=T)
roc10<-roc(nt4,ptestccox,smooth=T,ci=T)
plot(roc10,
     main="The ROC for Validation Cohort of Clinical Model",
     col="#D53E4F",
     grid=c(0.1, 0.1))
par(new=TRUE)
########two years#########

pdanger<-ptestccox
roc_ctest_2<-roc(nt5,ptestccox,ci=T)
roc11<-roc(nt5,pdanger,smooth=T,ci=T)
plot(roc11,
     
     col="#E7B800",
     grid=c(0.1, 0.1))
par(new=TRUE)
roc11
############three years###################
pdanger<-ptestccox[-mindex]
roc_ctest_3<-roc(nt6,pdanger,ci=T)
roc12<-roc(nt6,pdanger,smooth=T,ci=T)


plot(roc12,
     
     col="#2E9FDF",
     grid=c(0.1, 0.1))


roc.test(roc_test_1, roc_ctest_1)
roc.test(roc_test_2, roc_ctest_2)
roc.test(roc_test_3, roc_ctest_3)

roc.test(roc_train_1, roc_ctrain_1)
roc.test(roc_train_2, roc_ctrain_2)
roc.test(roc_train_3, roc_ctrain_3)

roc_train_1
roc_train_2
roc_train_3
roc_test_1
roc_test_2
roc_test_3

roc_ctrain_1
roc_ctrain_2
roc_ctrain_3
roc_ctest_1
roc_ctest_2
roc_ctest_3
 ####################################################
################combined model###################

roc_gtrain_1<-roc(n1,nomp,ci=T)
roc21<-roc(n1,nomp,smooth=T,ci=T)
plot(roc21,
     main="The ROC for Training Cohort of Nomogramme",
     col="#D53E4F",
     grid=c(0.1, 0.1))
par(new=TRUE)
########two years#########

pdanger<-nomp[-gindex]
roc_gtrain_2<-roc(n2,pdanger,ci=T)
roc22<-roc(n2,pdanger,smooth=T,ci=T)
plot(roc22,
     
     col="#E7B800",
     grid=c(0.1, 0.1))
par(new=TRUE)
############three years###################
pdanger<-nomp[-jindex]
roc_gtrain_3<-roc(n3,pdanger,ci=T)
roc23<-roc(n3,pdanger,smooth=T,ci=T)

plot(roc23,
    
     col="#2E9FDF",
     grid=c(0.1, 0.1))



#############test roc combine model############################




roc_gtest_1<-roc(nt4,nomtest,ci=T)
roc24<-roc(nt4,nomtest,smooth=T,ci=T)
plot(roc24,
     main="The ROC for Validation Cohort of Nomogramme",
     col="#D53E4F",
     grid=c(0.1, 0.1))
par(new=TRUE)
########two years#########

pdanger<-nomtest
roc_gtest_2<-roc(nt5,pdanger,ci=T)
roc25<-roc(nt5,pdanger,smooth=T,ci=T)
plot(roc25,
     
     col="#E7B800",
     grid=c(0.1, 0.1))
par(new=TRUE)
############three years###################
pdanger<-nomtest[-mindex]
roc_gtest_3<-roc(nt6,pdanger,ci=T)
roc26<-roc(nt6,pdanger,smooth=T,ci=T)


plot(roc26,
     
     col="#2E9FDF",
     grid=c(0.1, 0.1))

roc_gtrain_1
roc_gtrain_2
roc_gtrain_3
roc_gtest_1
roc_gtest_2
roc_gtest_3

roc.test(roc_train_1, roc_gtrain_1)
roc.test(roc_train_2, roc_gtrain_2)
roc.test(roc_train_3, roc_gtrain_3)
roc.test(roc_test_1, roc_gtest_1)
roc.test(roc_test_2, roc_gtest_2)
roc.test(roc_test_3, roc_gtest_3)

roc.test(roc_ctrain_1, roc_gtrain_1)
roc.test(roc_ctrain_2, roc_gtrain_2)
roc.test(roc_ctrain_3, roc_gtrain_3)
roc.test(roc_ctest_1, roc_gtest_1)
roc.test(roc_ctest_2, roc_gtest_2)
roc.test(roc_ctest_3, roc_gtest_3)

roc.test(roc21, roc7)
roc.test(roc22, roc8)
roc.test(roc23, roc9)

roc.test(roc24, roc10)
roc.test(roc25, roc11)
roc.test(roc26, roc12)
