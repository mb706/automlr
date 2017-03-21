
#install.packages("farff")

rm(list=objects()[grep('^(train|predict)Learner.[0-9]*$',objects())])
rm(list=objects()[objects() =="last.dump"])


library("OpenML")
source('init.R')
upstart()
attachNamespace("OpenML")

roxygenise('..')
devtools::load_all("..")

setOMLConfig(apikey = "c1994bdb7ecb3c6f3c8f3b35f4b47f1f")  # the public read only key


r <- listOMLDataSets()
head(r)

t <- listOMLTasks()
head(t)

flows = listOMLFlows()
head(flows)

flows[56:63, ]


rr <- listOMLRuns(flow.id = 56)
head(rr)

rre <- listOMLRunEvaluations(flow.id = 56)
head(rre)

irisdata.oml = getOMLDataSet(data.id = 61L)

iristask.oml = getOMLTask(data.id = 61L)



str(irisdata.oml)

getOMLTaskTypeList()

OpenML::getOMLTaskTypeList

getOMLDataSet



task = getOMLTask(10)

lrn = makeLearner("classif.rpart")

res = runTaskMlr(task, lrn)

res

listFilterMethods(FALSE)

