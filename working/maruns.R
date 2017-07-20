
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




datas = c(38, 46, 179, 184, 554, 772, 917, 1049, 1111, 1120, 1128, 293, 389)



datasets = list()
for (d in datas) {
  x = getOMLDataSet(data.id = d)
  datasets[[x$desc$name]] = convertOMLDataSetToMlr(x)
}


fri_c1_1000_25

quake

which(names(datasets) == "quake")
quake = datasets[[6]]
fri_c1_1000_25 = datasets[[7]]
save(quake, file = "../../notes/quake.Rsv")
save(fri_c1_1000_25, file = "../../notes/fri_c1_1000_25.Rsv")


r <- listOMLDataSets()
head(r)

OpenML::

t <- listOMLTasks()
head(t)

flows = listOMLFlows()
head(flows)

flows[56:63, ]


rr <- listOMLRuns(flow.id = 56)
head(rr)

rre <- listOMLRunEvaluations(flow.id = 56)
head(rre)

omlrun <- getOMLRun(6)

getOMLRun


function (run.id, cache.only = FALSE, verbosity = NULL)
{
    id = asCount(run.id)
    assertFlag(cache.only)
    down = downloadOMLObject(id, object = "run", cache.only = cache.only,
        verbosity = verbosity)
    f = down$files
    doc = down$doc
    parseData = function(path) {
        path.ds = paste(path, "oml:dataset", sep = "/")
        ns.datasets = getNodeSet(doc, path.ds)
        datasets = lapply(seq_along(ns.datasets), function(i) {
            list(data.id = xmlRValR(doc, paste(path.ds, "[",
                i, "]/oml:did", sep = "")), name = xmlRValS(doc,
                paste(path.ds, "[", i, "]/oml:name", sep = "")),
                url = xmlRValS(doc, paste(path.ds, "[", i, "]/oml:url",
                  sep = "")))
        })
        datasets = convertListOfRowsToDataFrame(datasets, strings.as.factors = FALSE)
        path.fls = paste(path, "oml:file", sep = "/")
        ns.fls = getNodeSet(doc, path.fls)
        files = lapply(seq_along(ns.fls), function(i) {
            list(data.id = xmlRValR(doc, paste(path.fls, "[",
                i, "]/oml:did", sep = "")), name = xmlRValS(doc,
                paste(path.fls, "[", i, "]/oml:name", sep = "")),
                url = xmlRValS(doc, paste(path.fls, "[", i, "]/oml:url",
                  sep = "")))
        })
        files = convertListOfRowsToDataFrame(files, strings.as.factors = FALSE)
        path.evals = paste(path, "oml:evaluation", sep = "/")
        ns.evals = getNodeSet(doc, path.evals)
        evals = setDF(rbindlist(lapply(ns.evals, function(node) {
            children = xmlChildren(node)
            row = list(as.integer(xmlValue(children[["did"]])),
                xmlValue(children[["name"]]), xmlValue(children[["flow_id"]]),
                xmlValue(children[["label"]]), as.numeric(xmlValue(children[["value"]])),
                as.numeric(xmlValue(children[["stdev"]])), xmlValue(children[["array_data"]]),
                as.integer(xmlValue(children[["sample_size"]])))
            cv.info = xmlAttrs(node)[c("repeat", "fold")]
            if (is.null(cv.info))
                cv.info = c(NA, NA)
            row = c(row, cv.info)
            names(row) = c("data.id", "name", "flow_id", "label",
                "value", "stdev", "array.data", "sample.size",
                "repeat", "fold")
            row
        }), fill = TRUE))
        makeOMLIOData(datasets = datasets, files = files, evaluations = evals)
    }
    run.args = filterNull(list(run.id = xmlREValI(doc, "/oml:run/oml:run_id"),
        uploader = xmlREValI(doc, "/oml:run/oml:uploader"), uploader.name = xmlOValS(doc,
            "/oml:run/oml:uploader.name"), task.id = xmlREValI(doc,
            "/oml:run/oml:task_id"), task.type = xmlOValS(doc,
            "/oml:run/oml:task_type"), task.evaluation.measure = xmlOValS(doc,
            "/oml:task_evaluation_measure"), flow.id = xmlRValI(doc,
            "/oml:run/oml:flow_id"), flow.name = xmlOValS(doc,
            "/oml:run/oml:flow_name"), setup.id = xmlREValI(doc,
            "/oml:run/oml:setup_id"), setup.string = xmlOValS(doc,
            "/oml:run/oml:setup_string"), error.message = xmlOValS(doc,
            "/oml:run/oml:error_message"), tags = xmlOValsMultNsS(doc,
            "/oml:run/oml:tag"), input.data = parseData("/oml:run/oml:input_data"),
        output.data = parseData("/oml:run/oml:output_data"),
        parameter.setting = list()))
    ns.pars = getNodeSet(doc, "/oml:run/oml:parameter_setting")
    run.args[["parameter.setting"]] = lapply(seq_along(ns.pars),
        function(i) {
            args = filterNull(list(name = xmlRValS(doc, paste("/oml:run/oml:parameter_setting[",
                i, "]/oml:name", sep = "")), value = xmlRValS(doc,
                paste("/oml:run/oml:parameter_setting[", i, "]/oml:value",
                  sep = "")), component = xmlOValS(doc, paste("/oml:run/oml:parameter_setting[",
                i, "]/oml:component", sep = ""))))
            do.call(makeOMLRunParameter, args)
        })
    par.names = vcapply(run.args[["parameter.setting"]], function(x) x$name)
    run.args[["parameter.setting"]] = setNames(run.args[["parameter.setting"]],
        par.names)
    f = findCachedRun(run.args$run.id)
    if (!f$predictions.arff$found) {
        message("No URL found to retrieve predictions from.")
        pred = NULL
    }
    else {
        pred = arff.reader(f$predictions.arff$path)
    }
    run.args[["predictions"]] = pred
    return(do.call(makeOMLRun, run.args))
}

omlrun$parameter.setting

irisdata.oml = getOMLDataSet(data.id = 61L)

iristask.oml = getOMLTask(task.id = 59L)

str(iristask.oml)



str(irisdata.oml)

getOMLTaskTypeList()

OpenML::getOMLTaskTypeList

getOMLDataSet



task = getOMLTask(10)

lrn = makeLearner("classif.rpart")

res = runTaskMlr(task, lrn)

res

listFilterMethods(FALSE)



fl <- getOMLFlow(flow.id = 4796)

fl1 <- readRDS(file = fl$binary.path)

fl$parameters

fl1
