
#install.packages("Rmpi", configure.args = c("--with-Rmpi-include=/usr/include/openmpi-x86_64/", "--with-Rmpi-libpath=/usr/lib64/openmpi/lib/",
#                   "--with-Rmpi-type=OPENMPI"))


options(width=150)
devtools::load_all("../../ParamHelpers")
devtools::load_all("../../mlr")
devtools::load_all("../../smoof")
devtools::load_all("../../mlrMBO")
library('testthat')

library(roxygen2)
roxygenise('..')

devtools::load_all("..")
options(error=dump.frames)



# check whether parallelization on the resample level (all)
# and on the tune level (random, ...?) work.

# resampling timeout cases to test:
#
#  - all runs below timeout --> normal behaviour
#  - first run above firstIterTimeout --> error, others not run
#  - first run below firstIterTimeout, above normal timeout --> others are run, first is imputed
#  - first run below firstIterTimeout, above normal timeout --> others imputed when they time out
#  - first run below normal timeout --> others imputed when they time out
#  - run below firstIterTimeout, above normal timeout, resampling only 1 iter --> error
#  - run below firstIterTimeout, above normal timeout, not resampling --> error
#
# CROSS WITH:
#
# - parallelizing the resampling
# - parallelizing some other level
# - not parallelizing
#

# TODO: check whether isResampleParallel works inside the parallelization

# can check whether on slave by 'getPMOption("on.slave")'

# wrap tryCatch so that it doesn't catch out of time errors.
# wrap checkResultsAndStopWithErrorsMessages to 
 
assignInNamespace("checkResultsAndStopWithErrorsMessages", ns = "parallelMap", function(...) { Sys.sleep(2) ; print("checkresults") })

####
# the setTimeLimit stuff

library("parallelMap")
library("RSQLite")

tmp <- dbConnect(SQLite(), "/tmp/b")
tmp2 <- dbConnect(SQLite(), "/tmp/b")
tmp3 <- dbConnect(SQLite(), "/tmp/b")

dbRollback(tmp)

dbBegin(tmp)
dbBegin(tmp2)

file = tempfile()
conn1 = dbConnect(SQLite(), file)
conn2 = dbConnect(SQLite(), file)

dbGetQuery(conn1, "begin immediate transaction")
e = try(dbGetQuery(conn2, "begin immediate transaction"), silent = TRUE)
conditionMessage(attr(e, "condition"))





dbGetQuery(tmp, "begin immediate transaction")
dbGetQuery(tmp2, "begin immediate transaction")

dbCommit(tmp)
dbCommit(tmp2)

dbDisconnect(tmp)




tmp <- NULL
tmp2 <- NULL
tmp3 <- NULL
gc()

dbRemoveTable(tmp, "test3")
dbRemoveTable(tmp, "object")
dbWriteTable(tmp, "test3", data.frame(a=integer(0), b=character(0)))
dbGetQuery(tmp, "alter table test3 add column c BLOB;")

dbGetQuery(tmp, "create table object (object blob)")
dbGetPreparedQuery(tmp, "insert into object values (:c)", data.frame(c=I(list(serialize(list(new.env()), NULL)))))

c = sqliteQuickColumn(tmp, "object", "object")
unserialize(c[[1]])
str(c)
dbIsValid(tmp)

dbQuoteIdentifier((unserialize(serialize(tmp, NULL))), "test")

cbind(data.frame(a=1, b=2), c=3)

dbDisconnect(tmp)

dbIsValid(tmp)
dbGetInfo(tmp)

dbGetRowCount(tmp, "object")

q = dbQuoteIdentifier(tmp, 'te"\'st')

cat(sprintf("test %s\n", q))
str(q)

iris[TRUE, 1]

dbGetQuery(tmp, "select count(*) from object")[[1]]

dbGetQuery(tmp, "insert into test3 values (1, 'test', 'a')")

dbGetPreparedQuery(tmp, "insert into test3 values (:a, :b, :c)", data.frame(a=99, b="test", c=I(list(serialize(list(new.env()), NULL)))))

dbGetPreparedQuery(tmp, "insert into test3 values (:a, :b, :c)", cbind(data.frame(a=NA, b="test"), list(c=I(list(serialize(list(new.env()), NULL))))))

dbGetQuery(tmp, sprintf("select rowid, %s from test3", dbQuoteIdentifier(tmp, ":c")))

dbGetQuery(tmp, "select rowid, a from test3")
dbGetQuery(tmp, "select * from test3")
dbGetQuery(tmp2, "select a from test3")
dbGetQuery(tmp, "select a from test3")

dbGetQuery(tmp, "update test3 set a=a+1 where rowid=1")
dbGetQuery(tmp2, "update test3 set a=a+1 where rowid=1")

dbGetQuery(tmp2, "create table o2 (object blob)")
gc()

number=checkmate::asInteger(NA)
row=checkmate::asInteger(2)
(str = sprintf("update test3 set a=%s where rowid=%s", number, row))


dbGetPreparedQuery(tmp, sprintf("update test3 set a=:number, %s=:n2 where rowid=:row", dbQuoteIdentifier(tmp, ":c")), data.frame(number=number, n2=9, row=row))



x = dbQuoteIdentifier(tmp, c("colid", "rowid"))
paste(x, "1", sep = "=", collapse = ", ")

dbListFields(tmp, "test3")

dbGetQuery(tmp, "PRAGMA table_info(test3)")

dbGetQuery(tmp, "select a, b from test3")

dbGetQuery(tmp, "replace into test3 (rowid, *) values (1, 1, 0, 0)")

x
x$c[[1]]

x[TRUE, ]

lapply(x[TRUE, ], unserialize)

x$
y = as.list(x)
y$c = 0
y
x[c("a", "b", "x")]

x$a

dbReadTable(tmp, "object")
dbReadTable(tmp, "test3")

dbListTables(tmp)

paste0("test", "a", "b", NULL, NULL, "c")
s = dbQuoteString(tmp, serialize(list(new.env()), NULL))
catf("%s", s)

paste0(":", c("a", "b", "c"), collapse=",")

data.frame(g = I(lapply(list(list(new.env()), new.env()), function(x) serialize(x, NULL))))

x = serialize(list(new.env()), NULL)
x

parallelStartSocket()
parallelLibrary("parallelMap")
parallelLibrary("RSQLite")
parallelMap(function(i, t) dbReadTable(dbConnect(SQLite(), "/tmp/b"), 'test')[i, 2], 1:3, more.args=list(t=tmp))
parallelStop()

duplicate = function(expr) { for (i in 1:2) substitute(eval.parent(expr)) }


# modes:
# local, multicore, socket, mpi, BatchJobs


setTimeLimit()
parallelLibrary("parallelMap")
parallelStartSocket(2)  # frame not visible
parallelStartLocal()  # frame visible
parallelStartMulticore(2)  # frame visible
parallelStartMPI(2)  # frame not visible
#parallelStartBatchJobs(2)  # does not work
z <- NULL
(z <- withRestarts({withCallingHandlers(y <- dopar(), error = exitOnTimeout); TRUE }, automlr.timeout = function() { print("oto"); FALSE }))
z
parallelStop()

y
z

identical(parallelGetOptions()$settings$mode, "mpi")

parallelGetOptions()

parallelGetOptions

parallelMap:::print.ParallelMapOptions

f = function(i) {
  getFrameVar = function(fname, varname) {
    calls = sys.calls()
    calls[[length(calls) - 1]] = NULL
    callnames = sapply(calls, function(x) try(as.character(x[[1]]), silent = TRUE))
    frameno = tail(which(callnames == fname), n = 1)
    if (length(frameno) < 1) {
      return(NULL)
    }
    sys.frame(frameno)[[varname]]
  }
  print("starting sleep")
  stop("reached elapsed time limit")
  print("done sleeping")
  getFrameVar("dopar", "a")
}
debugonce(parallelMap:::checkResultsAndStopWithErrorsMessages)
dopar = function() {
  a = 1
  on.exit(setTimeLimit())
#  setTimeLimit(elapsed = 2, transient = TRUE)
  parallelMap(f, 1:2)
}

exitOnTimeout = function(cond) {
  setTimeLimit()
  timeoutMessage = "reached elapsed time limit"
  print("hi from ontimeout. message was:")
  print(conditionMessage(cond))
  if (conditionMessage(cond) == timeoutMessage) {
    return(FALSE)
    invokeRestart("automlr.timeout")
  }
  signalCondition(cond)
}


f2 = function(i) {
  asin(0)
}

dopar2 = function() {
  myasin = base::asin
  myAIN = function(what, value, ns) {
    w = options("warn")
    on.exit(options(w))
    options(warn = -1)
    where = as.environment(paste("package", ns, sep = ":"))
    if (bindingIsLocked(what, where)) {
      unlockBinding(what, where)
      assign(what, value, where)
      lockBinding(what, where)
    } else {
      assign(what, value, where)
    }
  }
  on.exit(myAIN("asin", myasin, "base"))
  myAIN("asin", function(...) { print('asin called') ; print(sys.calls()) ; FALSE }, "base") 
  asin(0)
  parallelMap(f2, 1:2)
}

dopar2()

parallelStartSocket(2)  # frame not visible, modifications not visible, [timeouterror caught?]
parallelStartLocal()  # frame visible, modification visible
parallelStartMulticore(2)  # frame visible, modification visible
#parallelStartMPI(2)  # frame not visible
#parallelStartBatchJobs(2)  # does not work
dopar2()
parallelStop()


f3 = function(i) {
  tryCatch(Sys.sleep(4), error = function(e) print("yo"))
}

dopar3 = function() {
  on.exit(setTimeLimit())
  setTimeLimit(elapsed = 1, transient = TRUE)
  parallelMap(f3, 1:4)
}


parallelStartSocket(2)  # frame not visible, modifications not visible, timeouterror does not carry over but stays local, callinghandlers stay local.
parallelStartLocal()  # frame visible, modification visible, timeouterror carries over, callinghandlers carry over
parallelStartMulticore(2)  # frame visible, modification visible, timeouterror carries over AND stays local, callinghandlers stay local --> good for jumping back.
#parallelStartMPI(2)  # frame not visible
#parallelStartBatchJobs(2)  # can't test this right now.
dopar3()
parallelStop()


# TODO: test if callinghandlers carry over.


fx = function() {
  setTimeLimit(elapsed = 1, transient = TRUE)
  on.exit(print("a"), add = TRUE)
  on.exit(Sys.sleep(3), add = TRUE)
  on.exit(print("b"), add = TRUE)
  print("hi")
#  Sys.sleep(3)
  print("end")
}


fx()
setTimeLimit()


TRUE && stop("test")

withCallingHandlers(
    withRestarts(
        withCallingHandlers(fx(), error = function(...) { print(list(...)) ; print("inner ch") ; print(sys.calls()); invokeRestart("automlr.timeout", 'yo')} ),
        automlr.timeout = function(x) { print(x); FALSE }),
    error = function(...) { print(list(...)) ; print("outer ch") ; print(sys.calls())} )


b = function() {
  z = 3
  Sys.sleep(1)
  on.exit(setTimeLimit(elapsed = 1e-15, transient = TRUE))
}

a = function() {
  y = 2
  Sys.sleep(3)
  b()
  Sys.sleep(2.1)
  Sys.sleep(2.2)
}

a()


oto = function(cond) {
  if (conditionMessage(cond) == "test") {
    invokeRestart("test")
  }
}

oto2 = function(cond) {
  if (conditionMessage(cond) == "test2") {
    invokeRestart("test2")
  }
}


withRestarts({
    withCallingHandlers({
        withRestarts({
            withCallingHandlers({
                stop("test2")
                0
              }, error = oto)
          }, test = function() 2)
      }, error = oto2)
  }, test2 = function() 3)



z <- 1
withRestarts({
    x <- z
    y <- 3
    print("here we go")
    if (x < y) {
      invokeRestart("zts")
    }
    print("done")
  }, zts = function() {z <<- z + 1})

gz



zz = tryCatch({
   a()
   try(signalCondition(structure(list(message="msg"), class=c("myerrclass", "condition"))))
    'x'
  },
         error = function(e) { print("error") ; e},
         myerrclass = function(e) { print("myerr") ; 2 })

str(zz)

ff = function(exp) {
  print(parent.frame())
  system.time(exp)
}

ss = function(expr) {
  x = 9
  eval.parent(substitute(do.call(ff, list(quote(expr)))))
}

x = 1
ss({ print(x) ; Sys.sleep(2) ; x <- 3})
x

x = 1
ff({ print(x) ; Sys.sleep(2) ; x <- 3})
x

tryCatch({ stop("test") }, error = function() print("test") )

withCallingHandlers(withCallingHandlers(stop('yo'), error=function(c) print("handler 1")), error = function(c) print("handler 2"))


res = tryCatch(Sys.sleep(10), condition = function(e) e)


withCallingHandlers(
  for (i in 1:10) {
    print(i)
    Sys.sleep(1)
  },
    interrupt = function(c) print(c))


