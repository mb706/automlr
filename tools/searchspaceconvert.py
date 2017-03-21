#!/usr/bin/env python
# coding: utf-8
import itertools
import re

outfile = "../R/mlrLearners.R"
infile = "learners.org"
prefixfile = "learners.prefix"


def reduceDescription(stringlist, info, debug=False):
    varis = []
    fixis = []
    defis = []
    state = "pre"
    dstring = ""
    for line in stringlist:
        if not line:
            continue
        if state == "exiting":  # should have exited already
            raise Exception("should have exited. %s" % (info,))
        if debug:
            dstring = line.strip(' -').split("::")[1].strip()
            if dstring.find("MANUAL") < 0:  # if not manual, can strip some meta info
                dstring = dstring.split(":", maxsplit=1)[1]
                dstring = re.sub(r"\{[^}]*\}", "", dstring)
                dstring = '  # ' + re.sub(r"[^a-zA-Z]req:[^:]*", "", dstring).strip()
            else:
                dstring = ""
        if line == "**** Variable Parameters:":
            assert(not varis)
            state = "variable"
        elif line == "**** Default Parameters:":
            assert(not defis)
            state = "default"
        elif line == "**** Changed (fixed) Parameters:":
            assert(not fixis)
            state = "fixed"
        elif line.startswith("**** "):
            print("got unknown context line %s, continuing" % (line,))
        if line.startswith("**** "):
            continue
        if line.startswith("** "):
            state = "exiting"
            continue
        if state == "pre":
            if line.startswith('   -'):
                raise Exception("found indentation in pre context")
            else:
                continue
        assert(line.startswith('   -'))
        if state == "variable":
            varis.append(parsevari(line) + dstring)
        elif state == "fixed":
            fixis.append(parsefixi(line) + dstring)
        elif state == "default":
            defis.append(parsedefi(line) + dstring)
        else:
            print("out of state: '%s' in %s" % (line, info))
    return (varis, defis, fixis)


def parsevari(line):
    idstring = ""
    dummystring = ""
    reqstring = ""
    lenstring = ""
    expstring = ""
    versionstring = ""
    sides = line.strip(" -").split("::")
    assert(len(sides) == 2)
    name = sides[0].strip()
    info = sides[1].strip()
    if info.find("MANUAL") >= 0:
        inner = re.search("MANUAL\{[^}]*\}", info)
        if inner:
            return inner.group()[6:].strip("{}")
        else:
            return '## sp(%s, ...) # %s' % tuple(sides)
    versionmatch = re.search(r"(.*)VERSION\{([^}]*)\}(.*)", info)
    if versionmatch:
        info = "%s %s" % (versionmatch.group(1), versionmatch.group(3))
        versionstring = ', version = "%s"' % (versionmatch.group(2),)
    idmatch = re.search(r"\{[^}]*\}", info)
    if idmatch:
        idstring = ', id = "%s"' % (idmatch.group().strip("{}"),)
    if info.find("DUMMY") >= 0:
        dummystring = ', special = "dummy"'
        assert(info.find("INJECT") == -1)
    elif info.find("INJECT") >= 0:
        dummystring = ', special = "inject"'
    reqposition = info.find("req:")
    if reqposition >= 0:
        reqstring = info[reqposition + 4:]
        reqstring = ", req = quote(%s)" % reqstring.strip()
    lenmatch = re.search(r"len\([0-9]+\)", info)
    if lenmatch:
        lenstring = ', dim = %s' % (lenmatch.group().strip("len()"),)

    # filter out number-sign-delimited formulas
    splits = info.split("#")
    info = "#".join(splits[::2])  # replace all formulas by single '#'
    info = info.split(":")[0]  # remove comment part after ':'
    formulae = splits[1::2][:info.count("#")]

    if info.find("..") >= 0:  # this is a numeric range
        if info.find("(") >= 0:
            # this is a secondary range. We just strip everything before the '('
            # but we have to take care to remove the right amount of formulae
            info = info[info.find("("):]
            nfcount = info.count("#")
            nremove = nfcount - len(formulae)
            formulae = formulae[nremove:]
            info = re.sub("[() ]+", " ", info).strip()
        intness = info.find("int") >= 0
        info = info.strip("int ")
        rng = info.split(" ")[0].strip(" ,:").split("..")
        if rng[0] == "#":
            rng[0] = "quote(%s)" % formulae.pop(0)
        if rng[1] == "#":
            rng[1] = "quote(%s)" % formulae.pop(0)
        
        if  len(info.split(" ")) > 1 and info.split(" ")[-1].find("exp") >= 0:
            if info.split(" ")[-1].find("invexp") >= 0:
                expstring = ', "invexp"'
            else:
                expstring = ', "invexp"'
        
        return 'sp("%s", "%s", c(%s, %s)%s%s%s%s%s%s)' % (name,
                                                          "int" if intness else "real",
                                                          rng[0], rng[1], expstring,
                                                          idstring, dummystring, reqstring, lenstring,
                                                          versionstring)
    values = [x.strip() for x in info.split(",")]
    if len(values) == 2 and "TRUE" in values and "FALSE" in values:
        return 'sp("%s", "bool"%s%s%s%s)' % (name, idstring, dummystring, reqstring, lenstring)
    if not all(re.match(r"^[0-9.][-+e0-9.]*$", x) for x in values):
        values = ['"%s"' % (x,) for x in values]
    return 'sp("%s", "cat", c(%s)%s%s%s%s%s)' % (name, ", ".join(values),
                                                 idstring, dummystring, reqstring, lenstring,
                                                 versionstring)


def parsefixi(line):
    return parseone(line, "fix")


def parsedefi(line):
    return parseone(line, "def")


def parseone(line, tp):
    dummystring = ""
    versionstring = ""
    sides = line.strip(" -").split("::")
    assert(len(sides) == 2)
    name = sides[0].strip()
    info = sides[1].strip()
    if info.find("MANUAL") >= 0:
        inner = re.search("MANUAL\{[^}]*\}", info)
        if inner:
            return inner.group()[6:].strip("{}")
        else:
            return '## sp(%s, ...) # %s' % tuple(sides)
    versionmatch = re.search(r"(.*)VERSION\{([^}]*)\}(.*)", info)
    if versionmatch:
        info = "%s %s" % (versionmatch.group(1), versionmatch.group(3))
        versionstring = ', version = "%s"' % (versionmatch.group(2),)
    assert(info.find('req:') == -1)
    if info.find("DUMMY") >= 0:
        dummystring = ', special = "dummy"'
        assert(info.find("INJECT") == -1)
    elif info.find("INJECT") >= 0:
        dummystring = ', special = "inject"'
    assert(not re.search(r"\{[^}]*\}", info))
    val = re.findall(r"[-+_a-zA-Z0-9.!#]+", info)[0]
    if val[-1] == "!":
        val = val[:-1]
        assert(tp == "def")
        tp = "fixdef"
    if val != "##":
        assert("#" not in val)
    if not(val == "TRUE" or val == "FALSE" or val == "NULL"):
        if not re.match(r"^[0-9.][-+e0-9.]*", val):
            val = '"%s"' % val
    return 'sp("%s", "%s", %s%s%s)' % (name, tp, val, dummystring, versionstring)


def rdi(i):
    return reduceDescription(clisted[i], cheadings[i])


def makeDS(content):
    return "            " + ",\n            ".join(content)


def completeOutput(includeManual):
    manuals = []
    nonmanuals = []
    for cl, ch in zip(clisted, cheadings):
        varis, defis, fixis = reduceDescription(cl, ch)
        hasmanual = any(x.startswith("##") for x in itertools.chain(varis, defis, fixis))
        completeString = '    autolearner("%s"' % ch
        css = []
        if varis:
            css.append("# ** vp\n" + makeDS(varis))
        if fixis:
            css.append("# ** cp\n" + makeDS(fixis))
        if defis:
            css.append("# ** dp\n" + makeDS(defis))
        cscomplete = ",\n".join(css)
        if cscomplete:
            completeString += ",\n        list(\n" + cscomplete + ")"
        completeString += ")"
        if hasmanual:
            manuals.append(completeString)
        else:
            nonmanuals.append(completeString)
    retstring = "mlrLearnersNoWrap = makeNamedAlList(\n"
    if not includeManual:
        manuals = []
    if manuals:
        retstring += "##### some adjustment required:\n" + ",\n".join(manuals)
    if manuals and nonmanuals:
        retstring += "\n"
    if nonmanuals:
        retstring += "##### automatically generated:\n" + ",\n".join(nonmanuals)
    retstring += ")"
    return retstring


c = list(x.strip('\n') for x in open(infile))
for i, line in enumerate(c):
    if line.startswith("* classif"):
        break
content = c[i + 1:]
clisted = [list(x[1]) for x in itertools.groupby(content, lambda x: x.startswith('*** ')) if not x[0]]
del clisted[0]
cheadings = [x.strip("* ") for x in content if x.startswith('*** ')]


f = open(outfile, "w")
for l in open(prefixfile):
    f.write(l)
f.write(completeOutput(False))
