#!/usr/bin/env python
# coding: utf-8
from collections import OrderedDict
import re

outfile = "../R/mlrLearners.R"
infile = "learners.org"
prefixfile = "learners.prefix"


def splitList(slist, pattern):
    """Split a list of strings into list of lists of strings, divided at regex 'pattern'"""
    matchindices = [i for i in range(len(slist)) if re.match(pattern, slist[i])]
    matchindices.append(len(slist))
    start = matchindices.pop(0)
    result = OrderedDict()
    for i in matchindices:
        assert slist[start] not in result
        result[slist[start]] = slist[(start + 1):i]
        start = i
    return result


def parseVersion(meta):
    m = re.search(r"VERSION\{([^}]*)\}", meta)
    if m:
        return 'version = "%s"' % (m.group(1),)
    return None


def parseSpecial(meta):
    isDummy = "DUMMY" in meta
    isInject = "INJECT" in meta
    if isDummy and isInject:
        raise Exception("Meta %s contained DUMMY and INJECT both." % (meta,))
    if isDummy:
        return 'special = "dummy"'
    elif isInject:
        return 'special = "inject"'
    else:
        return None


def parseRequires(meta):
    m = re.search(r"req: *(.*)$", meta)
    if m:
        return 'req = quote(%s)' % (m.group(1),)
    return None


def parseId(meta):
    m = re.search(r"(?<!VERSION){([^}]*)\}", meta)
    if m:
        return 'id = "%s"' % (m.group(1),)
    return None


def parseLen(meta):
    m = re.search(r"len\(([^)]*)\)", meta)
    if m:
        return 'dim = %d' % (int(m.group(1)),)
    return None


def parseRange(varrange, formulae, deftype):
    assert deftype in ["var", "fix", "def"]
    varrange = varrange.strip()

    trafostring = None

    nummatch = r"(?P<int>int)? *(?P<x>[-+0-9]*\.?[0-9][-+0-9e]*|#)?\.\.(?P<y>[-+0-9]*\.?[0-9][-+0-9e]*|#)? *"
    nummatch += r"(\( *(?P<x0>[-+0-9]*\.?[0-9][-+0-9e]*|#)\.\.(?P<y0>[-+0-9]*\.?[0-9][-+0-9e]*|#) *\))? *"
    nummatch += r"(?P<trafo>(inv)?exp)?"
    mnum = re.search(nummatch, varrange)
    if mnum:
        # numeric range
        assert deftype == "var"
        dic = mnum.groupdict()
        vartype = "int" if dic["int"] else "real"
        for entry in ["x", "y", "x0", "y0"]:
            if dic[entry] == "#":
                dic[entry] = "quote(%s)" % (formulae.pop(0),)
            elif dic[entry] is not None:
                # check we actually have numbers
                try:
                    if vartype == "int":
                        int(dic[entry])
                    else:
                        float(dic[entry])
                except ValueError:
                    raise Exception("Range %s contained bad numbers" % (varrange,))
        if dic["x0"] is not None:
            dic["x"] = dic["x0"]
            dic["y"] = dic["y0"]
        assert dic["x"] is not None
        assert dic["y"] is not None
        rangestring = "c(%s, %s)" % (dic["x"], dic["y"])
        if dic["trafo"]:
            trafostring = '"%s"' % (dic["trafo"],)
    elif deftype == "var":
        assert "#" not in varrange
        assert not formulae
        values = [x.strip() for x in varrange.split(",")]
        assert len(values) > 0
        assert all(re.match(r"^[-+a-zA-Z._0-9]*$", x) for x in values)
        if set(values) == set(["TRUE", "FALSE"]):
            vartype = "bool"
            rangestring = None
        else:
            vartype = "cat"
            if not all(re.match(r"^[-+]?[0-9.][-+e0-9.]*$", x) for x in values):
                values = ['"%s"' % (x,) for x in values]
            rangestring = "c(%s)" % (", ".join(values),)
    else:
        assert "," not in varrange
        vartype = deftype
        if varrange == "#":
            assert formulae == [""]
            assert deftype == "def"
            rangestring = '"##"'
        else:
            assert "#" not in varrange
            assert not formulae
            if varrange[-1] == "!":
                varrange = varrange[:-1]
                assert deftype == "def"
                vartype = "fixdef"
            assert "!" not in varrange
            if varrange != "TRUE" and varrange != "FALSE" and varrange != "NULL" and \
               not re.match(r"^[-+]?[0-9.][-+e0-9.]*", varrange):
                varrange = '"%s"' % (varrange,)
            rangestring = varrange
    vartype = '"%s"' % (vartype,)
    return (vartype, rangestring, trafostring)


def parseVarLine(line, deftype):
    try:
        splits = line.split("#")
        line = "#".join(splits[::2])
        formulae = splits[1::2]
        matchstring = r"   - (?P<varname>[-a-zA-Z._0-9]+) :: (?P<range>[^:]*)(: (?P<meta>[^#]*))?$"
        m = re.match(matchstring, line)
        if not m:
            raise Exception("Line %s didn't match." % (line,))
        varname = '"%s"' % (m.groupdict()["varname"],)
        varrange = m.groupdict()["range"]
        varmeta = m.groupdict()["meta"]
        if varmeta is None:
            varmeta = ""
        vartype, varrange, vartrafo = parseRange(varrange, formulae, deftype)

        inserts = [varname, vartype, varrange, vartrafo, parseId(varmeta), parseSpecial(varmeta),
                   parseRequires(varmeta), parseLen(varmeta), parseVersion(varmeta)]

        return 'sp(%s)' % (", ".join(ins for ins in inserts if ins is not None),)
    except Exception:
        print("Line %s generated error" % (line,))
        raise


def completeOutput(learners):
    def makeDS(content):
        return "            " + ",\n            ".join(content)
    defstrings = []
    for lname, lstrings in learners.items():
        lname = lname.strip("* ")
        lsplit = splitList(lstrings, r"\*\*\*\* ")
        varidx = '**** Variable Parameters:'
        fixidx = '**** Changed (fixed) Parameters:'
        defidx = '**** Default Parameters:'
        assert set(lsplit).issubset(set([varidx, fixidx, defidx]))
        varis = [parseVarLine(l, "var") for l in lsplit[varidx]] if varidx in lsplit else None
        fixis = [parseVarLine(l, "fix") for l in lsplit[fixidx]] if fixidx in lsplit else None
        defis = [parseVarLine(l, "def") for l in lsplit[defidx]] if defidx in lsplit else None
        completeString = '    autolearner("%s"' % (lname,)
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
        defstrings.append(completeString)
    retstring = "mlrLearnersNoWrap = makeNamedAlList(\n##### automatically generated:\n"
    retstring += ",\n".join(defstrings)
    retstring += ")"
    return retstring


def makeLearners(cat):
    learners = OrderedDict()
    for subcat in splitList(cat, r"\*\* .*").values():
        newlearners = splitList(subcat, r"\*\*\* .*")
        for k, v in newlearners.items():
            assert k not in learners
            learners[k] = v
    return learners


filetext = list(x.strip('\n') for x in open(infile))

cat = splitList(filetext, r"\* .*")["* classif"]

if __name__ == "__main__":
    with open(outfile, "w") as f:
        for l in open(prefixfile):
            f.write(l)
        f.write(completeOutput(makeLearners(cat)))
