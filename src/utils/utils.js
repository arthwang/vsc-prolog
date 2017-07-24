"use strict";
exports.__esModule = true;
var fs = require("fs");
var YAML = require("yamljs");
var cp = require("child_process");
var jsesc = require("jsesc");
var vscode_1 = require("vscode");
var Utils = (function () {
    function Utils() {
    }
    Utils.getPredDescriptions = function (pred) {
        if (Utils.snippets[pred]) {
            return Utils.snippets[pred].description;
        }
        return "";
    };
    Utils.init = function (context) {
        Utils.loadSnippets(context);
        Utils.genPredicateModules(context);
    };
    Utils.loadSnippets = function (context) {
        if (Utils.snippets) {
            return;
        }
        var snippetsPath = context.extensionPath + "/snippets/prolog.json";
        var snippets = fs.readFileSync(snippetsPath, "utf8").toString();
        Utils.snippets = JSON.parse(snippets);
    };
    Utils.genPredicateModules = function (context) {
        Utils.loadSnippets(context);
        Utils.predModules = new Object();
        var pred, mod;
        for (var p in Utils.snippets) {
            if (p.indexOf(":") > 0) {
                _a = p.split(":"), mod = _a[0], pred = _a[1];
                if (Utils.predModules[pred]) {
                    Utils.predModules[pred] = Utils.predModules[pred].concat(mod);
                }
                else {
                    Utils.predModules[pred] = [mod];
                }
            }
        }
        var _a;
    };
    Utils.getPredModules = function (pred) {
        return Utils.predModules[pred] ? Utils.predModules[pred] : [];
    };
    Utils.getBuiltinNames = function () {
        var builtins = Object.getOwnPropertyNames(Utils.snippets);
        builtins = builtins.filter(function (name) {
            return !/:/.test(name) && /\//.test(name);
        });
        builtins = builtins.map(function (name) {
            return name.match(/(.+)\//)[1];
        });
        builtins = builtins.filter(function (item, index, original) {
            return !/\W/.test(item) && original.indexOf(item) == index;
        });
        return builtins;
    };
    Utils.getPredicateUnderCursor = function (doc, position) {
        var wordRange = doc.getWordRangeAtPosition(position);
        if (!wordRange) {
            return "";
        }
        var predName = doc.getText(wordRange);
        var re = new RegExp("^" + predName + "\\s*\\(");
        var text = doc
            .getText()
            .split("\n")
            .slice(position.line)
            .join("")
            .slice(wordRange.start.character)
            .replace(/\s+/g, " ");
        if (re.test(text)) {
            var i = text.indexOf("(") + 1;
            var matched = 1;
            while (matched > 0) {
                if (text.charAt(i) === "(") {
                    matched++;
                    i++;
                    continue;
                }
                if (text.charAt(i) === ")") {
                    matched--;
                    i++;
                    continue;
                }
                i++;
            }
            text = text.slice(0, i);
        }
        else {
            text = predName;
        }
        return text;
    };
    Utils.getPredicateArity = function (pred) {
        var re = /^\w+\((.+)\)$/;
        if (!re.test(pred)) {
            return 0;
        }
        var args = ["-f", "none", "-q"];
        var plCode = "\n      outputArity :-\n        read(Term),\n        functor(Term, _, Arity),\n        format(\"arity=~d~n\", [Arity]).\n    ";
        var result = Utils.execPrologSync(args, plCode, "outputArity", pred, /arity=(\d+)/);
        return result ? parseInt(result[1]) : -1;
    };
    Utils.execPrologSync = function (args, clause, call, inputTerm, resultReg) {
        var section = vscode_1.workspace.getConfiguration("prolog");
        var exec = section.get("executablePath", "swipl");
        var plCode = jsesc(clause, { quotes: "double" });
        var input = "\n      open_string(\"" + plCode + "\", Stream), \n      load_files(runprolog, [stream(Stream)]).\n      " + call + ". \n      " + inputTerm + ".\n      halt.\n    ";
        var runOptions = {
            cwd: vscode_1.workspace.rootPath,
            encoding: "utf8",
            input: input
        };
        var prologProcess = cp.spawnSync(exec, args, runOptions);
        if (prologProcess.status === 0) {
            var output = prologProcess.stdout.toString();
            var err = prologProcess.stderr.toString();
            var match = output.match(resultReg);
            return match ? match : null;
        }
        else {
            console.log("Error: " + prologProcess.error.message);
            return null;
        }
    };
    Utils.insertBuiltinsToSyntaxFile = function (context) {
        var syntaxFile = context.extensionPath + "/syntaxes/prolog.tmLanguage.yaml";
        YAML.load(syntaxFile, function (obj) {
            var builtins = Utils.getBuiltinNames().join("|");
            obj.repository.builtin.patterns[1].match = "\\b(" + builtins + ")\\b";
            var newSnippets = YAML.stringify(obj, 5);
            fs.writeFile(syntaxFile, newSnippets, function (err) {
                return console.error(err);
            });
        });
    };
    return Utils;
}());
Utils.snippets = null;
Utils.predModules = null;
exports["default"] = Utils;
