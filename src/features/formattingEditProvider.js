"use strict";
exports.__esModule = true;
("use strict");
var cp = require("child_process");
var vscode_1 = require("vscode");
var PrologDocumentFormatter = (function () {
    function PrologDocumentFormatter() {
        this._section = vscode_1.workspace.getConfiguration("prolog");
        this._tabSize = this._section.get("format.tabSize", 4);
        this._insertSpaces = this._section.get("format.insertSpaces", true);
        this._tabDistance = this._insertSpaces ? 0 : this._tabSize;
        this._executable = this._section.get("executablePath", "swipl");
        this._args = ["-f", "none", "--nodebug", "-q"];
        this._outputChannel = vscode_1.window.createOutputChannel("PrologFormatter");
    }
    PrologDocumentFormatter.prototype.validRange = function (doc, initRange) {
        // let textLines: string[] = doc.getText().split("\n");
        var startLine = initRange.start.line, endLine = initRange.end.line;
        var endRe = /\.\s*$/, endRe1 = /%.*\.\s*$/;
        while (startLine > 0 &&
            !(endRe.test(doc.lineAt(startLine - 1).text) &&
                !endRe1.test(doc.lineAt(startLine - 1).text)))
            startLine--;
        while (/^\s*$/.test(doc.lineAt(startLine).text))
            startLine++;
        while (endLine < doc.lineCount &&
            !(endRe.test(doc.lineAt(endLine).text) &&
                !endRe1.test(doc.lineAt(endLine).text)))
            endLine++;
        return new vscode_1.Range(startLine, 0, endLine, 200);
    };
    PrologDocumentFormatter.prototype.provideDocumentRangeFormattingEdits = function (doc, range, options, token) {
        return this.getFormattedCode(doc, range);
    };
    PrologDocumentFormatter.prototype.provideDocumentFormattingEdits = function (doc) {
        var range = new vscode_1.Range(0, 0, doc.lineCount - 1, 200);
        return this.getFormattedCode(doc, range);
    };
    PrologDocumentFormatter.prototype.provideOnTypeFormattingEdits = function (doc, position, ch, options, token) {
        var lineTxt = doc.lineAt(position.line).text;
        if (ch === "." && !/^\s*\./.test(lineTxt) && /\.$/.test(lineTxt)) {
            var range = new vscode_1.Range(position.line, 0, position.line, position.character);
            return this.getFormattedCode(doc, range);
        }
        else {
            return [];
        }
    };
    PrologDocumentFormatter.prototype.getFormattedCode = function (doc, range) {
        var _this = this;
        var textEdits = [];
        var validRange = this.validRange(doc, range);
        var goals = "\n      consult('" + __dirname + "/formatter.pl').\n\n      read_and_portray_term(" + this._tabSize + ", " + this._tabDistance + ").\n\n      " + doc.getText(validRange) + "\n    ";
        var runOptions = {
            cwd: vscode_1.workspace.rootPath,
            encoding: "utf8",
            input: goals
        };
        var prologProcess = cp.spawnSync(this._executable, this._args, runOptions);
        if (prologProcess.status === 0) {
            var txt = prologProcess.stdout.toString();
            txt = txt
                .replace(/^true\.\s*/, "")
                .replace(/^\s*Stream.*?\n\s*\n/, "")
                .replace(/@#&\n*end_of_file[\s\S]*?$/, "")
                .trim();
            if (txt === "") {
                return [];
            }
            var txtArrays = txt.split("@#&\n");
            txtArrays.shift();
            var varsMsg = prologProcess.stderr.toString();
            if (/error/.test(varsMsg)) {
                this._outputChannel.append("Error:" + varsMsg);
                return;
            }
            var vars = varsMsg.match(/'[^']*'/g);
            var varsArrays_1 = vars.map(function (item) {
                return item.replace(/'/g, "").split(",");
            });
            txt = txtArrays
                .map(function (clause, i) {
                return _this.restoreVariableNames(clause, varsArrays_1[i]);
            })
                .join("");
            txt = this.getTextWithComments(doc, validRange, txt);
            textEdits = [new vscode_1.TextEdit(validRange, txt)];
        }
        else {
            this._outputChannel.append("Error: " + prologProcess.error.message);
        }
        return textEdits;
    };
    PrologDocumentFormatter.prototype.getTextWithComments = function (doc, range, formatedText) {
        var origTxt = doc.getText(range);
        var chars = origTxt.length;
        var txtWithComm = "";
        var lastOrigPos = 0;
        var commReg = /\s*\/\*[\s\S]*?\*\/\n*|\s*%.*?\n+/g;
        var commMatchs;
        while ((commMatchs = commReg.exec(origTxt))) {
            var origSeg = origTxt.slice(lastOrigPos, commMatchs.index);
            var noSpaceOrig = origSeg.replace(/\s|\n|\t|\(|\)/g, "");
            lastOrigPos = commMatchs.index + commMatchs[0].length;
            var i = 0, noSpaceFormatted = "";
            while (i < chars) {
                if (noSpaceFormatted === noSpaceOrig) {
                    var tail = origSeg.match(/[()]*$/)[0].length;
                    txtWithComm += formatedText.slice(0, i + tail) + commMatchs[0];
                    formatedText = formatedText.slice(i + tail).replace(/^\n/, "");
                    break;
                }
                var char = formatedText.charAt(i);
                if (char !== " " &&
                    char !== "\n" &&
                    char !== "\t" &&
                    char !== "(" &&
                    char !== ")") {
                    noSpaceFormatted += char;
                }
                i++;
            }
        }
        return txtWithComm + formatedText;
    };
    PrologDocumentFormatter.prototype.restoreVariableNames = function (text, vars) {
        if (vars.length === 1 && vars[0] === "") {
            return text;
        }
        if (vars.length === 0) {
            return text;
        }
        var dups = this.getDups(vars);
        dups.newVars.forEach(function (pair) {
            var _a = pair.split(":"), abc = _a[0], orig = _a[1];
            text = text.replace(new RegExp("\\b" + abc + "\\b", "g"), orig);
        });
        return this.restoreVariableNames(text, dups.dup);
    };
    PrologDocumentFormatter.prototype.getDups = function (vars) {
        var left = new Array(vars.length);
        var right = new Array(vars.length);
        for (var i = 0; i < vars.length; i++) {
            _a = vars[i].split(":"), left[i] = _a[0], right[i] = _a[1];
        }
        var dup = [];
        var index;
        for (var i = 0; i < vars.length; i++) {
            if ((index = right.indexOf(left[i])) > -1) {
                var tmp = right[index] + right[index];
                while (right.indexOf(tmp) > -1) {
                    tmp += right[index];
                }
                vars[index] = left[index] + ":" + tmp;
                dup.push(tmp + ":" + right[index]);
            }
        }
        return {
            newVars: vars,
            dup: dup
        };
        var _a;
    };
    return PrologDocumentFormatter;
}());
exports["default"] = PrologDocumentFormatter;
