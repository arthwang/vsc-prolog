"use strict";
exports.__esModule = true;
var vscode_1 = require("vscode");
function loadEditHelpers(subscriptions) {
    subscriptions.push(vscode_1.languages.setLanguageConfiguration("prolog", {
        indentationRules: {
            decreaseIndentPattern: /(.*:-\s*|.*-->\s*|.*:->\s*|.*:<-\s*|\s*\)|\s*\])$/,
            increaseIndentPattern: /(.*:-\s*|.*-->\s*|.*:->\s*|.*:<-\s*|.+\[|.+\()$/
        },
        wordPattern: /(-?\d*\.\d\w*)|([^\`\~\!\@\#\%\^\&\*\(\)\-\=\+\[\{\]\}\\\|\;\:\'\"\,\.\<\>\/\?\s]+)/g,
        onEnterRules: [
            {
                beforeText: /.+\.$/,
                action: { indentAction: vscode_1.IndentAction.Outdent }
            },
            {
                beforeText: /.+\([^\)]*$/,
                action: { indentAction: vscode_1.IndentAction.Indent }
            },
            {
                // e.g. /** | */
                beforeText: /^\s*\/\*\*(?!\/)([^\*]|\*(?!\/))*$/,
                afterText: /^\s*\*\/$/,
                action: {
                    indentAction: vscode_1.IndentAction.IndentOutdent,
                    appendText: " * "
                }
            },
            {
                // e.g. /** ...|
                beforeText: /^\s*\/\*\*(?!\/)([^\*]|\*(?!\/))*$/,
                action: { indentAction: vscode_1.IndentAction.None, appendText: " * " }
            },
            {
                // e.g.  * ...|
                beforeText: /^(\t|(\ \ ))*\ \*(\ ([^\*]|\*(?!\/))*)?$/,
                action: { indentAction: vscode_1.IndentAction.None, appendText: "* " }
            },
            {
                // e.g.  */|
                beforeText: /^(\t|(\ \ ))*\ \*\/\s*$/,
                action: { indentAction: vscode_1.IndentAction.None, removeText: 1 }
            },
            {
                // e.g.  *-----*/|
                beforeText: /^(\t|(\ \ ))*\ \*[^/]*\*\/\s*$/,
                action: { indentAction: vscode_1.IndentAction.None, removeText: 1 }
            }
        ]
    }));
    function getPreviousClauseHead(doc, line) {
        if (line <= 0) {
            return "";
        }
        var txt = doc.lineAt(line).text;
        var regex = new RegExp("^\\s*(.+)(:-|-->)");
        if (regex.test(txt)) {
            return txt.match(regex)[1];
        }
        regex = new RegExp("^\\s*(.+)\\.$");
        if (regex.test(txt)) {
            var i = line - 1;
            while (/^\s*$/.test(doc.lineAt(i).text))
                i--;
            if (doc.lineAt(i).text.endsWith(".")) {
                return txt.match(regex)[1];
            }
        }
        return getPreviousClauseHead(doc, line - 1);
    }
    function isRecursive(doc, line) {
        if (line <= 0) {
            return false;
        }
        var i = line - 1;
        while (/^\s*$/.test(doc.lineAt(i).text))
            i--;
        return /,$/.test(doc.lineAt(i).text) ? true : false;
    }
    function nextRecursiceParams(doc, line, originalHead) {
        if (!/\(/.test(originalHead)) {
            return originalHead;
        }
        var regex = new RegExp("([^(]+)\\((.+)\\)\\s*$");
        var match = originalHead.match(regex);
        var origList = match[2].split(",");
        var newList = origList.map(function (param) {
            var param1 = param.trim();
            var match = param1.match(/^\[.+\|(.+)\]$/);
            if (match) {
                return match[1];
            }
            else if (/^[A-Z]/.test(param1)) {
                var i = line;
                while (!/:-/.test(doc.lineAt(i).text)) {
                    var match_1 = doc
                        .lineAt(i)
                        .text.match("^\\s*(\\w+)\\s+is\\s+.*\\b" + param1 + "\\b");
                    if (match_1) {
                        return match_1[1];
                    }
                    else {
                        i--;
                    }
                }
                return param1;
            }
            else
                return param1;
        });
        return match[1] + "(" + newList.join(", ") + ")";
    }
    vscode_1.workspace.onDidChangeTextDocument(function (e) {
        var lastChange = e.contentChanges[0];
        var lastChar = lastChange.text;
        var range = lastChange.range;
        var start = range.start;
        var line = start.line;
        var col = start.character;
        var editor = vscode_1.window.activeTextEditor;
        var lineTxt = e.document.lineAt(line).text;
        if (lastChar === "_") {
            var before_1 = lineTxt.substring(0, col);
            var after_1 = lineTxt.substring(col + 1);
            if (before_1.lastIndexOf(")") < before_1.lastIndexOf("(") &&
                /\W$/.test(before_1) &&
                /^\w/.test(after_1)) {
                var varLength_1 = after_1.match("^(\\w+)\\b")[1].length;
                editor.edit(function (edit) {
                    edit["delete"](new vscode_1.Range(new vscode_1.Position(line, col + 1), new vscode_1.Position(line, col + varLength_1 + 1)));
                });
            }
        }
        else if (/^\s*\.$/.test(lineTxt)) {
            var prevHead_1 = getPreviousClauseHead(e.document, line - 1);
            if (isRecursive(e.document, line)) {
                prevHead_1 = nextRecursiceParams(e.document, line - 1, prevHead_1);
            }
            editor
                .edit(function (edit) {
                edit.replace(new vscode_1.Range(start, new vscode_1.Position(line, col + 1)), prevHead_1);
            })
                .then(function () {
                var loc = prevHead_1.indexOf("(");
                loc = loc > -1 ? loc + 1 : prevHead_1.length - 1;
                var end = new vscode_1.Position(line, col + loc);
                editor.selection = new vscode_1.Selection(end, end);
            });
        }
        else {
            return;
        }
    }, null, subscriptions);
}
exports.loadEditHelpers = loadEditHelpers;
