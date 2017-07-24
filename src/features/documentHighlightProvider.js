"use strict";
exports.__esModule = true;
var vscode_1 = require("vscode");
var PrologDocumentHighlightProvider = (function () {
    function PrologDocumentHighlightProvider() {
    }
    PrologDocumentHighlightProvider.prototype.provideDocumentHighlights = function (doc, position, token) {
        var docHilite = [];
        var wordRange = doc.getWordRangeAtPosition(position);
        if (!wordRange) {
            return;
        }
        var symbol = doc.getText(wordRange);
        var symbolLen = symbol.length;
        var line = 0;
        var re = new RegExp("\\b" + symbol + "\\b", "g");
        while (line < doc.lineCount) {
            var lineTxt = doc.lineAt(line).text;
            var match = re.exec(lineTxt);
            while (match) {
                docHilite.push(new vscode_1.DocumentHighlight(new vscode_1.Range(line, match["index"], line, match["index"] + symbolLen)));
                match = re.exec(lineTxt);
            }
            line++;
        }
        return docHilite;
    };
    return PrologDocumentHighlightProvider;
}());
exports["default"] = PrologDocumentHighlightProvider;
