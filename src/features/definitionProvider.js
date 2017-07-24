"use strict";
exports.__esModule = true;
var vscode_1 = require("vscode");
var utils_1 = require("../utils/utils");
var PrologDefinitionProvider = (function () {
    function PrologDefinitionProvider() {
    }
    PrologDefinitionProvider.prototype.provideDefinition = function (doc, position, token) {
        var location = null;
        var pred = utils_1["default"].getPredicateUnderCursor(doc, position);
        if (pred === "") {
            return null;
        }
        var exec = vscode_1.workspace
            .getConfiguration("prolog")
            .get("executablePath", "swipl");
        var args = ["-q", doc.fileName];
        var prologCode = "\n    source_location:-\n      read(Term),\n      current_module(Module),\n      predicate_property(Module:Term, file(File)),\n      predicate_property(Module:Term, line_count(Line)),\n      format(\"File:~s;Line:~d~n\", [File, Line]).\n      ";
        var result;
        if (doc.isDirty) {
            doc.save().then(function (_) {
                result = utils_1["default"].execPrologSync(args, prologCode, "source_location", pred, /File:(.+);Line:(\d+)/);
            });
        }
        else {
            result = utils_1["default"].execPrologSync(args, prologCode, "source_location", pred, /File:(.+);Line:(\d+)/);
        }
        if (result) {
            var fileName = result[1];
            var lineNum = parseInt(result[2]);
            location = new vscode_1.Location(vscode_1.Uri.file(fileName), new vscode_1.Position(lineNum - 1, 0));
        }
        return location;
    };
    return PrologDefinitionProvider;
}());
exports.PrologDefinitionProvider = PrologDefinitionProvider;
