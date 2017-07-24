"use strict";
exports.__esModule = true;
var vscode_1 = require("vscode");
var utils_1 = require("../utils/utils");
var PrologHoverProvider = (function () {
    function PrologHoverProvider() {
    }
    // escape markdown syntax tokens: http://daringfireball.net/projects/markdown/syntax#backslash
    PrologHoverProvider.prototype.textToMarkedString = function (text) {
        return text.replace(/[\\`*_{}[\]()#+\-.!]/g, "\\$&");
    };
    PrologHoverProvider.prototype.provideHover = function (doc, position, token) {
        var wordRange = doc.getWordRangeAtPosition(position);
        if (!wordRange) {
            return;
        }
        var pred = utils_1["default"].getPredicateUnderCursor(doc, position);
        if (pred === "") {
            return;
        }
        var arity = utils_1["default"].getPredicateArity(pred);
        if (arity < 0) {
            return;
        }
        var functor = arity > 0 ? pred.substring(0, pred.indexOf("(")) : pred;
        var pi = functor + "/" + arity;
        var modules = utils_1["default"].getPredModules(pi);
        var contents = [];
        var desc = utils_1["default"].getPredDescriptions(pi);
        if (desc !== "") {
            contents.push({ language: "prolog", value: desc });
        }
        if (modules.length > 0) {
            modules.forEach(function (module) {
                contents.push(module + ":" + pi + "\n");
                var desc = utils_1["default"].getPredDescriptions(module + ":" + pi);
                contents.push({ language: "prolog", value: desc });
            });
        }
        return contents === [] ? null : new vscode_1.Hover(contents, wordRange);
    };
    return PrologHoverProvider;
}());
exports["default"] = PrologHoverProvider;
