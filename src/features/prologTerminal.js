"use strict";
exports.__esModule = true;
var utils_1 = require("../utils/utils");
var vscode_1 = require("vscode");
var PrologTerminal = (function () {
    function PrologTerminal() {
        var _this = this;
        this._terminal = null;
        this._termClosed = true;
        if (PrologTerminal._instance) {
            throw new Error("Error: Instatiation failed. Use PrologTerminal.getInstance() instead of new.");
        }
        this._onClose = vscode_1.window.onDidCloseTerminal(function (terminal) {
            _this._termClosed = true;
        });
        PrologTerminal._instance = this;
    }
    PrologTerminal.prototype.createPrologTerm = function () {
        if (!this._termClosed) {
            return;
        }
        var section = vscode_1.workspace.getConfiguration("prolog");
        if (section) {
            var executable = section.get("executablePath", "swipl");
            var args = section.get("terminal.runtimeArgs");
            this._terminal = vscode_1.window.createTerminal("Prolog", executable, args);
            this._termClosed = false;
        }
        else {
            throw new Error("configuration settings error: prolog");
        }
    };
    PrologTerminal.getInstance = function () {
        return PrologTerminal._instance;
    };
    PrologTerminal.prototype.sendString = function (text) {
        this.createPrologTerm();
        if (!text.endsWith(".")) {
            text += ".";
        }
        this._terminal.sendText(text);
        this._terminal.show(false);
    };
    PrologTerminal.prototype.loadDocument = function () {
        var _this = this;
        this.createPrologTerm();
        var doc = vscode_1.window.activeTextEditor.document;
        var goals = "['" + doc.fileName + "']";
        if (doc.isDirty) {
            doc.save().then(function (_) {
                _this.sendString(goals);
            });
        }
        else {
            this.sendString(goals);
        }
    };
    PrologTerminal.prototype.queryGoalUnderCursor = function () {
        var editor = vscode_1.window.activeTextEditor;
        var doc = editor.document;
        var pred = utils_1["default"].getPredicateUnderCursor(doc, editor.selection.active);
        this.loadDocument();
        if (!pred) {
            return;
        }
        this.sendString(pred);
    };
    return PrologTerminal;
}());
PrologTerminal._instance = new PrologTerminal();
exports["default"] = PrologTerminal;
