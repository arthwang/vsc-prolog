"use strict";
exports.__esModule = true;
("use strict");
var jsesc = require("jsesc");
var process_promises_1 = require("process-promises");
var vscode_1 = require("vscode");
var utils_1 = require("../utils/utils");
var path_1 = require("path");
var RunTrigger;
(function (RunTrigger) {
    RunTrigger[RunTrigger["onType"] = 0] = "onType";
    RunTrigger[RunTrigger["onSave"] = 1] = "onSave";
})(RunTrigger = exports.RunTrigger || (exports.RunTrigger = {}));
var PrologLinter = (function () {
    function PrologLinter(context) {
        this.context = context;
        this.diagnostics = {};
        this.sortedDiagIndex = {};
        this.swiRegex = /([^:]+):\s*([^:]+):(\d+):((\d+):)?((\d+):)?\s*(.*)/;
        this.timer = null;
        this.outputChannel = null;
        this.executable = null;
        this.commandAddDynamicId = "prolog.addDynamicDirective";
        this.commandAddDynamic = vscode_1.commands.registerCommand(this.commandAddDynamicId, this.addDynamicDirective, this);
        this.commandAddUseModuleId = "prolog.addUseModule";
        this.commandAddUseModule = vscode_1.commands.registerCommand(this.commandAddUseModuleId, this.addUseModule, this);
        this.commandExportPredicateId = "prolog.exportPredicate";
        this.commandExportPredicate = vscode_1.commands.registerCommand(this.commandExportPredicateId, this.exportPredicateUnderCursor, this);
    }
    PrologLinter.prototype.getDirectiveLines = function (doc, declarativePredicate, range) {
        var textlines = doc.getText().split("\n");
        var re = new RegExp("^:-\\s+\\(?\\s*" + declarativePredicate + "\\b");
        var lines = [];
        var line = 0;
        while (line < textlines.length) {
            if (re.test(textlines[line])) {
                lines = lines.concat(line);
            }
            line++;
        }
        if (lines.length > 0) {
            return lines;
        }
        line = -1;
        textlines.filter(function (item, index) {
            if (/^:-/.test(item)) {
                line = index;
                return true;
            }
        });
        while (line >= 0 && !/.+\.(\s*%.*)*/.test(textlines[line])) {
            line++;
        }
        if (line >= 0 && line < range.start.line) {
            return [++line];
        }
        line = 0;
        var inComment = /\s*\/\*/.test(textlines[0]);
        while (inComment) {
            if (/\*\//.test(textlines[line])) {
                inComment = false;
                line++;
                break;
            }
            line++;
        }
        return [line];
    };
    PrologLinter.prototype.addDynamicDirective = function (doc, predicate, uri, range) {
        var edit = new vscode_1.WorkspaceEdit();
        var line = this.getDirectiveLines(doc, "dynamic", range)[0];
        var text = doc.lineAt(line).text;
        var pos;
        if (/:-\s+\(?dynamic/.test(text)) {
            var startChar = text.indexOf("dynamic") + 7;
            pos = new vscode_1.Position(line, startChar);
            edit.insert(uri, pos, " " + predicate + ",");
        }
        else {
            pos = new vscode_1.Position(line + 1, 0);
            edit.insert(uri, pos, ":- dynamic " + predicate + ".\n");
        }
        var result = null;
        try {
            result = vscode_1.workspace.applyEdit(edit);
        }
        catch (e) {
            console.log("Error in add dynamic declaration: " + e);
        }
        return result;
    };
    PrologLinter.prototype.addUseModule = function (doc, predicate, module, uri, range) {
        var edit = new vscode_1.WorkspaceEdit();
        var lines = this.getDirectiveLines(doc, "use_module", range);
        var pred = predicate.match(/(.+)\/\d+/)[1];
        var re = new RegExp("^:-\\s+use_module\\s*\\(\\s*.+\\b" + module + "\\b");
        var directiveLine = -1;
        var pos;
        lines.forEach(function (line) {
            if (re.test(doc.lineAt(line).text)) {
                directiveLine = line;
            }
        });
        if (directiveLine >= 0) {
            var line = directiveLine;
            while (doc.lineAt(line).text.indexOf("[") < 0)
                line++;
            var startChar = doc.lineAt(line).text.indexOf("[");
            pos = new vscode_1.Position(line, startChar + 1);
            edit.insert(uri, pos, predicate + ",");
        }
        else {
            pos = new vscode_1.Position(lines[lines.length - 1], 0);
            edit.insert(uri, pos, ":- use_module(library(" + module + "), [" + predicate + "]).\n");
        }
        var result = null;
        try {
            result = vscode_1.workspace.applyEdit(edit);
        }
        catch (e) {
            console.log("Error in add dynamic declaration: " + e);
        }
        return result;
    };
    PrologLinter.prototype.provideCodeActions = function (document, range, context, token) {
        var _this = this;
        var codeActions = [];
        context.diagnostics.forEach(function (diagnostic) {
            var regex = /Predicate (.+) not defined/;
            var match = diagnostic.message.match(regex);
            if (match[1]) {
                var pred_1 = match[1];
                var modules = utils_1["default"].getPredModules(pred_1);
                if (modules.length > 0) {
                    modules.forEach(function (module) {
                        codeActions.push({
                            title: "Add ':- use_module(library(" + module + "), [" + pred_1 + "]).'",
                            command: _this.commandAddUseModuleId,
                            arguments: [
                                document,
                                pred_1,
                                module,
                                document.uri,
                                diagnostic.range
                            ]
                        });
                    });
                }
                else {
                    var match_1 = document.getText().match(/:-\s*module\((\w+),/);
                    var module_1 = "";
                    if (match_1) {
                        module_1 = match_1[1];
                    }
                    if (pred_1.indexOf(":") > -1) {
                        var _a = pred_1.split(":"), mod = _a[0], pred1 = _a[1];
                        if (mod === module_1) {
                            pred_1 = pred1;
                        }
                    }
                    codeActions.push({
                        title: "Add ':- dynamic " + pred_1 + ".'",
                        command: _this.commandAddDynamicId,
                        arguments: [document, pred_1, document.uri, diagnostic.range]
                    });
                }
            }
        });
        return codeActions;
    };
    PrologLinter.prototype.parseIssue = function (issue) {
        var match = issue.match(this.swiRegex);
        if (match == null)
            return null;
        var fileName = match[2];
        var severity;
        if (match[1] == "ERROR")
            severity = vscode_1.DiagnosticSeverity.Error;
        else if (match[1] == "Warning")
            severity = vscode_1.DiagnosticSeverity.Warning;
        var line = parseInt(match[3]) - 1;
        // move up to above line if the line to mark error is empty
        line = line < 0 ? 0 : line;
        var fromCol = match[5] ? parseInt(match[5]) : 0;
        fromCol = fromCol < 0 ? 0 : fromCol;
        var toCol = match[7] ? parseInt(match[7]) : 200;
        var fromPos = new vscode_1.Position(line, fromCol);
        var toPos = new vscode_1.Position(line, toCol);
        var range = new vscode_1.Range(fromPos, toPos);
        var showMsg = match[1] + ":\t" + fileName + ": Line " + line + ": " + match[8];
        this.outputChannel.append(showMsg + "\n");
        var diag = new vscode_1.Diagnostic(range, match[8], severity);
        if (diag) {
            if (!this.diagnostics[fileName]) {
                this.diagnostics[fileName] = [diag];
            }
            else {
                this.diagnostics[fileName].push(diag);
            }
        }
    };
    PrologLinter.prototype.doPlint = function (textDocument) {
        var _this = this;
        if (textDocument.languageId != "prolog") {
            return;
        }
        // if (this.diagnosticCollection.get(textDocument.uri)) {
        //   return;
        // }
        // let diagnostics: Diagnostic[] = [];
        this.diagnostics[textDocument.uri.fsPath] = [];
        var options = vscode_1.workspace.rootPath ? { cwd: vscode_1.workspace.rootPath } : undefined;
        var args;
        if (this.trigger === RunTrigger.onSave) {
            args = ["-g", "halt", "-l", textDocument.fileName];
        }
        if (this.trigger === RunTrigger.onType) {
            args = ["-q"];
        }
        var lineErr = "";
        process_promises_1.spawn(this.executable, args, options)
            .on("process", function (process) {
            if (process.pid && _this.trigger === RunTrigger.onType) {
                var goals = 'open_string("' +
                    jsesc(textDocument.getText(), { quotes: "double" }) +
                    "\", S),load_files('" +
                    textDocument.fileName +
                    "', [stream(S)]), list_undefined, halt.\n";
                process.stdin.write(goals);
                process.stdin.end();
            }
            _this.outputChannel.clear();
        })
            .on("stderr", function (errStr) {
            if (/Warning:\s*$|The predicates below are not defined|at runtime using assert/.test(errStr)) {
            }
            else if (/which is referenced by/.test(errStr)) {
                var regex = /Warning:\s*(.+),/;
                var match = errStr.match(regex);
                lineErr = " Predicate " + match[1] + " not defined";
            }
            else if (/clause of /.test(errStr)) {
                var regex = /^(Warning:\s*([^:]+):)(\d+):(\d+)?/;
                var match = errStr.match(regex);
                var fileName = match[2];
                // if (textDocument.uri.fsPath === fileName) {
                var line = parseInt(match[3]);
                var char = match[4] ? parseInt(match[4]) : 0;
                // while (!/:-|-->/.test(textDocument.lineAt(line).text)) line--;
                var range = textDocument.getWordRangeAtPosition(new vscode_1.Position(line, char));
                // line++;
                var rangeStr = line + ":" + char + ":200: ";
                var lineMsg = match[1] + rangeStr + lineErr;
                _this.parseIssue(lineMsg + "\n");
                // let diagnostic: Diagnostic = this.parseIssue(
                //   textDocument,
                //   lineMsg + "\n"
                // );
                // if (diagnostic) {
                //   diagnostics.push(diagnostic);
                // }
                // this.outputChannel.append(lineMsg + "\n");
                // }
            }
            else if (/:\s*$/.test(errStr)) {
                lineErr = errStr;
            }
            else {
                if (errStr.startsWith("ERROR") || errStr.startsWith("Warning")) {
                    lineErr = errStr;
                }
                else {
                    lineErr = lineErr.concat(errStr);
                }
                _this.parseIssue(lineErr + "\n");
                // let diag = this.parseIssue(textDocument, lineErr);
                // if (diag) {
                //   diagnostics.push(diag);
                // }
                // this.outputChannel.append(lineErr + "\n");
            }
        })
            .then(function (result) {
            for (var doc in _this.diagnostics) {
                var index = _this.diagnostics[doc]
                    .map(function (diag, i) {
                    return [diag.range.start.line, i];
                })
                    .sort(function (a, b) {
                    return a[0] - b[0];
                });
                _this.sortedDiagIndex[doc] = index.map(function (item) {
                    return item[1];
                });
                _this.diagnosticCollection.set(vscode_1.Uri.file(doc), _this.diagnostics[doc]);
            }
        })["catch"](function (error) {
            var message = null;
            if (error.code === "ENOENT") {
                message = "Cannot lint the prolog file. The Prolog executable was not found. Use the 'prolog.executablePath' setting to configure";
            }
            else {
                message = error.message
                    ? error.message
                    : "Failed to run swipl using path: " + _this
                        .executable + ". Reason is unknown.";
            }
            console.log(message);
        });
    };
    PrologLinter.prototype.loadConfiguration = function () {
        var _this = this;
        var section = vscode_1.workspace.getConfiguration("prolog");
        if (section) {
            this.executable = section.get("executablePath", "swipl");
            var trigger = section.get("linter.run");
            this.trigger =
                trigger === "onSave" ? RunTrigger.onSave : RunTrigger.onType;
            if (this.documentListener) {
                this.documentListener.dispose();
            }
            if (this.openDocumentListener) {
                this.openDocumentListener.dispose();
            }
        }
        this.openDocumentListener = vscode_1.workspace.onDidOpenTextDocument(function (e) {
            _this.triggerLinter(e);
        });
        if (this.trigger === RunTrigger.onType) {
            this.delay = section.get("linter.delay");
            this.documentListener = vscode_1.workspace.onDidChangeTextDocument(function (e) {
                _this.triggerLinter(e.document);
            });
        }
        else {
            if (this.timer) {
                clearTimeout(this.timer);
            }
            this.documentListener = vscode_1.workspace.onDidSaveTextDocument(this.doPlint, this);
        }
        vscode_1.workspace.textDocuments.forEach(this.triggerLinter, this);
    };
    PrologLinter.prototype.triggerLinter = function (textDocument) {
        var _this = this;
        if (textDocument.languageId !== "prolog") {
            return;
        }
        if (this.trigger === RunTrigger.onType) {
            if (this.timer) {
                clearTimeout(this.timer);
            }
            this.timer = setTimeout(function () {
                _this.doPlint(textDocument);
            }, this.delay);
        }
        else {
            this.doPlint(textDocument);
        }
    };
    PrologLinter.prototype.activate = function () {
        var _this = this;
        var subscriptions = this.context.subscriptions;
        this.diagnosticCollection = vscode_1.languages.createDiagnosticCollection();
        vscode_1.workspace.onDidChangeConfiguration(this.loadConfiguration, this, subscriptions);
        this.loadConfiguration();
        if (this.outputChannel === null) {
            this.outputChannel = vscode_1.window.createOutputChannel("PrologLinter");
            this.outputChannel.clear();
        }
        if (this.trigger === RunTrigger.onSave) {
            vscode_1.workspace.onDidOpenTextDocument(this.doPlint, this, subscriptions);
        }
        // this.outputChannel.show();
        vscode_1.workspace.onDidCloseTextDocument(function (textDocument) {
            _this.diagnosticCollection["delete"](textDocument.uri);
        }, null, subscriptions);
    };
    PrologLinter.prototype.outputMsg = function (msg) {
        this.outputChannel.append(msg + "\n");
        this.outputChannel.show();
    };
    PrologLinter.prototype.exportPredicateUnderCursor = function () {
        var _this = this;
        var editor = vscode_1.window.activeTextEditor;
        var doc = editor.document;
        if (doc.isDirty) {
            doc.save().then(function (_) {
                _this.exportPredicateUnderCursor();
            });
        }
        var pos = editor.selection.active;
        var pred = utils_1["default"].getPredicateUnderCursor(doc, pos);
        var arity = utils_1["default"].getPredicateArity(pred);
        var wordRange = doc.getWordRangeAtPosition(pos);
        var predName = doc.getText(wordRange);
        var pi = predName + "/" + arity;
        if (arity < 0) {
            this.outputMsg(pred + " is not a valid predicate to export.");
            return;
        }
        var input = "\n    clause_location(Pred) :-\n      load_files('" + doc.fileName + "', [module(user)]),\n      (   functor(Pred, :, 2)\n      ->  Pred1 = pred\n      ;   context_module(Mod),\n          Pred1 = Mod:Pred\n      ),\n      clause(Pred1, _, R),\n      clause_property(R, file(File)),\n      clause_property(R, line_count(Line)), !,\n      format('File=~s;Line=~d~n', [File, Line]).\n    ";
        var clause_info = utils_1["default"].execPrologSync(["-q"], input, "clause_location(" + pred + ")", "true", /File=(.+);Line=(\d+)/);
        if (clause_info == null) {
            this.outputMsg(pred + " is not a valid predicate to export.");
            return;
        }
        if (clause_info[1] !== doc.fileName) {
            this.outputMsg(pred + " is not defined in active source file.");
            return;
        }
        input = "\n\n    rewrite_module_declaration(Module, PI) :-\n        setup_call_cleanup(\n            open('" + doc.fileName + "', read, S),\n            (   read_term(S, Term, [term_position(Pos)]),\n                stream_position_data(line_count, Pos, Line),\n                stream_position_data(line_position, Pos, Start),\n                (   Term=(:-module(Module1, Exports))\n                ->  (   memberchk(PI, Exports)\n                    ->  ReTerm=none,\n                        Action=none\n                    ;   NewExp=[PI|Exports],\n                        ReTerm=(:-module(Module1, NewExp)),\n                        Action=replace\n                    )\n                ;   ReTerm=(:-module(Module, [PI])),\n                    Action=insert\n                ),\n                format('Action=~s;Mod=~w;Line=~d;Start=~d;~n',\n                    [Action, ReTerm, Line, Start])\n            ),\n            close(S)\n        ).\n    ";
        var modname = path_1.basename(doc.fileName).split(".")[0];
        var modDec = utils_1["default"].execPrologSync(["-q"], input, "rewrite_module_declaration('" + modname + "', " + pi + ")", "true", /Action=(\w+);Mod=(.+);Line=(\d+);Start=(\d+)/);
        var action = modDec[1];
        var edit = new vscode_1.WorkspaceEdit();
        var lines = doc.getText().split("\n");
        var newModStr = modDec[2].replace(":-", ":- ") + ".\n\n";
        var modStartLine = parseInt(modDec[3]);
        var modStartChar = parseInt(modDec[4]);
        if (action === "insert") {
            edit.insert(vscode_1.Uri.file(doc.fileName), new vscode_1.Position(modStartLine - 1, modStartChar), newModStr);
            vscode_1.workspace.applyEdit(edit);
        }
        else if (action === "replace") {
            var modEndLine = parseInt(modDec[3]);
            while (!/\.\s*$/.test(lines[modEndLine - 1]))
                modEndLine++;
            var modEndChar = lines[modEndLine - 1].indexOf(".");
            var modRange = new vscode_1.Range(modStartLine - 1, modStartChar, modEndLine, modEndChar + 1);
            edit.replace(vscode_1.Uri.file(doc.fileName), modRange, newModStr);
            vscode_1.workspace.applyEdit(edit);
        }
        vscode_1.window
            .showInformationMessage("'" + pi + "' exported. Add structured comments to it?", "yes", "no")
            .then(function (answer) {
            if (answer !== "yes") {
                return;
            }
            // add comments
            var comm = "%!\t" + pred + "\n%\n%\n";
            // if (arity > 0) {
            //   let params = pred.match(/\(.*\)/)[1].split(",");
            //   if (params.length === arity) {
            //     params = params.map(param => {
            //       return "%\t@" + param;
            //     });
            //     comm += params.join("\n") + "\n";
            //   } else {
            //     this.outputMsg(
            //       "Can't generate parameters of the predicate. Edit manually please."
            //     );
            //   }
            // }
            edit = new vscode_1.WorkspaceEdit();
            edit.insert(vscode_1.Uri.file(doc.fileName), new vscode_1.Position(parseInt(clause_info[2]), 0), comm);
            vscode_1.workspace.applyEdit(edit);
        });
    };
    PrologLinter.prototype.dispose = function () {
        this.documentListener.dispose();
        this.openDocumentListener.dispose();
        this.diagnosticCollection.clear();
        this.diagnosticCollection.dispose();
        this.commandAddDynamic.dispose();
        this.commandAddUseModule.dispose();
        this.commandExportPredicate.dispose();
    };
    PrologLinter.prototype.nextErrLine = function () {
        this.gotoErrLine(0);
    };
    PrologLinter.prototype.prevErrLine = function () {
        this.gotoErrLine(1);
    };
    PrologLinter.prototype.gotoErrLine = function (direction) {
        var _this = this;
        //direction: 0: next, 1: previous
        var editor = vscode_1.window.activeTextEditor;
        var diagnostics = this.diagnosticCollection.get(editor.document.uri);
        if (diagnostics.length == 0) {
            return;
        }
        this.outputChannel.clear();
        var activeLine = editor.selection.active.line;
        var position, i;
        var si = this.sortedDiagIndex[editor.document.uri.fsPath];
        if (direction === 0) {
            i = 0;
            if (activeLine >= diagnostics[si.length - 1].range.start.line) {
                position = diagnostics[si[0]].range.start;
            }
            else {
                while (diagnostics[si[i]].range.start.line <= activeLine) {
                    i = i === si.length - 1 ? 0 : i + 1;
                }
                position = diagnostics[si[i]].range.start;
            }
        }
        else {
            i = si.length - 1;
            if (activeLine <= diagnostics[si[0]].range.start.line) {
                position = diagnostics[si[i]].range.start;
            }
            else {
                while (diagnostics[si[i]].range.start.line >= activeLine) {
                    i = i === 0 ? si.length - 1 : i - 1;
                }
                position = diagnostics[si[i]].range.start;
            }
        }
        editor.revealRange(diagnostics[si[i]].range, vscode_1.TextEditorRevealType.InCenter);
        diagnostics.forEach(function (item) {
            if (item.range.start.line === position.line) {
                var severity = item.severity === vscode_1.DiagnosticSeverity.Error
                    ? "ERROR:\t\t"
                    : "Warning:\t";
                _this.outputChannel.append(severity + item.message + "\n");
            }
        });
        editor.selection = new vscode_1.Selection(position, position);
        this.outputChannel.show();
    };
    return PrologLinter;
}());
exports["default"] = PrologLinter;
