"use strict";
exports.__esModule = true;
// The module 'vscode' contains the VS Code extensibility API
// Import the module and reference it with the alias vscode in your code below
var prologLinter_1 = require("./features/prologLinter");
var vscode_1 = require("vscode");
var prologTerminal_1 = require("./features/prologTerminal");
var editHelpers_1 = require("./features/editHelpers");
var utils_1 = require("./utils/utils");
var hoverProvider_1 = require("./features/hoverProvider");
var documentHighlightProvider_1 = require("./features/documentHighlightProvider");
var formattingEditProvider_1 = require("./features/formattingEditProvider");
var definitionProvider_1 = require("./features/definitionProvider");
// this method is called when your extension is activated
// your extension is activated the very first time the command is executed
function activate(context) {
    // Use the console to output diagnostic information (console.log) and errors (console.error)
    // This line of code will only be executed once when your extension is activated
    console.log('Congratulations, your extension "vsc-prolog" is now active!');
    // let prologDebugger = new PrologDebugger();
    var PROLOG_MODE = { language: "prolog", scheme: "file" };
    utils_1["default"].init(context);
    var linter = new prologLinter_1["default"](context);
    linter.activate();
    editHelpers_1.loadEditHelpers(context.subscriptions);
    var myCommands = [
        {
            command: "prolog.linter.nextErrLine",
            callback: function () {
                linter.nextErrLine();
            }
        },
        {
            command: "prolog.linter.prevErrLine",
            callback: function () {
                linter.prevErrLine();
            }
        },
        // {
        //   command: "prolog.linter.exportPredicate",
        //   callback: () => {
        //     linter.exportPredicateUnderCursor();
        //   }
        // },
        {
            command: "prolog.load.document",
            callback: function () {
                prologTerminal_1["default"].getInstance().loadDocument();
            }
        },
        {
            command: "prolog.query.goal",
            callback: function () {
                prologTerminal_1["default"].getInstance().queryGoalUnderCursor();
            }
        }
    ];
    myCommands.map(function (command) {
        context.subscriptions.push(vscode_1.commands.registerCommand(command.command, command.callback));
    });
    context.subscriptions.push(vscode_1.languages.registerCodeActionsProvider(PROLOG_MODE, linter));
    context.subscriptions.push(vscode_1.languages.registerHoverProvider(PROLOG_MODE, new hoverProvider_1["default"]()));
    context.subscriptions.push(vscode_1.languages.registerDocumentHighlightProvider(PROLOG_MODE, new documentHighlightProvider_1["default"]()));
    context.subscriptions.push(vscode_1.languages.registerDocumentRangeFormattingEditProvider(PROLOG_MODE, new formattingEditProvider_1["default"]()));
    context.subscriptions.push(vscode_1.languages.registerOnTypeFormattingEditProvider(PROLOG_MODE, new formattingEditProvider_1["default"](), ".", "\n"));
    context.subscriptions.push(vscode_1.languages.registerDocumentFormattingEditProvider(PROLOG_MODE, new formattingEditProvider_1["default"]()));
    context.subscriptions.push(vscode_1.languages.registerDefinitionProvider(PROLOG_MODE, new definitionProvider_1.PrologDefinitionProvider()));
    // context.subscriptions.push(prologDebugger);
}
exports.activate = activate;
// this method is called when your extension is deactivated
function deactivate() { }
exports.deactivate = deactivate;
