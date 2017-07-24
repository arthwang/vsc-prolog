"use strict";
// The module 'vscode' contains the VS Code extensibility API
// Import the module and reference it with the alias vscode in your code below
import PrologLinter from "./features/prologLinter";
import {
  commands,
  DocumentFilter,
  ExtensionContext,
  Terminal,
  TextDocument,
  window,
  languages,
  DocumentHighlightProvider
} from "vscode";
import * as jsesc from "jsesc";
import PrologTerminal from "./features/prologTerminal";
import { loadEditHelpers } from "./features/editHelpers";
import Utils from "./utils/utils";
import PrologHoverProvider from "./features/hoverProvider";
import PrologDocumentHighlightProvider from "./features/documentHighlightProvider";
import PrologDocumentFormatter from "./features/formattingEditProvider";
import { PrologDefinitionProvider } from "./features/definitionProvider";

// this method is called when your extension is activated
// your extension is activated the very first time the command is executed
export function activate(context: ExtensionContext) {
  // Use the console to output diagnostic information (console.log) and errors (console.error)
  // This line of code will only be executed once when your extension is activated
  console.log('Congratulations, your extension "vsc-prolog" is now active!');

  // let prologDebugger = new PrologDebugger();

  const PROLOG_MODE: DocumentFilter = { language: "prolog", scheme: "file" };

  Utils.init(context);

  let linter = new PrologLinter(context);
  linter.activate();

  loadEditHelpers(context.subscriptions);

  let myCommands = [
    {
      command: "prolog.linter.nextErrLine",
      callback: () => {
        linter.nextErrLine();
      }
    },
    {
      command: "prolog.linter.prevErrLine",
      callback: () => {
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
      callback: () => {
        PrologTerminal.getInstance().loadDocument();
      }
    },
    {
      command: "prolog.query.goal",
      callback: () => {
        PrologTerminal.getInstance().queryGoalUnderCursor();
      }
    }
  ];
  myCommands.map(command => {
    context.subscriptions.push(
      commands.registerCommand(command.command, command.callback)
    );
  });

  context.subscriptions.push(
    languages.registerCodeActionsProvider(PROLOG_MODE, linter)
  );
  context.subscriptions.push(
    languages.registerHoverProvider(PROLOG_MODE, new PrologHoverProvider())
  );
  context.subscriptions.push(
    languages.registerDocumentHighlightProvider(
      PROLOG_MODE,
      new PrologDocumentHighlightProvider()
    )
  );
  context.subscriptions.push(
    languages.registerDocumentRangeFormattingEditProvider(
      PROLOG_MODE,
      new PrologDocumentFormatter()
    )
  );
  context.subscriptions.push(
    languages.registerOnTypeFormattingEditProvider(
      PROLOG_MODE,
      new PrologDocumentFormatter(),
      ".",
      "\n"
    )
  );
  context.subscriptions.push(
    languages.registerDocumentFormattingEditProvider(
      PROLOG_MODE,
      new PrologDocumentFormatter()
    )
  );
  context.subscriptions.push(
    languages.registerDefinitionProvider(
      PROLOG_MODE,
      new PrologDefinitionProvider()
    )
  );
  // context.subscriptions.push(prologDebugger);
}

// this method is called when your extension is deactivated
export function deactivate() {}
