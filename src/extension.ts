"use strict";
// The module 'vscode' contains the VS Code extensibility API
// Import the module and reference it with the alias vscode in your code below
import {
  commands,
  DocumentFilter,
  ExtensionContext,
  Terminal,
  TextDocument,
  window,
  languages,
  DocumentHighlightProvider,
  Location,
  workspace
} from "vscode";
import PrologTerminal from "./features/prologTerminal";
import { loadEditHelpers } from "./features/editHelpers";
import { Utils } from "./utils/utils";
import PrologHoverProvider from "./features/hoverProvider";
import PrologDocumentHighlightProvider from "./features/documentHighlightProvider";
import PrologDocumentFormatter from "./features/formattingEditProvider";
import { PrologDefinitionProvider } from "./features/definitionProvider";
import { PrologReferenceProvider } from "./features/referenceProvider";
import PrologLinter from "./features/prologLinter";
import { PrologRefactor } from "./features/prologRefactor";
import { ensureSymlink, remove } from "fs-extra-plus";

async function initForDialect(context: ExtensionContext) {
  const section = workspace.getConfiguration("prolog");
  const dialect = section.get<string>("dialect");
  const exec = section.get<string>("executablePath", "swipl");

  Utils.DIALECT = dialect;
  Utils.RUNTIMEPATH = exec;
  Utils.LOGTALK = section.get<string>("logtalk.starter", "none");
  const exPath = context.extensionPath;
  const symLinks = [
    {
      path: `${exPath}/syntaxes`,
      srcFile: `prolog.${dialect}.tmLanguage.json`,
      targetFile: "prolog.tmLanguage.json"
    },
    {
      path: `${exPath}/snippets`,
      srcFile: `prolog.${dialect}.json`,
      targetFile: "prolog.json"
    }
  ];
  await Promise.all(
    symLinks.map(async link => {
      await remove(`${link.path}/${link.targetFile}`);
      return await ensureSymlink(
        `${link.path}/${link.srcFile}`,
        `${link.path}/${link.targetFile}`
      );
    })
  );
}
// this method is called when your extension is activated
// your extension is activated the very first time the command is executed
export async function activate(context: ExtensionContext) {
  console.log('Congratulations, your extension "vsc-prolog" is now active!');

  await initForDialect(context);

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
    },
    {
      command: "prolog.refactorPredicate",
      callback: () => {
        new PrologRefactor().refactorPredUnderCursor();
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
  context.subscriptions.push(
    languages.registerReferenceProvider(
      PROLOG_MODE,
      new PrologReferenceProvider()
    )
  );
  // context.subscriptions.push(prologDebugger);
}

// this method is called when your extension is deactivated
export function deactivate() {}
