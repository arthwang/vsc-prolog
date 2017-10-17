"use strict";

import { Utils } from "../utils/utils";
import {
  Terminal,
  window,
  workspace,
  TextDocument,
  Disposable,
  OutputChannel,
  TextEditor
} from "vscode";

import { extname } from "path";

export default class PrologTerminal {
  private static _instance: PrologTerminal = new PrologTerminal();
  private static _terminal: Terminal;
  private static _document: TextDocument;
  private static _docType: string = "prolog";
  private static _openedAs: string = "prolog";

  constructor() {}

  public static init(): Disposable {
    return (<any>window).onDidCloseTerminal(terminal => {
      terminal.dispose();
    });
  }

  private static createPrologTerm() {
    if (PrologTerminal._terminal) {
      if (PrologTerminal._openedAs === PrologTerminal._docType) {
        return;
      } else {
        PrologTerminal._terminal.dispose();
      }
    }

    let section = workspace.getConfiguration("prolog");
    let title = "Prolog";
    if (section) {
      let executable = section.get<string>("executablePath", "swipl");
      let args = section.get<string[]>("terminal.runtimeArgs");
      PrologTerminal._openedAs = "prolog";
      if (PrologTerminal._docType === "logtalk" && Utils.LOGTALK !== "none") {
        executable = Utils.LOGTALK;
        args = [];
        title = "Logtlak";
        PrologTerminal._openedAs = "logtalk";
      }
      PrologTerminal._terminal = (<any>window).createTerminal(
        title,
        executable,
        args
      );
    } else {
      throw new Error("configuration settings error: prolog");
    }
  }

  public static sendString(text: string) {
    PrologTerminal.createPrologTerm();
    if (!text.endsWith(".")) {
      text += ".";
    }
    PrologTerminal._terminal.sendText(text);
    PrologTerminal._terminal.show(false);
  }

  public static loadDocument() {
    PrologTerminal._document = window.activeTextEditor.document;
    PrologTerminal._docType =
      extname(PrologTerminal._document.fileName) === ".lgt"
        ? "logtalk"
        : "prolog";
    PrologTerminal.createPrologTerm();
    let goals = "['" + PrologTerminal._document.fileName + "']";
    if (PrologTerminal._document.isDirty) {
      PrologTerminal._document.save().then(_ => {
        PrologTerminal.sendString(goals);
      });
    } else {
      PrologTerminal.sendString(goals);
    }
  }

  public static queryGoalUnderCursor() {
    let editor: TextEditor = window.activeTextEditor;
    let doc: TextDocument = editor.document;
    let pred = Utils.getPredicateUnderCursor(doc, editor.selection.active);
    if (!pred) {
      return;
    }
    PrologTerminal.loadDocument();
    let goal = pred.wholePred;
    if (goal.indexOf(":") > -1) {
      goal = goal.split(":")[1];
    }
    PrologTerminal.sendString(goal);
  }
}
