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

export default class PrologTerminal {
  private static _instance: PrologTerminal = new PrologTerminal();
  private _terminal: Terminal = null;
  private _onClose: Disposable;
  private _termClosed: boolean = true;

  constructor() {
    if (PrologTerminal._instance) {
      throw new Error(
        "Error: Instatiation failed. Use PrologTerminal.getInstance() instead of new."
      );
    }

    this._onClose = (<any>window).onDidCloseTerminal(terminal => {
      this._termClosed = true;
    });
    PrologTerminal._instance = this;
  }

  private createPrologTerm() {
    if (!this._termClosed) {
      return;
    }

    let section = workspace.getConfiguration("prolog");
    if (section) {
      let executable = section.get<string>("executablePath", "swipl");
      let args = section.get<string[]>("terminal.runtimeArgs");
      this._terminal = (<any>window).createTerminal("Prolog", executable, args);
      this._termClosed = false;
    } else {
      throw new Error("configuration settings error: prolog");
    }
  }

  public static getInstance(): PrologTerminal {
    return PrologTerminal._instance;
  }

  public sendString(text: string) {
    this.createPrologTerm();
    if (!text.endsWith(".")) {
      text += ".";
    }
    this._terminal.sendText(text);
    this._terminal.show(false);
  }

  public loadDocument() {
    this.createPrologTerm();
    let doc: TextDocument = window.activeTextEditor.document;
    let goals = "['" + doc.fileName + "']";
    if (doc.isDirty) {
      doc.save().then(_ => {
        this.sendString(goals);
      });
    } else {
      this.sendString(goals);
    }
  }

  public queryGoalUnderCursor() {
    let editor: TextEditor = window.activeTextEditor;
    let doc: TextDocument = editor.document;
    let pred = Utils.getPredicateUnderCursor(doc, editor.selection.active);
    this.loadDocument();
    if (!pred) {
      return;
    }
    this.sendString(pred.wholePred);
  }
}
