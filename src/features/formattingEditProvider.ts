("use strict");
import { Stream } from "stream";
import { SpawnResult } from "process-promises";
import * as cp from "child_process";
import {
  CancellationToken,
  DocumentFormattingEditProvider,
  DocumentRangeFormattingEditProvider,
  FormattingOptions,
  OnTypeFormattingEditProvider,
  OutputChannel,
  Range,
  TextDocument,
  TextEdit,
  window,
  workspace,
  WorkspaceConfiguration,
  Position
} from "vscode";
import * as jsesc from "jsesc";
import { spawn } from "process-promises";

export default class PrologDocumentFormatter
  implements DocumentRangeFormattingEditProvider,
    DocumentFormattingEditProvider,
    OnTypeFormattingEditProvider {
  private _section: WorkspaceConfiguration;
  private _tabSize: number;
  private _insertSpaces: boolean;
  private _tabDistance: number;
  private _executable: string;
  private _args: string[];
  private _outputChannel: OutputChannel;
  constructor() {
    this._section = workspace.getConfiguration("prolog");
    this._tabSize = this._section.get("format.tabSize", 4);
    this._insertSpaces = this._section.get("format.insertSpaces", true);
    this._tabDistance = this._insertSpaces ? 0 : this._tabSize;
    this._executable = this._section.get("executablePath", "swipl");
    this._args = ["-f", "none", "--nodebug", "-q"];
    this._outputChannel = window.createOutputChannel("PrologFormatter");
  }
  private validRange(doc: TextDocument, initRange: Range): Range {
    // let textLines: string[] = doc.getText().split("\n");
    let startLine = initRange.start.line,
      endLine = initRange.end.line;
    let endRe = /\.\s*$/,
      endRe1 = /%.*\.\s*$/;
    while (
      startLine > 0 &&
      !(
        endRe.test(doc.lineAt(startLine - 1).text) &&
        !endRe1.test(doc.lineAt(startLine - 1).text)
      )
    )
      startLine--;
    while (/^\s*$/.test(doc.lineAt(startLine).text)) startLine++;
    while (
      endLine < doc.lineCount &&
      !(
        endRe.test(doc.lineAt(endLine).text) &&
        !endRe1.test(doc.lineAt(endLine).text)
      )
    )
      endLine++;
    return new Range(startLine, 0, endLine, 200);
  }
  public provideDocumentRangeFormattingEdits(
    doc: TextDocument,
    range: Range,
    options: FormattingOptions,
    token: CancellationToken
  ): TextEdit[] | Thenable<TextEdit[]> {
    return this.getFormattedCode(doc, range);
  }

  public provideDocumentFormattingEdits(doc: TextDocument) {
    let range = new Range(0, 0, doc.lineCount - 1, 200);
    return this.getFormattedCode(doc, range);
  }

  public provideOnTypeFormattingEdits(
    doc: TextDocument,
    position: Position,
    ch: string,
    options: FormattingOptions,
    token: CancellationToken
  ): TextEdit[] | Thenable<TextEdit[]> {
    let lineTxt = doc.lineAt(position.line).text;
    if (ch === "." && !/^\s*\./.test(lineTxt) && /\.$/.test(lineTxt)) {
      let range = new Range(
        position.line,
        0,
        position.line,
        position.character
      );
      return this.getFormattedCode(doc, range);
    } else {
      return [];
    }
  }

  private getFormattedCode(doc: TextDocument, range: Range): TextEdit[] {
    let textEdits: TextEdit[] = [];
    let validRange = this.validRange(doc, range);
    let goals = `
      consult('${__dirname}/formatter.pl').\n
      read_and_portray_term(${this._tabSize}, ${this._tabDistance}).\n
      ${doc.getText(validRange)}
    `;
    let runOptions = {
      cwd: workspace.rootPath,
      encoding: "utf8",
      input: goals
    };
    let prologProcess = cp.spawnSync(this._executable, this._args, runOptions);
    if (prologProcess.status === 0) {
      let txt = prologProcess.stdout.toString();
      txt = txt
        .replace(/^true\.\s*/, "")
        .replace(/^\s*Stream.*?\n\s*\n/, "")
        .replace(/@#&\n*end_of_file[\s\S]*?$/, "")
        .trim();
      if (txt === "") {
        return;
      }
      let txtArrays: string[] = txt.split("@#&\n");
      txtArrays.shift();

      let varsMsg = prologProcess.stderr.toString();
      if (/error/.test(varsMsg)) {
        this._outputChannel.append("Error:" + varsMsg);
        return;
      }
      let vars = varsMsg.match(/'[^']*'/g);
      let varsArrays: Array<string>[] = vars.map(item => {
        return item.replace(/'/g, "").split(",");
      });

      txt = txtArrays
        .map((clause, i) => {
          return this.restoreVariableNames(clause, varsArrays[i]);
        })
        .join("");
      txt = this.getTextWithComments(doc, validRange, txt);
      textEdits = [new TextEdit(validRange, txt)];
    } else {
      this._outputChannel.append("Error: " + prologProcess.error.message);
    }
    return textEdits;
  }

  private getTextWithComments(
    doc: TextDocument,
    range: Range,
    formatedText: string
  ): string {
    let origTxt = doc.getText(range);
    let chars = origTxt.length;
    let txtWithComm = "";
    let lastOrigPos = 0;
    let commReg = /\s*\/\*[\s\S]*?\*\/\n*|\s*%.*?\n+/g;
    let commMatchs: RegExpExecArray;
    while ((commMatchs = commReg.exec(origTxt))) {
      let origSeg = origTxt.slice(lastOrigPos, commMatchs.index);
      let noSpaceOrig = origSeg.replace(/\s|\n|\t|\(|\)/g, "");
      lastOrigPos = commMatchs.index + commMatchs[0].length;
      let i = 0,
        noSpaceFormatted: string = "";
      while (i < chars) {
        if (noSpaceFormatted === noSpaceOrig) {
          let tail = origSeg.match(/[()]*$/)[0].length;
          txtWithComm += formatedText.slice(0, i + tail) + commMatchs[0];
          formatedText = formatedText.slice(i + tail).replace(/^\n/, "");
          break;
        }

        let char = formatedText.charAt(i);
        if (
          char !== " " &&
          char !== "\n" &&
          char !== "\t" &&
          char !== "(" &&
          char !== ")"
        ) {
          noSpaceFormatted += char;
        }
        i++;
      }
    }
    return txtWithComm + formatedText;
  }

  private restoreVariableNames(text: string, vars: string[]): string {
    if (vars.length === 1 && vars[0] === "") {
      return text;
    }
    if (vars.length === 0) {
      return text;
    }
    let dups: { newVars: string[]; dup: string[] } = this.getDups(vars);
    dups.newVars.forEach(pair => {
      let [abc, orig] = pair.split(":");
      text = text.replace(new RegExp("\\b" + abc + "\\b", "g"), orig);
    });
    return this.restoreVariableNames(text, dups.dup);
  }

  private getDups(vars: string[]) {
    let left: string[] = new Array<string>(vars.length);
    let right: string[] = new Array<string>(vars.length);
    for (let i = 0; i < vars.length; i++) {
      [left[i], right[i]] = vars[i].split(":");
    }
    let dup: string[] = [];
    let index: number;
    for (let i = 0; i < vars.length; i++) {
      if ((index = right.indexOf(left[i])) > -1) {
        let tmp = right[index] + right[index];
        while (right.indexOf(tmp) > -1) {
          tmp += right[index];
        }
        vars[index] = left[index] + ":" + tmp;
        dup.push(tmp + ":" + right[index]);
      }
    }

    return {
      newVars: vars,
      dup: dup
    };
  }
}
