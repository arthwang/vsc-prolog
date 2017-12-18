import * as os from "os";
("use strict");
import { spawn } from "process-promises";
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
  Position,
  extensions
} from "vscode";
import * as jsesc from "js-string-escape";
// import * as jsesc from "jsesc";
import { Utils } from "../utils/utils";
import { extname } from "path";
import * as path from "path";

interface IComment {
  location: number; // character location in the range
  comment: string;
}

interface ITermInfo {
  startLine: number;
  startChar: number;
  isValid: boolean;
  termStr: string;
  comments: IComment[];
  endLine?: number;
  endChar?: number;
  charsSofar?: number;
}

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
  private _textEdits: TextEdit[] = [];
  private _currentTermInfo: ITermInfo = null;
  private _startChars: number;

  constructor() {
    this._section = workspace.getConfiguration("prolog");
    this._tabSize = this._section.get("format.tabSize", 4);
    this._insertSpaces = this._section.get("format.insertSpaces", true);
    this._tabDistance = this._insertSpaces ? 0 : this._tabSize;
    this._executable = this._section.get("executablePath", "swipl");
    this._args = [];
    this._outputChannel = window.createOutputChannel("PrologFormatter");
  }

  private getClauseHeadStart(doc: TextDocument, line: number): Position {
    const headReg = /^\s*[\s\S]+?(?=:-|-->)/;
    const lineTxt = doc.lineAt(line).text;
    let match = lineTxt.match(headReg);
    if (match) {
      let firstNonSpcIndex = lineTxt.match(/[^\s]/).index;
      return new Position(line, firstNonSpcIndex);
    }
    line--;
    if (line < 0) {
      line = 0;
      return new Position(0, 0);
    }
    return this.getClauseHeadStart(doc, line);
  }

  private getClauseEnd(doc: TextDocument, line: number): Position {
    let lineTxt = doc.lineAt(line).text;
    let dotIndex = lineTxt.indexOf(".");
    while (dotIndex > -1) {
      if (this.isClauseEndDot(doc, new Position(line, dotIndex))) {
        return new Position(line, dotIndex + 1);
      }
      dotIndex = lineTxt.indexOf(".", dotIndex + 1);
    }
    line++;
    if (line === doc.lineCount) {
      line--;
      return new Position(line, lineTxt.length);
    }
    return this.getClauseEnd(doc, line);
  }

  private isClauseEndDot(doc: TextDocument, pos: Position): boolean {
    const txt = doc.getText();
    const offset = doc.offsetAt(pos);
    const subtxt = txt
      .slice(0, offset + 1)
      .replace(/\/\*[\s\S]*?\*\//g, "")
      .replace(/\\'/g, "")
      .replace(/\\"/g, "")
      .replace(/"[^\"]*?"/g, "")
      .replace(/'[^\']*?'/g, "")
      .replace(/%.*\n/g, "");
    const open = subtxt.lastIndexOf("/*");
    const close = subtxt.lastIndexOf("*/");
    return (
      txt.charAt(offset - 1) !== "." &&
      txt.charAt(offset + 1) !== "." &&
      (txt.charAt(offset + 1) === "" ||
        /^\W/.test(txt.slice(offset + 1, offset + 2))) &&
      subtxt.indexOf("'") === -1 &&
      subtxt.indexOf('"') === -1 &&
      !/%[^\n]*$/.test(subtxt) &&
      (open === -1 || open < close)
    );
  }

  private validRange(doc: TextDocument, initRange: Range): Range {
    const docTxt = doc.getText();
    let end = docTxt.indexOf(".", doc.offsetAt(initRange.end) - 1);
    while (end > -1) {
      if (this.isClauseEndDot(doc, doc.positionAt(end))) {
        break;
      }
      end = docTxt.indexOf(".", end + 1);
    }
    if (end === -1) {
      end = docTxt.length - 1;
    }
    let endPos = doc.positionAt(end + 1);

    let start = docTxt.slice(0, doc.offsetAt(initRange.start)).lastIndexOf(".");
    while (start > -1) {
      if (this.isClauseEndDot(doc, doc.positionAt(start))) {
        break;
      }
      start = docTxt.slice(0, start - 1).lastIndexOf(".");
    }

    if (start === -1) {
      start = 0;
    }

    if (start > 0) {
      let nonTermStart = 0;
      let re: RegExp = /^\s+|^%.*\n|^\/\*.*?\*\//;
      let txt = docTxt.slice(start + 1);
      let match = txt.match(re);
      while (match) {
        nonTermStart += match[0].length;
        match = txt.slice(nonTermStart).match(re);
      }
      start += nonTermStart;
    }
    let startPos = doc.positionAt(start === 0 ? 0 : start + 1);

    return startPos && endPos ? new Range(startPos, endPos) : null;
  }
  public provideDocumentRangeFormattingEdits(
    doc: TextDocument,
    range: Range,
    options: FormattingOptions,
    token: CancellationToken
  ): TextEdit[] | Thenable<TextEdit[]> {
    return this.getTextEdits(doc, this.validRange(doc, range));
  }

  public provideDocumentFormattingEdits(
    doc: TextDocument
  ): TextEdit[] | Thenable<TextEdit[]> {
    return this.getTextEdits(
      doc,
      new Range(
        0,
        0,
        doc.lineCount - 1,
        doc.lineAt(doc.lineCount - 1).text.length
      )
    );
  }

  public provideOnTypeFormattingEdits(
    doc: TextDocument,
    position: Position,
    ch: string,
    options: FormattingOptions,
    token: CancellationToken
  ): TextEdit[] | Thenable<TextEdit[]> {
    if (
      ch === "." &&
      doc.languageId === "prolog" &&
      this.isClauseEndDot(
        doc,
        new Position(position.line, position.character - 1)
      )
    ) {
      let range = new Range(
        position.line,
        0,
        position.line,
        position.character - 1
      );
      return this.getTextEdits(doc, this.validRange(doc, range));
    } else {
      return [];
    }
  }

  private outputMsg(msg: string) {
    this._outputChannel.append(msg);
    this._outputChannel.show(true);
  }

  private async getTextEdits(doc: TextDocument, range: Range) {
    await this.getFormattedCode(doc, range);
    return this._textEdits;
  }

  private async getFormattedCode(doc: TextDocument, range: Range) {
    this._textEdits = [];
    this._currentTermInfo = null;
    let docText = jsesc(doc.getText());
    let rangeTxt = jsesc(doc.getText(range));
    let goals: string;

    switch (Utils.DIALECT) {
      case "swi":
        this._args = ["--nodebug", "-q"];
        let pfile = jsesc(path.resolve(`${__dirname}/formatter_swi`));
        goals = `
          use_module('${pfile}').
          formatter:format_prolog_source(${this._tabSize}, ${
          this._tabDistance
        }, "${rangeTxt}", "${docText}").
        `;

        break;
      case "ecl":
        let efile = jsesc(path.resolve(`${__dirname}/formatter`));
        this._args = ["-f", efile];
        rangeTxt += " end_of_file.";
        goals = `
          format_prolog_source("${rangeTxt}", "${docText}").
        `;
        break;
      default:
        break;
    }

    let termStr = "";
    let prologProc = null;

    try {
      let prologChild = await spawn(this._executable, this._args, {
        cwd: workspace.rootPath
      })
        .on("process", proc => {
          if (proc.pid) {
            prologProc = proc;
            proc.stdin.write(goals);
            proc.stdin.end();
          }
        })
        .on("stdout", data => {
          // console.log("data:" + data);
          if (/::::::ALLOVER/.test(data)) {
            this.resolveTerms(doc, termStr, range, true);
          }
          if (/TERMSEGMENTBEGIN:::/.test(data)) {
            this.resolveTerms(doc, termStr, range);
            termStr = data + "\n";
          } else {
            termStr += data + "\n";
          }
        })
        .on("stderr", err => {
          // console.log("formatting err:" + err);
          // this.outputMsg(err);
        })
        .on("close", _ => {
          console.log("closed");
        });
    } catch (error) {
      let message: string = null;
      if ((<any>error).code === "ENOENT") {
        message = `Cannot debug the prolog file. The Prolog executable was not found. Correct the 'prolog.executablePath' configure please.`;
      } else {
        message = error.message
          ? error.message
          : `Failed to run swipl using path: ${
              this._executable
            }. Reason is unknown.`;
      }
    }
  }

  private resolveTerms(
    doc: TextDocument,
    text: string,
    range: Range,
    last: boolean = false
  ) {
    if (!/TERMSEGMENTBEGIN:::/.test(text)) {
      return;
    }
    let varsRe = /VARIABLESBEGIN:::\[([\s\S]*?)\]:::VARIABLESEND/;
    let termRe = /TERMBEGIN:::\n([\s\S]+?):::TERMEND/;
    let termPosRe = /TERMPOSBEGIN:::(\d+):::TERMPOSEND/;
    let termEndRe = /TERMENDBEGIN:::(\d+):::TERMENDEND/;
    let commsRe = /COMMENTSBIGIN:::([\s\S]*?):::COMMENTSEND/;
    let term = text.match(termRe),
      vars = text.match(varsRe),
      termPos = text.match(termPosRe),
      termEnd = text.match(termEndRe),
      comms = text.match(commsRe);
    let termCharA = parseInt(termPos[1]);
    let termCharZ = termEnd ? parseInt(termEnd[1]) : undefined;
    let commsArr: IComment[] =
      comms && comms[1] ? JSON.parse(comms[1]).comments : [];

    switch (Utils.DIALECT) {
      case "swi":
        this.resolveTermsSwi(doc, range, last, term, vars, termCharA, commsArr);
        break;
      case "ecl":
        // comments inside of clause
        let commReg = /\/\*[\s\S]*?\*\/|%.*?(?=\n)/g;
        let lastTermEnd = termCharA;
        if (commsArr && commsArr[0]) {
          lastTermEnd = commsArr[0].location;
        }
        let origTxt = doc
          .getText()
          .slice(
            doc.offsetAt(range.start) + termCharA,
            doc.offsetAt(range.start) + termCharZ
          );
        let match: RegExpExecArray;
        while ((match = commReg.exec(origTxt)) !== null) {
          let m = match[0];
          let comm: IComment = null;
          if (m.startsWith("%")) {
            let commPos = doc.positionAt(
              doc.offsetAt(range.start) + termCharA + match.index
            );
            let lineStr = doc.lineAt(commPos.line).text;
            comm = this.handleLineComment(
              doc,
              lineStr,
              m,
              match.index,
              commPos.character,
              commsArr
            );
          } else {
            comm = {
              location: match.index,
              comment: m
            };
          }
          if (comm !== null) {
            commsArr.push(comm);
          }
        }
        this.resolveTermsEcl(
          doc,
          range,
          last,
          term,
          vars,
          termCharA,
          termCharZ,
          commsArr
        );
        break;
      default:
        break;
    }
  }

  private handleLineComment(
    doc: TextDocument,
    lineStr: string,
    originalMatched: string,
    index: number,
    charPos: number,
    commsArr: IComment[]
  ): IComment {
    if (lineStr.replace(/^\s*/, "") === originalMatched) {
      return { location: index, comment: originalMatched };
    }
    let i = charPos;
    let docText = jsesc(doc.getText(), { quotes: "double" });
    while (i > -1) {
      let termStr = lineStr.slice(0, i).replace(/(,|;|\.)\s*$/, "");
      if (Utils.isValidEclTerm(docText, termStr)) {
        return { location: index + i - charPos, comment: lineStr.slice(i) };
      }
      i = lineStr.indexOf("%", i + 1);
    }
    return null;
  }
  private resolveTermsSwi(
    doc: TextDocument,
    range: Range,
    last: boolean,
    term: RegExpMatchArray,
    vars: RegExpMatchArray,
    termCharA: number,
    commsArr: IComment[]
  ) {
    let formattedTerm = this.restoreVariableNames(term[1], vars[1].split(","));
    if (last) {
      termCharA++; // end_of_file offset of memory file
    }
    if (commsArr.length > 0) {
      termCharA =
        termCharA < commsArr[0].location ? termCharA : commsArr[0].location;
      commsArr.forEach((comm: IComment) => {
        comm.location -= termCharA;
      });
    }

    if (!this._currentTermInfo) {
      this._startChars = doc.offsetAt(range.start);
      this._currentTermInfo = {
        charsSofar: 0,
        startLine: range.start.line,
        startChar: range.start.character,
        isValid: vars[1] === "givingup" ? false : true,
        termStr: formattedTerm,
        comments: commsArr
      };
    } else {
      let endPos = doc.positionAt(termCharA + this._startChars);
      this._currentTermInfo.endLine = endPos.line;
      this._currentTermInfo.endChar = endPos.character;
      if (this._currentTermInfo.isValid) {
        // preserve original gaps between terms
        let lastAfterTerm = doc
          .getText()
          .slice(this._currentTermInfo.charsSofar, termCharA + this._startChars)
          .match(/\s*$/)[0];
        this._currentTermInfo.termStr = this._currentTermInfo.termStr.replace(
          /\s*$/, // replace new line produced by portray_clause with original gaps
          lastAfterTerm
        );
        this.generateTextEdit(doc);
      }

      this._currentTermInfo.charsSofar = termCharA + this._startChars;
      this._currentTermInfo.startLine = this._currentTermInfo.endLine;
      this._currentTermInfo.startChar = this._currentTermInfo.endChar;
      this._currentTermInfo.termStr = formattedTerm;
      this._currentTermInfo.isValid = vars[1] === "givingup" ? false : true;
      this._currentTermInfo.comments = commsArr;
      if (last) {
        this._currentTermInfo.endLine = range.end.line;
        this._currentTermInfo.endChar = range.end.character;
        if (this._currentTermInfo.comments.length > 0) {
          this._currentTermInfo.termStr = "";
          this.generateTextEdit(doc);
        }
      }
    }
  }

  private resolveTermsEcl(
    doc: TextDocument,
    range: Range,
    last: boolean,
    term: RegExpMatchArray,
    vars: RegExpMatchArray,
    termCharA: number,
    termCharZ: number,
    commsArr: IComment[]
  ) {
    let formattedTerm = this.restoreVariableNames(term[1], vars[1].split(","))
      .replace(/\b_\d+\b/g, "_")
      .replace(/\s*$/, "");
    termCharA += doc.offsetAt(range.start);
    termCharZ += doc.offsetAt(range.start);
    this._currentTermInfo = {
      startLine: doc.positionAt(termCharA).line,
      startChar: doc.positionAt(termCharA).character,
      endLine: doc.positionAt(termCharZ).line,
      endChar: doc.positionAt(termCharZ).character,
      isValid: true,
      termStr: formattedTerm,
      comments: commsArr
    };
    this.generateTextEdit(doc);
  }
  private generateTextEdit(doc: TextDocument) {
    let termRange = new Range(
      this._currentTermInfo.startLine,
      this._currentTermInfo.startChar,
      this._currentTermInfo.endLine,
      this._currentTermInfo.endChar
    );
    if (this._currentTermInfo.comments.length > 0) {
      let newComms = this.mergeComments(
        doc,
        termRange,
        this._currentTermInfo.comments
      );
      this._currentTermInfo.termStr = this.getTextWithComments(
        doc,
        termRange,
        this._currentTermInfo.termStr,
        newComms
      );
    }
    if (this._currentTermInfo.termStr !== "") {
      this._textEdits.push(
        new TextEdit(termRange, this._currentTermInfo.termStr)
      );
    }
  }

  // merge adjcent comments between which there are only spaces, including new lines
  private mergeComments(
    doc: TextDocument,
    range: Range,
    comms: IComment[]
  ): IComment[] {
    let origTxt = doc.getText(range);
    let newComms: IComment[] = [];
    newComms.push(comms[0]);
    let i = 1;
    while (i < comms.length) {
      let loc = comms[i].location;
      let last = newComms.length - 1;
      let lastLoc = newComms[last].location;
      let lastComm = newComms[last].comment;
      let lastEnd = lastLoc + lastComm.length;
      let middleTxt = origTxt.slice(lastEnd, comms[i].location);
      if (middleTxt.replace(/\s|\r?\n|\t/g, "").length === 0) {
        newComms[last].comment += middleTxt + comms[i].comment;
      } else {
        newComms.push(comms[i]);
      }
      i++;
    }
    return newComms;
  }
  private getTextWithComments(
    doc: TextDocument,
    range: Range,
    formatedText: string,
    comms: IComment[]
  ): string {
    let origTxt = doc.getText(range);

    let chars = origTxt.length;
    let txtWithComm = "";
    let lastOrigPos = 0;
    for (let i = 0; i < comms.length; i++) {
      let index = comms[i].location;
      let comment = comms[i].comment;
      let origSeg = origTxt.slice(lastOrigPos, index);

      let noSpaceOrig = origSeg.replace(/[\s()]/g, "");
      lastOrigPos = index + comment.length;
      let j = 0,
        noSpaceFormatted: string = "";
      while (j < chars) {
        if (noSpaceFormatted === noSpaceOrig) {
          if (origTxt.slice(index + comment.length).startsWith(os.EOL)) {
            comment += os.EOL;
            lastOrigPos += os.EOL.length;
          }
          // if (origTxt.charAt(index + comment.length) === os.EOL) {
          //   comment += os.EOL;
          //   lastOrigPos += os.EOL.length;
          // }

          let tail = origSeg.match(/([()\s]*[()])?(\s*)$/);
          let spaces = tail[2];
          if (spaces.length > 0) {
            comment = spaces + comment;
          }
          let tail1 = tail[1] ? tail[1] : "";
          txtWithComm += formatedText.slice(0, j) + tail1 + comment;
          formatedText = formatedText
            .slice(j + tail1.length)
            .replace(/^\r?\n/, "");
          break;
        }

        let char = formatedText.charAt(j);
        if (!/[\s()]/.test(char)) {
          noSpaceFormatted += char;
        }
        j++;
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
      text = text.replace(
        new RegExp("\\b" + abc.trim() + "\\b", "g"),
        orig.trim()
      );
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
