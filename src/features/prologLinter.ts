("use strict");
import { truncate, truncateSync } from "fs";
import * as jsesc from "jsesc";
import { spawn } from "process-promises";
import {
  CancellationToken,
  CodeActionContext,
  CodeActionProvider,
  Command,
  commands,
  Diagnostic,
  DiagnosticCollection,
  DiagnosticSeverity,
  Disposable,
  ExtensionContext,
  languages,
  OutputChannel,
  Position,
  Range,
  Selection,
  TextDocument,
  TextEditorRevealType,
  Uri,
  window,
  workspace,
  WorkspaceEdit
} from "vscode";
import Utils from "../utils/utils";
import { basename } from "path";

export enum RunTrigger {
  onType,
  onSave
}
export default class PrologLinter implements CodeActionProvider {
  private commandAddDynamic: Disposable;
  private commandAddDynamicId: string;
  private commandAddUseModule: Disposable;
  private commandAddUseModuleId: string;
  private commandExportPredicate: Disposable;
  private commandExportPredicateId: string;

  private diagnosticCollection: DiagnosticCollection;
  private diagnostics: { [docName: string]: Diagnostic[] } = {};
  private filePathIds: { [id: string]: string } = {};
  private sortedDiagIndex: { [docName: string]: number[] } = {};
  private swiRegex = /([^:]+):\s*([^:]+):(\d+):((\d+):)?((\d+):)?\s*(.*)/;
  private executable: string;
  private trigger: RunTrigger;
  private timer: NodeJS.Timer = null;
  private delay: number;
  private documentListener: Disposable;
  private openDocumentListener: Disposable;
  private outputChannel: OutputChannel = null;

  constructor(private context: ExtensionContext) {
    this.executable = null;
    this.commandAddDynamicId = "prolog.addDynamicDirective";
    this.commandAddDynamic = commands.registerCommand(
      this.commandAddDynamicId,
      this.addDynamicDirective,
      this
    );
    this.commandAddUseModuleId = "prolog.addUseModule";
    this.commandAddUseModule = commands.registerCommand(
      this.commandAddUseModuleId,
      this.addUseModule,
      this
    );
    this.commandExportPredicateId = "prolog.exportPredicate";
    this.commandExportPredicate = commands.registerCommand(
      this.commandExportPredicateId,
      this.exportPredicateUnderCursor,
      this
    );
  }

  private getDirectiveLines(
    doc: TextDocument,
    declarativePredicate: string,
    range: Range
  ): number[] {
    let textlines: string[] = doc.getText().split("\n");
    let re = new RegExp("^:-\\s+\\(?\\s*" + declarativePredicate + "\\b");
    let lines: number[] = [];
    let line = 0;
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
    textlines.filter((item, index) => {
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
    let inComment = /\s*\/\*/.test(textlines[0]);
    while (inComment) {
      if (/\*\//.test(textlines[line])) {
        inComment = false;
        line++;
        break;
      }
      line++;
    }

    return [line];
  }

  private addDynamicDirective(
    doc: TextDocument,
    predicate: string,
    uri: Uri,
    range: Range
  ): Thenable<boolean> {
    let edit = new WorkspaceEdit();

    let line = this.getDirectiveLines(doc, "dynamic", range)[0];

    let text = doc.lineAt(line).text;
    let pos: Position;
    if (/:-\s+\(?dynamic/.test(text)) {
      let startChar = text.indexOf("dynamic") + 7;
      pos = new Position(line, startChar);
      edit.insert(uri, pos, " " + predicate + ",");
    } else {
      pos = new Position(line + 1, 0);
      edit.insert(uri, pos, ":- dynamic " + predicate + ".\n");
    }

    let result: Thenable<boolean> = null;
    try {
      result = workspace.applyEdit(edit);
    } catch (e) {
      console.log("Error in add dynamic declaration: " + e);
    }
    return result;
  }

  private addUseModule(
    doc: TextDocument,
    predicate: string,
    module: string,
    uri: Uri,
    range: Range
  ): Thenable<boolean> {
    let edit = new WorkspaceEdit();
    let lines = this.getDirectiveLines(doc, "use_module", range);
    let pred: string = predicate.match(/(.+)\/\d+/)[1];
    let re = new RegExp("^:-\\s+use_module\\s*\\(\\s*.+\\b" + module + "\\b");
    let directiveLine: number = -1;
    let pos: Position;
    lines.forEach(line => {
      if (re.test(doc.lineAt(line).text)) {
        directiveLine = line;
      }
    });
    if (directiveLine >= 0) {
      let line = directiveLine;
      while (doc.lineAt(line).text.indexOf("[") < 0) line++;
      let startChar = doc.lineAt(line).text.indexOf("[");
      pos = new Position(line, startChar + 1);
      edit.insert(uri, pos, predicate + ",");
    } else {
      pos = new Position(lines[lines.length - 1], 0);
      edit.insert(
        uri,
        pos,
        `:- use_module(library(${module}), [${predicate}]).\n`
      );
    }

    let result: Thenable<boolean> = null;
    try {
      result = workspace.applyEdit(edit);
    } catch (e) {
      console.log("Error in add dynamic declaration: " + e);
    }
    return result;
  }

  provideCodeActions(
    document: TextDocument,
    range: Range,
    context: CodeActionContext,
    token: CancellationToken
  ): Command[] | Thenable<Command[]> {
    let codeActions: Command[] = [];
    context.diagnostics.forEach(diagnostic => {
      let regex = /Predicate (.+) not defined/;
      let match = diagnostic.message.match(regex);
      if (match[1]) {
        let pred = match[1];
        let modules = Utils.getPredModules(pred);
        if (modules.length > 0) {
          modules.forEach(module => {
            codeActions.push({
              title:
                "Add ':- use_module(library(" + module + "), [" + pred + "]).'",
              command: this.commandAddUseModuleId,
              arguments: [
                document,
                pred,
                module,
                document.uri,
                diagnostic.range
              ]
            });
          });
        }
        match = document.getText().match(/:-\s*module\((\w+),/);
        let module: string = "";
        if (match) {
          module = match[1];
        }
        if (pred.indexOf(":") > -1) {
          let [mod, pred1] = pred.split(":");
          if (mod === module) {
            pred = pred1;
          }
        }
        codeActions.push({
          title: "Add ':- dynamic " + pred + ".'",
          command: this.commandAddDynamicId,
          arguments: [document, pred, document.uri, diagnostic.range]
        });
      }
    });
    return codeActions;
  }
  private parseIssue(issue: string) {
    let match = issue.match(this.swiRegex);
    if (match == null) return null;
    let fileName = this.filePathIds[match[2]];
    let severity: DiagnosticSeverity;
    if (match[1] == "ERROR") severity = DiagnosticSeverity.Error;
    else if (match[1] == "Warning") severity = DiagnosticSeverity.Warning;
    let line = parseInt(match[3]) - 1;
    // move up to above line if the line to mark error is empty
    line = line < 0 ? 0 : line;
    let fromCol = match[5] ? parseInt(match[5]) : 0;
    fromCol = fromCol < 0 ? 0 : fromCol;
    let toCol = match[7] ? parseInt(match[7]) : 200;
    let fromPos = new Position(line, fromCol);
    let toPos = new Position(line, toCol);
    let range = new Range(fromPos, toPos);
    let diag = new Diagnostic(range, match[8], severity);
    if (diag) {
      if (!this.diagnostics[fileName]) {
        this.diagnostics[fileName] = [diag];
      } else {
        this.diagnostics[fileName].push(diag);
      }
    }
  }

  private doPlint(textDocument: TextDocument) {
    if (textDocument.languageId != "prolog") {
      return;
    }
    this.diagnostics[textDocument.uri.fsPath] = [];
    this.diagnosticCollection.delete(textDocument.uri);
    let options = workspace.rootPath ? { cwd: workspace.rootPath } : undefined;

    let args: string[];
    if (this.trigger === RunTrigger.onSave) {
      args = ["-g", "halt", "-l", textDocument.fileName];
    }
    if (this.trigger === RunTrigger.onType) {
      args = ["-q"];
    }

    let lineErr: string = "";
    let docTxt = jsesc(textDocument.getText(), { quotes: "double" });
    let fileId = textDocument.fileName.replace(/\//g, "");
    this.filePathIds[fileId] = textDocument.fileName;
    spawn(this.executable, args, options)
      .on("process", process => {
        if (process.pid && this.trigger === RunTrigger.onType) {
          let goals = `
            open_string("${docTxt}", S),
            load_files('${fileId}', [stream(S)]).
            list_undefined.
          `;
          process.stdin.write(goals);
          process.stdin.end();
        }
        this.outputChannel.clear();
      })
      .on("stderr", (errStr: string) => {
        if (/which is referenced by/.test(errStr)) {
          let regex = /Warning:\s*(.+),/;
          let match = errStr.match(regex);
          lineErr = " Predicate " + match[1] + " not defined";
        } else if (/clause of /.test(errStr)) {
          let regex = /^(Warning:\s*([^:]+):)(\d+):(\d+)?/;
          let match = errStr.match(regex);
          let fileName = match[2];
          let line = parseInt(match[3]);
          let char = match[4] ? parseInt(match[4]) : 0;
          let rangeStr = line + ":" + char + ":200: ";
          let lineMsg = match[1] + rangeStr + lineErr;
          this.parseIssue(lineMsg + "\n");
        } else if (/:\s*$/.test(errStr)) {
          lineErr = errStr;
        } else {
          if (errStr.startsWith("ERROR") || errStr.startsWith("Warning")) {
            lineErr = errStr;
          } else {
            lineErr = lineErr.concat(errStr);
          }
          this.parseIssue(lineErr + "\n");
        }
      })
      .then(result => {
        for (let doc in this.diagnostics) {
          let index = this.diagnostics[doc]
            .map((diag, i) => {
              return [diag.range.start.line, i];
            })
            .sort((a, b) => {
              return a[0] - b[0];
            });
          this.sortedDiagIndex[doc] = index.map(item => {
            return item[1];
          });
          this.diagnosticCollection.set(Uri.file(doc), this.diagnostics[doc]);
        }

        for (let doc in this.sortedDiagIndex) {
          let si = this.sortedDiagIndex[doc];
          for (let i = 0; i < si.length; i++) {
            let diag = this.diagnostics[doc][si[i]];
            let severity =
              diag.severity === DiagnosticSeverity.Error ? "ERROR" : "Warning";
            let msg = `${basename(doc)}:line:${diag.range.start.line +
              1}:\t${severity}:\t${diag.message}\n`;
            this.outputChannel.append(msg);
          }
          this.outputChannel.show();
        }
      })
      .catch(error => {
        let message: string = null;
        if ((<any>error).code === "ENOENT") {
          message = `Cannot lint the prolog file. The Prolog executable was not found. Use the 'prolog.executablePath' setting to configure`;
        } else {
          message = error.message
            ? error.message
            : `Failed to run swipl using path: ${this
                .executable}. Reason is unknown.`;
        }
        console.log(message);
      });
  }

  private loadConfiguration(): void {
    let section = workspace.getConfiguration("prolog");
    if (section) {
      this.executable = section.get<string>("executablePath", "swipl");
      let trigger = section.get<string>("linter.run");
      this.trigger =
        trigger === "onSave" ? RunTrigger.onSave : RunTrigger.onType;
      if (this.documentListener) {
        this.documentListener.dispose();
      }
      if (this.openDocumentListener) {
        this.openDocumentListener.dispose();
      }
    }

    this.openDocumentListener = workspace.onDidOpenTextDocument(e => {
      this.triggerLinter(e);
    });

    if (this.trigger === RunTrigger.onType) {
      this.delay = section.get<number>("linter.delay");
      this.documentListener = workspace.onDidChangeTextDocument(e => {
        this.triggerLinter(e.document);
      });
    } else {
      if (this.timer) {
        clearTimeout(this.timer);
      }
      this.documentListener = workspace.onDidSaveTextDocument(
        this.doPlint,
        this
      );
    }

    workspace.textDocuments.forEach(this.triggerLinter, this);
  }

  private triggerLinter(textDocument: TextDocument) {
    if (textDocument.languageId !== "prolog") {
      return;
    }
    if (this.trigger === RunTrigger.onType) {
      if (this.timer) {
        clearTimeout(this.timer);
      }
      this.timer = setTimeout(() => {
        this.doPlint(textDocument);
      }, this.delay);
    } else {
      this.doPlint(textDocument);
    }
  }

  public activate(): void {
    let subscriptions: Disposable[] = this.context.subscriptions;
    this.diagnosticCollection = languages.createDiagnosticCollection();

    workspace.onDidChangeConfiguration(
      this.loadConfiguration,
      this,
      subscriptions
    );
    this.loadConfiguration();
    if (this.outputChannel === null) {
      this.outputChannel = window.createOutputChannel("PrologLinter");
      this.outputChannel.clear();
    }
    if (this.trigger === RunTrigger.onSave) {
      workspace.onDidOpenTextDocument(this.doPlint, this, subscriptions);
    }
    // this.outputChannel.show();
    workspace.onDidCloseTextDocument(
      textDocument => {
        this.diagnosticCollection.delete(textDocument.uri);
      },
      null,
      subscriptions
    );
  }

  private outputMsg(msg: string) {
    this.outputChannel.append(msg + "\n");
    this.outputChannel.show();
  }

  public exportPredicateUnderCursor() {
    let editor = window.activeTextEditor;
    let doc = editor.document;
    let docTxt = jsesc(doc.getText(), { quotes: "double" });
    let fileId = doc.fileName.replace("///g", "");

    let pos = editor.selection.active;
    let pred = Utils.getPredicateUnderCursor(doc, pos);
    let arity = Utils.getPredicateArity(pred);
    let wordRange: Range = doc.getWordRangeAtPosition(pos);
    let predName: string = doc.getText(wordRange);
    let pi = predName + "/" + arity;

    if (arity < 0) {
      this.outputMsg(`${pred} is not a valid predicate to export.`);
      return;
    }
    let input = `
    clause_location(Pred) :-
      open_string("${docTxt}", S),
      load_files('${fileId}', [module(user), stream(S)]),
      close(S),
      (   functor(Pred, :, 2)
      ->  Pred1 = pred
      ;   context_module(Mod),
          Pred1 = Mod:Pred
      ),
      clause(Pred1, _, R),
      clause_property(R, file(File)),
      clause_property(R, line_count(Line)), !,
      format('File=~s;Line=~d~n', [File, Line]).
    `;
    let clause_info = Utils.execPrologSync(
      ["-q"],
      input,
      `clause_location(${pred})`,
      "true",
      /File=(.+);Line=(\d+)/
    );
    if (clause_info == null) {
      this.outputMsg(`${pred} is not a valid predicate to export.`);
      return;
    }
    if (clause_info[1] !== fileId) {
      this.outputMsg(`${pred} is not defined in active source file.`);
      return;
    }

    input = `
    rewrite_module_declaration(Module, PI) :-
        setup_call_cleanup(
            open_string("${docTxt}", S),
            (   read_term(S, Term, [term_position(Pos)]),
                stream_position_data(line_count, Pos, Line),
                stream_position_data(line_position, Pos, Start),
                (   Term=(:-module(Module1, Exports))
                ->  (   memberchk(PI, Exports)
                    ->  ReTerm=none,
                        Action=none
                    ;   NewExp=[PI|Exports],
                        ReTerm=(:-module(Module1, NewExp)),
                        Action=replace
                    )
                ;   ReTerm=(:-module(Module, [PI])),
                    Action=insert
                ),
                format('Action=~s;Mod=~w;Line=~d;Start=~d;~n',
                    [Action, ReTerm, Line, Start])
            ),
            close(S)
        ).
    `;
    let modname = basename(doc.fileName).split(".")[0];
    let modDec = Utils.execPrologSync(
      ["-q"],
      input,
      `rewrite_module_declaration('${modname}', ${pi})`,
      "true",
      /Action=(\w+);Mod=(.+);Line=(\d+);Start=(\d+)/
    );
    let action = modDec[1];
    let edit = new WorkspaceEdit();
    let lines = doc.getText().split("\n");
    let newModStr = modDec[2].replace(":-", ":- ") + ".\n\n";
    let modStartLine = parseInt(modDec[3]);
    let modStartChar = parseInt(modDec[4]);
    if (action === "insert") {
      edit.insert(
        Uri.file(doc.fileName),
        new Position(modStartLine - 1, modStartChar),
        newModStr
      );
      workspace.applyEdit(edit);
    } else if (action === "replace") {
      let modEndLine = parseInt(modDec[3]);
      while (!/\.\s*$/.test(lines[modEndLine - 1])) modEndLine++;
      let modEndChar = lines[modEndLine - 1].indexOf(".");
      let modRange = new Range(
        modStartLine - 1,
        modStartChar,
        modEndLine,
        modEndChar + 1
      );
      edit.replace(Uri.file(doc.fileName), modRange, newModStr);
      workspace.applyEdit(edit);
    }
    window
      .showInformationMessage(
        `'${pi}' exported. Add structured comments to it?`,
        "yes",
        "no"
      )
      .then(answer => {
        if (answer !== "yes") {
          return;
        }
        // add comments
        let comm = "%!\t" + pred + "\n%\n%\n";
        edit = new WorkspaceEdit();
        edit.insert(
          Uri.file(doc.fileName),
          new Position(parseInt(clause_info[2]), 0),
          comm
        );
        workspace.applyEdit(edit);
      });
  }

  public dispose(): void {
    this.documentListener.dispose();
    this.openDocumentListener.dispose();
    this.diagnosticCollection.clear();
    this.diagnosticCollection.dispose();
    this.commandAddDynamic.dispose();
    this.commandAddUseModule.dispose();
    this.commandExportPredicate.dispose();
  }

  public nextErrLine() {
    this.gotoErrLine(0);
  }
  public prevErrLine() {
    this.gotoErrLine(1);
  }

  private gotoErrLine(direction: number) {
    //direction: 0: next, 1: previous
    const editor = window.activeTextEditor;
    let diagnostics = this.diagnosticCollection.get(editor.document.uri);
    if (diagnostics.length == 0) {
      return;
    }
    this.outputChannel.clear();
    const activeLine = editor.selection.active.line;
    let position: Position, i: number;
    let si = this.sortedDiagIndex[editor.document.uri.fsPath];

    if (direction === 0) {
      i = 0;
      if (activeLine >= diagnostics[si[si.length - 1]].range.start.line) {
        position = diagnostics[si[0]].range.start;
      } else {
        while (diagnostics[si[i]].range.start.line <= activeLine) {
          i = i === si.length - 1 ? 0 : i + 1;
        }
        position = diagnostics[si[i]].range.start;
      }
    } else {
      i = si.length - 1;
      if (activeLine <= diagnostics[si[0]].range.start.line) {
        position = diagnostics[si[i]].range.start;
      } else {
        while (diagnostics[si[i]].range.start.line >= activeLine) {
          i = i === 0 ? si.length - 1 : i - 1;
        }
        position = diagnostics[si[i]].range.start;
      }
    }
    editor.revealRange(diagnostics[si[i]].range, TextEditorRevealType.InCenter);

    diagnostics.forEach(item => {
      if (item.range.start.line === position.line) {
        let severity =
          item.severity === DiagnosticSeverity.Error
            ? "ERROR:\t\t"
            : "Warning:\t";
        this.outputChannel.append(severity + item.message + "\n");
      }
    });
    editor.selection = new Selection(position, position);
    this.outputChannel.show();
  }
}
