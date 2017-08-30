import {
  Location,
  TextDocument,
  Position,
  workspace,
  Uri,
  Range,
  OutputChannel,
  WorkspaceEdit,
  window
} from "vscode";
import { IPredicate, Utils } from "../utils/utils";
import { spawn } from "process-promises";
import * as fif from "find-in-files";
import * as fs from "fs";

interface IClauseRefs {
  [file: string]: { [clauseLine: number]: number };
}

export class PrologRefactor {
  private _executable: string;
  private _locations: Location[] = [];
  private _clauseRefs: IClauseRefs = null;
  private _outputChannel: OutputChannel;
  private _isBuiltin: boolean = false;
  private _defLocFound: boolean = false;

  // pick predicate at pos in doc
  constructor() {
    let section = workspace.getConfiguration("prolog");
    this._executable = section.get<string>("executablePath", "swipl");
    this._outputChannel = window.createOutputChannel("PrologFormatter");
    this._locations = [];
    this._clauseRefs = {};
  }

  // pick predicate at pos in doc
  public refactorPredUnderCursor() {
    let doc: TextDocument = window.activeTextEditor.document;
    let pos: Position = window.activeTextEditor.selection.active;

    let pred: IPredicate = Utils.getPredicateUnderCursor(doc, pos);
    this.findFilesAndRefs(pred, true).then(refLocs => {
      if (this._isBuiltin) {
        window
          .showInformationMessage(
            `'${pred.pi}' is a builtin predicate, so its definition cannot be refactored. Are you still SURE to refactor its all references?`,
            "yes",
            "no"
          )
          .then(answer => {
            if (answer !== "yes") {
              return;
            }
            this.applyRefactoring(pred, refLocs);
          });
      } else {
        window
          .showInformationMessage(
            `'Are you SURE to refactor '${pred.pi}' in all its references and definition? You'd better to commit current stage of the VCS to rollback refactoring if necessary.`,
            "yes",
            "no"
          )
          .then(answer => {
            if (answer !== "yes") {
              return;
            }
            this.applyRefactoring(pred, refLocs);
          });
      }
    });
  }

  private async applyRefactoring(pred: IPredicate, refLocs: Location[]) {
    let newPredName = await window.showInputBox({
      prompt: `Input new predicate name to replace ${pred.functor}`,
      placeHolder: pred.functor,
      ignoreFocusOut: true,
      validateInput: value => {
        if (/\s/.test(value)) {
          return "Predicate name must not contain any spaces, tab and new line.";
        }
        if (/^[A-Z_0-9]/.test(value)) {
          return "Predicate name must not start with capital letters, digitals and underscore.";
        }
        if (/^$/.test(value)) {
          return "$ is reserved as starting letter of predicate names.";
        }
        return null;
      }
    });
    await Promise.all(
      refLocs.map(async refLoc => {
        let edit = new WorkspaceEdit();
        edit.replace(refLoc.uri, refLoc.range, newPredName);
        return await workspace.applyEdit(edit);
      })
    );
    await Promise.all(
      workspace.textDocuments.map(async doc => {
        return await doc.save();
      })
    );

    return;
  }

  public async findAllRefs(): Promise<Location[]> {
    let doc: TextDocument = window.activeTextEditor.document;
    let pos: Position = window.activeTextEditor.selection.active;

    let pred: IPredicate = Utils.getPredicateUnderCursor(doc, pos);
    return await this.findFilesAndRefs(pred);
  }

  public async findFilesAndRefs(
    pred: IPredicate,
    includingDefLoc = false
  ): Promise<Location[]> {
    await Promise.all(
      workspace.textDocuments.map(async doc => {
        return await doc.save();
      })
    );

    let files = await fif.find(pred.functor, workspace.rootPath, ".pl$");
    for (let file in files) {
      let defLoc = includingDefLoc && !this._defLocFound;
      await this.loadFileAndFindRefs(pred.pi, file, defLoc);
    }
    return this._locations;
  }

  private async loadFileAndFindRefs(
    pi: string,
    file: string,
    includingDefLoc = false
  ) {
    let input = `
        use_module('${__dirname}/findallrefs').
        load_files('${file}').
        findrefs:findrefs(${pi}, ${includingDefLoc}).
        halt.
      `;
    let runOptions = {
      cwd: workspace.rootPath,
      encoding: "utf8",
      input: input
    };

    try {
      await spawn(this._executable, ["-q"], { cwd: workspace.rootPath })
        .on("process", proc => {
          if (proc.pid) {
            proc.stdin.write(input);
            proc.stdin.end();
          }
        })
        .on("stdout", output => {
          if (/{"reference":"built_in or foreign"}/.test(output)) {
            this._isBuiltin = true;
          }
          if (/{"reference":"definition location found"}/.test(output)) {
            this._defLocFound = true;
          }
          let refReg = /\{"reference":\s*(\{.+?\})\}/g;
          let match: RegExpExecArray = refReg.exec(output);
          while (match) {
            let ref: { file: string; line: number; char: number } = JSON.parse(
              match[1]
            );
            //relocate if ref points to start of the clause
            let lines = fs
              .readFileSync(ref.file)
              .toString()
              .split("\n");
            let predName = pi.split("/")[0];
            if (predName.indexOf(":") > -1) {
              predName = predName.split(":")[1];
            }
            if (
              !new RegExp("^" + predName).test(lines[ref.line].slice(ref.char))
            ) {
              let clauseStart = ref.line;
              let start = ref.line;
              if (
                this._clauseRefs[ref.file] &&
                this._clauseRefs[ref.file][clauseStart]
              ) {
                start = this._clauseRefs[ref.file][clauseStart] + 1;
              }
              let str = lines.slice(start).join("\n");
              let index = str.indexOf(predName);
              if (index > -1) {
                str = str.slice(0, index);
                let strLines = str.split("\n");
                ref.line = start + strLines.length - 1;
                ref.char = strLines[strLines.length - 1].length;
                if (this._clauseRefs[ref.file]) {
                  this._clauseRefs[ref.file][clauseStart] = ref.line;
                } else {
                  this._clauseRefs[ref.file] = {};
                  this._clauseRefs[ref.file][clauseStart] = ref.line;
                }
              }
            }
            this._locations.push(
              new Location(
                Uri.file(ref.file),
                new Range(
                  ref.line,
                  ref.char,
                  ref.line,
                  ref.char + predName.length
                )
              )
            );
            match = refReg.exec(output);
          }
        })
        .on("stderr", err => {
          this._outputChannel.append(err + "\n");
          this._outputChannel.show();
        });
    } catch (error) {
      let message: string = null;
      if ((<any>error).code === "ENOENT") {
        message = `Cannot debug the prolog file. The Prolog executable was not found. Correct the 'prolog.executablePath' configure please.`;
      } else {
        message = error.message
          ? error.message
          : `Failed to run swipl using path: ${this
              ._executable}. Reason is unknown.`;
      }
    }
  }
}
