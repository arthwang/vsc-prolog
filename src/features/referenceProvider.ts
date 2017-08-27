import Utils from "../utils/utils";
import {
  ReferenceProvider,
  TextDocument,
  Position,
  ReferenceContext,
  CancellationToken,
  Location,
  workspace,
  Uri,
  Range,
  OutputChannel,
  window
} from "vscode";
import { spawn } from "process-promises";
import * as cp from "child_process";
import * as fif from "find-in-files";
import * as fs from "fs";

export class PrologReferenceProvider implements ReferenceProvider {
  private _locations: Location[] = [];
  private _outputChannel: OutputChannel;
  private _executable: string;
  private _clauseRefs: {
    [file: string]: { [clauseLine: number]: number };
  } = {};
  constructor() {
    this._outputChannel = window.createOutputChannel("PrologFormatter");
    let section = workspace.getConfiguration("prolog");
    this._executable = section.get<string>("executablePath", "swipl");
  }
  public async provideReferences(
    doc: TextDocument,
    position: Position,
    context: ReferenceContext,
    token: CancellationToken
  ): Promise<Location[]> {
    this._locations = [];
    this._clauseRefs = {};
    let pred = Utils.getPredicateUnderCursor(doc, position);
    await this.findFilesAndRefs(pred.functor, pred.pi);
    return this._locations;
  }

  private async findFilesAndRefs(predName: string, pi: string) {
    await Promise.all(
      workspace.textDocuments.map(async doc => {
        return await doc.save();
      })
    );

    let files = await fif.find(predName, workspace.rootPath, ".pl$");
    for (let file in files) {
      await this.loadFileAndFindRefs(pi, file);
    }
  }

  private async loadFileAndFindRefs(pi: string, file: string) {
    let predLen = pi.indexOf("/") !== -1 ? pi.indexOf("/") : pi.length;
    let input = `
        use_module('${__dirname}/findallrefs').
        load_files('${file}').
        findrefs:findrefs(${pi}).
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
          let refReg = /\{"reference":\s*(\{.+?\})\}/g;
          let match: RegExpExecArray = refReg.exec(output);
          while (match) {
            let ref: { file: string; line: number; char: number } = JSON.parse(
              match[1]
            );
            //relocate if ref points to start of the clause
            let lines = fs.readFileSync(ref.file).toString().split("\n");
            let predName = pi.split("/")[0];
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
                new Range(ref.line, ref.char, ref.line, ref.char + predLen)
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
