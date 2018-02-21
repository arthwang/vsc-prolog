import {
  CancellationToken,
  DefinitionProvider,
  Location,
  Position,
  Range,
  TextDocument,
  Uri,
  workspace
} from "vscode";
import * as cp from "child_process";
import { Utils } from "../utils/utils";
import * as path from "path";
import * as jsesc from "jsesc";

export class PrologDefinitionProvider implements DefinitionProvider {
  public provideDefinition(
    doc: TextDocument,
    position: Position,
    token: CancellationToken
  ): Location | Thenable<Location> {
    let location: Location = null;
    let pred = Utils.getPredicateUnderCursor(doc, position);
    if (!pred) {
      return null;
    }

    let exec = Utils.RUNTIMEPATH;
    let args: string[] = [],
      prologCode: string,
      result: string[],
      predToFind: string,
      runOptions: cp.SpawnSyncOptions;
    const fileLineRe = /File:(.+);Line:(\d+)/;

    switch (Utils.DIALECT) {
      case "swi":
        args = ["-q", doc.fileName];
        prologCode = `
        source_location:-
          (current_predicate(${pred.pi}) ->
            Pred = ${pred.wholePred}
          ; DcgArity is ${pred.arity + 2},
            functor(Term, ${pred.functor}, DcgArity),
            Pred = ${pred.module}:Term
          ),
          predicate_property(Pred, file(File)),
          predicate_property(Pred, line_count(Line)),
          format("File:~s;Line:~d~n", [File, Line]).
          `;
        if (doc.isDirty) {
          doc.save().then(_ => {
            result = Utils.execPrologSync(
              args,
              prologCode,
              "source_location",
              "",
              fileLineRe
            );
          });
        } else {
          result = Utils.execPrologSync(
            args,
            prologCode,
            "source_location",
            "",
            fileLineRe
          );
        }
        break;

      case "ecl":
        args = [];
        let lc = path.resolve(`${__dirname}/locate_clause`);
        predToFind = pred.pi.split(":")[1];
        prologCode = `ensure_loaded(['${lc}']),
          source_location('${jsesc(doc.fileName)}', ${predToFind}).
          `;
        runOptions = {
          cwd: workspace.rootPath,
          encoding: "utf8",
          input: prologCode
        };
        if (doc.isDirty) {
          doc.save().then(_ => {
            let syncPro = cp.spawnSync(Utils.RUNTIMEPATH, args, runOptions);
            if (syncPro.status === 0) {
              result = syncPro.stdout.toString().match(fileLineRe);
            }
          });
        } else {
          let syncPro = cp.spawnSync(Utils.RUNTIMEPATH, args, runOptions);
          if (syncPro.status === 0) {
            result = syncPro.stdout.toString().match(fileLineRe);
          }
        }
        break;

      default:
        break;
    }

    if (result) {
      let fileName: string = result[1];
      let lineNum: number = parseInt(result[2]);
      location = new Location(Uri.file(fileName), new Position(lineNum - 1, 0));
    }

    return location;
  }
}
