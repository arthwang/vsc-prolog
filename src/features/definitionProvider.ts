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
import * as jsesc from "jsesc";
import { Utils } from "../utils/utils";

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

    let exec = workspace
      .getConfiguration("prolog")
      .get("executablePath", "swipl");
    let args = ["-q", doc.fileName];
    let prologCode = `
    source_location:-
      read(Term),
      current_module(Module),
      predicate_property(Module:Term, file(File)),
      predicate_property(Module:Term, line_count(Line)),
      format("File:~s;Line:~d~n", [File, Line]).
      `;

    let result: string[];
    if (doc.isDirty) {
      doc.save().then(_ => {
        result = Utils.execPrologSync(
          args,
          prologCode,
          "source_location",
          pred.wholePred,
          /File:(.+);Line:(\d+)/
        );
      });
    } else {
      result = Utils.execPrologSync(
        args,
        prologCode,
        "source_location",
        pred.wholePred,
        /File:(.+);Line:(\d+)/
      );
    }

    if (result) {
      let fileName: string = result[1];
      let lineNum: number = parseInt(result[2]);
      location = new Location(Uri.file(fileName), new Position(lineNum - 1, 0));
    }

    return location;
  }
}
