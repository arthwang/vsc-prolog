import { PrologRefactor } from "./prologRefactor";
import { Utils } from "../utils/utils";
import {
  ReferenceProvider,
  TextDocument,
  Position,
  ReferenceContext,
  CancellationToken,
  Location,
  window
} from "vscode";
import { spawn } from "process-promises";

export class PrologReferenceProvider implements ReferenceProvider {
  constructor() {}
  public provideReferences(
    doc: TextDocument,
    position: Position,
    context: ReferenceContext,
    token: CancellationToken
  ): Promise<Location[]> {
    let pred = Utils.getPredicateUnderCursor(doc, position);
    return new PrologRefactor().findFilesAndRefs(pred);
  }
}
