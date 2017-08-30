import { PrologRefactor } from "./prologRefactor";
import { Utils } from "../utils/utils";
import {
  ReferenceProvider,
  TextDocument,
  Position,
  ReferenceContext,
  CancellationToken,
  Location
} from "vscode";
import { spawn } from "process-promises";

export class PrologReferenceProvider implements ReferenceProvider {
  constructor() {}
  public async provideReferences(
    doc: TextDocument,
    position: Position,
    context: ReferenceContext,
    token: CancellationToken
  ): Promise<Location[]> {
    let pred = Utils.getPredicateUnderCursor(doc, position);
    return await new PrologRefactor().findFilesAndRefs(pred);
  }
}
