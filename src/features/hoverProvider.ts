"use strict";
import {
  HoverProvider,
  MarkedString,
  Position,
  TextDocument,
  CancellationToken,
  Hover,
  Range
} from "vscode";
import Utils from "../utils/utils";

export default class PrologHoverProvider implements HoverProvider {
  // escape markdown syntax tokens: http://daringfireball.net/projects/markdown/syntax#backslash
  private textToMarkedString(text: string): MarkedString {
    return text.replace(/[\\`*_{}[\]()#+\-.!]/g, "\\$&");
  }
  public provideHover(
    doc: TextDocument,
    position: Position,
    token: CancellationToken
  ): Hover {
    let wordRange: Range = doc.getWordRangeAtPosition(position);
    if (!wordRange) {
      return;
    }
    let pred = Utils.getPredicateUnderCursor(doc, position);
    if (!pred) {
      return;
    }
    if (pred.arity < 0) {
      return;
    }
    // let functor = arity > 0 ? pred.substring(0, pred.indexOf("(")) : pred;
    // let pi = functor + "/" + arity;
    let modules: string[] = Utils.getPredModules(pred.pi);
    let contents: MarkedString[] = [];
    let desc = Utils.getPredDescriptions(pred.pi);
    if (desc !== "") {
      contents.push({ language: "prolog", value: desc });
    }
    if (modules.length > 0) {
      modules.forEach(module => {
        contents.push(module + ":" + pred.pi + "\n");
        let desc = Utils.getPredDescriptions(module + ":" + pred.pi);
        contents.push({ language: "prolog", value: desc });
      });
    }
    return contents === [] ? null : new Hover(contents, wordRange);
  }
}
