"use strict";
import * as fs from "fs";
import * as YAML from "yamljs";
import * as cp from "child_process";
import * as jsesc from "jsesc";
import { CompleterResult } from "readline";
import { error } from "util";
import {
  ExtensionContext,
  Position,
  Range,
  TextDocument,
  workspace
} from "vscode";

interface ISnippet {
  [predIndicator: string]: {
    prefix: string;
    body: string[];
    description: string;
  };
}
interface IPredModule {
  [predicate: string]: string[];
}
export default class Utils {
  private static snippets: ISnippet = null;
  private static predModules: IPredModule = null;
  constructor() {}
  public static getPredDescriptions(pred: string): string {
    if (Utils.snippets[pred]) {
      return Utils.snippets![pred].description;
    }
    return "";
  }

  public static init(context: ExtensionContext) {
    Utils.loadSnippets(context);
    Utils.genPredicateModules(context);
  }

  private static loadSnippets(context: ExtensionContext) {
    if (Utils.snippets) {
      return;
    }
    let snippetsPath =
      context.extensionPath + "/snippets/prolog.tmLanguage.json";
    let snippets = fs.readFileSync(snippetsPath, "utf8").toString();
    Utils.snippets = JSON.parse(snippets);
  }

  private static genPredicateModules(context: ExtensionContext) {
    Utils.loadSnippets(context);
    Utils.predModules = <IPredModule>new Object();
    let pred, mod: string;
    for (let p in Utils.snippets) {
      if (p.indexOf(":") > 0) {
        [mod, pred] = p.split(":");
        if (Utils.predModules[pred]) {
          Utils.predModules[pred] = Utils.predModules[pred].concat(mod);
        } else {
          Utils.predModules[pred] = [mod];
        }
      }
    }
  }

  public static getPredModules(pred: string): string[] {
    return Utils.predModules[pred] ? Utils.predModules[pred] : [];
  }

  public static getBuiltinNames(): string[] {
    let builtins: string[] = Object.getOwnPropertyNames(Utils.snippets);
    builtins = builtins.filter(name => {
      return !/:/.test(name) && /\//.test(name);
    });
    builtins = builtins.map(name => {
      return name.match(/(.+)\//)[1];
    });
    builtins = builtins.filter((item, index, original) => {
      return !/\W/.test(item) && original.indexOf(item) == index;
    });
    return builtins;
  }

  public static getPredicateUnderCursor(
    doc: TextDocument,
    position: Position
  ): string {
    let wordRange: Range = doc.getWordRangeAtPosition(position);
    if (!wordRange) {
      return "";
    }
    let predName: string = doc.getText(wordRange);
    let re = new RegExp("^" + predName + "\\s*\\(");
    let text = doc
      .getText()
      .split("\n")
      .slice(position.line)
      .join("")
      .slice(wordRange.start.character)
      .replace(/\s+/g, " ");

    if (re.test(text)) {
      let i = text.indexOf("(") + 1;
      let matched = 1;
      while (matched > 0) {
        if (text.charAt(i) === "(") {
          matched++;
          i++;
          continue;
        }
        if (text.charAt(i) === ")") {
          matched--;
          i++;
          continue;
        }
        i++;
      }
      text = text.slice(0, i);
    } else {
      text = predName;
    }
    return text;
  }

  public static getPredicateArity(pred: string): number {
    let re = /^\w+\((.+)\)$/;
    if (!re.test(pred)) {
      return 0;
    }
    let args = ["-f", "none", "-q"];
    let plCode = `
      outputArity :-
        read(Term),
        functor(Term, _, Arity),
        format("arity=~d~n", [Arity]).
    `;

    let result = Utils.execPrologSync(
      args,
      plCode,
      "outputArity",
      pred,
      /arity=(\d+)/
    );
    return result ? parseInt(result[1]) : -1;
  }

  public static execPrologSync(
    args: string[],
    clause: string,
    call: string,
    inputTerm: string,
    resultReg: RegExp
  ): string[] {
    let section = workspace.getConfiguration("prolog");
    let exec = section.get("executablePath", "swipl");

    let plCode = jsesc(clause, { quotes: "double" });
    let input = `
      open_string("${plCode}", Stream), 
      load_files(runprolog, [stream(Stream)]).
      ${call}. 
      ${inputTerm}.
      halt.
    `;

    let runOptions = {
      cwd: workspace.rootPath,
      encoding: "utf8",
      input: input
    };

    let prologProcess = cp.spawnSync(exec, args, runOptions);
    if (prologProcess.status === 0) {
      let output = prologProcess.stdout.toString();
      let err = prologProcess.stderr.toString();
      let match = output.match(resultReg);
      return match ? match : null;
    } else {
      console.log("Error: " + prologProcess.error.message);
      return null;
    }
  }

  public static insertBuiltinsToSyntaxFile(context: ExtensionContext) {
    let syntaxFile = context.extensionPath + "/syntaxes/prolog.tmLanguage.yaml";
    YAML.load(syntaxFile, obj => {
      let builtins: string = Utils.getBuiltinNames().join("|");
      obj.repository.builtin.patterns[1].match = "\\b(" + builtins + ")\\b";
      let newSnippets = YAML.stringify(obj, 5);
      fs.writeFile(syntaxFile, newSnippets, err => {
        return console.error(err);
      });
    });
  }
}
