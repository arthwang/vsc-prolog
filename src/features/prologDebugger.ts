import { LaunchRequestArguments } from "./prologDebugger";
import { EventEmitter } from "events";
import * as fs from "fs";
import { spawn } from "process-promises";
import Utils from "../utils/utils";
import { DebugProtocol } from "vscode-debugprotocol";
import { PrologDebugSession } from "./prologDebugSession";
import {
  StoppedEvent,
  StackFrame,
  Source,
  OutputEvent,
  TerminatedEvent
} from "vscode-debugadapter";
import { basename } from "path";
import * as Net from "net";

export interface ITraceCmds {
  continue: string[2];
  stepover: string[2];
  stepinto: string[2];
  stepout: string[2];
}
export interface LaunchRequestArguments
  extends DebugProtocol.LaunchRequestArguments {
  program?: string;
  args?: string[];
  cwd: string;
  runtimeExecutable?: string;
  runtimeArgs?: string[];
  env?: { [key: string]: string };
  startupQuery?: string;
  terminalDebuggerPort?: number;
  console?: string;
  traceCmds?: ITraceCmds;
}
export interface ITraceCmds {
  Run: string[];
  Stepin: string[];
  Stepover: string[];
  Stop: string[];
}

export interface IBreakPoint {
  sourceFile: string;
  line: number;
  id?: number;
}

interface ISourceLineLocations {
  [sourceFile: string]: number[];
}
export class PrologDebugger extends EventEmitter {
  private _prologProc = null;
  // private _traceCmds: ITraceCmds;
  // private _breakpoints: IBreakPoint[] = [];
  private _launchRequestArguments: LaunchRequestArguments;
  private _debugSession: PrologDebugSession;
  private _bpResponse: DebugProtocol.SetBreakpointsResponse;
  private _fbpResponse: DebugProtocol.SetFunctionBreakpointsResponse;
  private _soureLineLocations: ISourceLineLocations;

  // private _client: Net.Socket = null;

  constructor(
    launchRequestArguments: LaunchRequestArguments,
    debugSession: PrologDebugSession
  ) {
    super();
    this._launchRequestArguments = launchRequestArguments;
    this._debugSession = debugSession;
    this._soureLineLocations = {};
    // this._client = client || null;
    this.createPrologProc();

    console.log("prolog debugger constructed");
  }

  private getSourceLineLocations(source: string) {
    if (this._soureLineLocations[source]) {
      return;
    }
    let lines = fs.readFileSync(source).toString().split("\n");
    let lengths = lines.map(line => {
      return line.length + 1;
    });
    lengths.unshift(0);
    for (let i = 1; i < lengths.length; i++) {
      lengths[i] += lengths[i - 1];
    }
    this._soureLineLocations[source] = lengths;
  }

  private fromStartCharToLineChar(source: string, startChar: number) {
    this.getSourceLineLocations(source);
    let i = 0;
    for (; this._soureLineLocations[source][i] < startChar; i++);
    return {
      file: source,
      line: i + 1,
      startChar: startChar - this._soureLineLocations[source][i]
    };
  }
  private handleOutput(data: string) {
    let resObj;
    try {
      resObj = JSON.parse(data);
      if (Object.keys(resObj)[0] !== "response") {
        return;
      }
    } catch (error) {
      return;
    }

    switch (Object.keys(resObj.response)[0]) {
      case "breakpoints":
        this._bpResponse.body = {
          breakpoints: resObj.response.breakpoints
        };
        this.emit("responseBreakpoints", this._bpResponse);
        return;
      case "functionbps":
        this._fbpResponse.body = {
          breakpoints: resObj.response.functionbps
        };
        this.emit("responseFunctionBreakpoints", this._fbpResponse);
        return;
      case "frame":
        let frame = resObj.response.frame;
        this._debugSession.addStackFrame(frame);
        this._debugSession.sendEvent(
          new StoppedEvent(frame.name, PrologDebugSession.THREAD_ID)
        );
        return;
      case "variables":
        this._debugSession.setCurrentVariables(resObj.response.variables);
        return;

      default:
        break;
    }
  }

  public query(goal: string) {
    if (!/^\n$/.test(goal)) {
      goal = goal.replace(/\n+/g, "\n");
      let from = goal.indexOf(":");
      if (from < 0) {
        return;
      }
      this._prologProc.stdin.write(goal.substr(from + 1));
    }
  }

  private killPrologProc() {
    if (this._prologProc) {
      this._prologProc.kill();
    }
  }

  private filterOffOutput(data: string): boolean {
    const regs = [
      /^$/,
      /^TermToBeEvaluated/,
      /^EvalTermAtom/,
      /^EvalVarNames/,
      /^E =/,
      /^true\./
    ];
    for (let i = 0; i < regs.length; i++) {
      if (regs[i].test(data)) {
        return true;
      }
    }
    return false;
  }

  public get pid(): number {
    return this._prologProc.pid;
  }

  public initPrologDebugger() {
    this._prologProc.stdin.write(`
          use_module('${__dirname}/debugger').\n
          prolog_debugger:load_source_file('${this._launchRequestArguments
            .program}').
            `);
  }
  private async createPrologProc() {
    this.killPrologProc();
    let pp = await spawn(
      this._launchRequestArguments.runtimeExecutable,
      this._launchRequestArguments.runtimeArgs.concat("-q"),
      undefined
    )
      .on("process", proc => {
        if (proc.pid) {
          this._prologProc = proc;
          this.initPrologDebugger();
        }
      })
      .on("stdout", data => {
        // this._debugSession.debugOutput("\n" + data);
        if (/"response":/.test(data)) {
          this.handleOutput(data);
        } else if (!this.filterOffOutput(data)) {
          this._debugSession.debugOutput("\n" + data);
        }
      })
      .on("stderr", err => {
        this._debugSession.sendEvent(new OutputEvent(err + "\n", "stderr"));
      })
      .on("exit", () => {
        this._debugSession.sendEvent(new TerminatedEvent());
      })
      .then(result => {
        this._debugSession.debugOutput(
          "\nProlog process exit with code:" + result.exitCode
        );
      })
      .catch(error => {
        let message: string = null;
        if ((<any>error).code === "ENOENT") {
          message = `Cannot debug the prolog file. The Prolog executable was not found. Correct the 'prolog.executablePath' configure please.`;
        } else {
          message = error.message
            ? error.message
            : `Failed to run swipl using path: ${this._launchRequestArguments
                .runtimeExecutable}. Reason is unknown.`;
        }
        this._debugSession.debugOutput("\n" + message);
        throw new Error(error);
      });
  }

  private consult() {
    let fileName = this._launchRequestArguments.program;
    let goals = "['" + fileName + "'].\n";
    this._prologProc.stdin.write(goals);
  }

  public setBreakpoints(
    breakpoints: DebugProtocol.SetBreakpointsArguments,
    bpResponse: DebugProtocol.SetBreakpointsResponse
  ) {
    this._bpResponse = bpResponse;
    let path = breakpoints.source.path;
    let bps = breakpoints.breakpoints.map(bp => {
      return JSON.stringify({
        line: bp.line,
        column: bp.column,
        condition: bp.condition,
        hitCondition: bp.hitCondition
      });
    });
    let cmd = `cmd:prolog_debugger:set_breakpoints('${path}', ${JSON.stringify(
      bps.join(";")
    )}).\n`;
    this.query(cmd);
  }

  public setFunctionBreakpoints(
    args: DebugProtocol.SetFunctionBreakpointsArguments,
    response: DebugProtocol.SetFunctionBreakpointsResponse
  ) {
    let preds = args.breakpoints.map(bp => {
      return bp.name;
    });
    this._fbpResponse = response;
    let cmd = `cmd:prolog_debugger:spy_predicates([${preds}]).\n`;
    this.query(cmd);
  }
  public startup(goal: string) {
    let cmd = `cmd:prolog_debugger:startup(${goal}).\n`;
    this.query(cmd);
  }
  public dispose(): void {
    this.killPrologProc();
  }
}
