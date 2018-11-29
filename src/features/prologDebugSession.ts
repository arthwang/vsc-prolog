import {
  DebugSession,
  ErrorDestination,
  OutputEvent,
  InitializedEvent,
  StackFrame,
  Source,
  Scope,
  Thread
} from "vscode-debugadapter";
import { DebugProtocol } from "vscode-debugprotocol";
import {
  ITraceCmds,
  LaunchRequestArguments,
  PrologDebugger
} from "./prologDebugger";
import * as path from "path";
import { spawn, SpawnOptions } from "process-promises";

export class PrologDebugSession extends DebugSession {
  private static SCOPEREF = 1;
  public static THREAD_ID = 100;
  private _prologDebugger: PrologDebugger;
  private _runtimeExecutable: string;
  // private _runtimeArgs: string[];
  private _startupQuery: string;
  private _startFile: string;
  private _cwd: string;
  private _stopOnEntry: boolean;
  private _traceCmds: ITraceCmds;
  private _currentVariables: DebugProtocol.Variable[] = [];
  private _stackFrames: DebugProtocol.StackFrame[] = [];
  private _debugging: boolean;

  public constructor() {
    super();
    this.setDebuggerColumnsStartAt1(true);
    this.setDebuggerLinesStartAt1(true);
    this.setDebuggerPathFormat("native");
  }

  protected initializeRequest(
    response: DebugProtocol.InitializeResponse,
    args: DebugProtocol.InitializeRequestArguments
  ): void {
    response.body = {
      supportsConfigurationDoneRequest: true,
      supportTerminateDebuggee: true,
      supportsConditionalBreakpoints: true,
      supportsHitConditionalBreakpoints: true,
      supportsFunctionBreakpoints: true,
      supportsEvaluateForHovers: true,
      supportsExceptionOptions: true,
      supportsExceptionInfoRequest: true,
      exceptionBreakpointFilters: [
        {
          filter: "Notice",
          label: "Notices"
        },
        {
          filter: "Warning",
          label: "Warnings"
        },
        {
          filter: "Error",
          label: "Errors"
        },
        {
          filter: "Exception",
          label: "Exceptions"
        },
        {
          filter: "*",
          label: "Everything",
          default: true
        }
      ]
    };
    this.sendResponse(response);
  }

  protected attachRequest(
    response: DebugProtocol.AttachResponse,
    args: DebugProtocol.AttachRequestArguments
  ) {
    this.sendErrorResponse(
      response,
      new Error("Attach requests are not supported")
    );
    this.shutdown();
  }

  public addStackFrame(frame: {
    id: number;
    level: number;
    name: string;
    file: string;
    line: number;
    column: number;
  }) {
    this._stackFrames.unshift(
      new StackFrame(
        frame.id,
        `(${frame.level})${frame.name}`,
        new Source(
          path.basename(frame.file),
          this.convertDebuggerPathToClient(frame.file)
        ),
        this.convertDebuggerLineToClient(frame.line),
        this.convertDebuggerColumnToClient(frame.column)
      )
    );
  }
  public setCurrentVariables(vars: DebugProtocol.Variable[]) {
    this._currentVariables = [];
    while (vars.length > 0) {
      this._currentVariables.push(vars.pop());
    }
  }
  protected async launchRequest(
    response: DebugProtocol.LaunchResponse,
    args: LaunchRequestArguments
  ) {
    // window.showInformationMessage("hello");
    this._startupQuery = args.startupQuery || "start";
    this._startFile = path.resolve(args.program);
    this._cwd = args.cwd;
    this._runtimeExecutable = args.runtimeExecutable || "swipl";
    // this._runtimeArgs = args.runtimeArgs || null;
    this._stopOnEntry = typeof args.stopOnEntry ? args.stopOnEntry : true;
    this._traceCmds = args.traceCmds;
    this._prologDebugger = await new PrologDebugger(args, this);
    this._prologDebugger.addListener(
      "responseBreakpoints",
      (bps: DebugProtocol.SetBreakpointsResponse) => {
        this.sendResponse(bps);
      }
    );
    this._prologDebugger.addListener(
      "responseFunctionBreakpoints",
      (fbps: DebugProtocol.SetFunctionBreakpointsResponse) => {
        this.sendResponse(fbps);
      }
    );
    this.sendResponse(response);
    this.sendEvent(new InitializedEvent());
  }
  protected threadsRequest(response: DebugProtocol.ThreadsResponse): void {
    response.body = {
      threads: [new Thread(PrologDebugSession.THREAD_ID, "thread 1")]
    };
    this.sendResponse(response);
  }

  protected setBreakPointsRequest(
    response: DebugProtocol.SetBreakpointsResponse,
    args: DebugProtocol.SetBreakpointsArguments
  ) {
    if (this._debugging) {
      this.debugOutput(
        "Breakpoints set during debugging would take effect in next debugging process."
      );
      return;
    }
    this._prologDebugger.setBreakpoints(args, response);
  }

  protected setExceptionBreakPointsRequest(
    response: DebugProtocol.SetExceptionBreakpointsResponse,
    args: DebugProtocol.SetExceptionBreakpointsArguments
  ): void {
    this.sendResponse(response);
  }
  protected setFunctionBreakPointsRequest(
    response: DebugProtocol.SetFunctionBreakpointsResponse,
    args: DebugProtocol.SetFunctionBreakpointsArguments
  ): void {
    this._prologDebugger.setFunctionBreakpoints(args, response);
  }
  protected configurationDoneRequest(
    response: DebugProtocol.ConfigurationDoneResponse,
    args: DebugProtocol.ConfigurationDoneArguments
  ): void {
    this.sendResponse(response);
    this._prologDebugger.startup(`${this._startupQuery}`);
    if (!this._stopOnEntry) {
      this._prologDebugger.query(`cmd:${this._traceCmds.continue[1]}\n`);
    }
    this._debugging = true;
  }

  private evaluateExpression(exp: string) {
    const vars = this._currentVariables;
    for (let i = 0; i < vars.length; i++) {
      if (vars[i].name === exp) {
        return vars[i].value;
      }
    }
    return null;
  }
  protected evaluateRequest(
    response: DebugProtocol.EvaluateResponse,
    args: DebugProtocol.EvaluateArguments
  ): void {
    let val = this.evaluateExpression(args.expression);
    if (val) {
      response.body = {
        result: val,
        variablesReference: 0
      };
      this.sendResponse(response);
      return;
    } else {
      if (args.context === "repl") {
        const vars = this._currentVariables;
        let exp = args.expression.trim();
        if (exp.startsWith(":")) {
          //work around for input from stdin
          let input = "input" + args.expression;
          this._prologDebugger.query(input + "\n");
        } else {
          for (let i = 0; i < vars.length; i++) {
            let re = new RegExp("\\b" + vars[i].name + "\\b", "g");
            exp = exp.replace(re, vars[i].value);
          }
          this.debugOutput(args.expression);
          this.evaluate(exp);
        }
        response.body = { result: "", variablesReference: 0 };
      }
      this.sendResponse(response);
      return;
    }
  }
  protected stackTraceRequest(
    response: DebugProtocol.StackTraceResponse,
    args: DebugProtocol.StackTraceArguments
  ): void {
    response.body = {
      stackFrames: this._stackFrames
    };
    this.sendResponse(response);
  }
  protected variablesRequest(
    response: DebugProtocol.VariablesResponse,
    args: DebugProtocol.VariablesArguments
  ): void {
    const variables = new Array<DebugProtocol.Variable>();
    for (let i = 0; i < this._currentVariables.length; i++) {
      variables.push(this._currentVariables[i]);
    }
    response.body = {
      variables: variables
    };
    this.sendResponse(response);
  }
  protected scopesRequest(
    response: DebugProtocol.ScopesResponse,
    args: DebugProtocol.ScopesArguments
  ): void {
    const scopes = new Array<Scope>();
    scopes.push(new Scope("Local", PrologDebugSession.SCOPEREF++, false));
    response.body = {
      scopes: scopes
    };
    this.sendResponse(response);
  }
  protected continueRequest(
    response: DebugProtocol.ContinueResponse,
    args: DebugProtocol.ContinueArguments
  ): void {
    this._prologDebugger.query(`cmd:${this._traceCmds.continue[1]}\n`);
    this.sendResponse(response);
  }

  protected nextRequest(
    response: DebugProtocol.NextResponse,
    args: DebugProtocol.NextArguments
  ): void {
    this._prologDebugger.query(`cmd:${this._traceCmds.stepover[1]}\n`);
    this.sendResponse(response);
  }
  protected stepInRequest(
    response: DebugProtocol.StepInResponse,
    args: DebugProtocol.StepInArguments
  ): void {
    this._prologDebugger.query(`cmd:${this._traceCmds.stepinto[1]}\n`);
    this.sendResponse(response);
  }
  protected stepOutRequest(
    response: DebugProtocol.StepOutResponse,
    args: DebugProtocol.StepOutArguments
  ): void {
    this._prologDebugger.query(`cmd:${this._traceCmds.stepout[1]}\n`);
    this.sendResponse(response);
  }
  protected disconnectRequest(
    response: DebugProtocol.DisconnectResponse,
    args: DebugProtocol.DisconnectArguments
  ): void {
    this._debugging = false;
    this._prologDebugger.dispose();
    this.shutdown();
    this.sendResponse(response);
  }
  protected restartRequest(
    response: DebugProtocol.RestartResponse,
    args: DebugProtocol.RestartArguments
  ): void {
    this._debugging = false;
    this._prologDebugger.dispose();
    this.shutdown();
    this.sendResponse(response);
  }
  protected sendErrorResponse(
    response: DebugProtocol.Response,
    error: Error,
    dest?: ErrorDestination
  ): void;
  protected sendErrorResponse(
    response: DebugProtocol.Response,
    codeOrMessage: number | DebugProtocol.Message,
    format?: string,
    variables?: any,
    dest?: ErrorDestination
  ): void;
  protected sendErrorResponse(response: DebugProtocol.Response) {
    if (arguments[1] instanceof Error) {
      const error = arguments[1] as Error & {
        code?: number | string;
        errno?: number;
      };
      const dest = arguments[2] as ErrorDestination;
      let code: number;
      if (typeof error.code === "number") {
        code = error.code as number;
      } else if (typeof error.errno === "number") {
        code = error.errno;
      } else {
        code = 0;
      }
      super.sendErrorResponse(response, code, error.message, dest);
    } else {
      super.sendErrorResponse(
        response,
        arguments[1],
        arguments[2],
        arguments[3],
        arguments[4]
      );
    }
  }

  public debugOutput(msg: string) {
    this.sendEvent(new OutputEvent(msg));
  }
  private evaluate(expression: string) {
    let exec = this._runtimeExecutable;
    let args = ["-q", `${this._startFile}`];
    let input = `
    call((${expression})).
    halt.
    `;
    let spawnOptions: SpawnOptions = {
      cwd: this._cwd
    };

    spawn(exec, args, spawnOptions)
      .on("process", proc => {
        if (proc.pid) {
          proc.stdin.write(input);
          proc.stdin.end();
        }
      })
      .on("stdout", data => {
        this.debugOutput("\n" + data);
      })
      .on("stderr", err => {
        this.debugOutput("\n" + err);
      })
      .catch(err => {
        this.debugOutput(err.message);
      });
  }
}

DebugSession.run(PrologDebugSession);
