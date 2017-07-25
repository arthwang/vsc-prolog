import {
  DebugSession,
  ErrorDestination,
  OutputEvent,
  InitializedEvent,
  StoppedEvent,
  StackFrame,
  Source,
  Scope,
  Thread
} from "vscode-debugadapter";
import { DebugProtocol } from "vscode-debugprotocol";
import * as fs from "fs";
import { PrologDebugger, LaunchRequestArguments } from "./prologDebugger";
import { basename } from "path";
import Utils from "../utils/utils";
import * as cp from "child_process";
import { spawn, SpawnOptions } from "process-promises";
import { TerminalDebugServer } from "./terminalDebugServer";
import * as Net from "net";

export class PrologDebugSession extends DebugSession {
  private static SCOPEREF = 1;
  public static THREAD_ID = 100;
  // private _args: LaunchRequestArguments;
  private _supportsRunInTerminal: boolean;
  private _console: string;
  private _prologDebugger: PrologDebugger;
  private _runtimeExecutable: string;
  private _runtimeArgs: string[];
  private _startupQuery: string;
  private _startFile: string;
  private _cwd: string;
  private _currentVariables: DebugProtocol.Variable[] = [];
  private _stackFrames: DebugProtocol.StackFrame[] = [];

  public constructor() {
    super();
    this.setDebuggerColumnsStartAt1(true);
    this.setDebuggerLinesStartAt1(true);
    this.setDebuggerPathFormat("native");
  }

  public isInTerminal(): boolean {
    return this._supportsRunInTerminal && this._console !== "internalConsole";
  }
  protected initializeRequest(
    response: DebugProtocol.InitializeResponse,
    args: DebugProtocol.InitializeRequestArguments
  ): void {
    this._supportsRunInTerminal = args.supportsRunInTerminalRequest;
    // this.sendEvent(new InitializedEvent());
    response.body = {
      supportsConfigurationDoneRequest: true,
      supportTerminateDebuggee: true,
      supportsConditionalBreakpoints: true,
      supportsHitConditionalBreakpoints: true,
      supportsFunctionBreakpoints: true,
      supportsEvaluateForHovers: true,
      supportsExceptionOptions: true,
      supportsExceptionInfoRequest: true,
      // supportsStepBack: false,
      // supportsStepInTargetsRequest: true,
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
          basename(frame.file),
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
    this._startupQuery = args.startupQuery || "start";
    this._startFile = args.program;
    this._cwd = args.cwd;
    this._runtimeExecutable = args.runtimeExecutable || "swipl";
    this._runtimeArgs = args.runtimeArgs || null;
    this._console =
      typeof args.console === "string" ? args.console : "internalConsole";
    if (this.isInTerminal()) {
      const opts: DebugProtocol.RunInTerminalRequestArguments = {
        kind: args.console === "integratedTerminal" ? "integrated" : "external",
        cwd: args.cwd,
        title: "Prolog Debugger",
        env: args.env,
        args: [
          "node",
          `${__dirname}/terminalDebugServer.js`,
          `-p ${args.terminalDebuggerPort}`,
          `-e ${this._runtimeExecutable}`,
          `-a ${this._runtimeArgs.length > 0 ? this._runtimeArgs : null}`
        ]
      };
      this.runInTerminalRequest(opts, 5000, termResp => {
        if (termResp.success) {
          // require("wait-for-port")("localhost", 5959, err => {
          //   if (err) {
          //     this.debugOutput("Debugger in terminal error.");
          //     //close
          //   }
          setTimeout(_ => {
            let socket = Net.connect(5959, "localhost", _ => {
              this._prologDebugger = new PrologDebugger(args, this, socket);
              this._prologDebugger.initPrologDebugger();
              this.addListeners();
              this.sendResponse(response);
              this.sendEvent(new InitializedEvent());
            })
              .on("data", data => {
                this._prologDebugger.relayData(data.toString("utf8"));
              })
              .on("close", err => {
                this.debugOutput("connection to debugger closed.");
                socket.destroy();
              });
          }, 5000);
        } else {
          this.debugOutput(termResp.message);
        }
      });
    } else {
      this._prologDebugger = await new PrologDebugger(args, this);
      this.addListeners();
      this.sendResponse(response);
      this.sendEvent(new InitializedEvent());
    }
  }

  private addListeners() {
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
    // this._prologDebugger.startup(`${this._startupQuery}`);
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

        for (let i = 0; i < vars.length; i++) {
          let re = new RegExp("\\b" + vars[i].name + "\\b", "g");
          exp = exp.replace(re, vars[i].value);
        }

        this.debugOutput(args.expression);

        this.evaluate(exp);
        // this._prologDebugger.query("b\n");
        // this._prologDebugger.query(`call((${exp}).\n`);
        // this._prologDebugger.query("end_of_file.\n");

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
    this._prologDebugger.query("l\n");
    this.sendResponse(response);
  }

  protected nextRequest(
    response: DebugProtocol.NextResponse,
    args: DebugProtocol.NextArguments
  ): void {
    this._prologDebugger.query("s\n");
    this.sendResponse(response);
  }
  protected stepInRequest(
    response: DebugProtocol.StepInResponse,
    args: DebugProtocol.StepInArguments
  ): void {
    this._prologDebugger.query("c\n");
    this.sendResponse(response);
  }
  protected stepOutRequest(
    response: DebugProtocol.StepOutResponse,
    args: DebugProtocol.StepOutArguments
  ): void {
    this._prologDebugger.query("u\n");
    this.sendResponse(response);
  }
  protected disconnectRequest(
    response: DebugProtocol.DisconnectResponse,
    args: DebugProtocol.DisconnectArguments
  ): void {
    this.sendResponse(response);
  }
  protected restartRequest(
    response: DebugProtocol.RestartResponse,
    args: DebugProtocol.RestartArguments
  ): void {
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
