import { Location } from "vscode";
import * as Net from "net";
import { spawn } from "process-promises";

export class TerminalDebugServer {
  private _prologProc = null;
  private _runtimeArgs: string[];
  private _socket: Net.Socket = null;
  private _prologReady: boolean = false;
  private _serverReady: boolean = false;
  constructor(
    private _port: number = 5959,
    private _prologExec = "swipl",
    args: string[] | "null"
  ) {
    this._runtimeArgs = args == "null" ? [] : args;
    this.createDebugger().listen(this._port, _ => {
      console.log("Debugger is listening...");
      this._serverReady = true;
    });
    this.spawnProlog();
  }
  private createDebugger(): Net.Server {
    return Net.createServer(socket => {
      this._socket = socket;
      socket
        .on("data", data => {
          console.log("data:" + data.toString());
          this._queryProlog(data.toString());
        })
        .on("close", _ => {
          this._queryProlog("halt.\n");
          process.exit();
        })
        .on("end", _ => {
          console.log("server disconnected");
        });
    });
  }
  private spawnProlog() {
    console.log("spawning prolog ...");

    // spawn(this._prologExec, this._runtimeArgs.concat([]), undefined)
    spawn("swipl", ["-q"], undefined)
      .on("process", proc => {
        if (proc.pid) {
          console.log("proc pid:" + proc.pid);
          this._prologProc = proc;
          process.stdin.pipe(proc.stdin);
          proc.stdin.write("writeln('Prolog debug server started').\n");
          this._prologReady = true;
          // proc.stdout.pipe(process.stdout);
          // proc.stderr.pipe(process.stderr);
        }
      })
      .on("stdout", data => {
        console.log(data.toString());
        if (this._socket) {
          this._socket.write(data.toString() + "\n");
        }
      })
      .on("stderr", err => {
        console.log(err);
        if (this._socket) {
          this._socket.write("err:" + err + "\n");
        }
      })
      .on("close", _ => {
        if (this._prologProc.pid) {
          process.kill(this._prologProc.pid);
        }
      })
      .then(_ => {
        process.exit();
      })
      .catch(err => {
        console.log(err.message);
      });
  }
  public _queryProlog(goal: string) {
    this._prologProc.stdin.write(goal);
  }
  public debuggerReady(): boolean {
    return this._prologReady && this._serverReady;
  }
}

function startServer(): TerminalDebugServer {
  let argv = require("minimist")(process.argv.slice(2));
  console.dir(argv);
  return new TerminalDebugServer(argv.p, argv.e, argv.a);
}

startServer();
