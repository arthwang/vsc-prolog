import { read } from "fs";
import * as Net from "net";
import * as rlp from "readline-promise";

let client = new Net.Socket();

client.connect(5959, "localhost", _ => {
  // console.log("connected");
  // client.write("start.\n");
  // while (true) {
  // }
});

client.on("data", data => {
  console.log(data.toString());
});

client.on("close", _ => {
  console.log("connection closed");
  client.destroy();
});

rlp
  .createInterface({
    terminal: false,
    input: process.stdin
    // output: client
  })
  .each(line => {
    client.write(line + "\n");
  })
  .then(_ => {
    console.log("over");
  })
  .caught(err => {
    throw err;
  });
