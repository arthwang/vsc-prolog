"use strict";
exports.__esModule = true;
var Net = require("net");
var client = new Net.Socket();
client.connect(5959, "localhost", function (_) {
    console.log("connected");
    client.write("Hello, server! Love, Client.");
});
client.on("data", function (data) {
    console.log("Received: " + data);
    client.destroy();
});
client.on("close", function (_) {
    console.log("connection closed");
});
