import * as fse from "fs-extra-plus";
interface ISnippet {
  prefix: string;
  body: string;
  description: string;
}
interface ISnippets {
  [key: string]: ISnippet;
}
let snippets: ISnippets = {};
async function libsToSnippets(path: string, builtin: boolean) {
  let libs = await fse.listDirs(path);
  await Promise.all(
    libs.map(async lib => {
      let preds = await fse.listFiles(lib, ".txt");
      await Promise.all(
        preds.map(async (pred: string) => {
          let snippet = await fileToSnippet(pred);
          if (snippet) {
            let key = pred
              .split("/")
              .slice(-2)
              .join(":")
              .replace("-", "/")
              .replace(/\.txt$/, "");
            if (builtin) {
              key = key.split(":")[1];
            }
            snippets[key] = snippet;
          }
        })
      );
    })
  );
}
async function fileToSnippet(file: string) {
  if (/summary\.txt$/.test(file)) {
    return null;
  }
  try {
    let snippet: ISnippet | null = null;
    let txt = await fse.readFile(file, "utf8");
    let str = txt
      .toString()
      .replace(/\n\n+/g, "\n\n")
      .replace(/ {8,}/g, "    ")
      .trim();
    let match = str.match(/^(\w+)(\(([^\)]*)\))?/);
    let prefix: string = "",
      params: string = "",
      body: string;
    if (match) {
      prefix = match[1];
      body = prefix;
      if (match[2]) {
        params = match[3];
        let plist: string[] = params.split(",");
        body += "(";
        for (let i = 1; i <= plist.length; i++) {
          let mtch = plist[i - 1].match(/\w+/);
          if (mtch) {
            let pName = mtch[0];
            body += "${" + i + ":" + pName + "}";
            if (i < plist.length) {
              body += ", ";
            } else {
              body += ")$" + (i + 1) + "\n$0";
            }
          }
        }
      }

      snippet = {
        prefix: prefix,
        body: body,
        description: str
      };
    }
    return snippet;
  } catch (error) {
    console.log(error);
    return undefined;
  }
}

(async () => {
  let docRoot = "/opt/eclipseclp/doc/bips/";
  await libsToSnippets(docRoot + "kernel", true);
  await libsToSnippets(docRoot + "lib", false);
  await libsToSnippets(docRoot + "lib_public", false);
  await fse.writeJson("prolog.ecl.json", snippets);
})();
