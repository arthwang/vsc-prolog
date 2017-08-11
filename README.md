# VSC-Prolog

A VS Code extension which provides language support for Prolog (just SWI-Prolog now).

___
  [Features](#features) | [Settings](#extension-settings) | [Keybindings](#keybinds) | [Donation](#donation)

## Note before installation

This extension can be installed via extensions viewer of VS Code or 'Extensions: install extension' command from the command palette. The author notices that it is developed and tested in ***SWI-Prolog 7.4.2*** and ***VS Code 1.14*** on ***Debian 9.0*** (stretch). Other environments are not yet tested.

## Features
  * [Syntax highlighting](#syntax-highlighting)
  * [Snippets](#predicate-snippets)
  * [Information Hovers](#information-hovers)
  * [Linter](#grammar-linter)
  * [Edit helpers](#edit-helpers)
    * [Import/Dynamic helper](#import-or-dynamic-predicate-helper)
    * [Export helper](#export-helper)
    * [Recursion helper](#recursion-helper)
    * [Anonymous variable helper](#anonymous-variable-helper)
  * [Load active source file and query goals](#load-active-source-file-and-query-goals)
  * [Goto definition](#go-to-definition)
  * [Code formatter](#code-formatter)
  * [Debugger(experimental)](#debugger)
    * Leep, creep, skip, Up, Stop, Restart
    * Breakpoints, including conditional breakpoints and hit count breakpoints
    * Spy(function breakpoints)
    * Data inspection
    * Evaluation

## Feature descriptions and usages

### Syntax highlighting
  * Based on sublimeprolog
  * Builtin pattern support

  ![syntax](images/syntax.gif)

### Predicate snippets

  * Predicate templates auto-completion
  * Produced from source files with structured comments and html document of swipl system

  ![snippet](images/snippets.gif)

### Information hovers
  Hovers show Document information about the predicate under the mouse cursor.

  ![hover](images/hover.gif)

### Grammar linter
  * Lint for errors, warning and undefined predicates of the source file in active editor window and its imported file
  * Mesage shown in OUTPUT and PROBLEMS channels
  * Clicking lines in PROBLEMS channels brings cursor to the respective lines in the editor
  * Traverse error lines in the editor by commands, the error/warning message responding to the line is presented in OUTPUT channel.
    * Prolog: Goto next error line (default map to f8) 
    * Prolog: Goto previous error line (default map to shift-f8)
    
  ![linter](images/linter.gif)

### Edit helpers

#### Import or Dynamic predicate helper

  Clicking on the squiggle indicating 'undefined predicate' lights the yellow bulb in left of the line. A suggesition list is presented when you click the bulb. The suggestion includes 'add dynamic ' for the undefined predicate or 'import' it if VSC-Prolog finds it's exported from some module(s).
  ![import](images/import.gif)

#### Export helper

  Move cursor to the head of a clause or a fact to be exported and trigger the command 'Prolog: export predicate under cursor' via command palette or right click the predicate to pop up the Editor/context menu which contains the command. Then VSC-Prolog inserts :- module/2 if module is not defined or adds the predicate indicator to the exporting list otherwise. After that a message box displays that asks for if adding structured comment for the predicate and comment lines are inserted above the head of the clause if 'yes' chosen.

 ![export](images/export.gif)

#### Recursion helper

  Leading dot (only spaces before it) of a line repeat the above nearest predicate or the head of current clause if last line ends with comma, in which case the recursive variable change accordingly as intelligent as possible.
 ![recursion](images/recursion.gif)

#### Anonymous variables helper

  Typing underscore in front of any parameters makes the parameter an anonymous variable.
  ![anonyvar](images/anonymous.gif)

### Load active source file and query goals

  * Command 'Prolog: load document' 
    (default map to alt-x l) loads the source file in active editor into prolog process in the integrated terminal, opening it if not beeing opened. The prolog process provides a real prolog REPL console. 

  * Command 'Prolog: query goal under cursor'
    (default map to alt-x q) loads the source file and querys the goal under the cursor. You can call this command from editor context menu.
  ![loaddocument](images/loaddoc.gif)

### Go to definition
  * Go to definition
    Editor/context menu command 'Go to Definition' (default map to f12) brings the cursor to from the predicate to where it is defined.
  * Peek definition
    Editor/context menu command 'Peek Definition' (default ctrl-shift-f10) pops up a panel to display the definition.
   ![gotodef](images/gotodef.gif)

### Code formatter

  Code formatting is implemented via calling portray_clause/1, so the beautification style is depended on portray_clause. Thus two limits should be reminded.
  * Terms with any grammar grammar errors are not formatted
  * Terms with singleton variables, including named anonymous variables are not formatted, since portray_clause outputs singletons as anonymous variables resulting in user's intention alteration.

  VSC-Prolog formats codes for three scopes of active document in editor:
  * document scope
    
    Right click any area of active prolog source document in editor to pop up editor context menu, then trigger the command 'Format Document' (default map to alt+shift+f). The whole document would be formatted.

  * selection scope

    Select an range and right click on the selection to pop up editor context menu, then trigger the command 'Format Selection' (default map to ctrl+k ctrl+f). VSC-Prolog would enlarge the selection to include complete terms if necessary and format the selection.

    * clause scope

    VSC-Prolog formats the current clause or fact when the user types the end dot symbol.

    ![format](images/format.gif)
### Debugger

  The experimental debugger of VSC-Prolog tries to visualize the command line tracer of SWI-Prolog in VS Code. Read [VS Code handbook about debugging](https://code.visualstudio.com/docs/editor/debugging) for how VS Code debugging works generally.
  
  For the first time to debug in VS Code it is necessary to setup a launch.json file under .vscode in a project root directory. VS Code pops down a list of debug environments when you first click 'start debugging' button (f5) or the gear icon. The list contains 'Prolog' if VSC-Prolog extension is installed. A default launch.json file would be generated. Among the all settings, two must be set firstly: 'runtime executable' and 'startup query' according to your environment. 'runtime executable' points to your swipl executable path and 'startup query' refers to the goal you want to start debugging. ___There is only one file containing the 'startup goal' in a project.___ Refer to next section for detailed explanations about other settings.

  ![launch](images/launch.gif)
* Trace options

  VSC-Prolog debugger supports basic trace options: leep, creep, skip, up, stop and restart.

  ![debug](images/debug.gif)

* Breakpoints

  Before starting debugging, left click the editor margin on left of line number to toggle breakpoints. On debugging, the prolog process verifies the breakpoints and marks unverified ones with grey color.
  > Note the limit of VSC-Prolog: Breakpoints must be set before starting debugging. Any breakpoints set during debugging are unavailable until next debugging process.

  ![breakpoints](images/breakpoints.gif)

  * Conditional breakpoints

    Conditional breakpoints or expression breakpoints will be hit whenever the expression evaluates to true. Right click the red dot of the breakpoint to open the menu list which contains 'Edit Breakpoint' item that is a option list including 'Expression' and 'Hit Count'. Select 'Expression' to enter the condition for execution to stop. In VSC-Prolog, the expressions must be legal prolog predicates. Usually when the variables bind to some values the predicates success and the execution pauses at the breakpoint, as shown in the gif animation below where 'length(T, 1)' is the condition to break when the length of T is 1.

    ![condbkp](images/condbkp.gif)

  * Hit count breakpoints

    The 'hit count' controls how many times a breakpoint needs to be hit before it will 'break' execution. That is, how many times execution passes the breakpoint before it pauses. Right click the red dot of the breakpoint to open the menu list which contains 'Edit Breakpoint' item that is a option list including 'Expression' and 'Hit Count'. Select 'Hit Count' to enter the number of the least hits to pass before stop.

    ![hitbkp](images/hitbkp.gif)

  * Spy predicates

    VSC-Prolog implements spy predicates via function breakpoints in VS Code. 
    > Note: Spy predicates are not shown as breakpoints in editor. A 'spy breakpoint' is created by pressing the + button in the BREAKPOINTS section header and entering the predicate indicator.

    ![spy](images/spy.gif)

* Data inspection

    Variables with their binding states whithin current clause can be inspected in the VARIABLES section of the Debug view or by hovering over their source in the editor or in the WATCH section.

    ![binding](images/binding.gif)

    Debug Console is consist of output area and an input box. The output area displays all outputs from debugging and the input box can be used to evaluate prolog terms which you can use variables with current bound values.

    ![eval](images/eval.gif)
  
    > Note about input from stdin during debugging:
    > 
    > The input box is also used to accept user input during debugging. While  a program is waiting for input from user, the yellow arrow indicating trace location would disappear. At this time you ___type firstly a semicolon___ followed by your real input contents. Another important point to know is that the prompt would not show anywhere.

    ![input](images/input.gif)

## Requirements

Latest versions of VS code and SWI-Prolog installed.

## Extension Settings

  * There are seven configurable settings with default values in VSC-Prolog:

    * "prolog.executablePath": "/opt/swipl/bin/swipl"

      Points to the Prolog executable.

    * "prolog.linter.run": "onType"

      How to trigger the linter: onType or onSave. 'onType' means linting is in nearly real time manner (controlled by next setting: prolog.linter.delay) whileas 'onSave' linter is called when saving the document.

    * "prolog.linter.delay": 500

      The milliseconds to delay when using onType trigger, that is, when pausing over this milliseconds between two types the linter would be triggered.

    * "prolog.terminal.runtimeArgs": []

      Arguments of Prolog executable run in terminal. This prolog process is used to load and execute the program in active editor, including 'query goal under cursor' command.

    * "prolog.format.tabSize": 4
    
      The size of a tab in spaces, this and next setting affect the layout of formatted codes.

    * "prolog.format.insertSpaces": true,

      Prefer spaces over tabs

    * "prolog.debug.runtimeArgs": ["-q"]

      Arguments of Prolog executable run as debugger. This prolog process is used to debug programs.

  * Keybindings

    ![keybindings](images/keybindings.png)

## Known Issues

  * Formatting doesn't work when there are singleton variables including named underscore-starting singleton variables in the scopes to be formatted.

  * During debug tracing, prompt for stdin input doesn't display in debug console. 

  * Syntax highlighting does not support multiline regular expression match, that's choosen by design of Sublime tmlanguage.

## Release Notes

### 0.1.0

Initial release 

## Bug reporting

  Feel free to report bugs via [issues](https://github.com/arthwang/vsc-prolog/issues)

## Contributions

  [Pull requests](https://github.com/arthwang/vsc-prolog/pulls) are welcome.

## Acknowledgements

    I would like to thank the VS Code team of Microsoft for providing this powerful platform. I've read over many source files of extensions shipped with VS Code and other language support extensions such as php, java, etc. I thank Professor Jan Wielemaker who is known as the original author of SWI-Prolog for his many professional and patient helps about SWI-Prolog system when I consulted on SWI-Prolog forum. Some segments of codes of this extension are gratefully borrowed from other resources such as Sublimeprolog tmlanguage syntax yaml file, SWI-Prolog source files, etc.

## License

  [MIT](http://www.opensource.org/licenses/mit-license.php)

## Donation

  >If this extension works well for you, would you please donate a loaf of bread to encourage me, as a freelance programmer, to spend more time to improve it. Any amount is greatly appreciated.

   [PayPal](https://paypal.me/ArthurWang9)