const { LanguageClient } = require('vscode-languageclient')

module.exports = {
  activate(context) {
    
    let path = context.extensionPath;
    let server_exe = path + '/IDE.exe';
    // console.log(server_exe);

    const executable    = { command: server_exe, args: [] };
    const serverOptions = { run: executable, debug: executable };

    const clientOptions = {
      documentSelector: [{
        scheme:   'file',
        language: 'toylang',
      }],
    }
    
    const client = new LanguageClient(
      'toy-ide', 
      'Toy Haskell LSP example',
      serverOptions,
      clientOptions
    )

    context.subscriptions.push(client.start())
  }
}    