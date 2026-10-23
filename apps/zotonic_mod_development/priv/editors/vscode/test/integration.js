// Run with VS Code's --extensionDevelopmentPath and --extensionTestsPath flags.
const assert = require('node:assert/strict');
const vscode = require('vscode');
const path = require('node:path');
exports.run = async () => {
    const root = process.env.ZOTONIC_LSP_TEST_ROOT;
    const site = process.env.ZOTONIC_LSP_TEST_SITE;
    assert(root && site, 'Set ZOTONIC_LSP_TEST_ROOT and ZOTONIC_LSP_TEST_SITE');
    const config = vscode.workspace.getConfiguration('zotonic');
    const wrapped = process.env.ZOTONIC_LSP_TEST_WRAPPED === '1';
    await config.update('command', wrapped ? '/usr/bin/env' : path.join(root, 'bin/zotonic'), vscode.ConfigurationTarget.Global);
    await config.update('args', wrapped ? [path.join(root, 'bin/zotonic'), 'lsp'] : ['lsp'], vscode.ConfigurationTarget.Global);
    await config.update('pathMappings', wrapped ? [{ localRoot: root, remoteRoot: root }] : [], vscode.ConfigurationTarget.Global);
    await config.update('site', site, vscode.ConfigurationTarget.Global);
    await vscode.extensions.getExtension('zotonic.zotonic-templates').activate();
    const document = await vscode.workspace.openTextDocument(vscode.Uri.file(
        path.join(root, 'apps/zotonic_mod_development/priv/templates/admin_development.tpl')));
    await vscode.window.showTextDocument(document);
    assert.equal(document.languageId, 'zotonic');
    const edit = new vscode.WorkspaceEdit();
    edit.insert(document.uri, new vscode.Position(0, 0), '😀 {{ value|escape }}\n');
    await vscode.workspace.applyEdit(edit);
    const definitions = await vscode.commands.executeCommand('vscode.executeDefinitionProvider',
        document.uri, new vscode.Position(0, 13));
    assert(definitions.some(d => d.uri.fsPath.endsWith('filter_escape.erl')), JSON.stringify(definitions));
    const hover = await vscode.commands.executeCommand('vscode.executeHoverProvider',
        document.uri, new vscode.Position(0, 13));
    assert(hover.length > 0);
    // Discard the temporary in-memory edit; do not save the repository template.
    await vscode.commands.executeCommand('workbench.action.files.revert');
    console.log('Zotonic VS Code extension: real editor definition and hover passed.');
};
