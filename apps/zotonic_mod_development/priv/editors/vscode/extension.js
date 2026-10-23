// Copyright 2026 Marc Worrell. Licensed under the Apache License, Version 2.0.
const vscode = require('vscode');
const path = require('node:path');
const { LanguageClient } = require('vscode-languageclient/node');
let client;
let restartQueue = Promise.resolve();

/** Start one stdio session per window; the site choice belongs to the workspace. */
function activate(context) {
    const status = vscode.window.createStatusBarItem(vscode.StatusBarAlignment.Right, 20);
    status.command = 'zotonic.selectSite';
    status.tooltip = 'Choose the Zotonic site used to resolve templates';
    const updateStatus = () => {
        status.text = `Zotonic: ${vscode.workspace.getConfiguration('zotonic').get('site') || 'auto'}`;
        if (vscode.window.activeTextEditor?.document.languageId === 'zotonic') status.show();
        else status.hide();
    };
    const restart = () => {
        restartQueue = restartQueue.then(async () => {
            if (client) await client.stop();
            client = undefined;
            const config = vscode.workspace.getConfiguration('zotonic');
            const root = vscode.workspace.workspaceFolders?.[0]?.uri.fsPath;
            const command = config.get('command') || (root && path.join(root, 'bin', 'zotonic'));
            if (!command) throw new Error('Set zotonic.command to the absolute path of bin/zotonic.');
            client = new LanguageClient('zotonic', 'Zotonic Templates',
                { command, args: config.get('args') || ['lsp'], options: { shell: false } },
                {
                    documentSelector: [{ scheme: 'file', language: 'zotonic' }],
                    initializationOptions: { site: config.get('site') || '', pathMappings: config.get('pathMappings') || [] },
                    synchronize: { configurationSection: 'zotonic' }
                });
            await client.start();
            updateStatus();
        }).catch(error => vscode.window.showErrorMessage(`Zotonic: ${error.message}`));
        return restartQueue;
    };
    context.subscriptions.push(status,
        vscode.commands.registerCommand('zotonic.restart', restart),
        vscode.commands.registerCommand('zotonic.selectSite', async () => {
            try {
                if (!client?.isRunning()) await restart();
                if (!client?.isRunning()) return;
                const sites = await client.sendRequest('zotonic/sites', {});
                const selected = await vscode.window.showQuickPick([
                    { label: 'Automatic (resolve across sites)', site: '' },
                    ...sites.map(site => ({ label: site, site }))
                ], { placeHolder: 'Resolve shared templates using this site' });
                if (selected) {
                    await vscode.workspace.getConfiguration('zotonic').update('site', selected.site,
                        vscode.workspace.workspaceFolders ? vscode.ConfigurationTarget.Workspace : vscode.ConfigurationTarget.Global);
                }
            } catch (error) {
                vscode.window.showErrorMessage(`Zotonic: ${error.message}`);
            }
        }),
        vscode.workspace.onDidChangeConfiguration(event => {
            if (['command', 'args', 'pathMappings'].some(key => event.affectsConfiguration(`zotonic.${key}`))) restart();
            if (event.affectsConfiguration('zotonic.site')) updateStatus();
        }),
        vscode.window.onDidChangeActiveTextEditor(updateStatus));
    updateStatus();
    return restart();
}

async function deactivate() {
    await restartQueue;
    if (client) await client.stop();
}
module.exports = { activate, deactivate };
