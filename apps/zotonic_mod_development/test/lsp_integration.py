#!/usr/bin/env python3
"""Exercise the real stdio transport against a running development node.

Run from the checkout root: python3 apps/zotonic_mod_development/test/lsp_integration.py SITE
Uses unsaved buffers only; never edits or renders site templates.
"""
import json
import os
import pathlib
import queue
import subprocess
import sys
import threading

root = pathlib.Path(__file__).resolve().parents[3]
site = sys.argv[1]
inferred = '--inferred' in sys.argv[2:]
mapped = '--mapped' in sys.argv[2:] or inferred
client_root = pathlib.Path('/host/source é #') if mapped else root
command = [str(root / 'bin/zotonic-lsp')] if '--auto' in sys.argv[2:] else [str(root / 'bin/zotonic'), 'lsp']
environment = os.environ.copy()
if inferred:
    environment['ZOTONIC_LSP_PATH_MAPPINGS'] = json.dumps([{'localRoot': str(client_root), 'remoteRoot': str(root)}])
process = subprocess.Popen(command, cwd=root, env=environment,
                           stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
messages = queue.Queue()


def read_messages():
    try:
        while True:
            headers = {}
            while (line := process.stdout.readline()) != b'\r\n':
                if not line:
                    raise EOFError('Language server closed stdout')
                key, value = line.decode('ascii').split(':', 1)
                headers[key.lower()] = value.strip()
            messages.put(json.loads(process.stdout.read(int(headers['content-length']))))
    except Exception as error:
        messages.put(error)


threading.Thread(target=read_messages, daemon=True).start()


def send(method, params, request_id=None):
    message = {'jsonrpc': '2.0', 'method': method, 'params': params}
    if request_id is not None:
        message['id'] = request_id
    body = json.dumps(message, ensure_ascii=False).encode('utf-8')
    frame = f'Content-Length: {len(body)}\r\n\r\n'.encode() + body
    # Split both the header and multibyte body across writes.
    for start in range(0, len(frame), 13):
        process.stdin.write(frame[start:start + 13])
        process.stdin.flush()


def receive():
    result = messages.get(timeout=20)
    if isinstance(result, Exception):
        raise result
    return result


uri = (client_root / 'apps/zotonic_mod_admin/priv/templates/unsaved é #.tpl').as_uri()
version = 0


def buffer(text):
    global version
    version += 1
    if version == 1:
        send('textDocument/didOpen', {'textDocument': {
            'uri': uri, 'languageId': 'zotonic', 'version': version, 'text': text}})
    else:
        send('textDocument/didChange', {'textDocument': {'uri': uri, 'version': version},
                                      'contentChanges': [{'text': text}]})
    result = receive()
    assert result['method'] == 'textDocument/publishDiagnostics', result
    assert result['params']['version'] == version
    assert result['params']['uri'] == uri
    return result['params']['diagnostics']


def query(method, character, request_id):
    send('textDocument/' + method, {'textDocument': {'uri': uri},
                                  'position': {'line': 0, 'character': character}}, request_id)
    return receive()


try:
    options = {'site': site}
    if mapped and not inferred:
        options['pathMappings'] = [{'localRoot': str(client_root), 'remoteRoot': str(root)}]
    initialize = {'initializationOptions': options}
    if '--sublime' in sys.argv[2:]:
        initialize['clientInfo'] = {'name': 'Sublime Text LSP'}
    if '--markdown' in sys.argv[2:] or '--sublime' in sys.argv[2:]:
        initialize['capabilities'] = {'textDocument': {'hover': {'contentFormat': ['markdown', 'plaintext']}}}
    send('initialize', initialize, 1)
    assert receive()['result']['capabilities']['positionEncoding'] == 'utf-16'
    send('initialized', {})
    send('zotonic/sites', {}, 2)
    assert site in receive()['result']
    assert not buffer('😀 {{ value|escape }}')
    definition = query('definition', 13, 3)['result']
    assert any('filter_escape.erl' in target['uri'] for target in definition), definition
    if mapped:
        assert all(target['uri'].startswith(client_root.as_uri() + '/') for target in definition), definition
    hover = query('hover', 13, 4)['result']['contents']
    assert 'escape' in hover['value']
    assert '/_build/' not in hover['value']
    assert hover['kind'] == ('markdown' if '--markdown' in sys.argv[2:] or '--sublime' in sys.argv[2:] else 'plaintext')
    if '--sublime' in sys.argv[2:]:
        initialize['clientInfo'] = {'name': 'Sublime Text LSP'}
    if '--markdown' in sys.argv[2:] or '--sublime' in sys.argv[2:]:
        assert '**Filter**' in hover['value']
    assert buffer('{% if value %}')
    assert not buffer('{% if value %}fixed{% endif %}')
    assert not buffer('{% include "base.tpl" %}')
    assert query('definition', 15, 5)['result']
    assert 'Module priority' in query('hover', 15, 6)['result']['contents']['value']
    buffer('{% include "ba')
    assert any(item['label'] == 'base.tpl' for item in query('completion', 14, 7)['result'])
    buffer('{{ value|esc')
    assert any(item['label'] == 'escape' for item in query('completion', 12, 8)['result'])
    buffer('{{ m.')
    assert query('completion', 5, 9)['result']
    buffer('{% ')
    assert any(item['label'] == 'include' for item in query('completion', 3, 10)['result'])
    send('workspace/didChangeConfiguration', {'settings': {'zotonic': {'site': ''}}})
    buffer('{% include "base.tpl" %}')
    assert 'select' in query('definition', 15, 11)['error']['message'].lower()
    buffer('{{ value|escape }}')
    assert any('filter_escape.erl' in item['uri'] for item in query('definition', 11, 14)['result'])
    assert 'Module:' in query('hover', 11, 15)['result']['contents']['value']
    buffer('{{ m.rsc[1].title }}')
    assert any('m_rsc.erl' in item['uri'] for item in query('definition', 6, 16)['result'])
    model_hover = query('hover', 6, 19)['result']['contents']['value']
    if '--sublime' in sys.argv[2:]:
        assert '- Property: page_url; Description:' in model_hover, model_hover[:1800]
        assert '- Property: category_id; Description:' in model_hover
        assert '| Property' not in model_hover
    elif '--markdown' in sys.argv[2:]:
        assert '| Property' in model_hover
    buffer('{% wire delegate="mod_development" %}')
    assert any('mod_development.erl' in item['uri'] for item in query('definition', 22, 17)['result'])
    buffer('{% include "admin_development.tpl" %}')
    assert query('definition', 15, 18)['result']
    send('workspace/didChangeConfiguration', {'settings': {'zotonic': {'site': 'missing-site'}}})
    assert 'not running' in query('definition', 15, 12)['error']['message']
    send('textDocument/didClose', {'textDocument': {'uri': uri}})
    assert receive()['params']['diagnostics'] == []
    send('shutdown', {}, 13)
    assert receive()['result'] is None
    send('exit', {})
    assert process.wait(timeout=10) == 0
    print(('Mapped paths: ' if mapped else 'Local paths: ') + 'Passed: stdio framing, Unicode, diagnostics, definitions, hover, four completion kinds, site selection, shutdown.')
finally:
    if process.poll() is None:
        process.terminate()
        process.wait(timeout=10)
    errors = process.stderr.read().decode()
    if errors:
        print(errors, file=sys.stderr)
