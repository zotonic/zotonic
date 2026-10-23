// Run with: node --test apps/zotonic_mod_development/test/development-editor.test.js
const {test} = require('node:test');
const assert = require('node:assert/strict');
const {editorUrl, mapPath, validate} = require('../priv/lib/js/development-editor.js');
const settings = {mode: 'browser', preset: 'vscode', server_root: '/app/zotonic', host_root: '/Users/me/My Project'};

test('maps a directory boundary and encodes Unicode, spaces, and URL delimiters', () => {
    assert.equal(editorUrl(settings, '/app/zotonic/a & héllo.tpl', 42, 3),
        'vscode://file/Users/me/My%20Project/a%20%26%20h%C3%A9llo.tpl:42:3');
    assert.throws(() => mapPath('/app/zotonic-other/a.tpl', '/app/zotonic', '/host'));
    assert.throws(() => mapPath('/other/a.tpl', '/app/zotonic', '/host'));
});
test('Sublime query cannot acquire injected parameters', () => {
    const url = editorUrl({...settings, preset: 'sublime'}, '/app/zotonic/a&line=9.tpl', 42, 3);
    assert.equal(new URL(url).searchParams.get('line'), '42');
    assert.equal(new URL(url).searchParams.get('url'), 'file:///Users/me/My%20Project/a%26line%3D9.tpl');
});
test('custom helpers receive encoded filenames and Windows drive paths', () => {
    assert.equal(editorUrl({...settings, preset: 'custom', url: 'vim://open?file={file}&line={line}'}, '/app/zotonic/{line}.tpl', 8),
        'vim://open?file=%2FUsers%2Fme%2FMy%20Project%2F%7Bline%7D.tpl&line=8');
    assert.equal(editorUrl({...settings, host_root: 'C:\\Code'}, '/app/zotonic/a.tpl'),
        'vscode://file/C:/Code/a.tpl:1:1');
});
test('unsafe URLs, missing editor, and partial mappings cannot be saved', () => {
    for (const url of ['javascript://{file}', 'data://{path}', 'file://{path}', 'https://example.com/{file}', 'subl://open']) {
        assert.throws(() => validate({...settings, preset: 'custom', url}));
    }
    assert.throws(() => validate({...settings, preset: ''}));
    assert.throws(() => validate({...settings, host_root: ''}));
    assert.throws(() => validate({mode: 'local', local: ''}));
    assert.doesNotThrow(() => validate(settings));
});

test('Sublime nested file URLs round-trip reserved characters and spaces', () => {
    const filename = "/app/zotonic/héllo # ? %20 & + ' \\ {line}.tpl";
    const url = new URL(editorUrl({...settings, preset: 'sublime'}, filename, 17, 4));
    assert.deepEqual([...url.searchParams.keys()], ['url', 'line', 'column']);
    const fileUrl = new URL(url.searchParams.get('url'));
    assert.equal(fileUrl.hash, '');
    assert.equal(fileUrl.search, '');
    assert.equal(decodeURIComponent(fileUrl.pathname), filename.replace('/app/zotonic', '/Users/me/My Project'));
    assert.equal(url.searchParams.get('line'), '17');
});
test('Unix backslashes remain literal while Windows separators are normalized', () => {
    assert.equal(editorUrl(settings, '/app/zotonic/a\\b.tpl'),
        'vscode://file/Users/me/My%20Project/a%5Cb.tpl:1:1');
    assert.equal(mapPath('C:\\Code\\a b.tpl', 'C:\\Code', 'D:\\Working Tree'), 'D:/Working Tree/a b.tpl');
});
