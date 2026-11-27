/* Run with node --test apps/zotonic_mod_fileuploader/test/editor_dropzones_test.js */
const test = require('node:test');
const assert = require('node:assert/strict');
const fs = require('node:fs');
const vm = require('node:vm');
const source = fs.readFileSync('apps/zotonic_mod_fileuploader/priv/lib/js/z.fileuploader-editor.js', 'utf8');

function fixture() {
    let insertion;
    const markers = [];
    const listeners = new Map();
    const status = {setAttribute() {}, classList: {add() {}}};
    const zone = {append() {}, getAttribute() {}, querySelector: () => status};
    const range = {startContainer: {}, collapse() {}, setStartAfter() {}, insertNode: node => markers.push(node)};
    const doc = {
        caretRangeFromPoint: () => range,
        createElement: () => ({isConnected: true, setAttribute() {}, remove() { this.isConnected = false; }, insertAdjacentHTML() {}})
    };
    const editor = {
        readonly: false, removed: false,
        getParam: () => editor.readonly,
        getElement: () => ({dataset: {id: '123'}}), getContainer: () => ({after() {}}),
        getDoc: () => doc, getBody: () => ({contains: () => true}),
        on: (name, fn) => listeners.set(name, fn), off: name => listeners.delete(name),
        undoManager: {transact: fn => fn()}, nodeChanged() {}, fire() {}
    };
    const uploader = {clearDragState() {}, editorUpload: ({reserve}) => { insertion = reserve(); }};
    vm.runInNewContext(source, {
        window: {z_fileuploader: uploader, tinyMCEzMedia: {toHTML: () => '<img>'}},
        document: {getElementById: () => ({dataset: {}}), createElement: tag => tag === 'div' ? zone : status},
        crypto: {randomUUID: () => 'test'}
    });
    const event = {dataTransfer: {types: ['Files'], files: [{}]}, preventDefault() { this.prevented = true; }, stopPropagation() {}};
    return {editor, event, uploader, markers, listeners, insertion: () => insertion};
}

test('TinyMCE 4 rejects a pending insertion after becoming read-only', () => {
    const f = fixture();
    f.uploader.tinyMCEDrop(f.editor, f.event);
    f.editor.readonly = true;
    assert.equal(f.insertion().insert(123, 0), false);
    assert.equal(f.markers[0].isConnected, true);
    f.insertion().dispose();
    assert.equal(f.markers[0].isConnected, false);
});

test('TinyMCE editor removal releases bookmarks and refuses late insertion', () => {
    const f = fixture();
    f.uploader.tinyMCEDrop(f.editor, f.event);
    f.listeners.get('remove')();
    assert.equal(f.insertion().insert(123, 0), false);
    f.insertion().dispose();
    assert.equal(f.listeners.size, 0);
});

test('TinyMCE leaves internal text drags alone', () => {
    const f = fixture();
    f.event.dataTransfer.types = ['text/html'];
    f.uploader.tinyMCEDrop(f.editor, f.event);
    assert.equal(f.event.prevented, undefined);
    assert.equal(f.insertion(), undefined);
});
