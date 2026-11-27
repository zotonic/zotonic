const test = require('node:test');
const assert = require('node:assert/strict');
const fs = require('node:fs');
const {JSDOM} = require('../../zotonic_mod_editor_tiptap/priv/lib-src/tiptap/node_modules/jsdom');
const source = fs.readFileSync('apps/zotonic_mod_editor_tinymce/priv/lib/js/z.editor-tinymce-fullscreen.js', 'utf8');

test('fullscreen Save serializes editors, uses the original submitter, and respects disabled/read-only state', async () => {
    const dom = new JSDOM('<form><textarea></textarea><button type="submit" id="save_stay">Save</button><button type="submit" id="save_view">Save &amp; view</button><button type="button" data-admin-preview="toggle" id="preview">Preview</button><div id="editor"><div class="tox-editor-header"></div></div></form>', {runScripts: 'outside-only'});
    try {
        const {window} = dom;
        const doc = window.document;
        const handlers = new Map();
        const editor = {
            readonly: false, translate: text => text,
            execCommand: () => handlers.get('FullscreenStateChanged')({state: false}),
            getElement: () => doc.querySelector('textarea'), getContainer: () => doc.querySelector('#editor'),
            getParam: () => editor.readonly,
            on: (name, fn) => handlers.set(name, fn), off: name => handlers.delete(name)
        };
        let saves = 0;
        window.tinymce = {triggerSave: () => { doc.querySelector('textarea').value = 'Edited'; }};
        doc.querySelector('form').addEventListener('submit', event => {
            event.preventDefault();
            assert.ok(['save_stay', 'save_view'].includes(event.submitter.id));
            assert.equal(doc.querySelector('textarea').value, 'Edited');
            saves++;
        });
        window.eval(source);
        window.z_tinymce_fullscreen(editor);
        handlers.get('FullscreenStateChanged')({state: true});
        const save = doc.querySelector('[data-save-action]');
        save.click();
        assert.equal(saves, 1);
        doc.querySelector('[data-save-action=save_view]').click();
        assert.equal(saves, 2);
        doc.querySelector('#save_stay').disabled = true;
        await new Promise(resolve => setTimeout(resolve, 0));
        assert.equal(save.disabled, true);
        save.click();
        assert.equal(saves, 2);
        doc.querySelector('#save_stay').disabled = false;
        editor.readonly = true;
        handlers.get('SwitchMode')();
        assert.equal(save.disabled, true);
        assert.equal(doc.querySelectorAll('#preview').length, 1);
        assert.equal(doc.querySelectorAll('[data-admin-preview="toggle"]').length, 2);
        doc.querySelector('[data-fullscreen-close]').click();
        assert.equal(doc.querySelector('.z-tinymce-fullscreen-actions'), null);
        handlers.get('FullscreenStateChanged')({state: true});
        handlers.get('remove')();
        assert.equal(doc.querySelector('.z-tinymce-fullscreen-actions'), null);
    } finally { dom.window.close(); }
});
