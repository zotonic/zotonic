/* Run with node --test apps/zotonic_mod_fileuploader/test/upload_preflight_test.js */
const test = require('node:test');
const assert = require('node:assert/strict');
const crypto = require('node:crypto');
const fs = require('node:fs');
const vm = require('node:vm');
const hashSource = fs.readFileSync('apps/zotonic_mod_base/priv/lib/js/z.media-upload-hash.js', 'utf8');

test('incremental SHA-256 matches crypto across block and chunk boundaries', async () => {
    const context = vm.createContext({self: {addEventListener() {}}});
    vm.runInContext(hashSource, context);
    for (const size of [0,1,3,55,56,63,64,65,127,128,1048575,1048576,1048577,3145777]) {
        const bytes = crypto.randomBytes(size);
        assert.equal(await context.self.z_media_upload_hash(new Blob([bytes])),
            crypto.createHash('sha256').update(bytes).digest('hex'));
    }
});

test('completed reuse submits token without starting any block transfer', async () => {
    const subscriptions = {}, publications = [];
    let checked = false;
    let context;
    const self = {
        addEventListener() {},
        connect: () => Promise.resolve(),
        subscribe: (topic, callback) => { subscriptions[topic] = callback; },
        publish: (topic, payload) => publications.push({topic, payload}),
        call: async (topic, payload) => {
            assert.equal(topic, 'bridge/origin/model/fileuploader/post/new');
            assert.equal(payload.digest, crypto.createHash('sha256').update('test file').digest('hex'));
            checked = true;
            return {payload: {status: 'ok', result: {name: 'reuse', is_complete: true,
                size: 9, received: 9, missing: [], media_upload_token: 'signed-token'}}};
        }
    };
    context = vm.createContext({self, console, setTimeout, Date,
        XMLHttpRequest: function() { throw Error('Unexpected file transfer'); },
        importScripts: () => vm.runInContext(hashSource, context)});
    // Resolve the browser's absolute module import in this isolated test realm.
    vm.runInContext(hashSource, context);
    vm.runInContext(fs.readFileSync('apps/zotonic_mod_fileuploader/priv/lib/js/zotonic.fileuploader.worker.js','utf8')
        .replace('import "/lib/js/z.media-upload-hash.js";', ''), context);
    await new Promise(resolve => setTimeout(resolve, 0));
    const file = new Blob(['test file']); file.name = 'test.txt';
    subscriptions['model/fileuploader/post/new']({properties: {}, payload: {
        files: [{name: 'upload_file', file}], ready_topic: 'test/ready',
        ready_msg: {_type: 'postback_event', data: {q: []}}
    }});
    for (let i=0; i<100 && !publications.some(p => p.topic === 'test/ready'); i++) {
        await new Promise(resolve => setTimeout(resolve, 5));
    }
    assert(checked);
    const ready = publications.find(p => p.topic === 'test/ready');
    assert(ready);
    assert.equal(ready.payload.data.q[0].value.media_upload_token, 'signed-token');
    assert.equal(ready.payload.data.q[0].value._type, undefined);
});
