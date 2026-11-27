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
            if (topic.endsWith('/validate')) return {payload: {status: 'ok'}};
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

test('server rejection of a large file fails and clears worker busy state', async () => {
    const subscriptions = {}, publications = [];
    const self = {
        z_media_upload_hash: async () => undefined,
        connect: () => Promise.resolve(),
        subscribe: (topic, callback) => { subscriptions[topic] = callback; },
        publish: (topic, payload) => publications.push({topic, payload}),
        call: async () => ({payload: {status: 'error', error: 'eacces'}})
    };
    const context = vm.createContext({self, console: {log() {}, error() {}}, setTimeout, Date,
        XMLHttpRequest: function() { throw Error('Rejected files must not start a transfer'); }});
    vm.runInContext(fs.readFileSync('apps/zotonic_mod_fileuploader/priv/lib/js/zotonic.fileuploader.worker.js', 'utf8')
        .replace('import "/lib/js/z.media-upload-hash.js";', ''), context);
    await new Promise(resolve => setTimeout(resolve, 0));
    subscriptions['model/fileuploader/post/new']({properties: {}, payload: {
        files: [{name: 'file', file: {name: 'large.bin', size: 1048576, type: 'application/octet-stream'}}],
        failure_topic: 'test/failed', failure_msg: {target: 'test'}
    }});
    for (let i = 0; i < 100 && !publications.some(p => p.topic === 'test/failed'); i++) {
        await new Promise(resolve => setTimeout(resolve, 5));
    }
    assert(publications.some(p => p.topic === 'test/failed'));
    assert.deepEqual(publications.filter(p => p.topic === 'model/fileuploader/event/busy').map(p => p.payload), [true, false]);
});


for (const stage of ['hash', 'start', 'transfer']) {
    test(`cancellation during ${stage} never submits and cleans late sessions`, async () => {
        const subscriptions = {}, publications = [], xhrs = [];
        let resolveHash, resolveStart;
        const self = {
            z_media_upload_hash: () => stage === 'hash' ? new Promise(resolve => resolveHash = resolve) : Promise.resolve(),
            connect: () => Promise.resolve(),
            subscribe: (topic, callback) => { subscriptions[topic] = callback; },
            publish: (topic, payload) => publications.push({topic, payload}),
            call: topic => topic.endsWith('/validate') ? Promise.resolve({payload: {status: 'ok'}}) : new Promise(resolve => resolveStart = resolve)
        };
        class XHR {
            constructor() { this.listeners = {}; xhrs.push(this); }
            open() {}
            setRequestHeader() {}
            addEventListener(name, fn) { this.listeners[name] = fn; }
            send() {}
            abort() { this.aborted = true; this.listeners.abort(); this.listeners.loadend(); }
        }
        const context = vm.createContext({self, console, setTimeout, Date, XMLHttpRequest: XHR});
        vm.runInContext(fs.readFileSync('apps/zotonic_mod_fileuploader/priv/lib/js/zotonic.fileuploader.worker.js', 'utf8')
            .replace('import "/lib/js/z.media-upload-hash.js";', ''), context);
        await new Promise(resolve => setTimeout(resolve, 0));
        const file = new Blob(['cancellation test']); file.name = 'cancel.txt';
        const feedback = {target: 'zone', upload_id: 'cancel-test'};
        subscriptions['model/fileuploader/post/new']({properties: {}, payload: {
            files: [{name: 'file', file}], progress_msg: feedback, ready_topic: 'test/ready'
        }});
        await new Promise(resolve => setTimeout(resolve, 0));
        const response = {payload: {status: 'ok', result: {name: 'session', upload_url: '/upload', missing: []}}};
        if (stage === 'transfer') {
            resolveStart(response);
            await new Promise(resolve => setTimeout(resolve, 10));
            assert(xhrs.length > 0);
        }
        subscriptions['model/fileuploader/post/cancel']({payload: feedback});
        if (stage === 'hash') resolveHash();
        if (stage === 'start') resolveStart(response);
        await new Promise(resolve => setTimeout(resolve, 10));
        assert(!publications.some(p => p.topic === 'test/ready'));
        assert(publications.some(p => p.topic === 'model/fileuploader/event/cancelled' && p.payload.cancelled));
        assert.equal(context.model.requests.length, 0);
        assert.equal(context.model.files.length, 0);
        assert.equal(context.model.uploaders, 0);
        assert.equal(context.model.is_busy, false);
        if (stage !== 'hash') assert(publications.some(p => p.topic.endsWith('/delete/session')));
        if (stage === 'transfer') assert(xhrs.every(xhr => xhr.aborted));
    });
}


for (const stage of ['starting', 'transferring']) {
    test(`failed multi-file request cleans its ${stage} sibling`, async () => {
        const subscriptions = {}, publications = [], xhrs = [];
        let rejectValidation, resolveStart;
        const self = {
            z_media_upload_hash: async () => undefined,
            connect: () => Promise.resolve(),
            subscribe: (topic, callback) => { subscriptions[topic] = callback; },
            publish: (topic, payload) => publications.push({topic, payload}),
            call: (topic, payload) => {
                if (topic.endsWith('/validate')) {
                    return payload.filename === 'rejected.txt'
                        ? new Promise(resolve => rejectValidation = resolve)
                        : Promise.resolve({payload: {status: 'ok'}});
                }
                return new Promise(resolve => resolveStart = resolve);
            }
        };
        class XHR {
            constructor() { this.listeners = {}; xhrs.push(this); }
            open() {}
            setRequestHeader() {}
            addEventListener(name, fn) { this.listeners[name] = fn; }
            send() {}
            abort() { this.aborted = true; this.listeners.abort(); this.listeners.loadend(); }
        }
        const context = vm.createContext({self, console: {log() {}, error() {}}, setTimeout, Date, XMLHttpRequest: XHR});
        vm.runInContext(fs.readFileSync('apps/zotonic_mod_fileuploader/priv/lib/js/zotonic.fileuploader.worker.js', 'utf8')
            .replace('import "/lib/js/z.media-upload-hash.js";', ''), context);
        await new Promise(resolve => setTimeout(resolve, 0));
        const files = ['rejected.txt', 'sibling.txt'].map(name => {
            const file = new Blob(['test']); file.name = name;
            return {name, file};
        });
        subscriptions['model/fileuploader/post/new']({properties: {}, payload: {
            files, ready_topic: 'test/ready', failure_topic: 'test/failure'
        }});
        await new Promise(resolve => setTimeout(resolve, 0));
        const started = {payload: {status: 'ok', result: {name: 'sibling-session', upload_url: '/upload', missing: []}}};
        if (stage === 'transferring') {
            resolveStart(started);
            await new Promise(resolve => setTimeout(resolve, 10));
            assert(xhrs.length > 0);
        }
        rejectValidation({payload: {status: 'error', error: 'file_not_allowed'}});
        await new Promise(resolve => setTimeout(resolve, 0));
        if (stage === 'starting') resolveStart(started);
        await new Promise(resolve => setTimeout(resolve, 10));
        assert.equal(context.model.files.length, 0);
        assert.equal(context.model.requests.length, 0);
        assert.equal(context.model.uploaders, 0);
        assert.equal(context.model.is_busy, false);
        assert.equal(publications.filter(p => p.topic === 'test/failure').length, 1);
        assert(!publications.some(p => p.topic === 'test/ready'));
        assert(publications.some(p => p.topic.endsWith('/delete/sibling-session')));
        if (stage === 'transferring') assert(xhrs.every(xhr => xhr.aborted));
    });
}

for (const response of ['<html>Proxy error</html>', '{"status":"error"}', 'timeout']) {
    test(`chunk failure terminates after bounded retries: ${response}`, async () => {
        const subscriptions = {}, publications = [];
        let transfers = 0;
        const self = {
            z_media_upload_hash: async () => undefined,
            connect: () => Promise.resolve(),
            subscribe: (topic, callback) => { subscriptions[topic] = callback; },
            publish: (topic, payload) => {
                publications.push({topic, payload});
                if (topic.endsWith('/post/next')) setTimeout(() => subscriptions[topic]({payload}), 0);
            },
            call: async topic => ({payload: {status: 'ok', result: topic.endsWith('/validate') ? true
                : {name: 'broken', upload_url: '/upload', missing: []}}})
        };
        class XHR {
            static DONE = 4;
            constructor() { this.listeners = {}; }
            open() {}
            setRequestHeader() {}
            addEventListener(name, fn) { this.listeners[name] = fn; }
            send() {
                transfers++;
                setTimeout(() => {
                    if (response === 'timeout') {
                        this.listeners.timeout();
                    } else {
                        this.readyState = XHR.DONE;
                        this.status = 200;
                        this.responseText = response;
                        this.onreadystatechange();
                    }
                    this.listeners.loadend();
                }, 0);
            }
            abort() { this.listeners.abort(); this.listeners.loadend(); }
        }
        const context = vm.createContext({self, console: {log() {}, error() {}}, setTimeout, Date, XMLHttpRequest: XHR});
        vm.runInContext(fs.readFileSync('apps/zotonic_mod_fileuploader/priv/lib/js/zotonic.fileuploader.worker.js', 'utf8')
            .replace('import "/lib/js/z.media-upload-hash.js";', ''), context);
        await new Promise(resolve => setTimeout(resolve, 0));
        const file = new Blob(['test']); file.name = 'test.txt';
        subscriptions['model/fileuploader/post/new']({properties: {}, payload: {
            files: [{name: 'file', file}], ready_topic: 'test/ready', failure_topic: 'test/failure'
        }});
        for (let i = 0; i < 100 && context.model.is_busy; i++) {
            await new Promise(resolve => setTimeout(resolve, 5));
        }
        assert.equal(context.model.is_busy, false);
        assert.equal(context.model.uploaders, 0);
        assert.equal(transfers, 10);
        assert.equal(publications.filter(p => p.topic === 'test/failure').length, 1);
        assert(!publications.some(p => p.topic === 'test/ready'));
    });
}
