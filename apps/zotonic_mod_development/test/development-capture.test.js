const {test} = require('node:test');
const assert = require('node:assert/strict');
const fs = require('node:fs');
const vm = require('node:vm');
const source = fs.readFileSync(require.resolve('../priv/lib/js/development-capture.js'), 'utf8');

function harness(allowed, {install = true, storedId, readOnly = false, initialState = 'stopped', captures = []} = {}) {
    const elements = [];
    const calls = [];
    const timers = [];
    const printed = [];
    const storage = new Map(storedId ? [["zotonic.development.capture", storedId]] : []);
    let failure;
    let failurePath;
    let ownership = true;
    const logs = [];
    let state = initialState;
    const element = () => {
        const e = {listeners: {}, children: [], addEventListener(k, f) {this.listeners[k] = f;},
            append(...nodes) {this.children.push(...nodes);}};
        elements.push(e);
        return e;
    };
    const original = (...args) => printed.push(args);
    const ctx = {
        window: {}, console: {log: original}, location: {pathname: '/example'},
        document: {createElement: element, body: {append() {}}},
        sessionStorage: {getItem: k => storage.get(k), setItem: (k,v) => storage.set(k,v), removeItem: k => storage.delete(k)},
        setTimeout: fn => timers.push(fn),
        cotonic: {ready: Promise.resolve(), broker: {call: async (topic, payload) => {
            calls.push({topic, payload});
            if (readOnly && topic.includes('/post/')) {
                return {payload: {status: 'error', error: 'eacces'}};
            }
            if (topic.endsWith('/post/poll/foreign')) {
                return {payload: {status: 'error', error: 'badarg'}};
            }
            if ((failure && (!failurePath || topic.includes(failurePath))) || topic.endsWith('/expired')) {
                if (failure instanceof Error) throw failure;
                return {payload: {status: 'error', error: failure || 'enoent'}};
            }
            if (topic.endsWith('/get/list')) {
                return {payload: {status: 'ok', result: captures}};
            }
            if (topic.endsWith('/poll/cap') && state === 'active') {
                logs.push(...payload.entries.map(text => ({seq: logs.length + 1, text})));
            }
            if (topic.endsWith('/get/console/cap')) {
                return {payload: {status: 'ok', result: {entries: [...logs], truncated: false}}};
            }
            if (topic.endsWith('/get/trace/cap')) {
                return {payload: {status: 'ok', result: {capture: {id: 'cap', state}, nodes: [{filepath: '/page.tpl'}], edges: []}}};
            }
            if (topic.endsWith('/start/cap')) state = 'active';
            if (topic.endsWith('/stop/cap')) state = 'stopped';
            const resultId = topic.endsWith('/attach') ? 'cap' : topic.split('/').at(-1);
            return {payload: {status: 'ok', result: {id: resultId, state, console_allowed: allowed,
                write_allowed: allowed && !readOnly, is_owner: resultId === 'foreign' ? false : ownership}}};
        }}}
    };
    vm.runInNewContext(source, ctx);
    const button = element();
    if (install) ctx.window.z_development_capture.install(button, {trace: 'Trace', attach: 'Attach', stop: 'Stop'}, allowed);
    return {ctx, elements, calls, timers, original, printed, storage,
        setReadOnly: value => { readOnly = value; },
        setOwnership: value => { ownership = value; },
        failWith: (value, path) => { failure = value; failurePath = path; }};
}

test('attach, capture console without swallowing output, flush and restore on stop', async () => {
    const h = harness(true);
    const panel = h.elements[1];
    await panel.children[1].listeners.click();
    assert.notEqual(h.ctx.console.log, h.original);
    const circular = {}; circular.self = circular;
    h.ctx.console.log('hello', {value: 3}, circular);
    assert.equal(h.printed.length, 1);
    await h.timers.shift()();
    const poll = h.calls.find(c => c.topic.endsWith('/poll/cap') && c.payload.entries.length);
    assert.match(poll.payload.entries[0], /hello.*value.*unserializable/);
    await panel.children[2].listeners.click();
    assert.equal(h.ctx.console.log, h.original);
});

test('production attachment never wraps console.log', async () => {
    const h = harness(false);
    await h.elements[1].children[1].listeners.click();
    assert.equal(h.ctx.console.log, h.original);
    await h.timers.shift()();
    assert.equal(h.calls.at(-1).topic.endsWith('/get/status/cap'), true);
});


test('public API works before button installation and serializes concurrent attachments', async () => {
    const h = harness(true, {install: false});
    const api = h.ctx.window.z_development_capture;
    assert.equal(await api.status(), null);
    assert.equal(await api.stop(), null);
    const results = await Promise.all([api.attach(), api.attach()]);
    assert.equal(results[0].id, 'cap');
    assert.equal(results[1].id, 'cap');
    assert.equal(h.calls.filter(c => c.topic.endsWith('/attach')).length, 1);
    assert.equal(h.calls.filter(c => c.topic.endsWith('/start/cap')).length, 1);
    assert.equal((await api.status()).state, 'active');
    h.ctx.console.log('final message');
    assert.equal((await api.stop()).state, 'stopped');
    assert.equal(h.calls.at(-2).payload.entries[0], 'final message');
    assert.equal(h.ctx.console.log, h.original);
    assert.equal((await api.attach()).id, 'cap');
    assert.equal(h.calls.filter(c => c.topic.endsWith('/attach')).length, 1);
});

test('expired stored attachment is replaced automatically', async () => {
    const h = harness(true, {install: false, storedId: 'expired'});
    assert.equal((await h.ctx.window.z_development_capture.attach()).id, 'cap');
    assert.equal(h.storage.get('zotonic.development.capture'), 'cap');
    assert.equal(h.calls.filter(c => c.topic.endsWith('/attach')).length, 1);
});

test('permission errors reject without creating another attachment and allow recovery', async () => {
    const h = harness(true, {install: false});
    const api = h.ctx.window.z_development_capture;
    await api.attach();
    h.failWith('eacces');
    await assert.rejects(api.attach(), {message: 'eacces'});
    assert.equal(h.ctx.console.log, h.original);
    assert.equal(h.storage.get('zotonic.development.capture'), 'cap');
    assert.equal(h.calls.filter(c => c.topic.endsWith('/attach')).length, 1);
    h.failWith(undefined);
    assert.equal((await api.status()).state, 'active');
});

test('debug buttons call the public API methods', async () => {
    const h = harness(true);
    const api = h.ctx.window.z_development_capture;
    let attaches = 0, stops = 0;
    api.attach = async () => { attaches++; };
    api.stop = async () => { stops++; };
    await h.elements[1].children[1].listeners.click();
    await h.elements[1].children[2].listeners.click();
    assert.equal(attaches, 1);
    assert.equal(stops, 1);
});


test('evidence can be fetched entirely in the browser, including after stopping', async () => {
    const h = harness(true, {install: false});
    const api = h.ctx.window.z_development_capture;
    assert.equal(await api.trace(), null);
    assert.equal(await api.console(), null);
    assert.equal(h.calls.length, 0);
    await api.attach();
    h.ctx.console.log('lazy component rendered');
    // Evidence reads leave pending logs alone; the normal poll uploads them.
    assert.equal((await api.console()).entries.length, 0);
    await h.timers.shift()();
    const firstRead = h.calls.length;
    const trace = await api.trace();
    assert.equal(trace.capture.id, 'cap');
    assert.equal(trace.nodes[0].filepath, '/page.tpl');
    const logs = await api.console();
    assert.equal(logs.entries[0].text, 'lazy component rendered');
    assert.equal(logs.truncated, false);
    assert.equal(h.calls.slice(firstRead).every(c => c.topic.includes('/get/')), true);
    assert.equal((await api.console()).entries.length, 1);
    await api.stop();
    assert.equal((await api.trace()).capture.state, 'stopped');
    assert.equal((await api.console()).entries.length, 1);
    assert.equal(h.calls.filter(c => c.topic.endsWith('/start/cap')).length, 1);
});

test('expired evidence returns null without attaching', async () => {
    for (const method of ['trace', 'console']) {
        const h = harness(true, {install: false, storedId: 'expired'});
        assert.equal(await h.ctx.window.z_development_capture[method](), null);
        assert.equal(h.calls.some(c => c.topic.endsWith('/attach')), false);
        await h.ctx.window.z_development_capture.attach();
        h.failWith('enoent', `/get/${method}/`);
        assert.equal(await h.ctx.window.z_development_capture[method](), null);
        assert.equal(h.storage.has('zotonic.development.capture'), false);
        assert.equal(h.ctx.console.log, h.original);
    }
});

test('read-only admins can read evidence and status without posting or collecting logs', async () => {
    for (const install of [false, true]) {
        const h = harness(true, {install, storedId: 'cap', readOnly: true, initialState: 'active'});
        const api = h.ctx.window.z_development_capture;
        assert.equal((await api.status()).state, 'active');
        assert.equal((await api.trace()).nodes[0].filepath, '/page.tpl');
        assert.equal((await api.console()).entries.length, 0);
        h.ctx.console.log('do not capture');
        await h.timers.shift()();
        assert.equal(h.ctx.console.log, h.original);
        assert.equal(h.calls.every(c => c.topic.includes('/get/')), true);
        assert.equal(h.storage.get('zotonic.development.capture'), 'cap');
    }
});

test('switching to read-only discards buffered logs without a submission attempt', async () => {
    const h = harness(true, {install: false});
    const api = h.ctx.window.z_development_capture;
    await api.attach();
    h.ctx.console.log('pending log');
    h.setReadOnly(true);
    const firstRead = h.calls.length;
    assert.equal((await api.trace()).capture.id, 'cap');
    assert.equal((await api.console()).entries.length, 0);
    await h.timers.shift()();
    assert.equal(h.ctx.console.log, h.original);
    assert.equal(h.calls.slice(firstRead).every(c => c.topic.includes('/get/')), true);
    h.setReadOnly(false);
    await h.timers.shift()();
    assert.equal((await api.console()).entries.length, 0);
});

test('read-only clients can discover and inspect captures without attaching', async () => {
    const captures = [{id: 'cap', path: '/example', state: 'active', expires: 12345,
        truncated: false, trace_scope: 'session'}];
    const h = harness(true, {install: false, readOnly: true, captures});
    const api = h.ctx.window.z_development_capture;
    const result = await api.list();
    assert.deepEqual(result, captures);
    assert.equal(h.calls[0].topic, 'bridge/origin/model/development_capture/get/list');
    assert.equal((await api.trace(result[0].id)).capture.id, 'cap');
    assert.equal((await api.console(result[0].id)).entries.length, 0);
    assert.equal(h.calls.every(c => c.topic.includes('/get/')), true);
    assert.equal(h.storage.size, 0);
    assert.equal(h.ctx.console.log, h.original);
    assert.equal(await api.status(), null);
});

test('list and status expose ownership separately from write permission without session IDs', async () => {
    for (const readOnly of [true, false]) {
        const captures = [
            {id: 'cap', is_owner: true, write_allowed: !readOnly},
            {id: 'foreign', is_owner: false, write_allowed: !readOnly}
        ];
        const h = harness(true, {install: false, readOnly, captures});
        const api = h.ctx.window.z_development_capture;
        const listed = await api.list();
        assert.deepEqual(listed, captures);
        for (const capture of listed) {
            const status = await api.status(capture.id);
            assert.equal(status.is_owner, capture.is_owner);
            assert.equal(status.write_allowed, !readOnly);
            assert.equal('sid' in status, false);
            assert.equal('request_sid' in status, false);
        }
        assert.equal(h.calls.every(c => c.topic.includes('/get/')), true);
        assert.equal(h.storage.size, 0);
    }
});

test('list waits for the bridge and returns an empty array when no captures exist', async () => {
    const h = harness(true, {install: false, readOnly: true});
    let ready;
    h.ctx.cotonic.ready = new Promise(resolve => { ready = resolve; });
    const result = h.ctx.window.z_development_capture.list();
    await Promise.resolve();
    assert.equal(h.calls.length, 0);
    ready();
    assert.deepEqual(await result, []);
    assert.equal(h.calls.length, 1);
    assert.equal(h.storage.size, 0);
});

test('list success and errors preserve the attachment, console wrapper and buffered logs', async () => {
    const h = harness(true, {install: false});
    const api = h.ctx.window.z_development_capture;
    await api.attach();
    const wrapper = h.ctx.console.log;
    h.ctx.console.log('keep while listing');
    const firstRead = h.calls.length;
    assert.deepEqual(await api.list(), []);
    for (const failure of ['eacces', new Error('transport failed')]) {
        h.failWith(failure, '/get/list');
        await assert.rejects(api.list(), {message: typeof failure === 'string' ? failure : failure.message});
        assert.equal(h.storage.get('zotonic.development.capture'), 'cap');
        assert.equal(h.ctx.console.log, wrapper);
    }
    assert.equal(h.calls.slice(firstRead).every(c => c.topic.endsWith('/get/list')), true);
    h.failWith(undefined);
    await api.stop();
    assert.equal(h.calls.at(-2).payload.entries[0], 'keep while listing');
});

test('writable admins can start and stop discovered captures without attaching', async () => {
    const h = harness(true, {install: false, captures: [{id: 'cap', path: '/example'}]});
    const api = h.ctx.window.z_development_capture;
    const [{id}] = await api.list();
    assert.equal((await api.start(id)).state, 'active');
    assert.equal((await api.status(id)).state, 'active');
    assert.equal((await api.stop(id)).state, 'stopped');
    assert.deepEqual(h.calls.filter(c => c.topic.includes('/post/')).map(c => c.topic), [
        'bridge/origin/model/development_capture/post/start/cap',
        'bridge/origin/model/development_capture/post/stop/cap'
    ]);
    assert.equal(h.storage.size, 0);
    assert.equal(h.ctx.console.log, h.original);
    assert.equal(await api.status(), null);
});

test('explicit start and stop preserve the attachment and buffer on success or failure', async () => {
    for (const method of ['start', 'stop']) {
        const h = harness(true, {install: false});
        const api = h.ctx.window.z_development_capture;
        await api.attach();
        const wrapper = h.ctx.console.log;
        h.ctx.console.log('retain local logs');
        const firstCommand = h.calls.length;
        await api[method]('other');
        assert.equal(h.calls.at(-1).topic.endsWith(`/post/${method}/other`), true);
        assert.equal(await api[method]('expired'), null);
        // Explicit commands targeting the local ID must not forget or flush it.
        h.failWith('enoent');
        assert.equal(await api[method]('cap'), null);
        for (const failure of ['eacces', new Error('transport failed')]) {
            h.failWith(failure);
            await assert.rejects(api[method]('cap'), {
                message: typeof failure === 'string' ? failure : failure.message
            });
        }
        assert.equal(h.calls.slice(firstCommand).every(c => c.topic.includes(`/post/${method}/`)), true);
        assert.equal(h.storage.get('zotonic.development.capture'), 'cap');
        assert.equal(h.ctx.console.log, wrapper);
        h.failWith(undefined);
        await api.stop();
        assert.equal(h.calls.at(-2).payload.entries[0], 'retain local logs');
    }
});

test('read-only admins cannot start or stop discovered captures but can still inspect them', async () => {
    const h = harness(true, {install: false, readOnly: true, captures: [{id: 'cap'}]});
    const api = h.ctx.window.z_development_capture;
    const [{id}] = await api.list();
    for (const method of ['start', 'stop']) {
        await assert.rejects(api[method](id), {code: 'eacces'});
    }
    assert.equal((await api.status(id)).state, 'stopped');
    assert.equal((await api.trace(id)).capture.id, id);
    assert.equal((await api.console(id)).entries.length, 0);
    assert.equal(h.storage.size, 0);
    assert.equal(h.ctx.console.log, h.original);
});

test('explicit mutation IDs are validated and start requires an ID', async () => {
    const h = harness(true, {install: false});
    const api = h.ctx.window.z_development_capture;
    await assert.rejects(api.start(), {name: 'TypeError'});
    for (const method of ['start', 'stop']) {
        for (const id of ['', null, 42, {}, 'a/b', '+', '#', 'a\0b']) {
            await assert.rejects(api[method](id), {name: 'TypeError'});
        }
    }
    assert.equal(h.calls.length, 0);
    assert.equal(h.storage.size, 0);
});

test('flush submits pending logs once and leaves capture active for further logging', async () => {
    const h = harness(true, {install: false});
    const api = h.ctx.window.z_development_capture;
    await api.attach();
    const wrapper = h.ctx.console.log;
    h.ctx.console.log('first batch');
    const firstFlush = h.calls.length;
    const results = await Promise.all([api.flush(), api.flush()]);
    assert.equal(results.every(capture => capture.state === 'active'), true);
    assert.equal(h.ctx.console.log, wrapper);
    assert.equal(h.storage.get('zotonic.development.capture'), 'cap');
    assert.equal((await api.console()).entries.length, 1);
    assert.equal((await api.console()).entries[0].text, 'first batch');
    h.ctx.console.log('second batch');
    await api.flush();
    const logs = await api.console();
    assert.deepEqual(logs.entries.map(entry => entry.text), ['first batch', 'second batch']);
    assert.equal(h.calls.slice(firstFlush).some(c => /\/(attach|start|stop)(\/|$)/.test(c.topic)), false);
    assert.equal(h.calls.filter(c => c.topic.endsWith('/poll/cap') && c.payload.entries.length).length, 2);
});

test('flush handles unattached, expired and stopped captures without starting or posting', async () => {
    for (const storedId of [undefined, 'expired', 'cap']) {
        const h = harness(true, {install: false, storedId});
        // Let the initial background status check finish before observing flush.
        await h.ctx.window.z_development_capture.status();
        const firstFlush = h.calls.length;
        const result = await h.ctx.window.z_development_capture.flush();
        if (storedId === 'cap') assert.equal(result.state, 'stopped');
        else assert.equal(result, null);
        assert.equal(h.calls.slice(firstFlush).every(c => c.topic.includes('/get/')), true);
        assert.equal(h.ctx.console.log, h.original);
        assert.equal(h.storage.has('zotonic.development.capture'), storedId === 'cap');
    }
});

test('read-only flush rejects before posting and discards logs buffered before permission change', async () => {
    const h = harness(true, {install: false});
    const api = h.ctx.window.z_development_capture;
    await api.attach();
    h.ctx.console.log('must not submit');
    h.setReadOnly(true);
    const firstFlush = h.calls.length;
    await assert.rejects(api.flush(), {code: 'eacces'});
    assert.equal(h.calls.slice(firstFlush).every(c => c.topic.includes('/get/')), true);
    assert.equal(h.ctx.console.log, h.original);
    assert.equal(h.storage.get('zotonic.development.capture'), 'cap');
    assert.equal((await api.console()).entries.length, 0);
    h.setReadOnly(false);
    await api.flush();
    assert.equal((await api.console()).entries.length, 0);
});

test('flush propagates submission failures without retrying, stopping or losing the attachment', async () => {
    for (const failure of ['eacces', new Error('transport failed'), 'enoent']) {
        const h = harness(true, {install: false});
        const api = h.ctx.window.z_development_capture;
        await api.attach();
        h.ctx.console.log('pending batch');
        const firstFlush = h.calls.length;
        h.failWith(failure, '/post/poll/');
        if (failure === 'enoent') {
            assert.equal(await api.flush(), null);
            assert.equal(h.storage.size, 0);
        } else {
            await assert.rejects(api.flush(), {
                message: typeof failure === 'string' ? failure : failure.message
            });
            assert.equal(h.storage.get('zotonic.development.capture'), 'cap');
        }
        const posts = h.calls.slice(firstFlush).filter(c => c.topic.includes('/post/'));
        assert.equal(posts.length, 1);
        assert.equal(posts[0].topic.endsWith('/poll/cap'), true);
        assert.equal(h.ctx.console.log, h.original);
    }
});

test('detach clears local capture state and logs without stopping the server capture', async () => {
    const h = harness(true);
    const api = h.ctx.window.z_development_capture;
    await api.attach();
    h.ctx.console.log('discard this log');
    const before = h.calls.length;
    assert.equal(await api.detach(), null);
    assert.equal(h.calls.length, before);
    assert.equal(h.storage.size, 0);
    assert.equal(h.ctx.console.log, h.original);
    assert.equal(h.elements[1].children[3].textContent, '');
    assert.equal(await api.status(), null);
    await h.timers.shift()();
    assert.equal(h.calls.length, before);
    assert.equal((await api.status('cap')).state, 'active');
    assert.equal(await api.detach(), null);
    await api.attach();
    await api.flush();
    assert.equal((await api.console()).entries.length, 0);
});

test('detach works read-only and offline without waiting for bridge readiness', async () => {
    const h = harness(true, {install: false, storedId: 'cap', readOnly: true});
    let ready;
    h.ctx.cotonic.ready = new Promise(resolve => { ready = resolve; });
    const api = h.ctx.window.z_development_capture;
    const previousStatus = api.status();
    assert.equal(await api.detach(), null);
    assert.equal(h.storage.size, 0);
    assert.equal(h.ctx.console.log, h.original);
    assert.equal(await api.status(), null);
    assert.equal(h.calls.length, 0);
    ready();
    assert.equal(await previousStatus, null);
    assert.equal(h.calls.length, 0);
});

test('detach ignores an in-flight status reply and cancels queued local commands', async () => {
    const h = harness(true, {install: false});
    const api = h.ctx.window.z_development_capture;
    await api.attach();
    const originalCall = h.ctx.cotonic.broker.call;
    let release, entered;
    const started = new Promise(resolve => { entered = resolve; });
    const gate = new Promise(resolve => { release = resolve; });
    h.ctx.cotonic.broker.call = async (...args) => {
        const response = await originalCall(...args);
        entered();
        await gate;
        return response;
    };
    const status = api.status();
    const flush = api.flush();
    const attach = api.attach();
    await started;
    const before = h.calls.length;
    await api.detach();
    assert.equal(await api.status(), null);
    release();
    assert.deepEqual(await Promise.all([status, flush, attach]), [null, null, null]);
    assert.equal(h.calls.length, before);
    assert.equal(h.storage.size, 0);
    assert.equal(h.ctx.console.log, h.original);
    h.ctx.cotonic.broker.call = originalCall;
    await api.attach();
    assert.notEqual(h.ctx.console.log, h.original);
});

test('detach during an attach request does not save or start the returned capture', async () => {
    const h = harness(true, {install: false});
    const api = h.ctx.window.z_development_capture;
    const originalCall = h.ctx.cotonic.broker.call;
    let release, entered;
    const started = new Promise(resolve => { entered = resolve; });
    const gate = new Promise(resolve => { release = resolve; });
    h.ctx.cotonic.broker.call = async (...args) => {
        const response = await originalCall(...args);
        entered();
        await gate;
        return response;
    };
    const attach = api.attach();
    await started;
    await api.detach();
    release();
    assert.equal(await attach, null);
    assert.equal(h.calls.length, 1);
    assert.equal(h.calls[0].topic.endsWith('/post/attach'), true);
    assert.equal(h.storage.size, 0);
    assert.equal(h.ctx.console.log, h.original);
});

test('detach preserves a console.log replacement installed by another script', async () => {
    const h = harness(true, {install: false});
    const api = h.ctx.window.z_development_capture;
    await api.attach();
    const replacement = () => {};
    h.ctx.console.log = replacement;
    await api.detach();
    assert.equal(h.ctx.console.log, replacement);
});

test('attach by ID validates ownership and reuses or starts an existing capture', async () => {
    for (const initialState of ['active', 'stopped']) {
        const h = harness(true, {install: false, initialState});
        const api = h.ctx.window.z_development_capture;
        const results = await Promise.all([api.attach('cap'), api.attach('cap')]);
        assert.equal(results.every(c => c.id === 'cap' && c.state === 'active'), true);
        assert.equal(h.calls[0].topic.endsWith('/post/poll/cap'), true);
        assert.equal(h.calls[0].payload.entries.length, 0);
        assert.equal(h.calls.filter(c => c.topic.endsWith('/start/cap')).length, initialState === 'active' ? 0 : 1);
        assert.equal(h.calls.some(c => c.topic.endsWith('/attach')), false);
        assert.equal(h.storage.get('zotonic.development.capture'), 'cap');
        assert.notEqual(h.ctx.console.log, h.original);
        h.ctx.console.log('same capture buffer');
        await api.attach('cap');
        await api.flush();
        assert.equal((await api.console()).entries[0].text, 'same capture buffer');
    }
});

test('reconnect failures preserve the current attachment and pending logs', async () => {
    const h = harness(true, {install: false});
    const api = h.ctx.window.z_development_capture;
    await api.attach();
    const wrapper = h.ctx.console.log;
    h.ctx.console.log('keep original logs');
    const before = h.calls.length;
    await assert.rejects(api.attach('foreign'), {code: 'badarg'});
    await assert.rejects(api.attach('expired'), {code: 'enoent'});
    h.setReadOnly(true);
    await assert.rejects(api.attach('other'), {code: 'eacces'});
    h.setReadOnly(false);
    h.failWith(new Error('transport failed'), '/poll/');
    await assert.rejects(api.attach('other'), {message: 'transport failed'});
    assert.equal(h.storage.get('zotonic.development.capture'), 'cap');
    assert.equal(h.ctx.console.log, wrapper);
    assert.equal(h.calls.slice(before).every(c => c.topic.includes('/post/poll/') && c.payload.entries.length === 0), true);
    h.failWith(undefined);
    await api.flush();
    assert.equal((await api.console()).entries[0].text, 'keep original logs');
});

test('reconnect commits storage only after validation and discards logs when switching IDs', async () => {
    const h = harness(true, {install: false});
    const api = h.ctx.window.z_development_capture;
    await api.attach();
    h.ctx.console.log('old capture only');
    const originalCall = h.ctx.cotonic.broker.call;
    let release, entered;
    const started = new Promise(resolve => { entered = resolve; });
    const gate = new Promise(resolve => { release = resolve; });
    h.ctx.cotonic.broker.call = async (...args) => {
        const result = await originalCall(...args);
        entered();
        await gate;
        return result;
    };
    const attach = api.attach('other');
    await started;
    assert.equal(h.storage.get('zotonic.development.capture'), 'cap');
    assert.equal(h.calls.at(-1).payload.entries.length, 0);
    release();
    assert.equal((await attach).id, 'other');
    assert.equal(h.storage.get('zotonic.development.capture'), 'other');
    h.ctx.cotonic.broker.call = originalCall;
    await api.flush();
    assert.equal(h.calls.at(-1).topic.endsWith('/poll/other'), true);
    assert.equal(h.calls.at(-1).payload.entries.length, 0);
});

test('reconnect does not save an ID when starting fails or detach cancels validation', async () => {
    const h = harness(true, {install: false});
    const api = h.ctx.window.z_development_capture;
    h.failWith('eacces', '/start/');
    await assert.rejects(api.attach('cap'), {code: 'eacces'});
    assert.equal(h.storage.size, 0);
    h.failWith(undefined);
    const originalCall = h.ctx.cotonic.broker.call;
    let release, entered;
    const started = new Promise(resolve => { entered = resolve; });
    const gate = new Promise(resolve => { release = resolve; });
    h.ctx.cotonic.broker.call = async (...args) => {
        const result = await originalCall(...args);
        entered();
        await gate;
        return result;
    };
    const attach = api.attach('cap');
    await started;
    await api.detach();
    const before = h.calls.length;
    release();
    assert.equal(await attach, null);
    assert.equal(h.calls.length, before);
    assert.equal(h.storage.size, 0);
    assert.equal(h.ctx.console.log, h.original);
});

test('attach by ID rejects malformed IDs without network requests', async () => {
    const h = harness(true, {install: false});
    for (const id of ['', null, 42, {}, 'a/b', '+', '#', 'a\0b']) {
        await assert.rejects(h.ctx.window.z_development_capture.attach(id), {name: 'TypeError'});
    }
    assert.equal(h.calls.length, 0);
    assert.equal(h.storage.size, 0);
});

test('a foreign stored attachment never wraps console.log or submits logs', async () => {
    for (const install of [true, false]) {
        const h = harness(true, {install, storedId: 'foreign', initialState: 'active'});
        const api = h.ctx.window.z_development_capture;
        assert.equal((await api.status()).is_owner, false);
        assert.equal(h.ctx.console.log, h.original);
        h.ctx.console.log('not for the foreign capture');
        await h.timers.shift()();
        for (const method of ['flush', 'stop', 'attach']) {
            await assert.rejects(api[method](), {code: 'eacces'});
        }
        assert.equal(h.calls.every(c => c.topic.includes('/get/')), true);
        assert.equal(h.ctx.console.log, h.original);
        assert.equal(h.storage.get('zotonic.development.capture'), 'foreign');
        // Explicit control by a writable admin remains available without log submission.
        await api.stop('foreign');
        assert.equal(h.calls.at(-1).topic.endsWith('/post/stop/foreign'), true);
        await api.detach();
        assert.equal(h.storage.size, 0);
    }
});

test('lost or missing ownership drops buffered logs before every local submission path', async () => {
    for (const owner of [false, undefined, 'true']) {
        for (const method of ['status', 'flush', 'stop', 'attach', 'background']) {
            const h = harness(true, {install: false});
            const api = h.ctx.window.z_development_capture;
            await api.attach();
            const oldWrapper = h.ctx.console.log;
            h.ctx.console.log('discard after session change');
            h.setOwnership(owner);
            const before = h.calls.length;
            if (method === 'background') await h.timers.shift()();
            else if (method === 'status') await api.status();
            else await assert.rejects(api[method](), {code: 'eacces'});
            assert.equal(h.calls.slice(before).every(c => c.topic.includes('/get/')), true);
            assert.equal(h.ctx.console.log, h.original);
            assert.equal(h.storage.get('zotonic.development.capture'), 'cap');
            oldWrapper('a retained wrapper must not collect either');
            h.setOwnership(true);
            await api.flush();
            assert.equal((await api.console()).entries.length, 0);
        }
    }
});

test('explicit IDs support unattached readers without changing storage or collecting logs', async () => {
    for (const readOnly of [true, false]) {
        const h = harness(true, {install: false, readOnly, initialState: 'active'});
        const api = h.ctx.window.z_development_capture;
        assert.equal((await api.status('cap')).state, 'active');
        assert.equal((await api.trace('cap')).capture.id, 'cap');
        assert.equal((await api.console('cap')).entries.length, 0);
        assert.deepEqual(h.calls.map(c => c.topic), ['status', 'trace', 'console'].map(
            method => `bridge/origin/model/development_capture/get/${method}/cap`));
        assert.equal(h.storage.size, 0);
        assert.equal(h.ctx.console.log, h.original);
        assert.equal(await api.status(), null);
        await h.timers.shift()();
        assert.equal(h.calls.length, 3);
    }
});

test('explicit reads and failures preserve the current attachment and buffered logs', async () => {
    for (const method of ['status', 'trace', 'console']) {
        const h = harness(true, {install: false});
        const api = h.ctx.window.z_development_capture;
        await api.attach();
        const wrapper = h.ctx.console.log;
        h.ctx.console.log('keep this buffer');
        const firstRead = h.calls.length;
        await api[method]('other');
        assert.equal(h.calls.at(-1).topic.endsWith(`/get/${method}/other`), true);
        assert.equal(await api[method]('expired'), null);
        // Even an explicit lookup of the current ID must not forget it on failure.
        h.failWith('enoent');
        assert.equal(await api[method]('cap'), null);
        h.failWith('eacces');
        await assert.rejects(api[method]('cap'), {message: 'eacces'});
        assert.equal(h.calls.slice(firstRead).every(c => c.topic.includes('/get/')), true);
        assert.equal(h.storage.get('zotonic.development.capture'), 'cap');
        assert.equal(h.ctx.console.log, wrapper);
        h.failWith(undefined);
        await api.stop();
        assert.equal(h.calls.at(-2).payload.entries[0], 'keep this buffer');
    }
});

test('explicit IDs reject invalid topic segments without a request', async () => {
    const h = harness(true, {install: false, readOnly: true});
    const api = h.ctx.window.z_development_capture;
    for (const method of ['status', 'trace', 'console']) {
        for (const id of ['', null, 42, {}, 'a/b', '+', '#', 'a\0b']) {
            await assert.rejects(api[method](id), {name: 'TypeError'});
        }
    }
    assert.equal(h.calls.length, 0);
    assert.equal(h.storage.size, 0);
});

test('evidence read errors reject and preserve the attachment', async () => {
    for (const method of ['trace', 'console']) {
        const h = harness(true, {install: false});
        const api = h.ctx.window.z_development_capture;
        await api.attach();
        h.failWith('eacces', `/get/${method}/`);
        await assert.rejects(api[method](), {message: 'eacces'});
        assert.equal(h.storage.get('zotonic.development.capture'), 'cap');
        assert.equal(h.ctx.console.log, h.original);
    }
});
