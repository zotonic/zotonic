const {test} = require('node:test');
const assert = require('node:assert/strict');
const fs = require('node:fs');
const vm = require('node:vm');
const source = fs.readFileSync(require.resolve('../priv/lib/js/development-capture.js'), 'utf8');

function harness(allowed, {install = true, storedId} = {}) {
    const elements = [];
    const calls = [];
    const timers = [];
    const printed = [];
    const storage = new Map(storedId ? [["zotonic.development.capture", storedId]] : []);
    let failure;
    let failurePath;
    const logs = [];
    let state = 'stopped';
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
            if ((failure && (!failurePath || topic.includes(failurePath))) || topic.endsWith('/expired')) {
                return {payload: {status: 'error', error: failure || 'enoent'}};
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
            return {payload: {status: 'ok', result: {id: 'cap', state, console_allowed: allowed}}};
        }}}
    };
    vm.runInNewContext(source, ctx);
    const button = element();
    if (install) ctx.window.z_development_capture.install(button, {trace: 'Trace', attach: 'Attach', stop: 'Stop'}, allowed);
    return {ctx, elements, calls, timers, original, printed, storage, failWith: (value, path) => { failure = value; failurePath = path; }};
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
    assert.equal(h.calls.at(-1).payload.entries.length, 0);
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
    const trace = await api.trace();
    assert.equal(trace.capture.id, 'cap');
    assert.equal(trace.nodes[0].filepath, '/page.tpl');
    const logs = await api.console();
    assert.equal(logs.entries[0].text, 'lazy component rendered');
    assert.equal(logs.truncated, false);
    assert.equal(h.calls.at(-2).topic.endsWith('/poll/cap'), true);
    assert.equal(h.calls.at(-2).payload.entries.length, 1);
    assert.equal((await api.console()).entries.length, 1);
    await api.stop();
    assert.equal((await api.trace()).capture.state, 'stopped');
    assert.equal((await api.console()).entries.length, 1);
    assert.equal(h.calls.filter(c => c.topic.endsWith('/start/cap')).length, 1);
});

test('expired evidence returns null without attaching, including expiry between poll and fetch', async () => {
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
