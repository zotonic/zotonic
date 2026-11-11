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
            if (failure || topic.endsWith('/expired')) {
                return {payload: {status: 'error', error: failure || 'enoent'}};
            }
            if (topic.endsWith('/start/cap')) state = 'active';
            if (topic.endsWith('/stop/cap')) state = 'stopped';
            return {payload: {status: 'ok', result: {id: 'cap', state, console_allowed: allowed}}};
        }}}
    };
    vm.runInNewContext(source, ctx);
    const button = element();
    if (install) ctx.window.z_development_capture.install(button, {trace: 'Trace', attach: 'Attach', stop: 'Stop'}, allowed);
    return {ctx, elements, calls, timers, original, printed, storage, failWith: value => { failure = value; }};
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
