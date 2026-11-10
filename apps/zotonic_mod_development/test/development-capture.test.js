const {test} = require('node:test');
const assert = require('node:assert/strict');
const fs = require('node:fs');
const vm = require('node:vm');
const source = fs.readFileSync(require.resolve('../priv/lib/js/development-capture.js'), 'utf8');

function harness(allowed) {
    const elements = [];
    const calls = [];
    const timers = [];
    const printed = [];
    const storage = new Map();
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
            if (topic.endsWith('/start/cap')) state = 'active';
            if (topic.endsWith('/stop/cap')) state = 'stopped';
            return {payload: {status: 'ok', result: {id: 'cap', state, console_allowed: allowed}}};
        }}}
    };
    vm.runInNewContext(source, ctx);
    const button = element();
    ctx.window.z_development_capture.install(button, {trace: 'Trace', attach: 'Attach', stop: 'Stop'}, allowed);
    return {ctx, elements, calls, timers, original, printed};
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
