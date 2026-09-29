// Run with: node --test apps/zotonic_mod_mqtt/test/z.live.test.js
// Copyright 2026 Marc Worrell. Licensed under the Apache License, Version 2.0.
const { test } = require('node:test');
const assert = require('node:assert/strict');
const { readFileSync } = require('node:fs');
const { runInNewContext } = require('node:vm');
const { join } = require('node:path');

function setup() {
    let now = 0;
    let nextTimer = 0;
    const timers = new Map();
    const subscriptions = new Map();
    const elements = new Set(['results', 'summary']);
    const updates = [];
    const context = {
        window: {},
        performance: { now: () => now },
        document: { getElementById: id => elements.has(id) },
        setInterval: () => {},
        setTimeout: (fn, delay) => {
            const id = ++nextTimer;
            timers.set(id, { fn, due: now + delay });
            return id;
        },
        clearTimeout: id => timers.delete(id),
        cotonic: { broker: {
            subscribe: (topic, fn, options) => subscriptions.set(options.wid, { topic, fn }),
            unsubscribe: (_topic, options) => subscriptions.delete(options.wid),
            publish: () => {}
        } },
        z_queue_postback: (target, _postback, params) => updates.push({ target, params, time: now }),
        $: { widget: () => {}, ui: { live: {} } }
    };
    runInNewContext(readFileSync(join(__dirname, '../priv/lib/js/modules/z.live.js'), 'utf8'), context);
    return {
        live: context.window.z_live, updates, elements, timers, subscriptions,
        emit(topic, message) {
            for (const [wid, sub] of subscriptions) {
                if (sub.topic === topic) sub.fn(message, {}, { topic, wid });
            }
        },
        advance(ms) {
            const until = now + ms;
            while (true) {
                const next = [...timers].filter(([, t]) => t.due <= until)
                    .sort((a, b) => a[1].due - b[1].due)[0];
                if (!next) break;
                now = next[1].due;
                timers.delete(next[0]);
                next[1].fn();
            }
            now = until;
        }
    };
}

test('bursts across topics share one interval and retain the latest event', () => {
    const s = setup();
    s.live.subscribe(['sent', 'failed'], 'results', false, 'postback', 3000);
    s.emit('sent', 1);
    s.advance(50);
    s.emit('failed', 2);
    s.advance(49);
    assert.equal(s.updates.length, 0);
    s.advance(1);
    assert.equal(s.updates.length, 1);
    assert.equal(s.updates[0].params.topic, 'failed');
    assert.equal(s.updates[0].params.message, 2);
    s.emit('sent', 3);
    s.advance(2999);
    assert.equal(s.updates.length, 1);
    s.advance(1);
    assert.equal(s.updates[1].params.message, 3);
    assert.equal(s.updates[1].time, 3100);
    s.advance(10000);
    assert.equal(s.updates.length, 2);
});

test('continuous events refresh periodically and deliver the final event', () => {
    const s = setup();
    s.live.subscribe(['sent'], 'results', false, 'postback', 3000);
    for (let i = 0; i < 10; i++) {
        s.emit('sent', i);
        s.advance(1000);
    }
    assert.deepEqual(s.updates.map(u => u.time), [100, 3100, 6100, 9100]);
    s.advance(2000);
    assert.equal(s.updates.at(-1).params.message, 9);
    assert.equal(s.updates.at(-1).time, 9100);
});

test('default remains immediate; separate live targets have separate intervals', () => {
    const s = setup();
    s.live.subscribe(['sent'], 'summary', false, 'postback');
    s.live.subscribe(['sent'], 'results', false, 'postback', 3000);
    s.emit('sent', 1);
    s.emit('sent', 2);
    assert.deepEqual(s.updates.map(u => u.target), ['summary', 'summary']);
    s.advance(3000);
    assert.equal(s.updates.at(-1).target, 'results');
});

test('removed targets and pruned subscriptions do not refresh', () => {
    for (const prune of [false, true]) {
        const s = setup();
        s.live.subscribe(['sent', 'failed'], 'results', false, 'postback', 3000);
        s.emit('sent', 1);
        s.elements.delete('results');
        if (prune) s.live.prune();
        s.advance(3000);
        assert.equal(s.updates.length, 0);
        assert.equal(s.timers.size, 0);
        s.live.prune();
        assert.equal(s.subscriptions.size, 0);
    }
});


test('an idle interval restores the quick first refresh', () => {
    const s = setup();
    s.live.subscribe(['sent'], 'results', false, 'postback', 3000);
    s.emit('sent', 1);
    s.advance(100);
    s.emit('sent', 2);
    s.advance(3001);
    assert.equal(s.updates.at(-1).time, 3100);
    s.emit('sent', 3);
    s.advance(99);
    assert.equal(s.updates.length, 2);
    s.advance(1);
    assert.equal(s.updates.at(-1).time, 3201);
    assert.equal(s.updates.at(-1).params.message, 3);
    s.advance(10000);
    assert.equal(s.timers.size, 0);
    assert.equal(s.updates.length, 3);
});

test('MQTT wire callbacks retain the latest message, mapping and options', () => {
    const s = setup();
    const calls = [];
    const callback = s.live.throttle((...args) => calls.push(args), 3000);
    const first = [{ payload: 1 }, { id: 1 }, { topic: 'first', wid: 'wire' }];
    const latest = [{ payload: 2 }, { id: 2 }, { topic: 'second', wid: 'wire' }];
    callback(...first);
    s.advance(100);
    assert.deepEqual(calls, [first]);
    callback(...first);
    callback(...latest);
    s.advance(2999);
    assert.equal(calls.length, 1);
    s.advance(1);
    assert.deepEqual(calls, [first, latest]);
});

test('MQTT wire callbacks without throttle execute every notification immediately', () => {
    const s = setup();
    const calls = [];
    const callback = s.live.throttle(value => calls.push(value));
    callback(1);
    callback(2);
    assert.deepEqual(calls, [1, 2]);
    assert.equal(s.timers.size, 0);
});
