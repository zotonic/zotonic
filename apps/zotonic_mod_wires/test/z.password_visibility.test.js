// Run with: node --test apps/zotonic_mod_wires/test/z.password_visibility.test.js
const { test } = require('node:test');
const assert = require('node:assert/strict');
const { readFileSync } = require('node:fs');
const { runInNewContext } = require('node:vm');
const { join } = require('node:path');

const source = readFileSync(join(__dirname, '../priv/lib/js/apps/zotonic-wired.js'), 'utf8');
const handler = source.slice(source.indexOf('const z_password_visibility ='), source.indexOf('function zotonic_startup()'));

function setup() {
    const nodes = new Map();
    const subscriptions = new Map();
    const document = {
        getElementById: id => nodes.get(id),
        querySelectorAll: () => [...nodes.values()].filter(n => n.type === 'password' || n.type === 'text')
    };
    runInNewContext(handler + '\nz_password_visibility.init();', {
        document,
        cotonic: { broker: { subscribe: (topic, callback) => subscriptions.set(topic, callback) } }
    });
    const field = (id, form) => {
        const input = { id, type: 'password', value: 'unchanged secret', autocomplete: 'new-password' };
        const attrs = { 'aria-controls': id, 'aria-pressed': 'false' };
        const button = {
            id: 'toggle-' + id, form,
            querySelector: () => ({ setAttribute: (name, value) => { attrs[name] = value; } }),
            matches: () => true,
            getAttribute: name => attrs[name],
            setAttribute: (name, value) => { attrs[name] = value; }
        };
        nodes.set(id, input);
        nodes.set(button.id, button);
        return { input, button, attrs };
    };
    const click = button => subscriptions.get('model/password/post/toggle')({ payload: { message: { id: button.id } } });
    const rerender = (...buttons) => {
        nodes.set('logon-box', { matches: () => false, querySelectorAll: () => buttons });
        subscriptions.get('model/ui/event/dom-updated/+key')({ payload: { id: 'logon-box' } });
    };
    return { nodes, field, click, rerender };
}

test('toggle preserves the input, value, autocomplete and other password fields', () => {
    const s = setup();
    const first = s.field('password_reset1');
    const second = s.field('password_reset2');
    s.click(first.button);
    assert.equal(first.input.type, 'text');
    assert.equal(first.attrs['aria-pressed'], 'true');
    assert.equal(first.attrs.visibility, 'visible');
    assert.equal(second.input.type, 'password');
    s.click(first.button);
    assert.equal(first.input.type, 'password');
    assert.equal(first.attrs['aria-pressed'], 'false');
    assert.equal(first.attrs.visibility, 'hidden');
    assert.equal(first.input.value, 'unchanged secret');
    assert.equal(first.input.autocomplete, 'new-password');
    assert.equal(s.nodes.get('password_reset1'), first.input);
});

test('dynamic replacement needs no rebinding and rerenders remask reused inputs', () => {
    const s = setup();
    const old = s.field('password');
    s.click(old.button);
    s.rerender(old.button);
    assert.equal(old.input.type, 'password');
    assert.equal(old.attrs['aria-pressed'], 'false');
    const replacement = s.field('password');
    s.click(replacement.button);
    assert.equal(replacement.input.type, 'text');
    assert.equal(old.input.type, 'password');
});

test('target lookup stays inside the button form when ids occur in multiple forms', () => {
    const s = setup();
    const first = s.field('password');
    const second = s.field('password', { querySelectorAll: () => [second.input] });
    s.click(second.button);
    assert.equal(second.input.type, 'text');
    assert.equal(first.input.type, 'password');
});

test('stale buttons, missing targets and disabled inputs are harmless', () => {
    const s = setup();
    const field = s.field('password');
    field.input.disabled = true;
    s.click(field.button);
    assert.equal(field.input.type, 'password');
    s.nodes.delete('password');
    assert.doesNotThrow(() => s.click(field.button));
    s.nodes.delete(field.button.id);
    assert.doesNotThrow(() => s.click(field.button));
});
