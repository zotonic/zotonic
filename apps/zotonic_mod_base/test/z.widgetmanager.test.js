const { test } = require('node:test');
const assert = require('node:assert/strict');
const { readFileSync } = require('node:fs');
const { runInNewContext } = require('node:vm');
const { join } = require('node:path');
const source = readFileSync(join(__dirname, '../priv/lib/js/apps/z.widgetmanager.js'), 'utf8');
const tick = () => new Promise(resolve => setImmediate(resolve));

function setup(status = 'ok') {
    const calls = [], loads = [], warnings = [];
    let finish;
    const loaded = new Promise(resolve => { finish = resolve; });
    function $(element) {
        const wrapper = Object.create($.fn);
        wrapper[0] = element;
        wrapper.data = (key, value) => {
            if (value !== undefined) element.data[key] = value;
            return element.data[key];
        };
        return wrapper;
    }
    $.fn = {};
    $.extend = (...args) => args.length === 1 ? Object.assign($, args[0]) : Object.assign(...args);
    const context = {
        jQuery: $, document: {}, IncrementalDOM: { notifications: {} },
        cotonic: { ready: Promise.resolve(), broker: { call: async topic => {
            calls.push(topic);
            return { payload: { status, error: 'disabled', result: [{ url: '/timestamped.js' }] } };
        } } },
        z_assets: { ensure: assets => { loads.push(assets); return loaded; } },
        console: { warn: (...args) => warnings.push(args), error() {} }
    };
    runInNewContext(source, context);
    function element(name, options = {}) {
        const classes = ['do_' + name];
        classes.contains = name => classes.includes(name);
        return { className: classes[0], classList: classes, childNodes: [],
            isConnected: true, data: {}, getAttribute: () => JSON.stringify(options) };
    }
    return { $, calls, loads, warnings, finish, element, context };
}

test('missing widgets share loading and repeated scans initialize once with metadata', async () => {
    const s = setup(), initialized = [];
    const first = s.element('example', { option: 'element' }), second = s.element('example');
    s.$.widgetManager(first);
    s.$.widgetManager(first);
    s.$.widgetManager(second);
    await tick();
    assert.equal(s.calls.length, 1);
    assert.equal(s.loads.length, 1);
    assert.equal(initialized.length, 0);
    s.$.ui.example = { defaults: { option: 'default' } };
    s.$.fn.example = function(options) { initialized.push([this[0], options.option]); };
    s.$.widgetManager(first); // registered, but the pending initialization still owns this node
    s.finish();
    await tick();
    assert.deepEqual(initialized, [[first, 'element'], [second, 'default']]);
});

test('registered widgets remain synchronous without fetching', () => {
    const s = setup();
    let initialized = false;
    s.$.fn.example = () => { initialized = true; };
    s.$.widgetManager(s.element('example'));
    assert.equal(initialized, true);
    assert.equal(s.calls.length, 0);
});

test('disabled autoload does not load assets', async () => {
    const s = setup('error');
    s.$.widgetManager(s.element('example'));
    await tick();
    assert.equal(s.loads.length, 0);
    assert.equal(s.warnings.length, 1);
});

test('removed nodes and removed classes are skipped after loading', async () => {
    const s = setup();
    const removed = s.element('example'), changed = s.element('example');
    let initialized = 0;
    s.$.widgetManager(removed);
    s.$.widgetManager(changed);
    removed.isConnected = false;
    changed.classList.length = 0;
    s.$.fn.example = () => initialized++;
    s.finish();
    await tick();
    assert.equal(initialized, 0);
});

test('IncrementalDOM and dialog alias use the same loader', async () => {
    const s = setup();
    let initialized = 0;
    s.context.IncrementalDOM.notifications.nodesCreated([s.element('dialog')]);
    await tick();
    assert.equal(s.calls[0], 'bridge/origin/model/wires/get/widget/dialog');
    s.$.fn.show_dialog = () => initialized++;
    s.finish();
    await tick();
    assert.equal(initialized, 1);
});

test('invalid names are not requested; missing registrations warn', async () => {
    const s = setup();
    s.$.widgetManager(s.element('../evil'));
    s.$.widgetManager(s.element('unknown'));
    s.finish();
    await tick();
    assert.equal(s.calls.length, 1);
    assert.equal(s.warnings.length, 1);
});
