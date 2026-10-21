// Run with: node --test apps/zotonic_mod_wires/test/z.assets.test.js
const { test } = require('node:test');
const assert = require('node:assert/strict');
const { readFileSync } = require('node:fs');
const { runInNewContext } = require('node:vm');
const { join } = require('node:path');

const source = readFileSync(join(__dirname, '../priv/lib/js/apps/zotonic-wired.js'), 'utf8');
const loader = source.slice(source.indexOf('var z_assets ='), source.indexOf('/* Startup'));
const tick = () => new Promise(resolve => setImmediate(resolve));
const js = key => ({ key, url: '/' + key, type: 'js' });

function setup() {
    const elements = [];
    const errors = [];
    const scripts = [];
    const element = tag => {
        const listeners = new Map();
        return {
            tagName: tag.toUpperCase(), dataset: {},
            addEventListener: (event, fn) => listeners.set(event, fn),
            emit: event => listeners.get(event)?.(),
            remove() { elements.splice(elements.indexOf(this), 1); }
        };
    };
    const context = {
        document: {
            querySelectorAll: () => elements,
            createElement: element,
            head: { appendChild: el => elements.push(el) }
        },
        z_script_nonce: 'test-nonce',
        zotonic_eval: script => scripts.push(script),
        console: { error: (...args) => errors.push(args) }
    };
    runInNewContext(loader, context);
    return { assets: context.z_assets, elements, errors, scripts, element };
}

test('concurrent requests share loads; dependent JS is loaded in order', async () => {
    const s = setup();
    const first = s.assets.ensure([js('a.js'), js('b.js')]);
    const second = s.assets.ensure([js('a.js')]);
    assert.equal(s.elements.length, 1);
    assert.equal(s.elements[0].nonce, 'test-nonce');
    s.elements[0].emit('load');
    await tick();
    assert.equal(s.elements.length, 2);
    assert.equal(s.elements[1].src, '/b.js');
    s.elements[1].emit('load');
    await Promise.all([first, second]);
});

test('initial bundles register individual files and await pending loads', async () => {
    const s = setup();
    const initial = s.element('script');
    initial.dataset.zAssets = JSON.stringify(['a.js', 'b.js']);
    s.elements.push(initial);
    let finished = false;
    const ready = s.assets.ensure([js('b.js')]).then(() => { finished = true; });
    await tick();
    assert.equal(finished, false);
    assert.equal(s.elements.length, 1);
    initial.emit('load');
    await ready;
    await s.assets.ensure([js('a.js')]);
    assert.equal(s.elements.length, 1);
});

test('completed initial bundles do not load again', async () => {
    const s = setup();
    const initial = s.element('script');
    initial.dataset = { zAssets: '["a.js"]', zAssetState: 'loaded' };
    s.elements.push(initial);
    await s.assets.ensure([js('a.js')]);
    assert.equal(s.elements.length, 1);
});

test('updates without dependencies cannot overtake a loading update', async () => {
    const s = setup();
    const order = [];
    const first = s.assets.run([js('a.js')], () => order.push('first'));
    const second = s.assets.run([], () => order.push('second'));
    await tick();
    assert.deepEqual(order, []);
    s.elements[0].emit('load');
    await Promise.all([first, second]);
    assert.deepEqual(order, ['first', 'second']);
});

test('failure skips initialization, permits retry, and does not poison queue', async () => {
    const s = setup();
    let initialized = false;
    const failed = s.assets.run([js('a.js')], () => { initialized = true; });
    const rejected = assert.rejects(failed, /Asset failed/);
    await tick();
    s.elements[0].emit('error');
    await rejected;
    assert.equal(initialized, false);
    const retry = s.assets.run([js('a.js')], () => { initialized = true; });
    await tick();
    assert.equal(s.elements.length, 1);
    s.elements[0].emit('load');
    await retry;
    assert.equal(initialized, true);
});

test('apply waits for CSS, then asynchronous insertion, then initialization', async () => {
    const s = setup();
    let inserted = false;
    let release;
    const insertion = new Promise(resolve => { release = resolve; });
    const result = s.assets.apply({
        html: '<div></div>', javascript: 'init();',
        libs: [{key: 'style-print', url: '/style.css', type: 'css', media: 'print', rel: 'stylesheet'}]
    }, async html => {
        assert.equal(html, '<div></div>');
        inserted = true;
        await insertion;
    });
    await tick();
    assert.equal(inserted, false);
    assert.equal(s.elements[0].media, 'print');
    s.elements[0].emit('load');
    await tick();
    assert.equal(inserted, true);
    assert.deepEqual(s.scripts, []);
    release();
    await result;
    assert.deepEqual(s.scripts, ['init();']);
});
