/* Run with NODE_PATH pointing to the Tiptap package's node_modules (jsdom). */
const { test } = require('node:test');
const assert = require('node:assert/strict');
const { readFileSync } = require('node:fs');
const { join } = require('node:path');
const { JSDOM } = require('jsdom');
const lib = join(__dirname, '../priv/lib/js');

async function fixture(t, center = true) {
    const dom = new JSDOM('<style>.modal-dialog { height: 200px; margin-top: 30px }</style>', {
        pretendToBeVisual: true, runScripts: 'outside-only'
    });
    t.after(() => dom.window.close());
    const { window } = dom;
    let height = 200, viewport = 800, scroll = 0;
    Object.defineProperty(window.document.documentElement, 'clientHeight', { get: () => viewport });
    Object.defineProperty(window, 'innerHeight', { get: () => viewport });
    const observers = [];
    const frames = new Map();
    let sequence = 0;
    window.requestAnimationFrame = fn => { frames.set(++sequence, fn); return sequence; };
    window.cancelAnimationFrame = id => frames.delete(id);
    window.ResizeObserver = class {
        constructor(callback) { this.callback = callback; observers.push(this); }
        observe() {}
        disconnect() { this.disconnected = true; }
    };
    window.HTMLElement.prototype.getClientRects = function() { return this.isConnected ? [{}] : []; };
    window.HTMLElement.prototype.getBoundingClientRect = function() {
        const top = parseFloat(this.style.marginTop || '30') - scroll;
        return { top, bottom: top + height, height };
    };
    window.eval(readFileSync(join(lib, 'apps/jquery-latest.js'), 'utf8'));
    const $ = window.jQuery;
    $.ui = { dialog: {} };
    $.widget = () => {};
    $.fn.modal = function(action) {
        if (action === 'hide') this.trigger('hidden.bs.modal');
        return this;
    };
    window.z_editor_add = window.z_editor_remove = () => {};
    window.eval(readFileSync(join(lib, 'modules/z.dialog.js'), 'utf8'));
    $.dialogAdd({ text: '<p>Body</p>', center });
    await new Promise(resolve => setTimeout(resolve, 10));
    const modal = window.document.querySelector('.modal-dialog');
    const flush = () => {
        const callbacks = [...frames.values()];
        frames.clear();
        callbacks.forEach(fn => fn());
    };
    flush();
    return { $, window, modal, observers, frames, flush,
        margin: () => parseFloat(window.getComputedStyle(modal).marginTop),
        grow(value) { height = value; observers.forEach(o => o.callback()); flush(); },
        resize(value) { viewport = value; window.dispatchEvent(new window.Event('resize')); flush(); },
        scroll(value) { scroll = value; }
    };
}

test('growth moves only enough to fit; fitting growth and shrink leave position unchanged', async t => {
    const f = await fixture(t);
    const initial = f.margin();
    f.grow(300);
    assert.equal(f.margin(), initial);
    f.grow(600);
    assert.equal(f.margin(), 170);
    f.grow(250);
    assert.equal(f.margin(), 170);
    f.grow(1000);
    assert.equal(f.margin(), 30);
});

test('viewport resize reclaims space without recentering when the viewport grows', async t => {
    const f = await fixture(t);
    f.resize(400);
    assert.equal(f.margin(), 170);
    f.resize(900);
    assert.equal(f.margin(), 170);
});

test('scrolled dialogs do not move their top farther out of view', async t => {
    const f = await fixture(t);
    const initial = f.margin();
    f.scroll(initial);
    f.grow(1200);
    assert.equal(f.margin(), initial);
});

test('center false is not observed or moved', async t => {
    const f = await fixture(t, false);
    assert.equal(f.observers.length, 0);
    f.resize(100);
    assert.equal(f.margin(), 30);
});

test('removal disconnects the observer and cancels pending placement', async t => {
    const f = await fixture(t);
    f.observers[0].callback();
    assert.equal(f.frames.size, 1);
    f.$.dialogRemove(f.$('#zmodal'));
    assert.equal(f.observers[0].disconnected, true);
    assert.equal(f.frames.size, 0);
    f.resize(400);
    assert.equal(f.frames.size, 0);
});
