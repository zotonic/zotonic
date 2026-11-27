// Run with: node --test apps/zotonic_mod_wires/test/z.ready.test.js
const { test } = require('node:test');
const assert = require('node:assert/strict');
const { readFileSync } = require('node:fs');
const { runInNewContext } = require('node:vm');
const { join } = require('node:path');
const root = join(__dirname, '../');
const head = readFileSync(root + 'priv/templates/_html_head_wires.tpl', 'utf8').match(/<script[^>]*>([\s\S]*?)<\/script>/)[1];
const wired = readFileSync(root + 'priv/lib/js/apps/zotonic-wired.js', 'utf8');
const startup = wired.slice(wired.indexOf('cotonic.ready.then(zotonic_startup)'), wired.indexOf('// Lazy css loading'));
const tick = () => new Promise(setImmediate);
function setup(fail = false) {
    let start;
    const calls = [];
    const context = {
        cotonic: { ready: new Promise(resolve => { start = resolve; }) },
        document: { documentElement: { addEventListener() {}, removeEventListener() {} } },
        zotonic_startup() { calls.push('startup'); if (fail) throw new Error('startup failed'); }
    };
    runInNewContext(head, context);
    const ready = context.zotonic.ready;
    runInNewContext(startup, context);
    return { z: context.zotonic, ready, start, calls };
}
for (const first of ['startup', 'wires']) {
    test('ready waits for both, with ' + first + ' first', async () => {
        const s = setup();
        let ready = false;
        s.ready.then(() => { ready = true; });
        if (first === 'startup') s.start(); else s.z.wiresReadyResolve();
        await tick();
        assert.equal(ready, false);
        if (first === 'startup') s.z.wiresReadyResolve(); else s.start();
        await s.ready;
        assert.equal(ready, true);
        assert.deepEqual(s.calls, ['startup']);
        assert.equal(s.z.ready, s.ready);
        let late = false;
        await s.ready.then(() => { late = true; });
        assert.equal(late, true);
    });
}
test('existing loaded and wires promises remain independent', async () => {
    const s = setup();
    s.z.wiresLoadedResolve();
    await s.z.wiresLoaded;
    s.z.wiresReadyResolve();
    await s.z.wiresReady;
    assert.deepEqual(s.calls, []);
});
test('startup exceptions reject readiness', async () => {
    const s = setup(true);
    const rejected = assert.rejects(s.ready, /startup failed/);
    s.start();
    await rejected;
});
