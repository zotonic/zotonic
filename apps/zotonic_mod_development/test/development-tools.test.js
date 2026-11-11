const {test} = require('node:test');
const assert = require('node:assert/strict');
const fs = require('node:fs');
const vm = require('node:vm');
const source = fs.readFileSync(require.resolve('../priv/lib/js/development-tools.js'), 'utf8');

function harness(ready = Promise.resolve()) {
    const calls = [];
    let failure;
    const result = {example: 'server result'};
    const ctx = {window: {}, location: {pathname: '/nl/news/example'}, cotonic: {
        ready,
        broker: {call: async (topic, payload) => {
            calls.push({topic, payload: JSON.parse(JSON.stringify(payload))});
            if (failure === 'transport') throw new Error('disconnected');
            return {payload: failure ? {status: 'error', error: failure} : {status: 'ok', result}};
        }}
    }};
    vm.runInNewContext(source, ctx);
    return {api: ctx.window.z_development_tools, calls, result, fail: value => { failure = value; }};
}

test('inspection uses only GET model calls without a capture or debug button', async () => {
    const h = harness();
    assert.equal(await h.api.dispatch(), h.result);
    await h.api.dispatchTrace();
    await h.api.dispatchTrace('/en/other');
    await h.api.template('page.tpl');
    await h.api.template('page.tpl', 'article_name');
    await h.api.template('page.tpl', 42);
    await h.api.templateSource("page.tpl");
    await h.api.templateSource("page.tpl", 42);
    await h.api.templateSource("page.tpl", undefined, {candidate: 0});
    await h.api.templateSource("page.tpl", 42, {candidate: 1});
    await h.api.templateComposition("page.tpl");
    await h.api.templateComposition("page.tpl", 42, {candidate: 1});
    await h.api.templates();
    await h.api.templates({prefix: 'blocks/', offset: 50, limit: 100});
    await h.api.models();
    await h.api.models({prefix: "rsc", offset: 2, limit: 10});
    await h.api.modelDocumentation("rsc");
    await h.api.modules();
    assert.deepEqual(h.calls.map(c => [c.topic.replace('bridge/origin/model/development_tools/get/', ''), c.payload]), [
        ['dispatch/list', {}],
        ['dispatch/trace', {path: '/nl/news/example'}],
        ['dispatch/trace', {path: '/en/other'}],
        ['template/lookup', {name: 'page.tpl'}],
        ['template/lookup', {name: 'page.tpl', id: 'article_name'}],
        ['template/lookup', {name: 'page.tpl', id: 42}],
        ['template/source', {name: 'page.tpl'}],
        ['template/source', {name: 'page.tpl', id: 42}],
        ['template/source', {name: 'page.tpl', candidate: 0}],
        ['template/source', {name: 'page.tpl', id: 42, candidate: 1}],
        ['template/composition', {name: 'page.tpl'}],
        ['template/composition', {name: 'page.tpl', id: 42, candidate: 1}],
        ['templates', {prefix: '', offset: 0, limit: 50}],
        ['templates', {prefix: 'blocks/', offset: 50, limit: 100}],
        ['models', {prefix: '', offset: 0, limit: 50}],
        ['models', {prefix: 'rsc', offset: 2, limit: 10}],
        ['model/documentation', {name: 'rsc'}],
        ['modules', {}]
    ]);
    assert(h.calls.every(c => c.topic.startsWith('bridge/origin/model/development_tools/get/')));
});

test('waits for the browser bridge before requesting inspection', async () => {
    let resolve;
    const h = harness(new Promise(r => { resolve = r; }));
    const request = h.api.dispatchTrace();
    assert.equal(h.calls.length, 0);
    resolve();
    assert.equal(await request, h.result);
});

test('propagates authorization, lookup, validation and transport failures', async () => {
    const h = harness();
    for (const code of ['eacces', 'enoent', 'badarg', 'too_large', 'invalid_utf8']) {
        h.fail(code);
        await assert.rejects(h.api.template('page.tpl'), {message: code, code});
        await assert.rejects(h.api.templateSource('page.tpl'), {message: code, code});
        await assert.rejects(h.api.templateComposition('page.tpl'), {message: code, code});
        await assert.rejects(h.api.modelDocumentation('rsc'), {message: code, code});
    }
    h.fail('transport');
    await assert.rejects(h.api.dispatch(), {message: 'disconnected'});
    h.fail(undefined);
    assert.equal(await h.api.modules(), h.result);
});
