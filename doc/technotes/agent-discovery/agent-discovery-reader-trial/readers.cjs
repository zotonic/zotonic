// Dependencies are installed in an isolated temporary directory (see report).
const fs = require('node:fs');
const { JSDOM } = require('jsdom');
const { Readability } = require('@mozilla/readability');
const Turndown = require('turndown');
const cases = JSON.parse(fs.readFileSync(process.argv[2], 'utf8'));
const converter = new Turndown();
const rows = cases.flatMap(test => {
    const dom = new JSDOM(test.html, { url: 'https://example.com/reader-trial/page' });
    const body = converter.turndown(dom.window.document.body.innerHTML);
    const article = new Readability(dom.window.document).parse();
    return [
        { id: test.id, reader: 'turndown-body', output: body },
        { id: test.id, reader: 'readability-turndown', output: article ? converter.turndown(article.content) : '', parsed: !!article }
    ];
});
fs.writeFileSync(process.argv[3], JSON.stringify({
    versions: Object.fromEntries(['@mozilla/readability', 'jsdom', 'turndown'].map(name => [name, require(name + '/package.json').version])),
    rows
}, null, 2) + '\n');
