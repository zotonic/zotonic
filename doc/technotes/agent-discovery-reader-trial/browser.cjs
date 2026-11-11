// Diagnostic only: browser innerText is not an HTML-to-Markdown reader.
const fs = require('node:fs');
const { chromium } = require('playwright-core');
(async () => {
    const browser = await chromium.launch({
        executablePath: '/Applications/Google Chrome.app/Contents/MacOS/Google Chrome',
        headless: true
    });
    const page = await browser.newPage({ viewport: { width: 1200, height: 900 } });
    await page.route('**/*', route => route.abort());
    const rows = [];
    for (const test of JSON.parse(fs.readFileSync(process.argv[2], 'utf8'))) {
        await page.setContent(test.html);
        rows.push({ id: test.id, ...await page.evaluate(() => {
            const link = document.querySelector('a');
            const before = document.body.getBoundingClientRect().height;
            const output = document.body.innerText;
            const computed = link ? {
                display: getComputedStyle(link.parentElement).display,
                position: getComputedStyle(link.parentElement).position,
                clip: getComputedStyle(link.parentElement).clip,
                rects: link.getClientRects().length
            } : null;
            if (link) (link.closest('details') || link.parentElement).remove();
            const after = document.body.getBoundingClientRect().height;
            return { output, computed, bodyHeightBefore: before, bodyHeightAfter: after,
                     bodyHeightDelta: before - after };
        }) });
    }
    fs.writeFileSync(process.argv[3], JSON.stringify({ version: browser.version(), rows }, null, 2) + '\n');
    await browser.close();
})();
