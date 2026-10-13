import { build } from 'esbuild';
import { readFile, writeFile, mkdir } from 'node:fs/promises';
import path from 'node:path';

const result = await build({
    entryPoints: ['editor.js'],
    outfile: '../../lib/js/zotonic-editor-tiptap.js',
    bundle: true,
    format: 'iife',
    target: ['es2020'],
    minify: true,
    legalComments: 'eof',
    metafile: true
});

// Include the licenses of every package actually included in the browser bundle.
const packages = [...new Set(Object.keys(result.metafile.inputs)
    .filter(file => file.startsWith('node_modules/'))
    .map(file => file.split('/').slice(0, file.split('/')[1].startsWith('@') ? 3 : 2).join('/')))];
const notices = [];
for (const directory of packages.sort()) {
    const pkg = JSON.parse(await readFile(path.join(directory, 'package.json'), 'utf8'));
    let license;
    for (const filename of ['LICENSE', 'LICENSE.md', 'LICENSE.txt', 'license', 'LICENSE-MIT']) {
        try {
            license = await readFile(path.join(directory, filename), 'utf8');
            break;
        } catch (error) {
            if (error.code !== 'ENOENT') throw error;
        }
    }
    if (!license) throw new Error(`Missing license for ${pkg.name}`);
    notices.push(`${pkg.name} ${pkg.version} (${pkg.license})\n\n${license}`);
}
await mkdir('../../lib/js', { recursive: true });
await writeFile('../../lib/js/zotonic-editor-tiptap.LICENSE.txt', notices.join('\n\n--------------------\n\n'));
