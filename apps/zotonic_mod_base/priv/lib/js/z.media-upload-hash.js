/* Copyright 2026 Marc Worrell. Licensed under the Apache License, Version 2.0. */
"use strict";

// Incremental SHA-256 for Blob/File inputs. Keep memory bounded and run this in
// a worker: SubtleCrypto.digest requires the whole file in a single buffer.
self.z_media_upload_hash = async function(file) {
    const k = new Uint32Array([
        0x428a2f98,0x71374491,0xb5c0fbcf,0xe9b5dba5,0x3956c25b,0x59f111f1,0x923f82a4,0xab1c5ed5,
        0xd807aa98,0x12835b01,0x243185be,0x550c7dc3,0x72be5d74,0x80deb1fe,0x9bdc06a7,0xc19bf174,
        0xe49b69c1,0xefbe4786,0x0fc19dc6,0x240ca1cc,0x2de92c6f,0x4a7484aa,0x5cb0a9dc,0x76f988da,
        0x983e5152,0xa831c66d,0xb00327c8,0xbf597fc7,0xc6e00bf3,0xd5a79147,0x06ca6351,0x14292967,
        0x27b70a85,0x2e1b2138,0x4d2c6dfc,0x53380d13,0x650a7354,0x766a0abb,0x81c2c92e,0x92722c85,
        0xa2bfe8a1,0xa81a664b,0xc24b8b70,0xc76c51a3,0xd192e819,0xd6990624,0xf40e3585,0x106aa070,
        0x19a4c116,0x1e376c08,0x2748774c,0x34b0bcb5,0x391c0cb3,0x4ed8aa4a,0x5b9cca4f,0x682e6ff3,
        0x748f82ee,0x78a5636f,0x84c87814,0x8cc70208,0x90befffa,0xa4506ceb,0xbef9a3f7,0xc67178f2
    ]);
    const h = new Uint32Array([0x6a09e667,0xbb67ae85,0x3c6ef372,0xa54ff53a,0x510e527f,0x9b05688c,0x1f83d9ab,0x5be0cd19]);
    const w = new Uint32Array(64);
    const rotr = (x, n) => (x >>> n) | (x << (32-n));
    const block = (bytes, offset) => {
        const view = new DataView(bytes.buffer, bytes.byteOffset + offset, 64);
        for (let i=0; i<16; i++) w[i] = view.getUint32(i*4);
        for (let i=16; i<64; i++) {
            const x = w[i-15], y = w[i-2];
            w[i] = w[i-16] + (rotr(x,7)^rotr(x,18)^(x>>>3)) + w[i-7] + (rotr(y,17)^rotr(y,19)^(y>>>10));
        }
        let [a,b,c,d,e,f,g,j] = h;
        for (let i=0; i<64; i++) {
            const t1 = (j + (rotr(e,6)^rotr(e,11)^rotr(e,25)) + ((e&f)^(~e&g)) + k[i] + w[i]) >>> 0;
            const t2 = ((rotr(a,2)^rotr(a,13)^rotr(a,22)) + ((a&b)^(a&c)^(b&c))) >>> 0;
            j=g; g=f; f=e; e=(d+t1)>>>0; d=c; c=b; b=a; a=(t1+t2)>>>0;
        }
        [a,b,c,d,e,f,g,j].forEach((v,i) => { h[i] += v; });
    };
    const chunkSize = 1024*1024; // Multiple of the SHA-256 block size.
    let tail = new Uint8Array(0);
    for (let offset=0; offset<file.size; offset+=chunkSize) {
        const bytes = new Uint8Array(await file.slice(offset, offset+chunkSize).arrayBuffer());
        const end = bytes.length - bytes.length%64;
        for (let i=0; i<end; i+=64) block(bytes,i);
        tail = bytes.slice(end);
    }
    const final = new Uint8Array(tail.length < 56 ? 64 : 128);
    final.set(tail); final[tail.length] = 0x80;
    const length = new DataView(final.buffer);
    length.setUint32(final.length-8, Math.floor(file.size/0x20000000));
    length.setUint32(final.length-4, (file.size*8)>>>0);
    for (let i=0; i<final.length; i+=64) block(final,i);
    return Array.from(h, v => v.toString(16).padStart(8,"0")).join("");
};

// Also usable as a standalone worker by ordinary multipart forms.
self.addEventListener("message", async function(event) {
    if (event.data?.type !== "media-upload-hash") return;
    try {
        const digest = await self.z_media_upload_hash(event.data.file);
        self.postMessage({digest});
    } catch (_) {
        self.postMessage({error: "hash_failed"});
    }
});
