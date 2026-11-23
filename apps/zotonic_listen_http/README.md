# HTTP response compression

The listener uses Cowboy's gzip implementation with two small stream adapters:

```erlang
[cowboy_metrics_h,
 zotonic_compress_cleanup_h,
 cowboy_compress_h,
 zotonic_compress_policy_h,
 cowboy_stream_h]
```

Keep the cleanup, compression and policy handlers together, in this order.
Response commands travel back through the list: policy, compression, cleanup,
then metrics and the connection. The adapters process both complete `response`
commands and streamed `headers` commands returned from `init`, `data` and `info`.
Body commands and other stream commands pass through unchanged.

## Internal suppression signal

For a response without an existing Content-Encoding, the policy checks the
Content-Type. When it is not compressible, it temporarily adds
`content-encoding: identity` and `x-zotonic-compress-skip: 1`.
Cowboy treats the presence of Content-Encoding as a reason to disable compression
for the stream, including subsequent file/body chunks. The cleanup handler removes
both temporary headers before they reach metrics or the client. The marker is
reserved: the policy removes any controller-supplied marker before making its
decision. Existing encodings, including cached gzip variants, remain untouched.

This is an internal adapter protocol, not an HTTP content coding. RFC 9110 section
8.4 discourages sending Content-Encoding: identity, and compression inherent in
JPEG or XLSX is not HTTP content encoding. Neither temporary header is sent.
Controllers and export encoders need no changes.

The adapters depend on Cowboy's documented rule that an existing Content-Encoding
prevents compression. They do not inspect Cowboy's private state or duplicate zlib
handling. Cowboy still owns Accept-Encoding negotiation, ETag protection, Vary,
compression thresholds, buffering and zlib cleanup. Its threshold applies only to
complete responses; eligible streamed responses are compressed regardless of size.
Early errors follow Cowboy's unchanged, uncompressed early-error path.

On Cowboy upgrades, run `zotonic_compress_tests`: it exercises the actual Cowboy
handler, including gzip decoding, existing encodings, streamed chunks and skipped
sendfile bodies. Changing the handler order can leak the internal headers or cause
unwanted compression.

## MIME policy and audit

`z_media_identify:is_mime_compressible/1` is shared by this adapter and
`z_file_entry`'s cached gzip generation. It parses Content-Type case-insensitively,
accepts header parameters and iodata, and rejects missing or malformed types.

The allowlist includes:

- `text/*`, including HTML, CSS, CSV, calendar data and Zotonic's UBF exports.
- JavaScript and its `application/x-javascript` and `application/ecmascript` aliases.
- JSON, XML and structured `+json` / `+xml` suffixes, including SVG, Atom/RSS,
  JSON Feed, manifests and vendor-specific formats.
- URL-encoded form data, WebAssembly, and uncompressed `application/x-bert`
  exports (the exporter uses `term_to_binary/1`, without compression).
- Uncompressed `font/ttf` and `font/otf` fonts.

Compared with the old private file-cache list, structured suffixes, ECMAScript,
form data, WebAssembly, BERT and TTF/OTF are additions. Existing text, JavaScript,
JSON, XML and SVG support is retained. Text event streams retain Cowboy's default
flushing behavior; this change does not enable compression buffering.

JPEG, PNG, GIF, WebP, AVIF, audio/video, ZIP/GZIP, XLSX/other OpenXML,
OpenDocument and WOFF/WOFF2 are excluded. PDF, octet-stream and unknown formats
are also excluded conservatively, even though some individual files may compress.
The policy is deliberately not the inverse of `is_mime_compressed/1`: failing to
recognize a compressed format must not opt it into compression. That older media
classification helper is unchanged and is not used by this adapter.
