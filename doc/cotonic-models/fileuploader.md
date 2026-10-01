---
module: mod_fileuploader
keywords:
  - reference
  - frontend_developer
  - cotonic
  - javascript
  - model
  - file_uploads
  - upload
  - messaging_and_pubsub
---

# model/fileuploader

Queues browser files and uploads them in chunks through `mod_fileuploader`.
The `_fileuploader_worker.tpl` include starts the worker. It depends on
`bridge/origin` and advertises the capability `fileuploader` (without `model/`).

## post/new

Topic: `model/fileuploader/post/new`.

| Payload field | Meaning |
| --- | --- |
| `files` | Non-empty array of `{ name, file, upload }` objects. `file` is a browser `File`; `name` identifies the form field; `upload` optionally supplies the server upload name. |
| `ready_topic`, `ready_msg` | Destination and base payload for completion. |
| `failure_topic`, `failure_msg` | Destination and payload for failure. |
| `progress_topic`, `progress_msg` | Destination and base payload for progress updates. |
| `start_topic` | Optional destination for each file's upload state after server initialization. |

```javascript
cotonic.broker.publish("model/fileuploader/post/new", {
    files: [{ name: "document", file: fileInput.files[0] }],
    ready_topic: "my/upload/ready",
    ready_msg: {},
    failure_topic: "my/upload/failed",
    failure_msg: {},
    progress_topic: "my/upload/progress",
    progress_msg: {}
});
```

On normal completion, the worker adds a `fileuploader` array to `ready_msg`.
Each entry is `{ name, upload }`, where `upload` is the server upload identifier.
For a Zotonic `postback_event`, it instead appends fileuploader references to the
postback's query arguments. Progress payloads add `percentage`; failure publishes
the supplied failure payload.

When the request has a response topic, it receives the initial queue request
object. This acknowledgement does not mean that uploading has finished. Use the
configured completion and failure topics for the result. An empty file array
does not create an upload request; a call instead receives the immediate error
payload `{ status: "error", error: "No files" }` on its response topic.

The worker initializes files through
`bridge/origin/model/fileuploader/post/new`, then sends blocks to the returned
HTTP upload URL. This browser model and the server fileuploader model have
different payload contracts.

## post/delete/+name

Topic: `model/fileuploader/post/delete/+name`.

Delete the upload identified by its server upload name. The worker removes its
local upload state and requests deletion through the server model. The payload
is ignored and the command does not reply.

## post/next

Topic: `model/fileuploader/post/next`.

Internal scheduling trigger used when a block finishes. Applications should
submit files through `post/new` rather than manage this scheduling topic.

There is no fixed `model/fileuploader/event/...` stream: callers choose the
notification topics in each upload request.

## Implementation

[JavaScript source](../../apps/zotonic_mod_fileuploader/priv/lib/js/zotonic.fileuploader.worker.js).
