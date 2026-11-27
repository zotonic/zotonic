/**
 * Copyright 2021-2024 Marc Worrell <marc@worrell.nl>
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS-IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

"use strict";

import "/lib/js/z.media-upload-hash.js";


////////////////////////////////////////////////////////////////////////////////
// Model
//

const MAX_UPLOADERS = 5;            // Number of parallel XHR uploaders
const UPLOAD_BLOCKSIZE = 128*1024;  // 128KB for each uploaded block
const MAX_ERROR_COUNT = 10;         // Base number of errors
const MAX_ERROR_RATE = 2.0;         // Allow 200% failure rate during block uploads
const PROGRESS_INTERVAL = 500;      // Every 500msec progress for a request

var model = {
    is_subscribed: false,
    is_busy: false,
    files: [],              // Files being uploaded
    uploaders: 0,           // Number of uploaders busy uploading a single block from any file
    requests: []            // Upload requests, each with one or more files
};


////////////////////////////////////////////////////////////////////////////////
// Support functions
//

var startUploader = function(fileIndex) {
    let f = model.files[fileIndex];
    let offset;

    // File was deleted
    if (!f) return

    if (f.failed.length > 0) {
        offset = f.failed.pop();
        if (f.uploading.indexOf(offset) !== -1) {
            f.failed.push(offset);
            return;
        }
    } else if (f.offset < f.file.size) {
        offset = f.offset;
        if (f.uploading.indexOf(offset) !== -1) {
            return;
        }
        f.offset += UPLOAD_BLOCKSIZE;
        if (f.offset > f.file.size) {
            f.offset = f.file.size;
        }
    } else {
        return;
    }
    model.uploaders++;
    f.uploading.push(offset);

    const end = Math.min(f.file.size, offset + UPLOAD_BLOCKSIZE);
    const data = f.file.slice(offset, end, "application/octet-stream");
    const url = f.status.upload_url + "?offset=" + offset;
    const xhr = new XMLHttpRequest();

    f.requests.add(xhr);
    xhr.open("post", url);
    xhr.setRequestHeader("Content-Type", "application/octet-stream");
    xhr.setRequestHeader("x-no-cache", "1");
    const failedBlock = function() {
        if (f.req.cancelled) return;
        if (!f.failed.includes(offset)) f.failed.push(offset);
        f.error_count++;
    };
    xhr.onreadystatechange = function() {
        if (f.req.cancelled || xhr.readyState !== XMLHttpRequest.DONE) return;
        // Status zero is handled by the error/abort/timeout events below.
        if (xhr.status === 0) return;
        if (xhr.status >= 200 && xhr.status < 300) {
            try {
                const response = JSON.parse(xhr.responseText);
                if (response.status === "ok" && response.result) {
                    f.status = response.result;
                    f.req.uploaded_size += end - offset;
                    return;
                }
            } catch (_) {
                // A proxy or server can return HTML instead of an upload response.
            }
        }
        failedBlock();
    };
    xhr.timeout = 60000;
    xhr.addEventListener("error", failedBlock);
    xhr.addEventListener("abort", failedBlock);
    xhr.addEventListener("timeout", failedBlock);
    xhr.addEventListener("loadend", function() {
        f.requests.delete(xhr);
        f.uploading = f.uploading.filter(u => u !== offset);
        model.uploaders--;
        self.publish("model/fileuploader/post/next", {});
    });

    xhr.send(data);
}

function isFileDeleted(fileIndex, f) {
    return !model.files[fileIndex] ||
            model.files[fileIndex].status?.name !== f.status?.name;
}


////////////////////////////////////////////////////////////////////////////////
// Model
//

model.present = function(data) {
    let is_next = false;

    if (state.start(model) && data.is_depends_provided) {
        model.is_subscribed = true;
        self.subscribe("model/fileuploader/post/new", function(msg) {
            // The post consists of a files array and a topic/payload to
            // publish after the files have been uploaded.
            actions.newUpload(msg);
        });

        self.subscribe("model/fileuploader/post/delete/+name", function(_msg, bindings) {
            actions.deleteUpload(bindings.name);
        });

        self.subscribe("model/fileuploader/post/cancel", function(msg) {
            const {target, upload_id} = msg.payload || {};
            if (typeof target !== "string" || typeof upload_id !== "string") return;
            const req = model.requests.find(r => r.progress_msg.target === target
                && r.progress_msg.upload_id === upload_id);
            if (req) {
                req.cancelled = true;
                model.requests = model.requests.filter(r => r !== req);
                const files = model.files.filter(f => f.req === req);
                model.files = model.files.filter(f => f.req !== req);
                files.forEach(f => {
                    f.requests.forEach(xhr => xhr.abort());
                    if (f.status?.name) self.publish("bridge/origin/model/fileuploader/post/delete/" + f.status.name, {});
                });
                req.uploads.forEach(f => self.publish("bridge/origin/model/fileuploader/post/delete/" + f.upload, {}));
                actions.nextUpload();
            }
            // A missing request has already been handed to the server for saving.
            self.publish("model/fileuploader/event/cancelled", {target, upload_id, cancelled: !!req});
        });

        self.subscribe("model/fileuploader/post/next", function(_msg) {
            actions.nextUpload();
        });
    }

    if (data.upload) {
        // Message with upload request
        if (Array.isArray(data.upload.files) && data.upload.files.length > 0) {
            let files = [];
            let req = {
                ready_topic: data.upload.ready_topic,
                ready_msg: data.upload.ready_msg || {},
                failure_topic: data.upload.failure_topic,
                failure_msg: data.upload.failure_msg || {},
                progress_topic: data.upload.progress_topic,
                progress_msg: data.upload.progress_msg || {},
                total_size: 0,
                uploaded_size: 0,
                error_count: 0,
                upload_count: 0,    // Number of file uploads started on server and uploading
                wait_count: 0,      // Number of file uploads awaiting response from server before starting
                uploads: [],        // Completed uploads
                start: Date.now(),
                progress_published: 0
            };
            model.requests.push(req);

            for (let k = 0; k < data.upload.files.length; k++){
                const f = data.upload.files[k];
                req.wait_count++;
                req.total_size += f.file.size;

                self.call("bridge/origin/model/fileuploader/post/validate", {
                    filename: f.file.name, size: f.file.size
                }).then(resp => {
                    if (req.cancelled) return;
                    if (resp.payload.status !== "ok") throw resp.payload.error;
                    return self.z_media_upload_hash(f.file).catch(() => undefined);
                }).then(digest =>
                    req.cancelled ? undefined : self.call("bridge/origin/model/fileuploader/post/new", {
                        digest: digest,
                        name: f.upload,
                        filename: f.file.name,
                        size: f.file.size,
                        mime: f.file.type
                    }, { qos: 2 }
                )).then(function(resp) {
                    if (req.cancelled) {
                        if (resp?.payload.status === "ok") {
                            self.publish("bridge/origin/model/fileuploader/post/delete/" + resp.payload.result.name, {});
                        }
                        return;
                    }
                    const max_error = MAX_ERROR_COUNT + Math.floor((f.file.size / UPLOAD_BLOCKSIZE) * MAX_ERROR_RATE);
                    let upload = {
                        start: Date.now(),
                        file: f.file,
                        name: f.name,
                        status: {},
                        offset: 0,
                        uploading: [],
                        requests: new Set(),
                        failed: [],
                        error_count: 0,
                        max_error: max_error,
                        req: req
                    };
                    if (resp.payload.status == "ok") {
                        upload.status = resp.payload.result;
                    } else {
                        req.error = resp.payload.error;
                        upload.error_count = max_error;
                        upload.status = undefined;
                    }
                    model.files.push(upload);
                    req.upload_count++;
                    req.wait_count--;

                    setTimeout(actions.startUploader, 0);

                    if (data.upload.start_topic) {
                        self.publish(data.upload.start_topic, upload);
                    }
                }).catch(function(error) {
                    if (req.cancelled) return;
                    req.wait_count--;
                    req.error = error;
                    req.error_count++;
                    actions.nextUpload();
                });
            }
            if (data.response_topic) {
                self.publish(data.response_topic, req);
            }
            is_next = true;
        } else {
            console.log("Fileuploader request without files ", data.upload);
            self.publish(data.response_topic, { status: "error", error: "No files" });
        }
    } else if (data.delete_upload) {
        model.files = model.files.filter(f => f.status?.name !== data.delete_upload);
        model.requests = model.requests.map(r => {
            r.uploads = r.uploads.filter(u => u.status?.name !== data.delete_upload);
            return r;
        });
        self.publish(`bridge/origin/model/fileuploader/post/delete/${data.delete_upload}`);
    }

    let fs = [];

    // Mark request failed if any of its uploads has too high error count
    for (let i = 0; i < model.files.length; i++) {
        let f = model.files[i];
        if (f.error_count >= f.max_error) {
            f.req.error_count++;
        }
    }

    // Remove all files with failed req
    fs = [];
    for (let i = 0; i < model.files.length; i++) {
        const f = model.files[i];
        if (f.req.error_count == 0) {
            fs.push(f);
        } else {
            // Stop sibling transfers and ignore their late callbacks after failure.
            f.req.cancelled = true;
            f.requests.forEach(xhr => xhr.abort());
            if (f.status?.name) {
                self.publish("bridge/origin/model/fileuploader/post/delete/" + f.status.name, {});
            }
            f.req.upload_count--;
        }
    }
    model.files = fs;

    // Check all file uploads for status.is_complete
    fs = [];
    for (let i = 0; i < model.files.length; i++) {
        const f = model.files[i];
        if (f.status?.is_complete) {
            // File is complete - add it to the uploaded files and
            // remove it from the uploading files.
            f.req.uploads.push({
                name: f.name,
                upload: f.status.name,
                ...(f.status.media_upload_token ? {media_upload_token: f.status.media_upload_token} : {})
            });
            if (f.status.media_upload_token) f.req.uploaded_size += f.file.size;
            f.req.upload_count--;
        } else {
            // Start uploading missing blocks if the last block has been uploaded
            if (f.status?.missing && f.offset >= f.file.size) {
                for (let k = 0; k < f.status.missing.length; k++) {
                    const missingOffset = f.status.missing[k].start;
                    if (f.failed.indexOf(missingOffset) == -1) {
                        f.failed.push(missingOffset);
                    }
                }
            }
            fs.push(f);
        }
    }
    model.files = fs;

    // Check for requests with finished uploads or errors.
    let rs = [];
    for (let i = 0; i < model.requests.length; i++) {
        let r = model.requests[i];
        if (r.error_count > 0) {
            // Pending session replies must be cleaned up, never added to model.files.
            r.cancelled = true;
            r.uploads.forEach(f => self.publish("bridge/origin/model/fileuploader/post/delete/" + f.upload, {}));
            // failed - publish message to failure topic
            console.error("fileuploader failed", r);
            if (r.failure_topic) {
                self.publish(r.failure_topic, {...r.failure_msg, error: r.error});
            }
        } else if (r.upload_count == 0 && r.wait_count == 0) {
            // finished - publish result to ready topic
            if (r.ready_topic) {
                let msg = r.ready_msg;
                if (msg._type == 'postback_event') {
                    let q = msg.data.q || [];
                    // push name/value pairs
                    for (let k = 0; k < r.uploads.length; k++) {
                        q.push({
                            name: r.uploads[k].name,
                            value: r.uploads[k].media_upload_token
                                ? {media_upload_token: r.uploads[k].media_upload_token}
                                : {_type: 'fileuploader', name: r.uploads[k].upload}
                        })
                    }
                } else {
                    msg.fileuploader = r.uploads;
                }
                self.publish(r.ready_topic, msg);

                if (r.progress_topic) {
                    let msg = r.progress_msg;
                    msg.percentage = 100;
                    self.publish(r.progress_topic, msg);
                }
            }
        } else {
            // Still busy uploading - publish progress and keep req
            if (r.progress_topic) {
                let now = Date.now();

                if (now - r.progress_published > PROGRESS_INTERVAL) {
                    let percentage = 0;
                    let msg = r.progress_msg;

                    if (r.total_size > 0) {
                        percentage = Math.floor( (r.uploaded_size / r.total_size) * 100 );
                    }
                    msg.percentage = percentage;
                    self.publish(r.progress_topic, msg);
                    r.progress_published = now;
                }
            }
            rs.push(r);
        }
    }
    model.requests = rs;

    const isBusy = model.requests.length > 0;
    if (isBusy !== model.is_busy) {
        model.is_busy = isBusy;
        self.publish('model/fileuploader/event/busy', isBusy, {retain: true});
    }

    // Start upload of next block
    if (state.uploading(model) && !state.full(model)) {
        for (let i=0; i<model.files.length; i++) {
            let f = model.files[i];
            if (f.failed.length > 0 || f.offset < f.file.size) {
                startUploader(i);
                break;
            }
        }
        setTimeout(actions.startUploader, 0);
    }

    state.render(model);
};


////////////////////////////////////////////////////////////////////////////////
// View
//
var view = {} ;

// Initial State
view.init = function(model) {
    return view.ready(model) ;
}

// State representation of the ready state
view.ready = function(model) {
    return "";
}


//display the state representation
view.display = function(representation) {
}

// Display initial state
view.display(view.init(model)) ;


////////////////////////////////////////////////////////////////////////////////
// State
//
var state =  { view: view };

model.state = state ;

// Derive the state representation as a function of the system control state
state.representation = function(model) {
};

// Derive the current state of the system
state.start = function(model) {
    return !model.is_subscribed;
};

state.uploading = function(model) {
    for (let i=0; i<model.files.length; i++) {
        if (!model.files[i].status.is_complete) {
            return true;
        }
    }
    return false;
};

state.full = function(model) {
    return model.uploaders >= MAX_UPLOADERS;
};


// Next action predicate, derives whether
// the system is in a (control) state where
// an action needs to be invoked

state.nextAction = function (model) {
}

state.render = function(model) {
    state.representation(model)
    state.nextAction(model) ;
}


////////////////////////////////////////////////////////////////////////////////
// Actions
//

var actions = {} ;

actions.init = function(data) {
    data = data || {};
    model.present(data);
};

actions.dependsProvided = function() {
    let data = {};
    data.is_depends_provided = true;
    model.present(data);
};

actions.startUploader = function() {
    let data = {};
    data.is_start_uploader = true;
    model.present(data);
};

actions.nextUpload = function () {
    let data = {};
    data.is_next = true;
    model.present(data);
};

actions.newUpload = function(msg) {
    let data = {
        upload: msg.payload,
        response_topic: msg.properties.response_topic
    };
    model.present(data);
};

actions.deleteUpload = function(name) {
    let data = {
        delete_upload: name
    };
    model.present(data);
};

////////////////////////////////////////////////////////////////////////////////
// Worker Startup
//

self.on_init = function(args) {
    actions.init(args);
}

self.connect({
    depends: [ "bridge/origin", ],
    provides: [ "fileuploader" ]
}).then(
    function() {
        actions.dependsProvided();
    }
);
