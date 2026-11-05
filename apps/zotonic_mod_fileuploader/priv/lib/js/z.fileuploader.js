/* Fileuploader, using the fileuploader MQTT API. */

async function fileUploader(file) {
    let worker;
    let digest;
    try {
        digest = await new Promise((resolve, reject) => {
            worker = new Worker("/lib/js/z.media-upload-hash.js");
            worker.onmessage = event => event.data.digest ? resolve(event.data.digest) : reject();
            worker.onerror = reject;
            worker.postMessage({type: "media-upload-hash", file});
        });
    } catch (_) {
        // Hashing is an optimization; older browsers still upload normally.
    } finally {
        if (worker) worker.terminate();
    }
    const call = async (path, payload) => {
        const response = await cotonic.broker.call("bridge/origin/model/fileuploader/post/" + path, payload);
        if (response.payload.status !== "ok") throw new Error("upload-failed");
        return response.payload.result;
    };
    let status = await call("new", {filename: file.name, size: file.size, mime: file.type, digest});
    if (status.is_complete) return status;
    const blockSize = 128*1024;
    for (let offset=0; offset<file.size; offset+=blockSize) {
        status = await call("upload/" + status.name + "/" + offset,
            file.slice(offset, Math.min(file.size, offset+blockSize), "application/octet-stream"));
    }
    return status;
}

function fileInputUploader(element) {
    return Promise.all(Array.from(element.files, fileUploader));
}
