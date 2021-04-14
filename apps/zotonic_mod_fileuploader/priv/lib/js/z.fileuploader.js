/* Fileuploader, using the fileuploader MQTT API */

function fileUploader( f ) {
    const blockSize = 128*1024;
    const fileSize = f.size;
    let blocks = [];

    for (offs = 0; offs < fileSize; offs += blockSize ) {
        let end = Math.max(fileSize, offs + blockSize);
        blocks.push({
            start: offs,
            bytes: end - offs + 1
        })
    }

    reader = {
        file: f,
        filename: f.name,
        size: f.size,
        mime: f.type,
        reader: new FileReader(),
        received: 0,
        blocks: blocks,
        uploader: undefined
    }

    function upload() {
        if (reader.blocks.length == 0) {
            reader.resolve();
        } else {
            const b = reader.blocks.pop();
            const blob = reader.slice(b.start, b.bytes);

            cotonic.broker.mqtt
                .publish("bridge/origin/model/fileuploader/post/upload/"+b.name+"/"+b.start, blob)
                .then(
                    function(msg) {
                        if (msg.status == "ok") {
                            upload();
                        } else {
                            console.log("Error uploading on data post result", reader, msg);
                            reader.reject();
                        }
                    })
                .catch(
                    function(error) {
                        console.log("Error uploading on data post", reader, error);
                        reader.reject(new Error(msg));
                    }
                )
        }
    }

    return new Promise(
        function(resolve, reject) {
            reader.resolve = resolve;
            reader.reject = reject;

            cotonic.broker
                .call("$promised/bridge/origin/model/fileuploader/post/new", {
                    filename: reader.filename,
                    size: reader.size,
                    mime: reader.mime
                })
                .then(
                    function(msg) {
                        if (msg.payload.status == "ok") {
                            reader.name = msg.payload.result.name;
                            upload(resolve, reject)
                        } else {
                            reject(new Error(msg));
                        }
                    }
                )
                .catch(
                    function(error) {
                        console.log("Error uploading on init ", reader, error);
                        reject(new Error('upload-new'));
                    }
                )
        });
}

function fileInputUploader ( element ) {
    const files = element.files;
    let readers = [];

    for (let i = 0; i < files.length; i++) {
        readers[i] = fileUploader(files[i]);
    }
    return Promise.all(readers);
}
