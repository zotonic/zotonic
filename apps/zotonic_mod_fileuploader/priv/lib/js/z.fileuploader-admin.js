/* Drag/drop files in the admin */

(function(){
    let dropZone = document.getElementsByTagName('body')[0];

    function isUploadActive() {
        return !document.getElementById('zmodal')?.checkVisibility()
               && document.getElementsByTagName('body')[0].getAttribute('data-fileuploader') !== null;
    }

    function containsFiles(event) {
        if (event.dataTransfer.types) {
            for (let i = 0; i < event.dataTransfer.types.length; i++) {
                if (event.dataTransfer.types[i] == "Files") {
                    return true;
                }
            }
        }
        return false;
    }

    function uploadFiles(event) {
        let uploads = [];
        let files = event.dataTransfer.files;
        for (let i = 0; i < files.length; i++) {
            if (files[i].type && files[i].size > 0) {
                uploads.push({
                    name: "f",
                    file: files[i]
                });
            }
        }

        if (uploads.length > 0) {
            z_mask('body');

            let dataFileuploader = dropZone.getAttribute("data-fileuploader");

            if (dataFileuploader) {
                dataFileuploader = JSON.parse(dataFileuploader);
            }

            let ready_msg = {
                cmd: "drop",
                target: "body",
                data: dataFileuploader
            };
            let msg = {
                files: uploads,
                ready_msg: ready_msg,
                ready_topic: "bridge/origin/zotonic-transport/mod_fileuploader",
                progress_topic: "zotonic-transport/progress",
                progress_msg: { form_id: 'body' },
                failure_topic: "model/alert/post",
                failure_msg: { text: "Error during upload" }
            }
            cotonic.broker.publish("model/fileuploader/post/new", msg);
        }
    }

    dropZone.addEventListener('dragover', function(e) {
        if (isUploadActive() && containsFiles(e)) {
            e.stopPropagation();
            e.preventDefault();
            e.dataTransfer.dropEffect = 'copy';
            dropZone.classList.add("dragover");
            let dataFileuploader = dropZone.getAttribute("data-fileuploader");
            if (dataFileuploader) {
                dataFileuploader = JSON.parse(dataFileuploader);
                if (dataFileuploader.is_medium) {
                    dropZone.classList.add("dragover-medium");
                }
            }
        }
    });

    dropZone.addEventListener('dragleave', function(e) {
        if (isUploadActive() && containsFiles(e)) {
            e.stopPropagation();
            e.preventDefault();
            if (e.target.nodeName == 'BODY') {
                dropZone.classList.remove("dragover", "dragover-medium");
            }
        }
    });

    dropZone.addEventListener('drop', function(e) {
        if (isUploadActive() && containsFiles(e)) {
            e.stopPropagation();
            e.preventDefault();
            dropZone.classList.remove("dragover", "dragover-medium");
            uploadFiles(e);
        }
    });

})();

