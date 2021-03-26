/* Drag/drop files in the admin */

(function(){
    let dropZone = document.getElementsByTagName('body')[0];

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
            try { $('body').mask("", 0); } catch (e) {}

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
                progress_msg: { form_id: 'body' }
            }
            cotonic.broker.publish("model/fileuploader/post/new", msg);
        }
    }

    dropZone.addEventListener('dragover', function(e) {
        if (containsFiles(e)) {
            e.stopPropagation();
            e.preventDefault();
            e.dataTransfer.dropEffect = 'copy';
            dropZone.classList.add("dragover");
        }
    });

    dropZone.addEventListener('dragleave', function(e) {
        if (containsFiles(e)) {
            e.stopPropagation();
            e.preventDefault();
            if (e.target.nodeName == 'BODY') {
                dropZone.classList.remove("dragover");
            }
        }
    });

    dropZone.addEventListener('drop', function(e) {
        if (containsFiles(e)) {
            e.stopPropagation();
            e.preventDefault();
            dropZone.classList.remove("dragover");
            uploadFiles(e);
        }
    });

})();

