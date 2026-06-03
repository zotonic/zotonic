File storage
============


Current situation
-----------------

If a file is uploaded we:

    1. Make a path for the file, using the date and the filename.
    2. Call routines for possible pre-processing or file replacement, think
       of mp4 conversion, re-compression etc.
    3. Move the temporary/medium file to the archive directory
       (e.g. `archive/2023/10/01/filename.ext`).
    4. Insert a medium record in the database.
    5. Optionally mod_filestore queues the new entry for later upload.

If uploaded to the filestore, the archive path is registered with the remote storage URL.


Problem
-------

If a medium record is duplicated then the file must be duplicated as well, as the medium
record is where the file reference is administrated.

This copying happens a lot in environments like LearningStone, where courses with their
included video files are copied.


Solution
--------

Store the file based on its content hash. The medium records reference the file by content
hash. There will not be a direct relation between the stored file and its filename.


Tables for Content-Addressable-File-Storage (CAFS)
--------------------------------------------------

The CAFS table is the content-addressable storage table that administrates the actual files

 - hash
 - size
 - mime
 - exif
 - extracted-text
 - date created
 - date modified
 - flag-for-garbage-collect-check

File <-> CAFS table:

 - filename
 - optional preview operations ('' for original)
 - hash
 - date created

In the medium record:

 - Refer to filename


The 'medium' record filename refers to the 'filename_cafs' table.
The 'filename_cafs' table refers to the 'cafs' table.


Processing order
----------------

Before upload:

 - check if hash is already known and if a referring medium record is viewable
 - if already known and viewable, perform "instant" upload

After upload:

 - identify uploaded file
 - perform ACL checks
 - call modules:
      - maybe replace/pre-process file
      - extract mime / EXIF and text
 - perform ACL checks
 - calculate content hash (sha256)
 - if hash is known:
    - maybe update the cafs record with the new mime / EXIF / text
 - if hash is not known:
    - move file to the cafs system
      1) move file; and
      2) insert row into cafs table
 - generate unique filename
 - call modules for possible preview calculation
   - this optionally adds a new (temporary) file
 - store filename into filename_cafs table
 - store preview filename into filename_cafs table
 - store medium record

After delete of medium record:

  - find in filename_cafs table the hashes for the filename
  - mark all matching cafs entries for possible GC (on delete trigger in database)


Previews
--------

Previews are identified by the filepath which includes the operations to be performed on
the file.

 1. filename + serialized-operations = key in filename_cafs table -> take hash
 2. lookup hash in cafs table


Video render queue
------------------

 - Let the system upload the original video file
 - Insert a render task in the task queue
   - If task runs:
      - Start process
      - Return "retry" with new task args
   - If process is ready:
      - Delete task
   - If task retry is running:
      - Check if process is running
      - If is running:
         - delay
      - If not running:
         - restart process (increment retry count)

Render process:

   - Ensure file to tmp file
   - ffmpeg re-render on tmp file
   - ffmpeg preview on tmp file
   - Update medium with new file and preview_file
   - Delete task from task queue


Hashing in JS
-------------

Built-in hash functions:

   https://developer.mozilla.org/en-US/docs/Web/API/SubtleCrypto/digest

To check: can we incrementally calculate the hash of a file?

   https://github.com/browserify/sha.js


Example with built-in functions:

   const r = new FileReader();
   r.readAsArrayBuffer(file);
   r.onloadend = async function(entry) {
       const h = await crypto.subtle.digest('SHA-256', entry.target.result);
       // output: the sha256 digest hex encoded of the file
       const hashHex = hashArray
          .map((b) => b.toString(16).padStart(2, "0"))
          .join(""); // convert bytes to hex string
       // ...
   }

