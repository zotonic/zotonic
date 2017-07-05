/* z.filepreview.js
----------------------------------------------------------

@author Marc Worrell <marc@worrell.nl>
@author Robert Nyman

https://hacks.mozilla.org/2012/04/taking-pictures-with-the-camera-api-part-of-webapi/

Show a preview of the a selected file.

Copyright 2012 Marc Worrell
Copyright 2012 Robert Nyman

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.

---------------------------------------------------------- */

$.widget("ui.filepreview", {
    _init: function() {
        var takePicture = $(this.element),
            showPicture = $('#'+this.options.preview),
            self = this;
     
        if (takePicture.length && showPicture.length) {
            takePicture.on('change', function (event) {
                var files = event.target.files,
                    file;

                if (files && files.length > 0) {
                    file = files[0];
                    try {
                        // Get window.URL object
                        var URL = window.URL || window.webkitURL;
                        var imgURL = URL.createObjectURL(file);
                        showPicture
                            .attr('src', imgURL)
                            .addClass('has-preview');
                        URL.revokeObjectURL(imgURL);
                    }
                    catch (e) {
                        try {
                            // Fallback if createObjectURL is not supported
                            var fileReader = new FileReader();
                            fileReader.onload = function (event) {
                                showPicture
                                    .attr('src', event.target.result)
                                    .addClass('has-preview');
                            };
                            fileReader.readAsDataURL(file);
                        }
                        catch (e2) {
                            showPicture
                                .attr('src', this.options.placeholder)
                                .removeClass('has-preview');
                        }
                    }
                }
                else {
                    showPicture
                        .attr('src', this.options.placeholder)
                        .removeClass('has-preview');
                }
            });

            showPicture.on('click', function() { takePicture.click(); });
        }
    }
});

$.ui.filepreview.defaults = {
    preview: '',
    placeholder: '/lib/images/noun/photo-camera.png'
};

