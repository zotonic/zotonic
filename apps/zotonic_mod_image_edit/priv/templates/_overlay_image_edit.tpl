{% with m.image_edit.settings[id] as settings %}
<div class="image-edit-container">
    <div class="image-edit__original">

        {# Would be more logical to place these in the image wrapper, but Safari has a bug
         # where the crop divs are partially hidden if the image has transforms.
         # This works around that problem (strangely enough)
         #}
        <div class="image-edit-crop__wrapper">
            <div class="image-edit-crop image-edit-crop-left"></div>
            <div class="image-edit-crop image-edit-crop-right"></div>
            <div class="image-edit-crop image-edit-crop-top"></div>
            <div class="image-edit-crop image-edit-crop-bottom"></div>
            <div id="image-edit-crop-center" class="image-edit-crop-center" title="{_ Cropping center, click to remove _}"><span class="glyphicon glyphicon-record"></span></div>
        </div>

        <div class="image-edit__original__wrapper">
            {% image id original mediaclass="image-edit" %}
        </div>
    </div>

    <div class="image-edit__settings">
        {% wire id="image-edit-form"
                type="submit"
                postback={edit_form id=id on_success={overlay_close}}
                delegate=`mod_image_edit`
        %}
        <form id="image-edit-form" action="postback">

            <div class="form-group">
                <p>
                    <a href="#" id="image-edit-orientation-btn" class="btn btn-default">
                        <i class="fa fa-undo"></i> {_ Rotate _}
                    </a>
                </p>

                <input type="hidden" id="image-edit-orientation" name="rotate" value="{{ settings.rotate|default:'0' }}">

                <input type="hidden" id="image-crop-center-x" name="crop_center_x" value="{{ settings.crop_center_x|default:'-1' }}">
                <input type="hidden" id="image-crop-center-y" name="crop_center_y" value="{{ settings.crop_center_y|default:'-1' }}">

                {% comment %}
                    <input type="hidden" name="crop_center" id="crop_center" value="{{ id.crop_center }}">
                {% endcomment %}
            </div>

            <div class="form-group">
                <label>
                    <input type="checkbox" value="1" name="is_lossless" {% if settings.is_lossless %}checked{% endif %}>
                    {_ Lossless resize _}
                </label>
                <p class="help-block">{_ Keep logos and clip art sharp. _}</p>
            </div>

            <a class="collapse-toggle" role="button" data-toggle="collapse" href="#image-edit-crop" aria-expanded="false" aria-controls="collapseExample">{_ Crop image _}</a>
            <div class="collapse" id="image-edit-crop">
                <div class="form-group">
                    <label>{_ Crop top _}: <span></span>%</label>
                    <input class="form-control" id="image-edit-crop-top" type="range" name="crop_top" min="0" max="100" value="{{ settings.crop_top|default:'0' }}" step="0.1" list="image-edit-percentage">
                </div>
                <div class="form-group">
                    <label>{_ Crop left _}: <span></span>%</label>
                    <input class="form-control" id="image-edit-crop-left" type="range" name="crop_left" min="0" max="100" value="{{ settings.crop_left|default:'0' }}" step="0.1" list="image-edit-percentage">
                </div>
                <div class="form-group">
                    <label>{_ Crop right _}: <span></span>%</label>
                    <input class="form-control" id="image-edit-crop-right" type="range" name="crop_right" min="0" max="100" value="{{ settings.crop_right|default:'0' }}" step="0.1" list="image-edit-percentage">
                </div>
                <div class="form-group">
                    <label>{_ Crop bottom _}: <span></span>%</label>
                    <input class="form-control" id="image-edit-crop-bottom" type="range" name="crop_bottom" min="0" max="100" value="{{ settings.crop_bottom|default:'0' }}" step="0.1" list="image-edit-percentage">
                </div>
            </div>
            <p class="help-block">{_ Click the image to set the cropping center. _}</p>

            <a class="collapse-toggle" role="button" data-toggle="collapse" href="#image-edit-conbri" aria-expanded="false" aria-controls="collapseExample">{_ Contrast &amp; Brightness _}</a>
            <div class="collapse" id="image-edit-conbri">
                <div class="form-group">
                    <label>{_ Contrast _}: <span></span>%</label>
                    <input class="form-control" id="image-edit-contrast" name="contrast" type="range" min="-100" max="100" value="{{ settings.contrast|default:'0' }}" list="image-edit-range">
                </div>
                <div class="form-group">
                    <label>{_ Brightness _}: <span></span>%</label>
                    <input class="form-control" id="image-edit-brightness" name="brightness" type="range" min="-100" max="100" value="{{ settings.brightness|default:'0' }}" list="image-edit-range">
                </div>
            </div>

            <a class="collapse-toggle" role="button" data-toggle="collapse" href="#image-edit-tilpan" aria-expanded="false" aria-controls="collapseExample">{_ Roll, Tilt &amp; Pan _}</a>
            <div class="collapse" id="image-edit-tilpan">
                <div class="form-group">
                    <label>{_ Roll _}: <span></span>˚</label>
                    <input class="form-control" id="image-edit-roll" type="range" name="roll" min="-45" max="45" value="{{ settings.roll|default:'0' }}" list="image-edit-rotate-roll">
                </div>
                <div class="form-group">
                    <label>{_ Tilt _}: <span></span>˚</label>
                    <input class="form-control" id="image-edit-tilt" type="range" name="tilt" min="-180" max="180" value="{{ settings.tilt|default:'0' }}" list="image-edit-angles">
                </div>
                <div class="form-group">
                    <label>{_ Pan _}: <span></span>˚</label>
                    <input class="form-control" id="image-edit-pan" type="range" name="pan" min="-180" max="180" value="{{ settings.pan|default:'0' }}" list="image-edit-angles">
                </div>
            </div>

            <div class="form-actions">
                <a class="btn btn-default" id="image-edit-cancel">{_ Cancel _}</a>
                <a class="btn btn-default" id="image-edit-reset">{_ Reset _}</a>
                <button class="btn btn-primary">{_ Save _}</button>

                {% wire id="image-edit-cancel" action={overlay_close} %}
            </div>
        </form>

        <datalist id="image-edit-percentage" style="display:none">
            <option value="0" label="0%"></option>
            <option value="50" label="25%"></option>
            <option value="50" label="50%"></option>
            <option value="75" label="75%"></option>
            <option value="100" label="100%"></option>
        </datalist>

        <datalist id="image-edit-range" style="display:none">
            <option value="-100" label="-100"></option>
            <option value="-50" label="-50"></option>
            <option value="0" label="0"></option>
            <option value="50" label="50"></option>
            <option value="100" label="100"></option>
        </datalist>

        <datalist id="image-edit-angles" style="display:none">
            <option value="-180" label="-180˚"></option>
            <option value="-135"></option>
            <option value="-90" label="-90˚"></option>
            <option value="-45"></option>
            <option value="0" label="0"></option>
            <option value="45"></option>
            <option value="90" label="+90˚"></option>
            <option value="135"></option>
            <option value="180" label="+190˚"></option>
        </datalist>

        <datalist id="image-edit-rotate-roll" style="display:none">
            <option value="-45" label="-45˚"></option>
            <option value="-30" label="-30˚"></option>
            <option value="-15" label="-15˚"></option>
            <option value="0" label="0"></option>
            <option value="15" label="+15˚"></option>
            <option value="30" label="+30˚"></option>
            <option value="45" label="+45˚"></option>
        </datalist>
    </div>
</div>
{% endwith %}


{% javascript %}
    function image_edit_apply_css() {
        if ($(".image-edit__original__wrapper").length == 0) {
            return;
        }

        let orientation = $('#image-edit-orientation').val();

        let contrast = $('#image-edit-contrast').val();
        let brightness = $('#image-edit-brightness').val();
        let roll = $('#image-edit-roll').val();
        let pan = $('#image-edit-pan').val();
        let tilt = $('#image-edit-tilt').val();

        let cropLeft = parseFloat($("#image-edit-crop-left").val());
        let cropRight = parseFloat($("#image-edit-crop-right").val());
        let cropTop = parseFloat($("#image-edit-crop-top").val());
        let cropBottom = parseFloat($("#image-edit-crop-bottom").val());

        $('#image-edit-contrast').closest('.form-group').find('span').text(contrast);
        $('#image-edit-brightness').closest('.form-group').find('span').text(brightness);
        $('#image-edit-roll').closest('.form-group').find('span').text(roll);
        $('#image-edit-pan').closest('.form-group').find('span').text(pan);
        $('#image-edit-tilt').closest('.form-group').find('span').text(tilt);

        $('#image-edit-crop-left').closest('.form-group').find('span').text(cropLeft);
        $('#image-edit-crop-right').closest('.form-group').find('span').text(cropRight);
        $('#image-edit-crop-top').closest('.form-group').find('span').text(cropTop);
        $('#image-edit-crop-bottom').closest('.form-group').find('span').text(cropBottom);

        let is_rotated;
        let $wrapper = $(".image-edit__original__wrapper");
        let $crop_wrapper = $(".image-edit-crop__wrapper");
        let $img = $(".image-edit__original__wrapper img");

        switch (parseInt(orientation)) {
            case -90:
            case -270:
            case 90:
            case 270:
                is_rotated = true;
                break;
            default:
                is_rotated = false;
                break;
        }

        let w = $(".image-edit__original").width();
        let h = $(".image-edit-container").height();

        let iw = parseInt($img.attr('width'));
        let ih = parseInt($img.attr('height'));

        let img_h;
        let img_w;

        if (is_rotated) {
            let resize_w = w / ih;
            let resize_h = h / iw;

            let resize = Math.min(resize_w, resize_h);

            img_w = Math.round(iw * resize);
            img_h = Math.round(ih * resize);

            $wrapper.css({
                "width": img_w + "px",
                "height": img_h + "px",
                "margin-left": ((w - img_w) / 2) + "px",
                "margin-top": ((img_w - img_h) / 2) + "px"
            });
            $crop_wrapper.css({
                "width": img_w + "px",
                "height": img_h + "px",
                "margin-left": ((w - img_w) / 2) + "px",
                "margin-top": ((img_w - img_h) / 2) + "px"
            });
        } else {
            let resize_w = w / iw;
            let resize_h = h / ih;

            let resize = Math.min(resize_w, resize_h);

            img_w = Math.round(iw * resize);
            img_h = Math.round(ih * resize);

            $wrapper.css({
                "width": img_w + "px",
                "height": img_h + "px",
                "margin-left": ((w - img_w) / 2) + "px",
                "margin-top": "0px"
            });
            $crop_wrapper.css({
                "width": img_w + "px",
                "height": img_h + "px",
                "margin-left": ((w - img_w) / 2) + "px",
                "margin-top": "0px"
            });
        }

        let tfov = 0.5351837583;
        let diag = Math.sqrt( (img_w * img_w) + (img_h * img_h) );
        let focal = diag / (2 * tfov);

        $(".image-edit__original img").css({
            "transform":  "perspective(" + focal + "px)"
                        + " rotateZ(" + roll + "deg)"
                        + " rotateX(" + tilt + "deg)"
                        + " rotateY(" + pan + "deg)",
            "filter":   "contrast(" + (parseInt(contrast) + 100) + "%)"
                      + " brightness(" + (parseInt(brightness) + 100) + "%)"
        });

        $wrapper.css({
            "transform": "rotateZ(" + orientation + "deg)"
        });
        $crop_wrapper.css({
            "transform": "rotateZ(" + orientation + "deg)"
        });


        let cropLeft1 = cropLeft;
        let cropRight1 = cropRight;
        let cropBottom1 = cropBottom;
        let cropTop1 = cropTop;

        // Correct the left/right/bottom/top for the rotation
        switch (parseInt(orientation)) {
            case -90:
                cropRight1 = cropTop;
                cropBottom1 = cropRight;
                cropLeft1 = cropBottom;
                cropTop1 = cropLeft;
                break;
            case -180:
                cropLeft1 = cropRight;
                cropRight1 = cropLeft;
                cropBottom1 = cropTop;
                cropTop1 = cropBottom;
                break;
            case -270:
                cropTop1 = cropRight;
                cropRight1 = cropBottom;
                cropLeft1 = cropTop;
                cropBottom1 = cropLeft;
                break;
            default:
                break;
        }

        // Show the current crop using translucent absolute positioned divs.
        if (cropTop1) {
            $(".image-edit-crop-top").css({
                "display": "block",
                "bottom": (100 - cropTop1) + "%"
            });
        } else {
            $(".image-edit-crop-top").css({
                "display": "none"
            });
        }
        if (cropBottom1) {
            $(".image-edit-crop-bottom").css({
                "display": "block",
                "top": (100 - cropBottom1) + "%"
            });
        } else {
            $(".image-edit-crop-bottom").css({
                "display": "none"
            });
        }
        if (cropLeft1) {
            $(".image-edit-crop-left").css({
                "display": "block",
                "right": (100 - cropLeft1) + "%"
            });
        } else {
            $(".image-edit-crop-left").css({
                "display": "none"
            });
        }
        if (cropRight1) {
            $(".image-edit-crop-right").css({
                "display": "block",
                "left": (100 - cropRight1) + "%"
            });
        } else {
            $(".image-edit-crop-right").css({
                "display": "none"
            });
        }

        // Show the cropping point
        let $marker = $(".image-edit-crop-center");
        let cropX = parseFloat($('#image-crop-center-x').val());
        let cropY = parseFloat($('#image-crop-center-y').val());

        if (cropX && cropY) {
            let w = $crop_wrapper.width();
            let h = $crop_wrapper.height();

            let $marker_w = $marker.width();
            let $marker_h = $marker.height();

            $marker.css({
                "display": "block",
                "left": (w * cropX - $marker_w / 2) + "px",
                "top": (h * cropY - $marker_h / 2) + "px",
            });
            $('#image-edit-crop-center-remove').show();
        } else {
            $('#image-edit-crop-center-remove').hide();
            $marker.css({
                "display": "none"
            });
        }
    }

    image_edit_apply_css();

    $('#image-edit-form').on('input change', function() {
        image_edit_apply_css();
    });

    $('#image-edit-reset').on('click', function(ev) {
        ev.preventDefault();

        $('#image-edit-orientation').val(0);
        $('#image-edit-contrast').val(0);
        $('#image-edit-brightness').val(0);
        $('#image-edit-roll').val(0);
        $('#image-edit-pan').val(0);
        $('#image-edit-tilt').val(0);

        $("#image-edit-crop-left").val(0);
        $("#image-edit-crop-right").val(0);
        $("#image-edit-crop-top").val(0);
        $("#image-edit-crop-bottom").val(0);

        image_edit_apply_css();
    });

    $('#image-edit-orientation-btn').on('click', function(ev) {
        ev.preventDefault();

        let orientation = parseInt($('#image-edit-orientation').val());
        orientation = (orientation - 90) % 360;
        $('#image-edit-orientation').val(orientation);

        // Correct the left/right/bottom/top for the rotation
        let cropLeft = parseFloat($("#image-edit-crop-left").val());
        let cropRight = parseFloat($("#image-edit-crop-right").val());
        let cropTop = parseFloat($("#image-edit-crop-top").val());
        let cropBottom = parseFloat($("#image-edit-crop-bottom").val());

        $("#image-edit-crop-left").val(cropTop);
        $("#image-edit-crop-bottom").val(cropLeft);
        $("#image-edit-crop-right").val(cropBottom);
        $("#image-edit-crop-top").val(cropRight);

        image_edit_apply_css();
    });

    $('#image-edit-crop-center').on('click', function(ev) {
        ev.stopImmediatePropagation();
        $('#image-crop-center-x').val(-1);
        $('#image-crop-center-y').val(-1);
        image_edit_apply_css();
    });

    $('.image-edit-crop__wrapper').on('click', function(ev) {
        ev.preventDefault();
        let $crop_wrapper = $(".image-edit-crop__wrapper");
        let w = $crop_wrapper.width();
        let h = $crop_wrapper.height();

        let cropX = ev.offsetX / w;
        let cropY = ev.offsetY / h;

        $('#image-crop-center-x').val(cropX);
        $('#image-crop-center-y').val(cropY);

        image_edit_apply_css();
    });

    if (typeof window.image_edit_resize !== "function") {
        window.image_edit_apply_css = image_edit_apply_css;
        window.image_edit_onresize = function() {
            if ($(".image-edit__original__wrapper").length == 0) {
                window.removeEventListener("resize", window.image_edit_onresize);
                window.image_edit_onresize = undefined;
                window.image_edit_apply_css = undefined;
            } else if (typeof window.image_edit_apply_css == "function") {
                window.image_edit_apply_css();
            }
        }
        window.addEventListener("resize", window.image_edit_onresize);
    }



{% endjavascript %}
