$(function() {
    $.extend($.fn,
    {
        cropcenter: function(settings) {
            var config = {
                editable: true,
                hiddenInput: "#crop_center",
                removeButton: "#crop-center-remove",
                message: "#crop-center-message"
            };

            if (settings) {
                $.extend(config, settings);
            }

            var el = this;
            el.find("img").one('load', function() {

                // do stuff
                var $img = $(this);
                var scale = parseInt(el.attr("data-original-width"), 10) / $img.width();
                var offset_h = $img.position().left + parseInt($img.css("margin-left"), 10);
                var offset_v = $img.position().top + parseInt($img.css("margin-top"), 10);
                
                var $guide_h = $("<div>").attr("class", "admin-cropguide admin-cropguide-horiz").hide();
                var $guide_v = $("<div>").attr("class", "admin-cropguide admin-cropguide-vert").hide();

                el.append($guide_h).append($guide_v);

                function removeCrop() {
                    $guide_h.fadeOut();
                    $guide_v.fadeOut();
                    $(config.hiddenInput).val("");
                    $(config.removeButton).hide();
                    $(config.message).show();
                    return false;
                }
                
                function setCrop(x, y) {
                    // here x,y are in image coordinates
                    $guide_h.show().css("top", y/scale + offset_v);
                    $guide_v.show().css("left", x/scale + offset_h);
                    $(config.hiddenInput).val((x>=0?"+":"")+x + (y>=0?"+":"")+y);
                    $(config.removeButton).show();
                    $(config.message).hide();
                }
                
                function setCropFromEvent(e) {
                    var o = el.offset();
                    var x = e.pageX - o.left;
                    var y = e.pageY - o.top;
                    setCrop(Math.ceil(x*scale), Math.ceil(y*scale));
                }

                function setCropFromValue(val) {
                    var l = val.match(/([+-]\d+)([+-]\d+)/);
                    if (!l) {
                        removeCrop();
                    } else {
                        setCrop(parseInt(l[1], 10), parseInt(l[2], 10));
                    }
                }

                // hook up the events
                if (config.editable) {
                    var dragging = false;
                    el.mousedown(function(e) {
                        if (e.which == 1) {
                            dragging = true;
                            setCropFromEvent(e);
                        }
                    });
                    el.mousemove(function(e) {
                        if (dragging) setCropFromEvent(e);
                    });
                    el.mouseup(function() {
                        dragging = false;
                    });
                    $(config.removeButton).click(removeCrop);
                }
                setCropFromValue($(config.hiddenInput).val());
            }).each(function() {
                if (this.complete) {
                    $(this).load();
                }
            });   
        }
    });
});
