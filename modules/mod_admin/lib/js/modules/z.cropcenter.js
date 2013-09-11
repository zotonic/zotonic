$(function() {
    $.extend($.fn,
    {
        cropcenter: function(settings) {
            var config = {
                hiddenInput: "#crop_center",
                removeButton: "#crop-center-remove",
                message: "#crop-center-message"
            };

            if (settings) {
                $.extend(config, settings);
            }

            var el = this;

            var img = el.find("img");

            var scale = parseInt(el.attr("data-original-width"), 10) / img.width();
            
            var guide_h = $("<div>").attr("class", "cropguide horiz").hide();
            var guide_v = $("<div>").attr("class", "cropguide vert").hide();

            el.append(guide_h).append(guide_v);

            function removeCrop() {
                guide_h.fadeOut();
                guide_v.fadeOut();
                $(config.hiddenInput).val("");
                $(config.removeButton).hide();
                $(config.message).show();
                return false;
            }
            
            function setCrop(x, y) {
                // here x,y are in image coordinates
                guide_h.show().css("top", y/scale);
                guide_v.show().css("left", x/scale);

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
            setCropFromValue($(config.hiddenInput).val());
        }
    });

});
