$(function() {
    $.extend($.fn,
    {
        doCropCenterEditor: function(hiddenInput, removeButton, message) {

            var el = this;

            var img = el.find("img");

            var scale = parseInt(el.attr("data-original-width")) / img.width();
            
            var guide_h = $("<div>").attr("class", "cropguide horiz").hide();
            var guide_v = $("<div>").attr("class", "cropguide vert").hide();

            el.append(guide_h).append(guide_v);

            function removeCrop() {
                guide_h.fadeOut();
                guide_v.fadeOut();
                hiddenInput.val("");
                removeButton.hide();
                message.show();
                return false;
            }
            
            function setCrop(x, y) {
                // here x,y are in image coordinates
                guide_h.show().css("top", y/scale);
                guide_v.show().css("left", x/scale);

                hiddenInput.val((x>=0?"+":"")+x + (y>=0?"+":"")+y);
                removeButton.show();
                message.hide();
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
                    setCrop(parseInt(l[1]), parseInt(l[2]));
                }
            }

            // hook up the events
            
            var dragging = false;
            el.mousedown(function(e) {
                dragging = true;
                setCropFromEvent(e);
            });
            el.mousemove(function(e) {
                if (dragging) setCropFromEvent(e);
            });
            el.mouseup(function() {
                dragging = false;
            });

            removeButton.click(removeCrop);
            setCropFromValue(hiddenInput.val());
        }
    });

});
