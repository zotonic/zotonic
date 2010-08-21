window.ZotonicImageClipper =
(function()
 {
     var self = this;

     self.buildPopup = function()
     {
         self.popup = $("<div>")
             .appendTo(document.body)
             .hide()
             .css({position: "fixed", width: "400px", top: 5, right: 5, "background-color": "white", border: "1px solid #666", "z-index": 2000})
             .fadeIn();
         self.popup.append($("<h2>")
                           .text(window._clipper.c)
                           .css({padding:20,"font": "16px Verdana",color:"#666"})
                          );


         self.imageContainer = $("<div>").css({"padding-left":20, "padding-right":20})
             .hide()
             .appendTo(self.popup);

         self.buttonContainer = $("<div>").css({padding:20, "padding-top": 15})
             .hide()
             .appendTo(self.popup);

         // 1111
         self.form = $("<form>")
             .attr("method", "post") 
             .attr("action", window._clipper.u)
             .append($("<button>").text(window._clipper.b))
             .appendTo(self.buttonContainer);
         $("<input type=\"hidden\">")
             .attr("name", "referer")
             .val(document.location.href)
             .appendTo(self.form);

         $("<span>")
             .text("x")
             .css({"font-family": "Verdana", cursor: "pointer", "font-weight": "bold", color: "red", position: "absolute", top: 0, right: 5})
             .attr("title", "Close image clipper")
             .click(function(){self.destroy();})
             .appendTo(self.popup);

     };

     // Show the popup
     self.run = function()
     {
         self.images = [];
         self.seen = [];
         if (self.popup)
         {
             self.destroy();
         }
         self.buildPopup();

         $(document).bind("keyup.clipper", function(e) { if (e.keyCode == 27 && self.popup) self.destroy(); });

         if (!self.selection)
         {
             self.selection = $("<div>")
                 .hide()
                 .click(function(){self.selection.hide();self.addImage(self.currentImage);return false;})
                 .appendTo(document.body)
                 .css({position: "absolute", border: "5px solid red", "z-index": 1000, cursor: "pointer"});
             $(document.body).css({position:"relative"});
         }


         $("img").bind("mouseover.clipper", function() {
                           var im = $(this);var o=im.offset();
                           var w=im.outerWidth(true);var h=im.outerHeight(true);
                           if ((w<100&&h<100) || $.inArray(im.attr("src"), self.seen)!=-1) return;
                           self.selection.css({top:o.top-5,left:o.left-5,width:w,height:h});
                           self.selection.show();
                           self.currentImage = im;
                       });
     };

     self.destroy = function()
     {
         self.popup.remove();
         self.popup = null;
         if (self.selection) {
             self.selection.remove();
             self.selection = null;
         }
         $("img").unbind("mouseover.clipper");
         $("img").unbind("click.clipper");
         $(document).unbind("keyup.clipper");
     };


     // Add an image to the clipper
     self.addImage = function (im)
     {
         self.buttonContainer.show();
         self.imageContainer.show();
         
         var src = null;
         // First check if image links to a big picture
         if (im.parents("a:first").length) {
             var u = im.parents("a:first").attr("href");
             if (u.match(/\.(jpe?g|png|bmp|gif)$/i)) {
                 src = u;
             }
         }
         if (!src) src = im.attr("src");
         if (!src.match(/^https?:\/\//))
         {
             var l = document.location.href;
             if (src.match(/^\//)) {
                 // absolute URL; prepend hostname
                 src = l.replace(/^(https?:\/\/.*?)\/.*$/, "$1") + src;
             } else {
                 // relative url
                 src = l.replace(/^(.*\/).*$/, "$1") + src;
             }
         }
         if ($.inArray(src, self.images)!=-1) {
             return;
         }

         self.images.push(src);
         self.seen.push(im.attr("src"));

         $("<input>").css({visibility:"hidden",position:"absolute",top:0,left:0}).attr("name", "url").val(src).appendTo(self.form);
         var h = 60;

         $("<img>")
             .attr("src", im.attr("src"))
             .css({padding: 1, border: "1px solid #e0e0e0", "margin-right": 10, "margin-bottom": 10, height: h, width: h * (im.width()/im.height())})
             .appendTo(self.imageContainer);

     };

     // Check jQuery and then start
     if (typeof window.jQuery == 'undefined')
     {
         // Add jQuery script
         var script =document.createElement("script");
         script.src="http://code.jquery.com/jquery-1.4.2.min.js";
         document.body.appendChild(script);
         var tryRun = function() {
             if (typeof window.jQuery == 'undefined') {
                 setTimeout(tryRun, 100);
             } else {
                 self.run();
             }
         };
         setTimeout(tryRun, 100);
     }
     else
     {
         // First run
         self.run();
     }

     return self;
 })();
