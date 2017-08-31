(function () {

    var url = 'http://' + (location.host || 'localhost').split(':')[0] + ':35729/livereload.js?snipver=1';
    var script = document.createElement('script');

    script.src = url;

    document.head.appendChild(script);

}());
