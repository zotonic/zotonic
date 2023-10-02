<p class="text-right">
    <button id="graphviz_copy" class="btn btn-default">{_ Copy GraphViz dot data _}</button>
</p>

<div id="graphviz_svg" style="max-width: 100%; overflow: auto;"></div>

<div style="display: hidden">
    <textarea class="form-control" id="graphviz_data" disabled>{{ dot|escape }}</textarea>
</div>

{% javascript %}
    var svg = Viz($('#graphviz_data').val(), "svg");
    $('#graphviz_svg').html(svg);

    $('#graphviz_copy').on('click', function() {
        const text = $('#graphviz_data').val();
        navigator.clipboard.writeText(text).then(
            function() {
                z_growl_add("{_ Copied dot file to the clipboard. _}");
            },
            function(err) {
                z_growl_add("{_ Could not copy dot file to the clipboard. _}", false, 'error');
            });
    });
{% endjavascript %}
