<button id="graphviz_copy" class="btn btn-default pull-right">{_ Copy GraphViz dot data _}</button>
<p class="help-block">
    {% trans "Dependency graph of all templates. Only includes using a direct string value for the template are shown.<br>
    Includes using variable names or expressions are not shown.<br> <tt>{extends}</tt> are shown with a striped line, <tt>{overrules}</tt> are shown with a dotted line."
        extends="{% extends \"...\" %}"
        overrules="{% overrules %}"
    %}
</p>

<div id="graphviz_svg" style="max-width: 100%; overflow: auto;"></div>

<div style="display: none">
    <textarea class="form-control" id="graphviz_data" disabled>{{ dot|escape }}</textarea>
</div>

{% javascript %}
    var svg = Viz($('#graphviz_data').val(), "svg");
    $('#graphviz_svg').html(svg);
    $('#graphviz_svg title').remove();

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
