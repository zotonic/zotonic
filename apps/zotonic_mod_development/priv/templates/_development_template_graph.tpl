<div id="graphviz_svg" style="max-width: 100%; overflow: auto;"></div>

<div id="graphvis_data" style="display:none;">
    {{ dot|escape }}
</div>

{% javascript %}
    var svg = Viz($('#graphvis_data').text(), "svg");
    $('#graphviz_svg').html(svg);
{% endjavascript %}
