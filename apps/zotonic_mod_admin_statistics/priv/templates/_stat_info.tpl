<div class="col-md-4 col-lg-4 col-xs-2">
    <p>
    <span id="{{ id }}">#</span>
    <span>{{ name }}</span>
    </p>
</div>
{% javascript %}
$("#{{ id }}").data("render", {% if render %}{{ render }}{% else %}function(v) { return v; }{% endif %})
{% endjavascript %}
