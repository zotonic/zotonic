<div class="col-md-3 col-lg-2 col-sm-4 pull-left">
    <h3 id="{{ id }}" class="text-right">#</h3>
    <div class="text-center"><small>{{ name }}</small></div>
</div>
{% javascript %}
$("#{{ id }}").data("render", {% if render %}{{ render }}{% else %}function(v) { return v; }{% endif %})
{% endjavascript %}
