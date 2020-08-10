<div class="col-md-4 col-lg-3 col-sm-6 pull-left">
    <h3 id="{{ id }}" class="text-center">#</h3>
    <div class="text-center"><small>{{ name }}</small></div>
</div>
{% javascript %}
$("#{{ id }}").data("render", {% if render %}{{ render }}{% else %}function(v) { return v; }{% endif %})
{% endjavascript %}
