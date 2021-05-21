{% extends "_stats_panel.tpl" %}

{% block panel_title %}{_ Memory Usage _}{% endblock %}

{% block panel_body %}

<div class="progress">
    <div id="usage-process_memory-progress" class="progress-bar" style="background-color: gold"></div>
    <div id="usage-atom_memory-progress" class="progress-bar" style="background-color: dodgerblue"></div>
    <div id="usage-ets_memory-progress" class="progress-bar" style="background-color: lightgreen"></div>
    <div id="usage-binary_memory-progress" class="progress-bar" style="background-color: tomato"></div>
    <div id="usage-code_memory-progress" class="progress-bar" style="background-color: orchid"></div>
</div>
<div class="row">
    <div class="col-md-1"><span style="color: gold">&#9607;</span></div>
    <div class="col-md-2 meta">process</div>
    <div id="memory-processes" class="col-md-3 text-right">#</div>

    <div class="col-md-1"><span style="color: dodgerblue">&#9607;</span></div>
    <div class="col-md-2 meta">ets</div>
    <div id="memory-atom" class="col-md-3 text-right">#</div>
</div>
            
<div class="row">
    <div class="col-md-1"><span style="color: lightgreen">&#9607;</span></div>
    <div class="col-md-2 meta">atom</div>
    <div id="memory-ets" class="col-md-3 text-right">#</div>

    <div class="col-md-1"><span style="color: tomato">&#9607;</span></div>
    <div class="col-md-2 meta">binary</div>
    <div id="memory-binary" class="col-md-3 text-right">#</div>
</div>

<div class="row">
    <div class="col-md-1"><span style="color: orchid">&#9607;</span></div>
    <div class="col-md-2 meta">code</div>
    <div id="memory-code" class="col-md-3 text-right">#</div>

    <div class="col-md-1" style="color: lightgray">&#9607;</div>
    <div class="col-md-2 meta">other</div>
    <div id="memory-other" class="col-md-3 text-right">#</div>
</div>

<div class="row">
    <div class="col-md-12 text-right">
        <span class="meta">Total </span><span id="memory-total"></span>
    </div>

    {% javascript %}
    $("#memory-binary").data("render", to_human);
    $("#memory-atom").data("render", to_human);
    $("#memory-code").data("render", to_human);
    $("#memory-ets").data("render", to_human);
    $("#memory-processes").data("render", to_human);
    $("#memory-total").data("render", to_human);
    {% endjavascript %}
</div>

{% endblock %}



