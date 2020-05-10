{% extends "_stats_panel.tpl" %}

{% block panel_title %}{_ System Usage _}{% endblock %}

{% block panel_body %}
<div>
    <div class="row">
        <div class="col-md-6"><span class="meta">Atoms: </span><strong id="usage-atom-value">#</strong><span class="meta">%</span></div>
        <div class="col-md-6 text-right">
            <span id="system-atom_count">#</span>/<span id="system-atom_limit">#</span> 
        </div>
    </div>
    {% javascript %}
    $("#system-atom_count").data("render", function(v) { return v; });
    $("#system-atom_limit").data("render", function(v) { return v; });
    {% endjavascript %}
    <div class="progress">
        <div id="usage-atom-progress" class="progress-bar progress-bar-success" role="progressbar"
                                                                                style="width: 0%">
        </div>
    </div>
</div>

<div>
    <div class="row">
        <div class="col-md-6">
            <span class="meta">Ports: </span><strong id="usage-port-value" id="usage-port-value">#</strong><span class="meta">%</span>
        </div>
        <div class="col-md-6 text-right">
            <span id="system-port_count">#</span>/<span id="system-port_limit">#</span> 
        </div>
    </div>
    {% javascript %}
    $("#system-port_count").data("render", function(v) { return v; });
    $("#system-port_limit").data("render", function(v) { return v; });
    {% endjavascript %}
    <div class="progress">
        <div id="usage-port-progress" class="progress-bar progress-bar-success" style="width: 0%"> </div>
    </div>
</div>

<div>
    <div class="row">
        <div class="col-md-6"><span class="meta">Processes: </span><strong id="usage-process-value">#</strong><span class="meta">%</span></div>
        <div class="col-md-6 text-right">
            <span id="system-process_count">#</span>/<span id="system-process_limit">#</span> 
        </div>
    </div>
    {% javascript %}
    $("#system-process_count").data("render", function(v) { return v; });
    $("#system-process_limit").data("render", function(v) { return v; });
    {% endjavascript %}
    <div class="progress">
        <div id="usage-process-progress" class="progress-bar progress-bar-success" role="progressbar" style="width: 0%"></div>
    </div>
</div>

{% endblock %}

