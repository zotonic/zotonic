{% extends "admin_base.tpl" %}

{% block title %}{_ Statistics _}{% endblock %}

{% block content %}

<div class="row">

<div class="col-md-4 col-lg-4 col-sm-4 col-xs-">
  <div class="panel panel-default">
    <div class="panel-heading">System</div>
    <div class="panel-body">
        {% for id, name in [
                ["erlang-stats-system_info-process_count", "Process Count"],
                ["erlang-stats-statistics-run_queue", "Run Queue"],
                ["erlang-stats-system_info-port_count", "Port Count"]] %}
            {% include "_stat_info.tpl" id=id name=name %}
        {% endfor %}
    </div>
  </div>
</div>

<div class="col-md-4 col-lg-4 col-sm-4">
  <div class="panel panel-default">
    <div class="panel-heading">I/O</div>
    <div class="panel-body">
        {% for id, name in [
                ["erlang-stats-io-input", "Input"],
                ["erlang-stats-io-output", "Output"]] %}
            {% include "_stat_info.tpl" id=id name=name render="to_human" %}
        {% endfor %}

        {% for id, name in [
                ["erlang-stats-network-tcp_port_count", "Open TCP Connections"]] %}
            {% include "_stat_info.tpl" id=id name=name %}
        {% endfor %}
    </div>
  </div>
</div>

<div class="col-md-4 col-lg-4 col-sm-4">
  <div class="panel panel-default">
    <div class="panel-heading">Memory</div>
    <div class="panel-body">
      {% for id, name in [
                ["erlang-stats-memory-total", "Total"],
                ["erlang-stats-memory-binary", "Binary"],
                ["erlang-stats-memory-ets", "Ets"],
                ["erlang-stats-memory-code", "Code"],
                ["erlang-stats-memory-system", "System"]]  %}
            {% include "_stat_info.tpl" id=id name=name render="to_human" %}
        {% endfor %}
    </div>
  </div>
</div>

</div>

{% javascript %}
function to_human(value) {
    if(value < 512) // .5 kb
        return value + "<small>B</small>";

    if(value < 524288) // .5 Mb
        return (value/1024).toFixed(1) + "<small>Kb</small>";

    if(value < 536870912) // .5 Gb
        return (value/1024/1024).toFixed(1) + "<small>Mb</small>";

    if(value < 549755813888) // *.5 Tb
        return (value/1024/1024/1024).toFixed(1) + "<small>Gb</small>";

    return (value/1024/1024/1024/1024).toFixed(1) + "<small>Tb</small>";
}
{% endjavascript %}

{% javascript %}
    cotonic.broker.subscribe("bridge/origin/$SYS/cowmachine/#",function(msg, args) {
    })

    cotonic.broker.subscribe("bridge/origin/$SYS/db/#",function(msg, args) {
    })

    cotonic.broker.subscribe("bridge/origin/$SYS/erlang/#", function(msg, args) {
        var item = $("#" + msg.topic.replace(/\//g, "-"));
        if (item.length > 0) {
            item.html(item.data('render')(msg.payload));
        }
    });
{% endjavascript %}

{% endblock %}
