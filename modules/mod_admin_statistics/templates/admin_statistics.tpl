{% extends "admin_base.tpl" %}

{% block title %}{_ Statistics _}{% endblock %}

{% block content %}

<div class="row">

<div class="col-md-4 col-lg-4 col-sm-4 col-xs-">
  <div class="panel panel-default">
    <div class="panel-heading">Processes</div>
    <div class="panel-body">
      <dl class="dl-horizontal">
        <dt>Process Count</dt><dd id="erlang-stats-system_info-process_count"></dd>
        <dt>Run Queue</dt> <dd id="erlang-stats-statistics-run_queue"></dd>
      </dl>
    </div>
  </div>
</div>

<div class="col-md-4 col-lg-4 col-sm-4">
  <div class="panel panel-default">
    <div class="panel-heading">I/O</div>
    <div class="panel-body">
      <dl class="dl-horizontal">
        <dt>Port Count</dt> <dd id="erlang-stats-system_info-port_count"></dd>
        <dt>Input</dt> <dd id="erlang-stats-io-input"></dd>
        <dt>Output</dt> <dd id="erlang-stats-io-output"></dd>
      </dl>
    </div>
  </div>
</div>

<div class="col-md-4 col-lg-4 col-sm-4">
  <div class="panel panel-default">
    <div class="panel-heading">Memory</div>
    <div class="panel-body">
      <dl class="dl-horizontal">
        <dt>Total<dt><dd id="erlang-stats-memory-total"></dd>
        <dt>Binary<dt><dd id="erlang-stats-memory-binary"></dd>
        <dt>Ets<dt><dd id="erlang-stats-memory-ets"><dd>
        <dt>Code<dt><dd id="erlang-stats-memory-code"></dd>
        <dt>System<dt><dd id="erlang-stats-memory-system"></dd>
      </dl>
    </div>
  </div>
</div>

</div>

{% javascript %}
    pubzub.subscribe("erlang/stats/#", function(topic, msg) {
        $("#" + topic.replace(/\//g, "-")).text(msg.payload);
        
        console.log(topic);
    });
{% endjavascript %}

{% endblock %}
