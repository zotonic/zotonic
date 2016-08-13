{% extends "base_simple.tpl" %}
{% block title %}Connection Test{% endblock %}

{% block content %}

{% lib
    "js/modules/http_ping.js"
%}

<div class="container">

<nav class="navbar navbar-default">
  <div class="container">
    <div class="navbar-header">
      <button type="button" class="navbar-toggle collapsed" data-toggle="collapse" data-target="#navbar" aria-expanded="false" aria-controls="navbar">
        <span class="sr-only">Toggle navigation</span>
        <span class="icon-bar"></span>
        <span class="icon-bar"></span>
        <span class="icon-bar"></span>
      </button>
      <a class="navbar-brand" href="/">{{ m.site.title }}</a>
    </div>
    <div id="navbar" class="collapse navbar-collapse">
    </div>
  </div>
</nav>

<div class="row">
  <div class="well well-sm col-md-12 col-lg-12 col-sm-12">
    {% if q.no_websocket %}<strong>WebSockets are currently disabled</strong>
      {% button class="btn btn-default" text="Enable" action={redirect dispatch="connection_test"} %}
    {% else %}<strong>WebSockets are currently enabled</strong>
     {% button class="btn btn-default" text="Disable" action={redirect dispatch="connection_test" no_websocket=1 } %}
    {% endif %}
    {% button class="btn btn-default" text="Abort Connection" action={script script="abort_connection();"} %}
  </div>
</div>

{% if q.no_websocket %}
{% javascript %}
    delete window.WebSocket;
{% endjavascript %}
{% endif %}

{% javascript %}
function abort_connection() {
    if(z_ws) z_ws.close();
    if(z_comet) z_comet.abort();
}
{% endjavascript %}

<div class="row">
  <div class="col-md-12 col-lg-12 col-sm-12">
    <div class="panel panel-info">
      <div class="panel-heading">
          <h3 class="panel-title">Browser Connectivity</h3>
      </div>
      <div class="panel-body">
          <dl class="dl-horizontal">
             <dt>Ping Google</dt>
             <dd><span id="google-ping"></span><dd>
             <dt>Ping {{ m.site.title }}</dt>
             <dd><span id="ping"></span><dd>
         </dl>
      </div>
    </div>
  </div>
</div>

<div class="row">
  <div class="col-md-6 col-lg-6 col-sm-6">
    <div class="panel panel-info">
      <div class="panel-heading">
          <h3 class="panel-title">Zotonic Session Status</h3>
      </div>
      <div class="panel-body">
         <dl id="session-info" class="dl-horizontal">
            {% wire name="update_session_info" postback={session_info} target="session-info" %}
            {% wire action={script script="z_event('update_session_info');"} %}
         </dl>
      </div>
    </div>
  </div>
  <div class="col-md-6 col-lg-6 col-sm-6">
    <div class="panel panel-info">
      <div class="panel-heading">
          <h3 class="panel-title">Page Connection Status</h3>
      </div>
      <div class="panel-body">

      <dl class="dl-horizontal">
          <dt>Status</dt>
          <dd><span id="connect-status"></span></dd>

          <dt>PageId</dt>
          <dd><span id="z_pageid"></span></dd>

          <dt class="text-right">Session</dt>
          <dd><span id="z_session_valid"></span></dd>

          <dt class="text-right">Websocket Support?</dt>
          <dd><span id="ws_support"></span></dd>

          <dt class="text-right">Websocket</dt>
          <dd><span id="z_ws"></span></dd>

          <dt class="text-right">Pong Count</dt>
          <dd><span id="z_ws_pong_count" ></span></dd>

          <dt class="text-right">Comet</dt>
          <dd><span id="z_comet_status"></span></dd>

          <dt class="text-right">Reconnect Timeout</dt>
          <dd><span id="z_comet_reconnect_timeout"></span></dd>

          <dt class="text-right">Comet Poll Count</dt>
          <dd><span id="z_comet_poll_count"></span></dd>
      </dl>
      </div>
    </div>
   </div>
</div>


{% javascript %}

update();
var update_interval = setInterval(update, 1000);

var site_pinger = new http_ping("/close-connection", 2000);
var google_pinger = new http_ping("//www.google.com/gen_204", 2000);

function update() {
    z_event('update_session_info');

    function pinger_info(id, info) {
       if(info.tries) {
        $(id).text("Sent: " + info.tries + " Timeouts: " + info.loss.toFixed(1) + "%" +
            " Latency: " + info.latency + "ms" +
            " Avg: " + info.avg_latency.toFixed(1) + "ms" +
            " Min: " + info.min_latency.toFixed(1) + "ms" +
            " Max: " + info.max_latency.toFixed(1) + "ms"
            );
      }
    }

    pinger_info("#google-ping", google_pinger?(google_pinger.info()):{});
    pinger_info("#ping", site_pinger?(site_pinger.info()):{});

    $("#ws_support").text(window.WebSocket?"Yes":"No");

    $("#z_pageid").text(z_pageid);
    $("#z_session_valid").text(z_session_valid?"Session Valid":"Session Invalid");
    $("#z_comet_poll_count").text(z_comet_poll_count);
    $("#z_comet_reconnect_timeout").text(z_comet_reconnect_timeout);
    $("#z_ws_pong_count").text(z_ws_pong_count);
    $("#connect-status").text(z_stream_is_connected()?"Connected":"Not Connected");

    if(z_ws) {
        $("#z_ws").text("{_ Got a Websocket _}: readyState, " + z_ws.readyState);
    } else {
        $("#z_ws").text("{_ No Websocket _}");
    }

    if(z_comet) {
        $("#z_comet_status").text("{_ Got a Comet Connection _}: readyState, " + z_comet.readyState);
    } else {
        $("#z_comet_status").text("{_ No Comet Connection _}");
    }
}
{% endjavascript %}

{% endblock %}

