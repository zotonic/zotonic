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
      <a class="navbar-brand" href="/">
        {{ m.site.title }} &mdash; <small>{{ m.req.host|escape }}</small>
      </a>
    </div>
    <div id="navbar" class="collapse navbar-collapse">
    </div>
  </div>
</nav>

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
  {#
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
  #}
  <div class="col-md-6 col-lg-6 col-sm-6">
    <div class="panel panel-info">
      <div class="panel-heading">
          <h3 class="panel-title">MQTT Bridge Connection Status</h3>
      </div>
      <div class="panel-body">

      <dl class="dl-horizontal">
          <dt class="text-right">Websocket Support?</dt>
          <dd><span id="ws_support"></span></dd>

          <dt class="text-right">Websocket</dt>
          <dd><span id="connect-status"></span></dd>

          <dt class="text-right">MQTT Bridge</dt>
          <dd><span id="bridge-status"></span></dd>

          <dt class="text-right">Pong Count</dt>
          <dd><span id="pong-count">-</span></dd>

          <dt class="text-right">Pong Latency</dt>
          <dd><span id="pong-latency">-</span></dd>

          <dt class="text-right">Pong Errors</dt>
          <dd><span id="pong-error-count" >-</span></dd>
      </dl>
      </div>
    </div>
   </div>
</div>


{% javascript %}

var update_interval = setInterval(update, 1000);

var site_pinger = new http_ping("/close-connection", 2000);
var google_pinger = new http_ping("//www.google.com/gen_204", 2000);

var pong_count = 0;
var pong_error_count = 0;
var pong_latency_total = 0;
var pong_latency_max = 0;
var pong_latency_min = 5000;

var is_bridge_connected = false;

update();

function update() {

    {% comment %}z_event('update_session_info');{% endcomment %}

    function pinger_info(id, info) {
       if(info.tries) {
        document.getElementById(id).innerHTML =
            "Sent: " + info.tries +
            " Timeouts: <b>" + info.loss.toFixed(1) + "%</b>" +
            " Latency: <b>" + info.latency + "ms</b>" +
            " Avg: " + info.avg_latency.toFixed(1) + "ms" +
            " Min: " + info.min_latency.toFixed(1) + "ms" +
            " Max: " + info.max_latency.toFixed(1) + "ms";
      }
    }

    pinger_info("google-ping", google_pinger?(google_pinger.info()):{});
    pinger_info("ping", site_pinger?(site_pinger.info()):{});

    document.getElementById("ws_support").innerHTML = window.WebSocket?"Yes":"No";

    if (is_bridge_connected) {
      let startTime = new Date();

      cotonic.broker.call("bridge/origin/model/site/get/site", { qos: 1 })
        .then(
          function(msg) {
              let latency = Date.now() - startTime;

              pong_count++;
              pong_latency_total += latency;
              pong_latency_max = Math.max(latency, pong_latency_max);
              pong_latency_min = Math.min(latency, pong_latency_min);

              document.getElementById("pong-count").innerText = pong_count;

              let average = (pong_latency_total / pong_count).toFixed(1);

              document.getElementById("pong-latency").innerHTML =
                " <b>" + latency + "ms</b>" +
                " Avg: <b>" + average + "ms</b>" +
                " Min: " + pong_latency_max + "ms" +
                " Max: " + pong_latency_min + "ms";
          })
        .catch(
          function(e) {
            console.log(e);
              document.getElementById("pong-error-count").innerText = ++pong_error_count;
          });
    }
}

cotonic.broker.subscribe("session/origin/status", function(msg) {
    let connected;

    if (msg.payload.is_connected) {
      connected = "Connected";
    } else {
      connected = "Not connected";
    }
    document.getElementById("connect-status").innerText = connected;
});

cotonic.broker.subscribe("$bridge/origin/status", function(msg) {
    let connected;

    if (msg.payload.is_connected) {
      connected = "Connected";
      is_bridge_connected = true;
    } else {
      connected = "Not connected";
    }
    document.getElementById("bridge-status").innerText = connected;
});

{% endjavascript %}

{% endblock %}

