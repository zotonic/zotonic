{% extends "base_simple.tpl" %}

{% block title %}{_ Connection Test _}{% endblock %}

{% block content %}

{% lib
    "js/modules/http_ping.js"
%}


<div class="container">

<nav class="navbar navbar-default">
  <div class="container">
    <div class="navbar-header">
      <button type="button" class="navbar-toggle collapsed" data-toggle="collapse" data-target="#navbar" aria-expanded="false" aria-controls="navbar">
        <span class="sr-only">{_ Toggle navigation _}</span>
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

<h1>{_ Connection Test _}</h1>

<p>
  {_ On this page you can test the connection of your browser to our server and to the internet in general. _}<br>
  {_ Several aspects of the connection are tested _}:
</p>

<ul>
  <li>
    {_ General connection speed and quality, for which we test with a Google server and our own server. _}
  </li>
  <li>
    {_ The connection our site uses to talk with our own server. _}
  </li>
  <li>
    {_ If the authentication with the server works. _}
  </li>
  <li>
    {_ Support for offline work and communication between several tabs in your browser. _}
  </li>
</ul>

<h2>{_ Tests _}</h2>

<div class="row">
  <div class="col-md-12 col-lg-12 col-sm-12">
    <div class="panel panel-info">
      <div class="panel-heading">
          <h3 class="panel-title">{_ Browser Connectivity _}</h3>
      </div>
      <div class="panel-body">

          <p class="text-muted">
              {_ Your browser as it identifies itself with our server and your IP address. This is essential information for the help desk. _}
          </p>

          <dl class="dl-horizontal">
            <dt>{_ Browser _}</dt>
            <dd>{{ m.req.user_agent|escape }}</dd>
            <dt>{_ Client IP address _}</dt>
            <dd>{{ m.req.peer|escape }}</dd>
          </dl>

          <p class="text-muted">
            {_ The test below checks the connection between your machine, Google and our site. _}<br>
            {_ A latency of less than 50 milliseconds is very good, more than 250 milliseconds is slow. _}<br>
            {_ There should not be any timeouts, if there are timeouts then your internet connection is losing messages. _}
          </p>

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
          <h3 class="panel-title">{_ MQTT Bridge Connection Status _}</h3>
      </div>
      <div class="panel-body">

        <p class="text-muted">
          {_ This tests the communication with the server. _}
          {_ The <em>Pong Count</em> should increase every second, without any errors. _}
        </p>

        <dl class="dl-horizontal">
            <dt class="text-right">{_ Websocket Support? _}</dt>
            <dd><span id="ws_support"></span></dd>

            <dt class="text-right">{_ Websocket _}</dt>
            <dd><span id="connect-status"></span></dd>

            <dt class="text-right">{_ MQTT Bridge _}</dt>
            <dd><span id="bridge-status"></span></dd>

            <dt class="text-right">{_ Pong Count _}</dt>
            <dd><span id="pong-count">-</span></dd>

            <dt class="text-right">{_ Pong Latency _}</dt>
            <dd><span id="pong-latency">-</span></dd>

            <dt class="text-right">{_ Pong Errors _}</dt>
            <dd><span id="pong-error-count" >-</span></dd>
        </dl>
      </div>
    </div>
   </div>

    <div class="col-md-6 col-lg-6 col-sm-6">
      <div class="panel panel-info">
        <div class="panel-heading">
            <h3 class="panel-title">{_ Web Worker Status _}</h3>
        </div>
        <div class="panel-body">
          <p class="text-muted">
            {_ This tests the authentication with the server. _}
            {_ The authentication should show <em>Authenticated</em> or <em>Anonymous</em>. _}
          </p>

          <dl class="dl-horizontal">
            <dt class="text-right">{_ Auth Web Worker? _}</dt>
            <dd><span id="auth_support"></span></dd>
            <dt class="text-right">{_ Authentication _}</dt>
            <dd><span id="auth_status"></span></dd>
            <dt class="text-right">{_ Service Worker? _}</dt>
            <dd><span id="serviceWorker-active">{_ No _}</span></dd>
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

    document.getElementById("ws_support").innerHTML = window.WebSocket?"{_ Yes _}":"{_ No _}";

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
                " Min: " + pong_latency_min + "ms" +
                " Max: " + pong_latency_max + "ms";
          })
        .catch(
          function(e) {
            console.log(e);
              document.getElementById("pong-error-count").innerText = ++pong_error_count;
          });
    }

    if (navigator.serviceWorker && navigator.serviceWorker.controller) {
        cotonic.broker.publish("model/serviceWorker/post/broadcast/connection-test", { msg: "ping" });
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

cotonic.broker.subscribe("model/+model/event/ping", function(msg, bindings) {
    if (msg.payload == "pong") {
        if (document.getElementById(bindings.model + "_support")) {
          document.getElementById(bindings.model + "_support").innerText = "{_ Yes _}";
        }
    }
});

cotonic.broker.subscribe("model/auth/event/auth", function(msg) {
    if (msg.payload.status == "ok") {
      if (msg.payload.is_authenticated) {
          document.getElementById("auth_status").innerHTML = "{_ Authenticated _}:"
          + " <b>" + encodeURIComponent(msg.payload.username) + " </b> "
          + " <span class='text-muted'>(" + encodeURIComponent(msg.payload.user_id) + ")</span>";
      } else {
          document.getElementById("auth_status").innerText = "{_ Anonymous _}";
      }
    } else {
      document.getElementById("auth_status").innerHTML =
          "{_ No _}"
          + "<span class='text-muted'>" + encodeURIComponent(msg.payload.status) + "</span>";
    }
});

cotonic.broker.subscribe("model/serviceWorker/event/broadcast/connection-test", function(msg) {
    if (msg.payload.msg == "ping") {
        document.getElementById("serviceWorker-active").innerText = "{_ Yes _}";
    }
});

{% endjavascript %}

{% endblock %}

