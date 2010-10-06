<html>
<head>
	<meta http-equiv="content-type" content="text/html;charset=utf-8" />
	<title>Webmachine trace {{ filename|escape }}</title>

	{% lib 
		"css/wmtrace.css" 
		"js/wmtrace.js"
	%}
	
	<script type="text/javascript">
		var request = {{ request }};
		var response = {{ response }};
		var trace = {{ trace }};
	</script>
</head>

<body>
	<div id="zoompanel">
		<button id="zoomout">zoom out</button>
		<button id="zoomin">zoom in</button>

		<div id="canvaspanel">
                    <canvas id="v3map" width="3138" height="2184"></canvas>
                </div>
		
		<div id="sizetest"></div>
		<div id="preview">
			<div id="previewid"</div>
			<ul id="previewcalls"></ul>
		</div>

		<div id="infopanel">
			<div id="infocontrols">
				<div id="requesttab" class="selectedtab">Q</div>
				<div id="responsetab">R</div>
				<div id="decisiontab">D</div>
			</div>
			<div id="requestdetail">
				<div>
					<span id="requestmethod"></span>
					<span id="requestpath"></span>
				</div>
				<ul id="requestheaders"></ul>
				<div id="requestbody"></div>
			</div>
			<div id="responsedetail">
				<div id="responsecode"></div>
				<ul id="responseheaders"></ul>
				<div id="responsebody"></div>
			</div>
			<div id="decisiondetail">
				<div>Decision: <select id="decisionid"></select></div>
				<div>Calls: <select id="decisioncalls"></select></div>
				<div>Input:</div>
				<pre id="callinput"></pre>
				<div>Output:</div>
				<pre id="calloutput"></pre>
			</div>
		</div>
	</div>
</html>
