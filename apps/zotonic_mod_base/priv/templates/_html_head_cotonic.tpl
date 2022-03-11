{# Initialize Cotonic - set promises and pre-connect to the websocket
 #}
<script type="text/javascript" nonce="{{ m.req.csp_nonce }}">
var cotonic = cotonic || {};

{# Pre-connect to the websocket, this connection is used by
 # cotonic.mqtt_transport.ws.js for the origin mqtt bridge.
 #}
cotonic.readyResolve = null;
cotonic.ready = new Promise(function(resolve) { cotonic.readyResolve = resolve; });
cotonic.bridgeSocket = new WebSocket(
    window.location.origin.replace(/^http/, 'ws')+ '{{ `mqtt_transport`|url with z_language=`x-default` }}',
    [ 'mqtt' ]);
cotonic.bridgeSocket.binaryType = 'arraybuffer';

{# Click and submit events triggered before cotonic.ready are buffered
 # cotonic.model.ui will replay the buffered events.
 #}
cotonic.bufferEvent = function(event) {
    const topic = event.target.getAttribute( "data-on"+event.type+"-topic" );
    if (typeof topic === "string") {
        let cancel = event.target.getAttribute( "data-on"+event.type+"-cancel" );
        if (cancel === null) {
            if (event.cancelable) {
                event.preventDefault();
            }
            event.stopPropagation();
        } else {
            switch (cancel) {
                case "0":
                case "no":
                case "false":
                    cancel = false;
                    break;
                case "preventDefault":
                    if (event.cancelable) {
                        event.preventDefault();
                    }
                    break;
                default:
                    if (event.cancelable) {
                        event.preventDefault();
                    }
                    event.stopPropagation();
                    break;
            }
        }
        cotonic.bufferedEvents.push(event);
    }
};
cotonic.bufferedEvents = [];
document.addEventListener("submit", cotonic.bufferEvent);
document.addEventListener("click", cotonic.bufferEvent);
cotonic.ready.then(
    function() {
        document.removeEventListener("submit", cotonic.bufferEvent);
        document.removeEventListener("click", cotonic.bufferEvent);
    });
</script>
