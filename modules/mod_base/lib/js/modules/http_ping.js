/* HTTP Ping

@package: Channel.me 2015
@Author: MM Zeeman <mmzeeman@xs4all.nl>

Copyright 2015 Maas-Maarten Zeeman

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.

*/

/**
 * Inserts invisible img tags in the body.
 * Tries to load the src, when the onload or onerror are triggered it means 
 * that the resource was loadable. This is counted as a success.
 */

function http_ping(url, ping_interval) {
	var ping_tries = 0;
	var ping_errors = 0;
	var ping_success = 0;
	
	var ping_latency = 0;
	var ping_latency_total = 0;
	
	var ping_latency_min = ping_interval;
	var ping_latency_max = 0;
	
	var current_ping;
	
	function ping() {
	    if(current_ping) {
	        document.body.removeChild(current_ping);
	        ping_errors += 1;
	        
	        current_ping.onload = undefined;
	        current_ping.onerror = undefined;
	        current_ping.src = ""; /* cancel loading */
	        current_ping = undefined;
	    }
	    
	    function responded() {
	        ping_latency = Date.now() - start_time;
	        ping_latency_total += ping_latency;
	        ping_success += 1;
	        
	        ping_latency_max = Math.max(ping_latency, ping_latency_max);
	        ping_latency_min = Math.min(ping_latency, ping_latency_min);
	            
	        if(current_ping) { 
	            document.body.removeChild(current_ping);
	            current_ping = undefined;
	        }
	    }
	    
	    current_ping = document.createElement("img");
	    current_ping.onload = responded;
	    current_ping.onerror = responded;
	    current_ping.src = url;
	    current_ping.style.display = "none";
	    
	    document.body.appendChild(current_ping);
	    var start_time = Date.now();
	    
	    ping_tries += 1;
	};
	
	function loss() {
	    if(ping_success) 
	        return (ping_errors / ping_success) * 100;
	    
	    if(ping_errors) 
	        return 100.0;
	    
	    return 0.0;
	}
	
	this.info = function() {
	    return {
	    	    url: url,
		    tries: ping_tries,
		    success: ping_success,
		    errors: ping_errors,
		    loss: loss(),
		    latency: ping_latency,
		    avg_latency: ping_success?(ping_latency_total/ping_success):0,
		    min_latency: ping_latency_min,
		    max_latency: ping_latency_max
	    }
	}
	
	ping();
	setInterval(ping, ping_interval?ping_interval:1000);
}