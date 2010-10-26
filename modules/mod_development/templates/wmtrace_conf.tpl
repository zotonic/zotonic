{% extends "admin_base.tpl" %}

{% block title %}Webmachine Trace Configuration{% endblock %}

{% block content %}


	<div id="content" class="zp-85">
		<div class="block clearfix">

                        <h2>Webmachine Trace Configuration</h2>
                                             
                        <div class="clearfix">
                            <label for="trace_dd">Global tracing:</label>
                            {% wire id="trace_dd" type="change" postback={set_global} %}
                            <select name="trace_global" id="trace_dd">
                            <option {% ifequal trace_global "disable" %} selected {% endifequal %} value="disable">No global resource tracing</option>
                            <option {% ifequal trace_global "5xx" %} selected {% endifequal %} value="5xx">Log all requests with 5xx response code</option>
                            <option {% ifequal trace_global "4xx&5xx" %} selected {% endifequal %} value="4xx&5xx">Log all requests with 4xx and 5xx response codes</option>
                            </select>				
			</div>						
                        
                        <br /><hr />

                        <div class="clearfix">
                            {% button text="Add trace rule" action={dialog_open title="Select a resource" template="_wmtrace_add.tpl" resources=res} %}
                        </div>

                        <h3 class="above-list ">Traced resources</h3>
			<ul class="short-list">
				<li class="headers clearfix">
                                    <span class="zp-90">Resource name</span>
                                    <span class="zp-10 last">Options</span>
				</li>

			{% for traced_res in trace_conf %}
                                <li class="clearfix">	

                                    <span class="zp-90">{{ traced_res|escape }}</span>	
                                    <span class="zp-10">
                                            {% button text="Delete" postback={delete res_to_del=traced_res} %}
                                    </span>
                                </li>

			{% empty %}
				<li class="clearfix">
                                    No traced resources.
				</li>
			{% endfor %}

			</ul>

		</div>
	</div>

{% endblock %}
