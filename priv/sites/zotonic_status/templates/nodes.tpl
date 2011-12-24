{% extends "base.tpl" %}

{% block title %}{_ Nodes _}{% endblock %}

{% block content %}
<article id="content" class="zp-67">
	<div class="padding">
		<h1>{_ Zotonic nodes &amp; services _}</h1>
	
		<table id="nodes">

                    <tr>
                        <td></td>
                        <th>node1@localhost</th>
                        <th>node2@localhost</th>
                        <th>node3@localhost</th>
                    </tr>

                    <tr>
                        <th>
                            projectx:votechecker
                        </th>
                        <td class="running"><span class="running status">running</span></td>
                        <td class="running"><span class="running status">running</span></td>
                        <td class="running"><span class="running status">running</span></td>                    
                    </tr>

                    <tr>
                        <th>
                            projectx:votelogger
                        </th>
                        <td class="running"><span class="running status">running</span></td>
                        <td class="running"><span class="running status">running</span></td>
                        <td class="running"><span class="running status">running</span></td>                    
                    </tr>


                    <tr>
                        <th>
                            zynamo:kv
                        </th>
                        <td class="running"><span class="running status">running</span></td>
                        <td class="running"><span class="running status">running</span></td>
                        <td class="running"><span class="running status">running</span></td>                    
                    </tr>

		</table>
	</div>
</article>
{% endblock %}



{% block sidebar %}
<aside id="sidebar" class="zp-33">
	<div>
		{% all include "_z_system_button.tpl" %}
                {% all include "_z_trace_button.tpl" %}
	</div>
	<div style="clear:left" id="notices"></div>
</aside>
{% endblock %}


