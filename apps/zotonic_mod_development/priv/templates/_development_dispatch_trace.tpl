<table class="table table-striped">
	<thead>
		<tr>
			<th>{_ Path _}</th>
			<th>{_ Action _}</th>
			<th>{_ Bindings _}</th>
		</tr>
	</thead>
	<tbody>
{% for t in trace %}
	<tr>
		<td>
			{% if t.path|is_list %}
				/{{ t.path|join:"/"|escape }}
			{% else %}
				{{ t.path|escape }}
			{% endif %}
		</td>
		<td>
			{% if t.step == `try_match` %}
				{_ Start matching dispatch rules _}
			{% elseif t.step == `match` %}
				{_ Found matching dispatch rule _}: <tt>{{ t.args.dispatch }}</tt><br/>
				{_ Controller _}: <tt>{{ t.args.controller }}</tt>
				{% if t.args.controller_args %}
					<br/>{_ Options _}:
					{% print t.args.controller_args %}
				{% endif %}
			{% elseif t.step == `dispatch` %}
				{_ Final dispatch_}<br/>
				{_ Controller _}: <tt>{{ t.args.controller }}</tt>
				{% if t.args.controller_args %}
					<br/>{_ Options _}:
					{% print t.args.controller_args %}
				{% endif %}
			{% elseif t.step == `protocol_switch` %}
				{_ Switch protocol to _}: {{ t.args.protocol }}<br/>
				{_ New host _}: {{ t.args.host|escape }}
			{% elseif t.step == `forced_protocol_switch` %}
				{_ Forced switch of protocol to _}: {{ t.args.protocol }}<br/>
				{_ New host _}: {{ t.args.host|escape }}
			{% elseif t.step == `no_dispatch_match` %}
				{_ No matching dispatch rule found _}
			{% elseif t.step == `dispatch_rewrite` %}
				<tt>#dispatch_rewrite{}</tt> {_ notification _}<br/>
				{_ New path _}: <tt>/{{ t.args.path|join:"/"|escape }}</tt>
			{% elseif t.step == `notify_dispatch` %}
				<tt>#dispatch{}</tt> {_ notification, checking first return _}
			{% elseif t.step == `rewrite_id` %}
				<tt>#dispatch{}</tt> {_ found resource id _}: {{ t.args.id }}<br/>
				{% if t.args.path %}
					{_ New path _}: <tt>{{ t.args.path }}</tt>
				{% else %}
					{_ This resource is unknown or does not have a URI _}
				{% endif %}
			{% elseif t.step == `rewrite_match` %}
				<tt>#dispatch{}</tt> {_ found dispatch rule _}: <tt>{{ t.args.dispatch }}</tt><br/>
				{_ Controller _}: <tt>{{ t.args.controller }}</tt>
				{% if t.args.controller_args %}
					<br/>{_ Options _}:
					{% print t.args.controller_args %}
				{% endif %}
			{% elseif t.step == `rewrite_nomatch` %}
				<tt>#dispatch{}</tt> {_ found no match _}
			{% elseif t.step == `rewrite_redirect` %}
				<tt>#dispatch{}</tt> {_ redirects _}<br/>
				{_ Location _}: <tt>{{ t.args.location|escape }}</tt>
				{% if t.args.permanent %}
					<br/>{_ Permanent redirect _}
				{% endif %}
			{% elseif t.step == `redirect` %}
				{_ Redirect _}<br/>
				{_ Location _}: <tt>{{ t.args.location|escape }}</tt>
				{% if t.args.permanent %}
					<br/>{_ Permanent redirect _}
				{% endif %}
			{% else %}
				{{ t.step }}
			{% endif %}
		</td>
		<td>
			<dl class="dl-horizontal" style="margin-top: 0">
			{% for k,v in t.args.bindings %}
				<dt style="width:160px">{{ k }}</dt>
				<dd style="margin-left: 175px">
					{% if k == `zotonic_dispatch_path` or k == `zotonic_dispatch_path_rewrite` %}
						{% print v %}
					{% else %}
						{{ v|escape }}
					{% endif %}
				</dd>
			{% endfor %}
			</dl>
		</td>
	</tr>
{% endfor %}
	</tbody>
</table>
