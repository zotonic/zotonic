<table class="table table-striped" style="width: auto">
	<thead>
		<tr>
			<th>{_ Path _}</th>
			<th>{_ Action _}</th>
			<th>{_ Bindings _}</th>
		</tr>
	</thead>
	<tbody>
{% for _trace, path, what, args in trace %}
	<tr>
		<td>
			{% if what == `notify_dispatch` %}
				/{{ path|escape }}
			{% elseif path != `undefined` %}
				/{{ path|join:"/"|escape }}
			{% endif %}
		</td>
		<td>
			{% if what == `try_match` %}
				{_ Start matching dispatch rules _}
			{% elseif what == `match` %}
				{_ Found matching dispatch rule _}: <tt>{{ args.dispatch }}</tt><br/>
				{_ Controller _}: <tt>{{ args.controller }}</tt>
				{% if args.controller_args %}
					<br/>{_ Options _}:
					{% print args.controller_args %}
				{% endif %}
			{% elseif what == `protocol_switch` %}
				{_ Switch protocol to _}: {{ args.protocol }}<br/>
				{_ New host _}: {{ args.host|escape }}
			{% elseif what == `forced_protocol_switch` %}
				{_ Forced switch of protocol to _}: {{ args.protocol }}<br/>
				{_ New host _}: {{ args.host|escape }}
			{% elseif what == `no_dispatch_match` %}
				{_ No matching dispatch rule found _}
			{% elseif what == `dispatch_rewrite` %}
				<tt>#dispatch_rewrite{}</tt> {_ notification _}<br/>
				{_ New path _}: <tt>/{{ args.path|join:"/"|escape }}</tt>
			{% elseif what == `notify_dispatch` %}
				<tt>#dispatch{}</tt> {_ notification, checking first return _}
			{% elseif what == `rewrite_id` %}
				<tt>#dispatch{}</tt> {_ found resource id _}: {{ args.id }}<br/>
				{% if args.path %}
					{_ New path _}: <tt>{{ args.path }}</tt>
				{% else %}
					{_ This is resource is unknown or does not have an uri _}
				{% endif %}
			{% elseif what == `rewrite_match` %}
				<tt>#dispatch{}</tt> {_ found dispatch rule _}: <tt>{{ args.dispatch }}</tt><br/>
				{_ Controller _}: <tt>{{ args.controller }}</tt>
				{% if args.controller_args %}
					<br/>{_ Options _}:
					{% print args.controller_args %}
				{% endif %}
			{% elseif what == `rewrite_redirect` %}
				<tt>#dispatch{}</tt> {_ redirects _}<br/>
				{_ Location _}: <tt>{{ args.location|escape }}</tt>
				{% if args.permanent %}
					<br/>{_ Permanent redirect _}
				{% endif %}
			{% elseif what == `redirect` %}
				{_ Redirect _}<br/>
				{_ Location _}: <tt>{{ args.location|escape }}</tt>
				{% if args.permanent %}
					<br/>{_ Permanent redirect _}
				{% endif %}
			{% else %}
				{{ what }}
			{% endif %}
		</td>
		<td>
			<dl class="dl-horizontal" style="margin-top: 0">
			{% for k,v in args.bindings %}
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
