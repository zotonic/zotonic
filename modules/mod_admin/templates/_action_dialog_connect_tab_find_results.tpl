<div id="dialog-connect-results">
{% with m.search.paged[{fulltext text=text cat=cat page=1 pagelen=20}] as result %}
	<ul class="thumbnails" id="dialog-connect-results-ul">
		{% for id,_match in result %}
			{% catinclude "_action_dialog_connect_tab_find_results_li.tpl" id %}
		{% endfor %}
	</ul>

	{% lazy action={moreresults result=result target="dialog-connect-results-ul" 
						template="_action_dialog_connect_tab_find_results_li.tpl"
						catinclude 
						visible}
	%}
{% endwith %}
</div>
