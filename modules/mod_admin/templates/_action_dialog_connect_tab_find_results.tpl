<div id="dialog-connect-results">
    {% with m.search.paged[{fulltext text=text cat=cat page=1 pagelen=20}] as result %}
		{% include "_action_dialog_connect_tab_find_results_loop.tpl" id result=result %}

	    {% lazy action={moreresults result=result target="dialog-connect-results-ul" 
				template="_action_dialog_connect_tab_find_results_loop.tpl"
                is_result_render
				visible}
	%}
    {% javascript %}$.dialogReposition();{% endjavascript %}
{% endwith %}
</div>
