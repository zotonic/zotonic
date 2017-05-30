<div class="merge-results">
    {% with m.search.paged[{query text=text cat=cat page=1 pagelen=(12*3) id_exclude=id}] as result %}
        <div id="merge_loop_results" class="thumbnails">
            {% include "_merge_find_results_loop.tpl" result=result %}
        </div>
        {% lazy
            action={
                moreresults
                result=result
                target="merge_loop_results"
                template="_merge_find_results_loop.tpl"
                is_result_render
                visible
            }
        %}
    {% endwith %}
</div>
