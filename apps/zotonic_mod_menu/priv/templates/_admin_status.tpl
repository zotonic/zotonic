<div class="form-group">
    <div>
        {% button class="btn btn-default" text=_"Ensure <i>hasmenupart</i> connections"
                  postback={ensure_hasmenupart}
                  delegate=`mod_menu`
        %}
        <p class="help-block">{_ Check all menu pages and add <i>hasmenupart</i> connections for all of the 
    pages in the menu. _}</p>
    </div>
</div>
