<p>
    {_ This dialog lets you combine the recipients of this list with the recipients of another list. The result of this combination will be stored inside the current list, with the name _} <b>{{ id.title }}</b>.
</p>

{% wire type="submit" id=#form postback={mailinglist_combine id=id} delegate=`mod_mailinglist` %}
<form id="{{ #form }}" method="post" action="postback" class="form-horizontal">


    <div class="control-group">
        <label class="control-label" for="operation">{_ Operation _}</label>
        <div class="controls">

            <label class="radio">
                <input type="radio" name="operation" value="union" checked>
                    {_ <em>Add</em> all recipients to this list that are on the target list _}
            </label>
            <label class="radio">
                <input type="radio" name="operation" value="subtract">
                    {_ <em>Remove</em> all recipients from this list that are on the target list _}
            </label>
            <label class="radio">
                <input type="radio" name="operation" value="intersection">
                    {_ Only keep recipients on this list that appear <em>on both lists</em> _}
            </label>
        </div>
    </div>

    <div class="control-group">
        <label class="control-label" for="operation">{_ Target mailing list _}</label>
        <div class="controls">
            <select name="list_id" id="list_id">
                <option value="">{_ Select... _}</option>
                {% for id in m.search[{query cat=`mailinglist` pagelen=1000}] %}
                    <option value="{{ id }}">{{ id.title }}</option>
                {% endfor %}
            </select>
            {% validate id="list_id" type={presence} %}
        </div>
    </div>

    <div class="modal-footer">
	    {% button class="btn" text=_"Cancel" action={dialog_close} tag="a" %}
	    {% button class="btn btn-primary" text=_"Perform operation" %}
    </div>

</form>
