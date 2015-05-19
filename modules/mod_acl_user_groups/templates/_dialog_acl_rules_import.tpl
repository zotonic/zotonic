<p>
    {_ Upload the ACL rules file that you previously exported. Note: all current rules will be <b>replaced</b> by the rule definition file that is uploaded. _}
</p>

{% wire id=#form type="submit" delegate="admin_acl_rules" postback={acl_rule_import} %}
<form id="{{ #form }}" method="POST" action="postback" class="form">

    <div class="form-group row">
        <label class="control-label col-md-3" for="upload_file">{_ ACL rules file _}</label>
        <div class="col-md-9">
            <input class="form-control" type="file" id="upload_file" name="upload_file" />
            {% validate id="upload_file" type={presence} %}
        </div>
    </div>

    <div class="modal-footer">
        {% button class="btn btn-default" action={dialog_close} text="Cancel" tag="a" %}
        <button class="btn btn-primary" type="submit">{_ Import _}</button>
    </div>
</form>

