{#
Params
- winner_id
- loser_id
- left
- right
- left_id
- right_id
#}
{% wire id="merge-form" type="submit" postback={merge winner_id=winner_id loser_id=loser_id} delegate=`mod_admin_merge` %}
<form id="merge-form" method="post" action="postback">
    <fieldset class="form-horizontal">
        <input type="hidden" name="merge_action" value="merge_delete" />

        <h5 class="form-control-static"><label>{_ 1. Merge loser into winner _}</label></h5>
        <div class="form-group row">
            <div class="col-md-4">
                <div class="panel panel-default">
                    <div class="panel-body">
                        {{ left_id.title|truncate_html:80 }}
                    </div>
                </div>
            </div>
            <div class="col-md-2">
                <div class="panel panel-default" style="border: none; box-shadow: none; text-align: center">
                    <div class="panel-body" style="font-size: 200%;margin-top:-.5em">
                        {% if right %}&rarr;{% else %}&larr;{% endif %}
                    </div>
                </div>
            </div>
            <div class="col-md-4">
                <div class="panel panel-default">
                    <div class="panel-body">
                        {{ right_id.title|truncate_html:80 }}
                    </div>
                </div>
            </div>
        </div>
        <h5 class="form-control-static"><label>{_ 2. Delete loser _}</label></h5>
        <div class="form-group row">
            <div class="col-md-4">
                {% if right %}
                    <div class="panel panel-default">
                        <div class="panel-body">
                            <s>{{ left_id.title }}</s>
                        </div>
                    </div>
                {% endif %}
            </div>
            <div class="col-md-2">
                <div class="panel panel-default" style="border: none; box-shadow: none; text-align: center">
                    <div class="panel-body"></div>
                </div>
            </div>
            <div class="col-md-4">
                {% if left %}
                    <div class="panel panel-default">
                        <div class="panel-body">
                            <s>{{ right_id.title }}</s>
                        </div>
                    </div>
                {% endif %}
            </div>
        </div>
        <div class="form-group row">
            <div class="col-md-10 text-danger">
                <i class="fa fa-warning"></i> {_ Deletion cannot be undone. _}
            </div>
        </div>
    </fieldset>

    {% if left_id.language|sort != right_id.language|sort and not winner_id.is_a.category %}
        <div class="form-group">
            <div class="checkbox">
                <label>
                    <input type="checkbox" id="is_merge_trans" name="is_merge_trans" value="1" checked />
                    {_ Add missing languages to _} {{ winner_id.title }}
                </label>
            </div>
        </div>
    {% endif %}

    <div class="modal-footer clearfix">
        {% button class="btn btn-default" action={dialog_close} text=_"Cancel" tag="a" %}
        {% button class="btn btn-primary" text=_"Merge" %}
    </div>
</form>