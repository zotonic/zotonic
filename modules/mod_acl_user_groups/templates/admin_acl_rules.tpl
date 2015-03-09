{% extends "admin_acl_rules_base.tpl" %}

{% block active1 %}active{% endblock %}

{% block content_acl %}
    {% with q.kind|as_atom as kind %}
        
    <div class="acl-well">

        <div id="acl-rules">
            {% include "_acl_rules_list.tpl" %}
        </div>
        
    </div>

    <div class="acl-well">
        <h4>{_ New rule _}</h4>
        {% include "_admin_acl_rule_header.tpl" %}
        {% include "_admin_acl_rule_row.tpl" is_new %}
    </div>
    
    <div class="well acl-well">

        {% button text=_"Publish"
            class="btn btn-primary pull-right"
            action={confirm
                text=_"Are you sure you want to make all rules on this page permanent?"
                action={dialog_close}
                delegate=`admin_acl_rules`
                postback={publish
                    kind=kind
                }
            }
        %}

        {% button text=_"Try rules..."
            class="btn"
            action={dialog_open
                title=_"Try ACL rules"
                template="_dialog_acl_rules_try.tpl"
            }
        %}

        {% button text=_"Revert to published"
            class="btn"
            action={confirm
                text=_"Are you sure you want to restore all the ACL rules on this page to their currently published version?"
                action={dialog_close}
                delegate=`admin_acl_rules`
                postback={revert
                    kind=kind
                }
            }
        %}
        
    </div>

    {% endwith %}
    
{% endblock %}
