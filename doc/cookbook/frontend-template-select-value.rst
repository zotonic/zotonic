.. highlight:: django

Dynamic select options using a wired template
=============================================

Why
---

Suppose you want to wire a change event for a select box to update a
another select box, i.e. you want to wire the action to
use the selected value when rendering the template.

Assumptions
-----------

Readers are assumed to be comfortable editing templates and have
Zotonic Scomp knowledge.

How
---

We want to select an image from a page in 2 steps. First we select a page from the pages that have images. Then we select an image from the selected page.

This is how the main template will look::

    <div class="form-group">
        {% wire id="pages" type="change" action={update
            target="media_list"
            template="_media_list.tpl"
        }%}
        <select id="pages" class="form-control">
            <option value="">--Select a page--</option>
            {% for page_id in m.search[{query
                hasobjectpredicate='depiction'
                sort='+rsc.pivot_title'
            }] %}
                <option value="{{ page_id }}">
                    {{ m.rsc[page_id].title }}
                </option>
            {% endfor %}
        </select>
    </div>
    <div id="media_list">
        <!- contents will be updated -->
    </div>

When an item is selected (by the change event), element with id "media_list" will be replaced by ``_media_list.tpl`` which looks like this::

    {% with q.triggervalue as page_id %}
    <div class="form-group">
        <select class="form-control">
            <option value="">--Select an image--</option>
            {% for media_id in m.rsc[page_id].media %}
                <option value="{{media_id}}">
                    {{ m.rsc[media_id].title }}
                </option>
            {% endfor %}
        </select>
    </div>
    {% endwith %}

This template uses the q.triggervalue returned from the postback of
the wire event, and it contains the value of the selected option.

We can go one step further to show the selected image. ``_media_list.tpl`` also gets a wire action::

    {% with q.triggervalue as page_id %}
    <div class="form-group">
        {% wire id="images" type="change" action={update
            target="image"
            template="_image.tpl"
        }%}
        <select class="form-control" id="images">
            <option value="">--Select an image--</option>
            {% for media_id in m.rsc[page_id].media %}
                <option value="{{media_id}}">
                    {{ m.rsc[media_id].title }}
                </option>
            {% endfor %}
        </select>
    </div>
    <div id="image">
        <!- contents will be updated -->
    </div>
    {% endwith %}

And we show the image using ``_image.tpl``::

    {% with m.rsc[q.triggervalue].medium as medium %}
    <div class="form-group">
        {% image medium width=300 %}
    </div>
    {% endwith %}
