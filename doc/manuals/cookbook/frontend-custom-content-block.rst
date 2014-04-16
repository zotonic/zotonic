.. _manual-cookbook-frontend-custom-content-block:

How to add a custom Content Block
---------------------------------

Zotonic comes with a number of standard content blocks: Header, Text
and Embed page. Additional content blocks are provided by modules, for
example mod_survey uses content blocks extensively for composing
surveys. Content blocks allow a content manager to add sophisticated
sections of content, perhaps with configuration options. They could be
used for design reasons e.g. for multi-column layouts, image carousels
or floating elements. This cookbook item describes how to add a simple
custom content block.

In order to add a custom content block, we need to register it, by
editing an erlang file and adding two templates. For this tutorial
we'll add a content block which inserts the code for a music player
from jamendo.com, a Creative Commons music hosting site.

To register the content block we add an
``observe_admin_edit_blocks/3`` function to the site's erl file
e.g. zotonic/priv/sites/mysite/mysite.erl::


    %%====================================================================
    %% support functions go here
    %%====================================================================


    -export([
        observe_admin_edit_blocks/3
    ]).


    observe_admin_edit_blocks(#admin_edit_blocks{}, Menu, Context) ->
        [
             {100, ?__("Media", Context), [
                 {music_player, ?__("Music Player", Context)}
             ]}
         | Menu
        ].

The interesting parts are ``"Media"``, ``"Music Player"`` and
``music_player``. ``"Media"`` will appear as a section heading in the
``[+add block]`` drop down menu, below the ``Standard`` section.
``"Music Player"`` will appear below this, and is what the content
editor clicks on to instert the block. The ``music_player`` atom is
what Zotonic uses to figure out which templates to use, they will be
created in ``templates/blocks`` and will be called
``_admin_edit_block_li_music_player.tpl`` and
``_block_view_music_player.tpl``.

As the name suggests, ``_admin_edit_block_li_music_player.tpl`` is the
template used on the edit form::

    {% extends "admin_edit_widget_i18n.tpl" %}

    {% block widget_title %}{_ Block _}{% endblock %}
    {% block widget_show_minimized %}false{% endblock %}
    {% block widget_id %}edit-block-{{ #block }}{% endblock %}
    {% block widget_header %}{% endblock %}

    {% block widget_content %}
        <div class="control-group">
                <label class="control-label">URL of the music widget</label>
                <div class="controls">
                        <input type="text" id="block-{{name}}-url"
                                name="block-{{name}}-url"
                                value="{{ blk.url }}"
                                class="input-block-level"
                                placeholder="{_  Enter the url of the music widget _}"
                        />
                </div>
        </div>
    {% endblock %}

For the purpose of demonstration, we extend the
``admin_edit_widget_i18n.tpl`` template and use some template blocks.

Each time this block is added to the page, the form above will be displayed.
Here, we want to allow users to easily embed a music widget from Jamedo, an online music community.
The way do this is by adding extra input fields to the page through the block templates.

Please note the values of the attributes of the input fields in the block template.
For example, ``name="block-{{name}}-url"`` will expand to ``name="block-mus56h-url"``.
The ``name`` variable is a randomly generated string that is unique within every block template.
The last portion of the value of the attribute is the attribute to be added to the resource.
In this case, the attribute to be added will be ``url``.

Also note the value of the input's value attribute.
This ensures that if the attribute has already been saved against the resource being edited, then
the input will be filled with the saved value. Without this, the data entered will not be saved.

This is all you need to be able to add a block to the edit form. If
you update your site and restart, you should now be able to select the
new block. It just doesn't display anything yet so let's add
``_block_view_music_player.tpl``::

    <iframe id="widget" scrolling="no" frameborder="0" width="400"
            height="284" style="width: 400px; height: 284px;"
            src="{{ blk.url }}"></iframe>

In the block's frontend view template, a special variable called ``blk`` is available.
This ``blk`` variable holds the attributes or properties of the block.
Since we added only one attribute, ``url``, to the block's admin template, the ``blk`` variable will hold only two
properties: ``name``, and our custom attribute, ``url``.

So if the user supplied ``http://widgets.jamendo.com/v3/album/40728?autoplay=0&layout=standard&width=400``,
as the url to the music widget, the block's frontend view template will expand to::

        <iframe id="widget" scrolling="no" frameborder="0" width="400"
            height="284" style="width: 400px; height: 284px;"
            src="http://widgets.jamendo.com/v3/album/40728?autoplay=0&layout=standard&width=400">
        </iframe>

We can extend this custom block to allow the user to specify the widget's height, width, and frameborder.

All you have to do is add new input fields in the block's admin template.

.. _Jamendo: http://www.jamendo.com
