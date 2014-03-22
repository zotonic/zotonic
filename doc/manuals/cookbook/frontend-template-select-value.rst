.. highlight:: django

Dynamic select options using a wired template
=============================================

Why
---

Suppose you want to wire a change event for a select box to update a
div based on the selected value i.e. you want to wire the action to
use the selected value when rendering the template.

Assumptions
-----------

Readers are assumed to be comfortable editing templates and have
Zotonic Scomp knowledge.

How
---

This is how the main template will look::

  {% wire id="node_customer" type="change" action={ update target="mydiv" template="_items_list.tpl" }%} 
  <select name="node_customer" id="node_customer"> <option value="">--Select a Customer--</option> 
  {% for cid in m.search[{query cat="customer" sort='+rsc.pivot_title'}] %} 
      <option value="{{cid}}"> {{ m.rsc[cid].title }}</option> 
  {% endfor %}
  </select>
  <select name="item_list" id="item_list">
    <option value="">--Select an item--</option>
    <div id="mydiv">
    {# this is where we dynamically update our template #}
    </div> 
  </select> 

So, when an item is selected (on change) "mydiv" will be replace by
``_item_list.tpl`` which looks like this::

  {% for itemid in m.search[{query cat="node" hasobject=[q.triggervalue, "node_customer"] sort='+rsc.pivot_title'}] %}
    <option value="{{itemid}}"> {{ m.rsc[itemid].title }}</option> 
  {% endfor %}
  
This template uses the q.triggervalue returned from the postback of
the wire event to query for items of category "node" that have
q.triggervalue as the object id and a predicate of "node_customer".

This example shows one way of using the q.triggervalue to dynamically
render templates after the main template has been rendered.

