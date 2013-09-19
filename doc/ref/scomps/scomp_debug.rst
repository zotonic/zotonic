
.. include:: meta-debug.rst

Shows which variables are assigned for use in the current template’s
scope::

  {% debug %}

This displays a HTML table with columns for the variable's name and its
value, and displays one variable per row::

  <table>
  <tbody>
    <tr>
        <td>session_id</td>
        <td>
            <tt>"qUHoeKodUHXbpwrcl6Vj"</tt>
        </td>
    </tr>
    
    <tr>
        <td>q</td>
        <td>
            <tt>[{"zotonic_host","examplesite"},{"zotonic_dispatch","home"}]</tt>
        </td>
    </tr>
    
    <tr>
        <td>template</td>
        <td>
            <tt>"home.tpl"</tt>
        </td>
    </tr>
    
    <tr>
        <td>zotonic_dispatch</td>
        <td>
            <tt>home</tt>
        </td>
    </tr>
  </tbody>
  </table>
    
.. seealso:: :ref:`tag-print`

