.. highlight:: django
.. include:: meta-chart_pie3d.rst

Show a pie chart with 3D effect.

This scomp is just a convenient interface to the :ref:`{% chart_pie %}
<scomp-chart_pie>` scomp with the ``threed`` argument set to true.

For example::

  {% chart_pie3d data=[["firefox",23],
                       ["internet explorer", 67],
                       ["safari",4],
                       ["chrome",3],
                       ["other", 3]]
  %}

Gives::

   <img class='google_chart' alt='google chart' src='http://chart.apis.google.com/chart?&cht=p3&chts=909090,10&chs=300x150&chg=0,0,1,5&chf=bg,s,ffffff|c,s,ffffff&chdlp=b&chbh=-3,3,7&chxt=x&chxl=0:|firefox|internet%20explorer|safari|chrome|other&chxs=0,909090,10&chco=&chds=0,100&chd=t:23,67,4,3,3&chls=1,1,0' width='300' height='150'/>

Or, as an image:
  
.. image:: /img/scomp_chart_pie_chart3d.png
             
.. seealso:: :ref:`scomp-google_chart` and :ref:`scomp-chart_pie`.

