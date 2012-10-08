
.. include:: meta-chart_pie.rst

Show a pie chart.

This is an utility tag providing a simplified interface to the pie chart feature of the :ref:`{% google_chart %} <scomp-google_chart>` tag.  It has an easier way to define the data.

Example of simple pie chart::

   {% chart_pie data=[["firefox",23], ["internet explorer", 67], ["safari",4], 
      ["chrome",3], ["other", 3]] %}

This generates the following image tag::

   <img class='google_chart' alt='google chart' 
      src='http://chart.apis.google.com/chart?&amp;cht=p&amp;chts=909090,10&amp;chs=300x150&amp;chg=0,0,1,5&amp;chf=bg,s,ffffff|c,s,ffffff&amp;chdlp=b&amp;chbh=-3,3,7&amp;chxt=x&amp;chxl=0:|firefox|internet 
      explorer|safari|chrome|other&amp;chxs=0,909090,10&amp;chco=&amp;chds=0,100&amp;chd=t:23,67,4,3,3&amp;chls=1,1,0' width='300' height='150' />

`View the pie chart`_.

The tag chart_pie accepts the following arguments:

+---------------+-----------------------------------------------------------------------+------------------------------------+
|Argument       |Description                                                            |Example                             |
+===============+=======================================================================+====================================+
|data           |The data for the pie chart. A list of pairs of {label, value} or       |[{"nl",300},{uk,"200"}]             |
|               |[label, value].                                                        |                                    |
+---------------+-----------------------------------------------------------------------+------------------------------------+
|colors         |The colors for the pies.  A list of colors, when there are more data   |colors=["ffcc00","ccff00","00ffcc"] |
|               |points than colors then the colors are interpolated.  Colors are       |                                    |
|               |specified in hexadecimal. Defaults to Google default colors.           |                                    |
+---------------+-----------------------------------------------------------------------+------------------------------------+
|threed         |Set to true to have a 3D effect on the pie chart. Defaults to false.   |threed=true                         |
+---------------+-----------------------------------------------------------------------+------------------------------------+
|width          |The width of the generated pie chart, in pixels.  Defaults to 300.     |width=450                           |
+---------------+-----------------------------------------------------------------------+------------------------------------+
|height         |The height of the generated pie chart, in pixels.  Defaults to 150.    |height=200                          |
+---------------+-----------------------------------------------------------------------+------------------------------------+

Other arguments can be found at the :ref:`scomp-google_chart` tag.

.. seealso:: :ref:`scomp-google_chart` and :ref:`scomp-chart_pie3d`.

.. _View the pie chart: http://chart.apis.google.com/chart?&cht=p&chts=909090,10&chs=300x150&chg=0,0,1,5&chf=bg,s,ffffff|c,s,ffffff&chdlp=b&chbh=-3,3,7&chxt=x&chxl=0:|firefox|internet%20explorer|safari|chrome|other&chxs=0,909090,10&chco=&chds=0,100&chd=t:23,67,4,3,3&chls=1,1,0
