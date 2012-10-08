
.. include:: meta-google_chart.rst

Make charts with Google.

This tag interfaces to the `Google chart API`_ to dynamically generate charts.

For example::

   {% google_chart type="line" axis={axis position="bottom" labels=["jan","feb","mar"]}
      axis={axis position="left" labels=[0,25,50,75,100]} data=[{data values=[20,80,33]}] %}

Will generate the following image tag::

   <img class='google_chart' alt='google chart'
      src='http://chart.apis.google.com/chart?&amp;cht=lc&amp;chts=909090,10&amp;chs=300x150&amp;chg=0,0,1,5&amp;chf=bg,s,ffffff|c,s,ffffff&amp;chdlp=b&amp;chbh=-3,3,7&amp;chxt=x,y&amp;chxl=0:|jan|feb|mar|1:|0|25|50|75|100&amp;chxs=0,909090,10|1,909090,10&amp;chco=&amp;chds=0,100&amp;chd=t:20,80,33&amp;chls=1,1,0' 
      width='300' height='150'/>

`View the chart`_.

Google chart accepts the following arguments:

+-----------------+----------------------------------------------------------+-------------------------------+
|Argument         |Description                                               |Example                        |
+=================+==========================================================+===============================+
|type             |Kind of chart is generated. One of "line", "sparkline",   |type="sparkline"               |
|                 |"stacked_horizontal_bar", "stacked_vertical_bar",         |                               |
|                 |"grouped_horizontal_bar", "grouped_vertical_bar", "pie" or|                               |
|                 |"pie3d".  Defaults to "line".                             |                               |
+-----------------+----------------------------------------------------------+-------------------------------+
|id               |The id of the generated image tag.                        |id="mychart"                   |
+-----------------+----------------------------------------------------------+-------------------------------+
|class            |CSS class of the generated image tag. The class           |class="chart"                  |
|                 |"google_chart" is always added.                           |                               |
+-----------------+----------------------------------------------------------+-------------------------------+
|style            |CSS style attribute for the generated image tag.          |style="border: 1px solid #fcc" |
+-----------------+----------------------------------------------------------+-------------------------------+
|title            |Title shown on the chart.                                 |title="Browser shares"         |
+-----------------+----------------------------------------------------------+-------------------------------+
|color            |Color for the title.  Must be in hexadecimal, defaults to |color="ffcc00"                 |
|                 |"909090".                                                 |                               |
+-----------------+----------------------------------------------------------+-------------------------------+
|font_size        |Font size in pixels for the title. Defaults to 10.        |font_size=12                   |
+-----------------+----------------------------------------------------------+-------------------------------+
|width            |Width of the generated chart. Defaults to 300.            |width=450                      |
+-----------------+----------------------------------------------------------+-------------------------------+
|height           |Height of the generated chart.                            |height=200                     |
+-----------------+----------------------------------------------------------+-------------------------------+
|grid_x           |X axis grid step size.                                    |grid_x=10                      |
+-----------------+----------------------------------------------------------+-------------------------------+
|grid_y           |Y axis grid step size.                                    |grid_y=10                      |
+-----------------+----------------------------------------------------------+-------------------------------+
|grid_line_length |Length of line segment for the grid lines, defaults to 1. |grid_line_length=1             |
+-----------------+----------------------------------------------------------+-------------------------------+
|grid_blank_length|Length of gaps in the grid lines, defaults to 5.          |grid_blank_length=5            |
+-----------------+----------------------------------------------------------+-------------------------------+
|background_color |Background color for the complete chart.  in hexadecimal, |background_color="331133"      |
|                 |defaults to "ffffff".                                     |                               |
+-----------------+----------------------------------------------------------+-------------------------------+
|chart_color      |Background color for the chart area. In hexadecimal,      |chart_color="113311"           |
|                 |defaults to "ffffff".                                     |                               |
+-----------------+----------------------------------------------------------+-------------------------------+
|legend_location  |Where the legend is placed.  One of "top", "left",        |legend_location="right"        |
|                 |"bottom" or "right".  Defaults to "bottom".               |                               |
+-----------------+----------------------------------------------------------+-------------------------------+
|axis             |Description of an axis. You can given more than one axis  |                               |
|                 |argument. See the section `Axis styles and labels`_ below.|                               |
+-----------------+----------------------------------------------------------+-------------------------------+
|data             |The data to be shown. You can give more than one data     |                               |
|                 |argument. See `Data`_ definition below.                   |                               |
+-----------------+----------------------------------------------------------+-------------------------------+
|bar_space        |Space in pixels between the bar of a bar chart. Defaults  |bar_space=5                    |
|                 |to 3.                                                     |                               |
+-----------------+----------------------------------------------------------+-------------------------------+
|bar_group_space  |Space in pixels between the groups of bars of a bar       |bar_group_space=10             |
|                 |chart. Defaults to 7.                                     |                               |
+-----------------+----------------------------------------------------------+-------------------------------+


Axis styles and labels
----------------------

Axis styles and labels are available for line charts and bar charts.

An example for an axis argument is::

   axis={axis font_size=10 color="909090" position="top" labels=["jan", "feb", "mar"]}

This defines an axis that will be displayed above the chart. It has three labels and will be displayed in a 10 pixel font with color "909090".  You can give a multiple axis arguments and also a list of axes for each argument.

Valid arguments for an axis are:

+---------------+----------------------------------------------+------------------------+
|Argument       |Description                                   |Example                 |
+===============+==============================================+========================+
|font_size      |Size of the labels in pixels. Defaults to 10. |font_size=12            |
+---------------+----------------------------------------------+------------------------+
|color          |Color for the label texts, in hexadecimal     |color="cc0000"          |
|               |RGB. Defaults to "909090".                    |                        |
+---------------+----------------------------------------------+------------------------+
|position       |Which axis is defined. One of "top", "right", |position="bottom"       |
|               |"bottom" and "left". Defaults to "top". You   |                        |
|               |can have multiple definitions for a position. |                        |
+---------------+----------------------------------------------+------------------------+
|labels         |A list with labels displayed. The labels will |labels=[2006,2007,2008] |
|               |be evenly distributed over the axis.          |                        |
+---------------+----------------------------------------------+------------------------+


Data
----

All data definitions to be displayed. Each data definition is a record with arguments.  You can supply all data sets at once or with multiple data arguments.

Example with one data set::

   data=[{data line_width=1 min_value=1 max_value=100 values=[10,20,42,34,68,73,80]}]

Valid arguments for a data record are:

+---------------+--------------------------------------------------+-------------------+
|Argument       |Description                                       |Example            |
+===============+==================================================+===================+
|line_width     |The width of the line in pixels. Defaults to 1.   |line_width=2       |
+---------------+--------------------------------------------------+-------------------+
|line_length    |Length of line segment in pixels. Defaults to 1.  |line_length=1      |
+---------------+--------------------------------------------------+-------------------+
|blank_length   |Length of blank segment in pixels. Defaults to 0. |blank_length=1     |
+---------------+--------------------------------------------------+-------------------+
|min_value      |The minimum value for the data set, used for the  |min_value=-100     |
|               |axis. Defaults to 0.                              |                   |
+---------------+--------------------------------------------------+-------------------+
|max_value      |The maximum value for the data set, used for the  |max_value=100      |
|               |axis. Defaults to 100.                            |                   |
+---------------+--------------------------------------------------+-------------------+
|color          |The color used for this data set. Hexadecimal RGB |color="ffcc00"     |
|               |value.                                            |                   |
+---------------+--------------------------------------------------+-------------------+
|legend         |Label for the dataset as shown in the legend.     |legend="monthly    |
|               |                                                  |sales"             |
+---------------+--------------------------------------------------+-------------------+
|values         |The values for drawing the chart. Must be a list. |values=[0,10,5,8]  |
+---------------+--------------------------------------------------+-------------------+

.. seealso:: tags :ref:`scomp-chart_pie` and :ref:`scomp-chart_pie3d`.

.. _Google chart API: http://code.google.com/apis/chart/
.. _View the chart: http://chart.apis.google.com/chart?&cht=lc&chts=909090,10&chs=300x150&chg=0,0,1,5&chf=bg,s,ffffff|c,s,ffffff&chdlp=b&chbh=-3,3,7&chxt=x,y&chxl=0:|jan|feb|mar|1:|0|25|50|75|100&chxs=0,909090,10|1,909090,10&chco=&chds=0,100&chd=t:20,80,33&chls=1,1,0
