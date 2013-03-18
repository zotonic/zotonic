/* This is a adapted version of the duration histogram
   example by Mike Bostock: http://bl.ocks.org/mbostock/3048166 */

function histogram_duration_chart() {
    // declare private vars
    var formatTime = d3.time.format("%H:%M");
    var client_width, client_height;

    // declare public properties
    var props = {
        // Formatters for counts and times (converting numbers to Dates).
        format: {
            x: function(d) {
                return formatTime(new Date(2012, 0, 1, 0, d)); },
            y: d3.format(",.0f")
        },

        // chart margins and size
        margin: {top: 10, right: 30, bottom: 30, left: 50},
        width: 600,
        height: 200,

        // scales, axis and data
        x: d3.scale.linear(),
        y: d3.scale.linear(),
        axis: {
            x: d3.svg.axis(),
            y: d3.svg.axis()
        },

        // callback functions for updating axis ticks
        ticks: {
            x: null,
            y: null
        },

        // e.g. transition duration, in ms
        animation_speed: 500,
    };

    // this function is responsible for visualizing the chart
    function chart(selection) {
        // define chart redraw function
        chart.update = function(data) {
            var svg = d3.select(this).select("svg g");

             // update axis domains
            props.x.domain([0, d3.max(data, function(d){ return d.x })]);
            props.y.domain([0, d3.max(data, function(d){ return d.y })]);

            // update axis ticks
            if (props.ticks.x)
                props.ticks.x.apply(chart, [props.axis.x, data]);
            if (props.ticks.y)
                props.ticks.y.apply(chart, [props.axis.y, data]);

            function top(d) {
                return props.y(d.y);
            }

            function left(d, i) {
                return i > 0 ? props.x(data[i-1].x) : 0;
            }

            function width(d, i) {
                return props.x(d.x) - left(d, i) - 1;
            }

            // draw bars!
            var bar = svg.selectAll(".bar")
                .data(data);

            // update existing
            bar.transition()
                .attr("transform", function(d, i) {
                    return "translate("
                        + left(d, i) + ","
                        + top(d) + ")"; })
                .select("rect")
                .attr("width", function(d, i) { return width(d, i) })
                .attr("height", function(d) {
                    return client_height - top(d); });

            // enter added bars
            bar.enter().append("g")
                .attr("class", "bar")
                .attr("transform", function(d, i) {
                    return "translate("
                        + left(d, i) + ","
                        + top(d) + ")"; })
                .append("rect")
                .attr("x", 1)
                .attr("height", function(d) {
                    return client_height - top(d); })
                .style("fill-opacity", 1e-6)
                .transition()
                .duration(props.animation_speed)
                .style("fill-opacity", 1)
                .attr("width", function(d, i) { return width(d, i) })
                .each("end", function(){
                    d3.select(this).style("fill-opacity", null) });
            ;

            // exit dropped bars
            bar.exit().transition()
                .attr("height", 0)
                .style("fill-opacity", 1e-6)
                .remove();

            // redraw axis
            svg.select(".x.axis").transition()
                .duration(props.animation_speed)
                .call(props.axis.x);
            svg.select(".y.axis").transition()
                .duration(props.animation_speed)
                .call(props.axis.y);

            return chart;
        };

        // create chart svg object for each selected object
        selection.each(function (data) {
            // store reference to this chart in the node
            this.chart = chart;

            client_width = props.width
                - props.margin.left
                - props.margin.right;
            client_height = props.height
                - props.margin.top
                - props.margin.bottom;

            // produce sample graph if no data provided
            if (!data)
            {
                // Generate a log-normal distribution with a median of 30 minutes.
                var values = d3.range(1000)
                    .map(d3.random.logNormal(Math.log(30), .4));

                // dummy domain, needed to get proper ticks for the bins
                props.x.domain([0, 120]);

                // Generate a histogram using twenty uniformly-spaced bins.
                data = d3.layout.histogram()
                    .bins(props.x.ticks(20))(values);
            }

            // setup input domain and output range
            props.x
                .domain([0, d3.max(data, function(d){ return d.x })])
                .range([0, client_width]);
            props.y
                .domain([0, d3.max(data, function(d){ return d.y })])
                .range([client_height, 0]);

            // setup axis
            props.axis.x
                .scale(props.x)
                .orient("bottom")
                .tickFormat(props.format.x);
            props.axis.y
                .scale(props.y)
                .orient("left")
                .tickFormat(props.format.y);

            // create svg
            var svg = d3.select(this).append("svg")
                .attr("width", props.width)
                .attr("height", props.height)
                .append("g")
                .attr("transform", "translate("
                      + props.margin.left + ","
                      + props.margin.top + ")");

            // add axis
            svg.append("g")
                .attr("class", "x axis")
                .attr("transform", "translate(0," + client_height + ")");

            svg.append("g")
                .attr("class", "y axis");

            // draw initial chart, and we're done
            return chart.update.apply(this, [data]);
        });

        return chart;
    }

    chart.call = function(callback) {
        callback.apply(this, Array.prototype.slice.call(arguments, 1));
        return this;
    };

    z_charts.proxy_properties(props, chart);

    // return our properly propertized chart function object
    return chart;
}
