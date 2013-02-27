/* This is a adapted version of the duration histogram
   example by Mike Bostock: http://bl.ocks.org/mbostock/3048166 */

function histogram_duration_chart() {
    // declare private vars
    var formatTime = d3.time.format("%H:%M");

    // declare public properties
    var props = {
        // Formatters for counts and times (converting numbers to Dates).
        format: {
            x: function(d) {
                return formatTime(new Date(2012, 0, 1, 0, d)); },
            y: d3.format(",.0f")
        },

        // chart margins and size
        margin: {top: 10, right: 30, bottom: 30, left: 30},
        width: 600,
        height: 200,

        // scales, axis and data
        x: d3.scale.linear(),
        y: d3.scale.linear(),
        axis: {
            x: d3.svg.axis(),
            y: d3.svg.axis()
        },
        data: null, // [{x, y}]
    };

    // this function is responsible for visualizing the chart
    function chart(selection) {
        var client_width = props.width
            - props.margin.left
            - props.margin.right;
        var client_height = props.height
            - props.margin.top
            - props.margin.bottom;

        // produce sample graph if no data provided
        if (!props.data)
        {
            // Generate a log-normal distribution with a median of 30 minutes.
            var values = d3.range(1000)
                .map(d3.random.logNormal(Math.log(30), .4));

            // dummy domain, needed to get proper ticks for the bins
            props.x.domain([0, 120]);

            // Generate a histogram using twenty uniformly-spaced bins.
            props.data = d3.layout.histogram()
                .bins(props.x.ticks(20))(values);
        }

       // setup input domain and output range
        props.x
            .domain([0, d3.max(props.data, function(d){ return d.x })])
            .range([0, client_width]);
        props.y
            .domain([0, d3.max(props.data, function(d){ return d.y })])
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

        function top(d) {
            return props.y(d.y);
        }

        function left(d, i) {
            return i > 0 ? props.x(props.data[i-1].x) : 0;
        }

        function width(d, i) {
            return props.x(d.x) - left(d, i) - 1;
        }

        // draw chart!
        var svg = selection.append("svg")
            .attr("width", props.width)
            .attr("height", props.height)
            .append("g")
            .attr("transform", "translate("
                  + props.margin.left + ","
                  + props.margin.top + ")");

        var bar = svg.selectAll(".bar")
            .data(props.data)
            .enter().append("g")
            .attr("class", "bar")
            .attr("transform", function(d, i) {
                return "translate("
                    + left(d, i) + ","
                    + top(d) + ")"; });

        bar.append("rect")
            .attr("x", 1)
            .attr("width", function(d, i) { return width(d, i) })
            .attr("height", function(d) {
                return client_height - top(d); });

        /*bar.append("text")
            .attr("dy", ".75em")
            .attr("y", 6)
            .attr("x", function (d, i) { return width(d, i) / 2 })
            .attr("text-anchor", "middle")
            .text(function(d) { return props.format.y(d.y); });*/

        svg.append("g")
            .attr("class", "x axis")
            .attr("transform", "translate(0," + client_height + ")")
            .call(props.axis.x)
            ; /*.selectAll("text")
                .attr("transform", function(d, i) {
                return "translate(0," + (i % 2 ? "12" : "0") + ")" })*/

        svg.append("g")
            .attr("class", "y axis")
            .call(props.axis.y);

        return chart;
    }

    // define property setter/getter function
    // this is made generic, so it could just as well be moved
    // to a utils lib or something...
    function prop(obj, name, ret) {
        return function() {
            if (!arguments.length)
                return obj[name];
            obj[name] = arguments[0];
            return ret;
        };
    }

    // assign properties to chart
    // this function is also generic enough to be hosted else where...
    (function (dst, src){
        var name;
        for (name in src) {
            if (src.hasOwnProperty(name))
                dst[name] = prop(src, name, dst);
        }
    }(chart, props));

    // return our properly propertized chart function object
    return chart;
}
