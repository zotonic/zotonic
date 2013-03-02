function line_chart() {
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
        x: d3.time.scale(),
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

        // [{x, y}]
        data: null,

        line: d3.svg.line()
            .x(function(d){ return props.x(d.x); })
            .y(function(d){ return props.y(d.y); }),

        // e.g. transition duration, in ms
        animation_speed: 500,
    };

    function chart(selection) {
        root = selection;

        var client_width = props.width
            - props.margin.left
            - props.margin.right;
        var client_height = props.height
            - props.margin.top
            - props.margin.bottom;

        // produce sample graph if no data provided
        if (!props.data)
        {
            var random = d3.random.normal(10);
            props.data = d3.range(50).map(function(i){
                return { x: Date.now() + 10000 * i, y: random() } });
        }

        // setup axis
        props.axis.x
            .scale(props.x)
            .orient("bottom");
            //.tickFormat(props.format.x);
        props.axis.y
            .scale(props.y)
            .orient("left");
            //.tickFormat(props.format.y);

        function top(d) {
            return props.y(d.y);
        }

        function left(d, i) {
            return i > 0 ? props.x(props.data[i-1].x) : 0;
        }

        function width(d, i) {
            return props.x(d.x) - left(d, i) - 1;
        }

        // chart redraw function
        chart.update = function(data) {
            if (data) props.data = data;

            // input domain and output range
            props.x
                .domain([d3.min(props.data, function(d){ return d.x }),
                         d3.max(props.data, function(d){ return d.x })])
                .range([0, client_width]);
            props.y
                .domain([0, d3.max(props.data, function(d){ return d.y })])
                .range([client_height, 0]);

            // update axis ticks
            if (props.ticks.x)
                props.ticks.x.apply(chart, [props.axis.x, props.data]);
            if (props.ticks.y)
                props.ticks.y.apply(chart, [props.axis.y, props.data]);

            // redraw line!
            svg.select(".line path")
                .attr("d", props.line)
                .attr("transform", null);

            // slide line
            svg.select(".line")
                .transition()
                .duration(props.animation_speed);
//                .attr("transform", "translate(" + props.x(0) + ")");

            // redraw axis
            svg.select(".x.axis").transition()
                .duration(props.animation_speed)
                .call(props.axis.x);
            svg.select(".y.axis").transition()
                .duration(props.animation_speed)
                .call(props.axis.y);

            return chart;
        };

        // create svg
        var svg = root.append("svg")
            .attr("width", props.width)
            .attr("height", props.height)
            .append("g")
            .attr("transform", "translate("
                  + props.margin.left + ","
                  + props.margin.top + ")");

        svg.append("defs").append("clippath")
            .attr("id", "clip")
            .append("rect")
            .attr("width", client_width)
            .attr("height", client_height);

        // add axis
        svg.append("g")
            .attr("class", "x axis")
            .attr("transform", "translate(0," + client_height + ")");

        svg.append("g")
            .attr("class", "y axis");

        // add line
        svg.append("g")
            .attr("class", "line")
            .attr("clip-path", "url(#clip)")
            .append("path")
            .data([props.data]);


        // draw chart & we're done!
        return chart.update();
    }

    chart.call = function(callback) {
        callback.apply(this, Array.prototype.slice.call(arguments, 1));
        return this;
    };

    z_charts.proxy_properties(props, chart);

    return chart;
}
