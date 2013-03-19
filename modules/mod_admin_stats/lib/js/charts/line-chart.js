function line_chart() {
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

        line: d3.svg.line()
            .x(function(d){ return props.x(d.x); })
            .y(function(d){ return props.y(d.y); }),

        // e.g. transition duration, in ms
        animation_speed: 500,
    };

    function chart(selection) {
        chart.update = function(data) {
            var svg = d3.select(this).select("svg g");

            // input domain and output range
            // for now, we only look at all series for y input domain
            // for all else, we only look at the first series.
            props.x
                .domain([Date.now() - 300 * 1000, Date.now()])
                    //d3.min(data[0], function(d){ return d.x }),
                    //d3.max(data[0], function(d){ return d.x })])
                .range([0, client_width]);
            props.y
                .domain([0, 1.01 * d3.max(data, function(s) {
                    return d3.max(s, function(d){
                        return d.y })})])
                .range([client_height, 0]);

            // update axis ticks
            if (props.ticks.x)
                props.ticks.x.apply(chart, [props.axis.x, data[0]]);
            if (props.ticks.y)
                props.ticks.y.apply(chart, [props.axis.y, data[0]]);

            // redraw line!
            svg.selectAll("path.line")
                .transition()
                .duration(props.animation_speed)
                .attr("d", props.line);

            // redraw axis
            svg.select(".x.axis").transition()
                .duration(props.animation_speed)
                .call(props.axis.x);
            svg.select(".y.axis").transition()
                .duration(props.animation_speed)
                .call(props.axis.y);

            return chart;
        }

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
                data = [];
                for (var i = 0; i < 5; i++) {
                    var random = d3.random.normal(5+5*i);
                    data.push(d3.range(50).map(function(j){
                        return { x: Date.now() + 10000 * j, y: random() } }));
                }
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

            // create svg
            var svg = d3.select(this).append("svg")
                .attr("width", props.width)
                .attr("height", props.height)
                .append("g")
                .attr("transform", "translate("
                      + props.margin.left + ","
                      + props.margin.top + ")");

            svg.append("defs").append("svg:clipPath")
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

            // add line series
            svg.append("g")
                .attr("class", "series")
                .attr("clip-path", "url(#clip)")
                .selectAll("path").data(data)
                .enter()
                .append("path")
                .attr("class", function(d, i) { return "line serie-" + i });


            // draw chart & we're done!
            return chart.update.apply(this, [data]);
        });
    }

    chart.call = function(callback) {
        callback.apply(this, Array.prototype.slice.call(arguments, 1));
        return this;
    };

    z_charts.proxy_properties(props, chart);

    return chart;
}

// data -> { init: {x, y}, x: now, y: [values], max_length, series: [list]}
line_chart.append_value = function(data) {
    var d = data.datum;

    if (!d)
        d = data.datum = data.series.map(
            function() { return []; }
        );

    d.forEach(
        function(s, i) {
            if (data.max_length < s.push({x: data.x, y: data.y[this[i]]}))
                s.shift();
        },
        data.series);

    return data;
}
