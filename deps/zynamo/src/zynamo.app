{application, zynamo, [
        {description, "Zotonic simplified Dynamo like hash ring."},
        {vsn, "0.1.0"},
        {modules, [
            zynamo,
            zynamo_event, 
            zynamo_gossip,
            zynamo_manager,
            zynamo_ring,
            zynamo_sup,
            zynamo_hash,
            zynamo_data
        ]},
        {registered, [
            zynamo_sup,
            zynamo_gossip, 
            zynamo_manager,
            zynamo_event
        ]},
        {applications, [kernel, stdlib, crypto]},
        {mod, {zynamo, []}}
]}.
