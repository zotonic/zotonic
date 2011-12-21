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
            zynamo_random,
            zynamo_request,
            zynamo_request_fsm,
            zynamo_request_fsm_sup,
            zynamo_kv,
            
            merkerl,
            spiraltime,
            vclock
        ]},
        {registered, [
            zynamo_sup,
            zynamo_gossip, 
            zynamo_manager,
            zynamo_event,
            zynamo_random,
            zynamo_request_fsm_sup,
            zynamo_kv
        ]},
        {applications, [kernel, stdlib, crypto]},
        {mod, {zynamo, []}}
]}.
