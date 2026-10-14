-- Copyright 2026 Marc Worrell. Licensed under the Apache License, Version 2.0.
-- The installer fills this registry from z_language_data, including aliases.
-- Keep it versioned with the translation helper: no request-time table lookup.
CREATE OR REPLACE FUNCTION z_sparql_language_chain_v2(requested text)
RETURNS text[] LANGUAGE sql IMMUTABLE PARALLEL SAFE SECURITY INVOKER
AS $function$
    SELECT ARRAY(
        SELECT jsonb_array_elements_text('__LANGUAGE_CHAINS__'::jsonb -> lower(requested))
    )
$function$;
