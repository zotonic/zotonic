-- Copyright 2026 Marc Worrell. Licensed under the Apache License, Version 2.0.
-- Text overload also accepts varchar. Keep the JSONB implementation authoritative.
CREATE OR REPLACE FUNCTION z_sparql_translation_v2(
    source text, requested text, default_language text, allow_any boolean
) RETURNS jsonb
LANGUAGE sql IMMUTABLE PARALLEL SAFE SECURITY INVOKER
AS $function$
    SELECT z_sparql_translation_v2(to_jsonb(source), requested, default_language, allow_any)
$function$;
