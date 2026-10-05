-- Copyright 2026 Marc Worrell. Licensed under the Apache License, Version 2.0.
-- v2: Zotonic language chain, then default/en/any fallback. SQL NULL means unbound.
CREATE OR REPLACE FUNCTION z_sparql_translation_v2(
    source jsonb, requested text, default_language text, allow_any boolean
) RETURNS jsonb
LANGUAGE plpgsql IMMUTABLE PARALLEL SAFE SECURITY INVOKER
AS $function$
DECLARE
    translations jsonb;
    language text;
    preferences text[];
BEGIN
    IF source IS NULL OR source = 'null'::jsonb OR requested IS NULL THEN
        RETURN NULL;
    END IF;
    IF jsonb_typeof(source) = 'string' THEN
        RETURN jsonb_build_object('value', source);
    END IF;
    IF source ->> '_type' IS DISTINCT FROM 'trans'
       OR jsonb_typeof(source -> 'tr') IS DISTINCT FROM 'object' THEN
        RETURN NULL;
    END IF;
    translations := source -> 'tr';
    IF allow_any THEN
        preferences := z_sparql_language_chain_v2(requested);
        -- Match z_trans:lookup_fallback/3 for an unknown language code.
        IF cardinality(preferences) = 0 THEN
            RETURN NULL;
        END IF;
        preferences := preferences || ARRAY[lower(default_language), 'en'];
    ELSE
        preferences := ARRAY[lower(requested)];
    END IF;
    FOREACH language IN ARRAY preferences LOOP
        IF jsonb_typeof(translations -> language) = 'string' THEN
            RETURN jsonb_build_object('value', translations -> language, 'language', language);
        END IF;
    END LOOP;
    IF allow_any THEN
        SELECT key INTO language FROM jsonb_each(translations)
        WHERE jsonb_typeof(value) = 'string' ORDER BY key COLLATE "C" LIMIT 1;
        IF language IS NOT NULL THEN
            RETURN jsonb_build_object('value', translations -> language, 'language', language);
        END IF;
    END IF;
    RETURN NULL;
END;
$function$;
