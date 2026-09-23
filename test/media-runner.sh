#!/usr/bin/env bash
# Focused client tests: OTP 22.3, existing dependencies, no site or database.
set -eu
cd "$(dirname "$0")/.."
runner_erl=${ZOTONIC_TEST_ERL:-erl}
runner_erlc="$(dirname "$(command -v "$runner_erl")")/erlc"
runner_version=$("$runner_erl" -noshell -eval '{ok, V} = file:read_file(filename:join([code:root_dir(), "releases", erlang:system_info(otp_release), "OTP_VERSION"])), io:put_chars(V), halt().')
case "$runner_version" in
22.3|22.3.*) ;;
*)
    echo 'Set ZOTONIC_TEST_ERL to the Erlang 22.3 erl executable.' >&2
    exit 1
    ;;
esac
runner_test_dir=$(mktemp -d /tmp/zotonic-runner-tests.XXXXXX)
trap 'rm -rf "$runner_test_dir"' EXIT
export ERL_LIBS="$PWD/deps"
"$runner_erlc" -Werror -DTEST -Dnamespaced_dicts -Drand_only -I include -pa deps/lager/ebin \
    +'{parse_transform,lager_transform}' -o "$runner_test_dir" \
    src/support/z_config.erl src/support/z_media_runner*.erl \
    src/support/z_media_imagemagick.erl src/support/z_exec.erl \
    src/support/z_media_identify.erl src/support/z_media_preview.erl \
    modules/mod_video/mod_video.erl modules/mod_video/support/z_video_convert.erl \
    modules/mod_base/controllers/controller_media_runner_callback.erl \
    src/support/tests/z_media_runner*tests.erl src/support/tests/z_media_preview_tests.erl \
    src/zotonic_sup.erl
"$runner_erl" -noshell -pa ebin deps/*/ebin -pa "$runner_test_dir" \
    -eval 'case eunit:test([z_media_runner_config_tests, z_media_runner_tests, z_media_runner_integration_tests, z_media_preview_tests], [verbose]) of ok -> halt(0); _ -> halt(1) end.'
