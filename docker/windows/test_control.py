#!/usr/bin/env python3
"""Exercise WSL control flow with fake Docker/Git, without touching containers."""

import json
import os
from pathlib import Path
import subprocess
import tempfile
import unittest


CONTROL = Path(__file__).with_name("control.sh")
MOCK = r'''#!/usr/bin/env python3
import json, os, pathlib, sys
name = pathlib.Path(sys.argv[0]).name
args = sys.argv[1:]
with open(os.environ['TEST_LOG'], 'a') as log:
    log.write(json.dumps([name, args, os.getcwd(), os.environ.get('ZOTONIC_UID'), os.environ.get('ZOTONIC_GID')]) + '\n')
if name == 'id':
    print(os.environ.get('TEST_UID', '1000') if '-u' in args else '1001')
elif name == 'stat':
    print(os.environ.get('TEST_FS', 'ext2/ext3'))
elif name == 'realpath':
    print(os.environ.get('TEST_REALPATH', args[-1]))
elif name == 'git':
    if 'clone' in args:
        dest = pathlib.Path(args[-1])
        (dest / '.git').mkdir(parents=True)
        (dest / 'bin').mkdir()
        (dest / 'bin/zotonic').touch()
        (dest / 'docker-compose.yml').touch()
elif name == 'docker':
    if args[0] == 'version':
        print(os.environ.get('TEST_CLIENT_OS', 'linux'))
    elif args[0] == 'info':
        print('linux')
    elif args[0] == 'inspect':
        print('false')
    elif 'ps' in args:
        print('test-container')
    elif 'exec' in args and os.environ.get('TEST_START_FAILED'):
        sys.exit(1)
    elif 'up' in args:
        sys.exit(int(os.environ.get('TEST_BUILD_EXIT', '0')))
    elif 'down' in args:
        sys.exit(int(os.environ.get('TEST_STOP_EXIT', '0')))
'''


class ControlTests(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory(prefix="zotonic-wsl-")
        self.addCleanup(self.temp.cleanup)
        self.root = Path(self.temp.name).resolve()
        self.home = self.root / "home"
        self.checkout = self.home / "projects/zotonic"
        self.checkout.mkdir(parents=True)
        (self.checkout / "docker-compose.yml").touch()
        self.log = self.root / "calls.jsonl"
        mock_dir = self.root / "mock"
        mock_dir.mkdir()
        for name in ("docker", "git", "id", "stat", "realpath"):
            file = mock_dir / name
            file.write_text(MOCK)
            file.chmod(0o755)
        self.env = dict(os.environ, HOME=str(self.home),
                        PATH=str(mock_dir) + os.pathsep + os.environ["PATH"],
                        TEST_LOG=str(self.log), WSL_DISTRO_NAME="Ubuntu",
                        ZOTONIC_UID="0", ZOTONIC_GID="0")

    def run_control(self, action, checkout=None, **env):
        return subprocess.run(["bash", str(CONTROL), action, str(checkout or self.checkout)],
                              env=dict(self.env, **env), cwd="/tmp",
                              capture_output=True, text=True)

    def calls(self, name="docker"):
        return [row for row in map(json.loads, self.log.read_text().splitlines()) if row[0] == name]

    def test_start_waits_for_container_and_maps_normal_user(self):
        result = self.run_control("start")
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertIn("Zotonic is ready", result.stdout)
        up = next(row for row in self.calls() if "up" in row[1])
        self.assertEqual(up[2:], [str(self.checkout), "1000", "1001"])
        self.assertIn("--build", up[1])
        self.assertIn("--project-name", up[1])
        self.assertTrue(any("http://127.0.0.1:8000/" in row[1] for row in self.calls()))

    def test_build_failure_does_not_report_ready(self):
        result = self.run_control("start", TEST_BUILD_EXIT="17")
        self.assertEqual(result.returncode, 17)
        self.assertNotIn("Zotonic is ready", result.stdout)

    def test_stopped_container_reports_logs_and_failure(self):
        result = self.run_control("start", TEST_START_FAILED="1")
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("stopped during startup", result.stderr)
        self.assertTrue(any("logs" in row[1] for row in self.calls()))

    def test_stop_preserves_volumes_and_propagates_failure(self):
        result = self.run_control("stop", TEST_STOP_EXIT="19")
        self.assertEqual(result.returncode, 19)
        down = next(row[1] for row in self.calls() if "down" in row[1])
        self.assertNotIn("-v", down)
        self.assertNotIn("--volumes", down)
        self.assertIn("--timeout", down)
        self.assertNotIn("Database and checkout files were kept", result.stdout)

    def test_root_and_windows_storage_are_rejected_before_docker(self):
        for env in ({"TEST_UID": "0"}, {"TEST_FS": "9p"}, {"TEST_REALPATH": "/mnt/c/zotonic"}):
            with self.subTest(env=env):
                self.log.write_text("")
                result = self.run_control("start", **env)
                self.assertNotEqual(result.returncode, 0)
                self.assertEqual(self.calls(), [])

    def test_setup_clones_only_a_missing_checkout(self):
        target = self.home / "new-zotonic"
        first = self.run_control("setup", target)
        self.assertEqual(first.returncode, 0, first.stderr)
        marker = target / "local-edit.txt"
        marker.write_text("keep my work")
        second = self.run_control("setup", target)
        self.assertEqual(second.returncode, 0, second.stderr)
        self.assertEqual(marker.read_text(), "keep my work")
        self.assertEqual(sum("clone" in row[1] for row in self.calls("git")), 1)
        self.assertFalse(any("reset" in row[1] or "pull" in row[1] for row in self.calls("git")))

    def test_windows_docker_cli_is_rejected_before_mounting(self):
        result = self.run_control("start", TEST_CLIENT_OS="windows")
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("WSL 2 integration", result.stderr)
        self.assertFalse(any("up" in row[1] for row in self.calls()))

    def test_setup_leaves_other_existing_directory_untouched(self):
        marker = self.checkout / "important.txt"
        marker.write_text("keep")
        result = self.run_control("setup")
        self.assertNotEqual(result.returncode, 0)
        self.assertEqual(marker.read_text(), "keep")
        self.assertEqual(self.calls("git"), [])

    def test_start_and_stop_use_same_isolated_project(self):
        self.assertEqual(self.run_control("start").returncode, 0)
        self.assertEqual(self.run_control("stop").returncode, 0)
        commands = [row[1] for row in self.calls() if "up" in row[1] or "down" in row[1]]
        names = [args[args.index("--project-name") + 1] for args in commands]
        self.assertEqual(names[0], names[1])
        self.assertTrue(names[0].startswith("zotonic-wsl-"))


if __name__ == "__main__":
    unittest.main()
