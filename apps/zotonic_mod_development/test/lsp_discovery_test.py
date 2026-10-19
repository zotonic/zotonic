"""Unit tests for opaque command selection; no Docker daemon is required."""
import importlib.machinery
import importlib.util
import json
from pathlib import Path
import unittest
from unittest.mock import patch

ROOT = Path(__file__).resolve().parents[3]
loader = importlib.machinery.SourceFileLoader('zotonic_lsp', str(ROOT / 'bin/zotonic-lsp'))
spec = importlib.util.spec_from_loader(loader.name, loader)
lsp = importlib.util.module_from_spec(spec)
loader.exec_module(lsp)


def container(name, mounts=None):
    return {'Id': name, 'Name': '/' + name, 'State': {'Running': True},
            'Config': {'Labels': {}, 'User': '', 'Env': []},
            'Mounts': mounts if mounts is not None else [
                {'Type': 'bind', 'Source': str(ROOT), 'Destination': '/opt/zotonic'}]}


class DiscoveryTests(unittest.TestCase):
    def test_local_does_not_consult_docker(self):
        with patch.object(lsp, 'probe', return_value=True), patch.object(lsp, 'output') as output:
            self.assertEqual([str(ROOT / 'bin/zotonic'), 'lsp'], lsp.discover(ROOT))
            output.assert_not_called()

    def test_local_override_does_not_fall_back(self):
        with patch.object(lsp, 'probe', return_value=False), patch.object(lsp, 'output') as output:
            with self.assertRaises(lsp.DiscoveryError):
                lsp.discover(ROOT, mode='local')
            output.assert_not_called()

    def test_docker_fallback_maps_bind_mounts(self):
        with patch.object(lsp, 'probe', side_effect=lambda c: c[0] == 'docker'), \
             patch.object(lsp, 'output', side_effect=[b'abc\n', json.dumps([container('abc')]).encode()]):
            command = lsp.discover(ROOT)
        self.assertEqual(['docker', 'exec', '-i', '--user', 'zotonic'], command[:5])
        self.assertEqual(['abc', '/opt/zotonic/bin/zotonic', 'lsp'], command[-3:])
        self.assertIn('HOME=/opt/zotonic', command)
        mapping = next(v.split('=', 1)[1] for v in command if v.startswith('ZOTONIC_LSP_PATH_MAPPINGS='))
        self.assertEqual([{'localRoot': str(ROOT), 'remoteRoot': '/opt/zotonic'}], json.loads(mapping))

    def test_multiple_working_containers_require_choice(self):
        with patch.object(lsp, 'probe', side_effect=lambda c: c[0] == 'docker'), \
             patch.object(lsp, 'output', side_effect=[b'a\nb\n', json.dumps([container('a'), container('b')]).encode()]):
            with self.assertRaisesRegex(lsp.DiscoveryError, 'Multiple Zotonic containers: a, b'):
                lsp.discover(ROOT)

    def test_explicit_container_skips_local_and_listing(self):
        with patch.object(lsp, 'probe', return_value=True) as probe, \
             patch.object(lsp, 'output', return_value=json.dumps([container('chosen')]).encode()) as output:
            lsp.discover(ROOT, container='chosen')
            self.assertEqual(1, probe.call_count)
            output.assert_called_once_with(['docker', 'inspect', '--type', 'container', 'chosen'])

    def test_unrelated_container_is_not_probed(self):
        self.assertIsNone(lsp.container_command(container('database', []), ROOT, 'docker'))
        unrelated = container('other', [{'Type': 'bind', 'Source': str(ROOT) + '-other', 'Destination': '/opt/zotonic'}])
        self.assertIsNone(lsp.container_command(unrelated, ROOT, 'docker'))

    def test_nested_checkout_and_explicit_environment(self):
        info = container('nested', [{'Type': 'bind', 'Source': str(ROOT.parent), 'Destination': '/sources with spaces'}])
        info['Config']['Env'] = ['HOME=/home/developer', 'ZOTONIC_CONFIG_DIR=/config']
        info['Config']['Labels'] = {'org.zotonic.lsp.user': '1001'}
        command = lsp.container_command(info, ROOT, 'docker')
        self.assertIn('HOME=/home/developer', command)
        self.assertIn('ZOTONIC_CONFIG_DIR=/config', command)
        self.assertEqual('/sources with spaces/' + ROOT.name + '/bin/zotonic', command[-2])
        self.assertEqual('1001', command[4])


if __name__ == '__main__':
    unittest.main()
