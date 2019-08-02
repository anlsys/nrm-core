import unittest

from nri.node import node


class TestTopo(unittest.TestCase):
    def test_sample(self):
        with node() as n:
            coreids = n.topology.list_core_ids()
            puids = n.topology.list_processing_unit_ids()
            assert len(coreids) > 0
            assert len(puids) > 0
            print(coreids)
            print(puids)


if __name__ == "__main__":
    unittest.main()
