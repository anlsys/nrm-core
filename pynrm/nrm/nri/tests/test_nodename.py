import unittest

from nri.node import node


class TestTopo(unittest.TestCase):
    def test_nodename(self):
        with node() as n:
            print(n.nodename)
            assert len(n.nodename) > 0


if __name__ == "__main__":
    unittest.main()
