import unittest

from nri.node import node


class TestTemp(unittest.TestCase):
    def test_temp(self):
        with node() as n:
            temp = n._coretemp.sample()
            print(temp)
            assert len(temp.core_t_celcius) > 0
            assert len(temp.pkg_t_celcius) > 0
            for i in n.topology.list_core_ids():
                i in temp.core_t_celcius
            for i in n.topology.list_processing_unit_ids():
                i in temp.pkg_t_celcius


if __name__ == "__main__":
    unittest.main()
