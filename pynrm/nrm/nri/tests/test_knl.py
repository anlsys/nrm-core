import unittest

from nri.node import node
from nri.types.constructors import mkPkgID, mkMicroWatts, mkCoreID, mkHz
from nri.types.definitions import Success, PcapControl


class TestKNL(unittest.TestCase):
    def test_rapl_enabled(self):
        with node() as n:
            assert n.read_rapl_config().packageConfig[mkPkgID(0)].enabled

    def test_sample(self):
        with node() as n:
            assert len(n.sample().energySamples) > 0
            assert len(n.sample().tempSamples) > 0

    def test_do_control_pcap(self):
        with node() as n:
            assert (
                n.do_control_pcap(
                    PcapControl({mkPkgID(0): mkMicroWatts(200000000)})
                )
                == Success
            )

    def test_do_control_freq(self):
        with node() as n:
            assert (
                n.do_control_freq(PcapControl({mkCoreID(0): mkHz(200)}))
                == Success
            )


if __name__ == "__main__":
    unittest.main()
