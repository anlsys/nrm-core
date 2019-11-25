import nrm.sharedlib
import os
import subprocess
import shutil

lib = nrm.sharedlib.UnsafeLib(os.environ["PYNRMSO"])


def _doesntThrow(f, *args, **kwargs):
    try:
        f(*args, **kwargs)
        return True
    except:
        return False


def _exitCodeBool(*args, **kwargs):
    return _doesntThrow(subprocess.check_call, *args, **kwargs)


class Remote(object):
    def __init__(self, target):
        self.target = target

    def start_daemon(self, configuration):
        """ start nrmd """
        if self.check_daemon():
            self.stop_daemon()
        return subprocess.check_call(
            [
                "ssh",
                "%s" % (self.target),
                "/home/cc/.nix-profile/bin/daemonize /home/cc/.nix-profile/bin/nrmd "
                + configuration,
            ],
            stderr=subprocess.STDOUT,
        )

    def check_daemon(self):
        """ checks if nrmd is alive """
        return _exitCodeBool(["ssh", "%s" % self.target, "pgrep nrmd"])

    def stop_daemon(self):
        """ stops nrmd """
        subprocess.check_call(["ssh", "%s" % self.target, "pkill nrmd"])


class Local(object):
    def __init__(self):
        pass

    def start_daemon(self, configuration):
        """ start nrmd """
        if self.check_daemon():
            self.stop_daemon()
        return subprocess.check_call(
            ["daemonize", shutil.which("nrmd"), configuration], stderr=subprocess.STDOUT
        )

    def check_daemon(self):
        """ checks if nrmd is alive """
        return _exitCodeBool(["pgrep", "-f", "nrmd"]) or _exitCodeBool(
            ["pgrep", "nrmd"]
        )

    def stop_daemon(self):
        """ stops nrmd """
        if not (
            _exitCodeBool(["pkill", "-f", "nrmd"]) or _exitCodeBool(["pkill", "nrmd"])
        ):
            raise (Exception)
