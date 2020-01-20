import nrm.sharedlib
import os
import yaml
import json
import subprocess
import shutil
import nrm.messaging

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

    def run_workload(self, workload):
        """ Runs a workload via NRM. The `nrmd` daemon must be running. """
        pass

    def workload_finished(self):
        """ Checks NRM to see whether all tasks are finished. """
        return True

    def workload_recv(self):
        """ Receive a message from NRM's upstream API. """
        pass

    def workload_send(self, message):
        """ Send a message to NRM's upstream API. """
        pass

    def workload_exit_status(self):
        """ Check the workload's exit status. """
        pass


class Local(object):
    def __init__(self):
        self.commonOpts = lib.defaultCommonOpts()

    def start_daemon(self, configuration, outfile="/tmp/nrmd_out"):
        """ start nrmd """
        if self.check_daemon():
            self.stop_daemon()
        subprocess.Popen(
            [
                "daemonize",
                "-o",
                outfile,
                shutil.which("nrmd"),
                "-y",
                yaml.dump(configuration),
            ],
            stderr=subprocess.STDOUT,
            stdin=subprocess.PIPE,
        )
        self.upstreampub = nrm.messaging.UpstreamPubClient(
            lib.pubAddress(self.commonOpts)
        )
        print("connecting")
        self.upstreampub.connect(wait=False)
        print("connected to %s" % lib.pubAddress(self.commonOpts))
        return

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

    def run_workload(self, workloads):
        """ Runs a workload via NRM. The `nrmd` daemon must be running. """
        for w in workloads:
            print(yaml.dump(w["manifest"]))
            lib.run(
                self.commonOpts,
                lib.simpleRun(
                    w["cmd"],
                    w["args"],
                    list(dict(os.environ).items()),
                    yaml.dump(w["manifest"]),
                    w["sliceID"],
                ),
            )

    def workload_finished(self):
        """ Checks NRM to see whether all tasks are finished. """
        return lib.finished(self.commonOpts)

    def workload_recv(self):
        """ Receive a message from NRM's upstream API. """
        print("receiving")
        return self.upstreampub.recv()

    def workload_send(self, message):
        """ Send a message to NRM's upstream API. """
        pass

    def workload_exit_status(self):
        """ Check the workload's exit status. """
        pass
