{ hnrm, declInput }: let pkgs = import hnrm {}; in {
  jobsets = pkgs.runCommand "spec.json" {} ''
    cat <<EOF
    ${builtins.toXML declInput}
    EOF
    cat > $out <<EOF
    {
        "master": {
            "enabled": 1,
            "hidden": false,
            "description": "jobsets",
            "nixexprinput": "hnrm",
            "nixexprpath": "release.nix",
            "checkinterval": 300,
            "schedulingshares": 100,
            "enableemail": false,
            "emailoverride": "fre@freux.fr",
            "keepnr": 3,
            "inputs": {
              "hnrm": { "type": "git", "value": "https://xgitlab.cels.anl.gov/argo/hnrm.git", "emailresponsible": false }
            }
        }
    }
    EOF
  '';
}
