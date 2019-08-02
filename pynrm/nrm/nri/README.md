### nri

minimal typed python3.7 sysfs/msr node resource interface. 

goal: to be a high software quality library for machine sensors/actuators for
select platforms.

- [nri/node.py](nri/node.py) interface class 
- [nri/types](nri/types.py) underlying types

this repository is part of the [NRM](https://xgitlab.cels.anl.gov/argo/nrm/)
project, head there for more information.

#### features

sensors

- temperature
- power use

actuators

- frequency
- powercap

configuration readers

- basic node administrative information
- hwloc xml xpath queries
- RAPL configuration

### hacking

python3.7 dev env with code analysis tools:
```
$ nix-shell
```

pass/fail code quality tools are pytest,black,flake8,mypy(no `Any` allowed via
grepping),pytype. run using: 

```
make passfail
```  

qualitative quality tools are pycov, mypy type coverage report. run using : 

```
make qualitative
```  

`make merge-pyi` can merge pytype interface files (be careful).
